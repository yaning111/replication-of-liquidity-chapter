setwd("D:/replication of Bali/chapter 13 liquidity/compute illiquidity/")
remove(list = ls())
gc()
library(lmtest)
library(sandwich)
library(tidyverse)
library(lubridate)
library(data.table)
library(stats)
library(PerformanceAnalytics)
library(haven)
library(readxl)
library(broom)
#remotes::install_github("DavisVaughan/slider")
memory.limit(size = 56000)
library(slider)     # for rolling window operations (https://github.com/DavisVaughan/slider)
library(kableExtra)

crsp <- read_dta("D:/replication of Bali/Part I The CRSP Sample in R/crsp daily data incluidng price and volume.dta")
crsp <- filter(crsp,shrcd %in% c(10, 11))
crsp <- crsp %>%
  mutate(ret_excess = ret - rf_ff,
         mkt_ff_excess = mkt_ff - rf_ff) %>%
  select(-c("shrcd","exchcd","siccd"))

crsp <- crsp%>%filter(prc>0&vol>0)
crsp <- mutate(crsp,illi=abs(ret)*1000000/(abs(prc)*(vol)))
#a <- filter(crsp,permno==10001)

#####
illiquidity_computation <- function(x, window, freq) {
  # drop missing values
  x <- na.omit(x)
  
  # determine minimum number of observations depending on window size and data frequency
  if (freq == "monthly") {
    if (window == 12) {check <- 10}
    if (window == 24) {check <- 20}
    if (window == 36) {check <- 24}
    if (window == 60) {check <- 24}
  }
  if (freq == "daily") {
    if (window == 1) {check <- 15}
    if (window == 3) {check <- 50}
    if (window == 6) {check <- 100}
    if (window == 12) {check <- 200}
  }
  
  # check if minimum number of obervations is satisfied
  if (nrow(x) < check) {
    return(tibble(illi = NA))
  } else {
    illi = mean(x$illi)
    return(tibble(illi))
  }
}
###
rolling_illiquidity_computation <- function(data, window, freq = "daily", col_name, ...) {
  # prepare data
  x <- data %>%
    select(date = date_start,  illi) %>%
    arrange(date)
  
  # slide across dates
  illis <- slide_index(x, x$date, ~illiquidity_computation(.x, window, freq ),
                       .before = months(window-1), .complete = FALSE) %>%
    bind_rows()
  
  # collect output
  col_name <- quo_name(col_name)
  mutate(data, !! col_name := illis$illi)
}
####
illi_monthly <- crsp %>%
  group_by(permno)%>%
  group_modify(~rolling_illiquidity_computation(.x, window = 1, freq = "daily", col_name = "illi_1m"))%>%
  group_modify(~rolling_illiquidity_computation(.x, window = 3, freq = "daily", col_name = "illi_3m")) %>%
  group_modify(~rolling_illiquidity_computation(.x, window = 6, freq = "daily", col_name = "illi_6m")) %>%
  group_modify(~rolling_illiquidity_computation(.x, window = 12, freq = "daily", col_name = "illi_12m")) %>%
  distinct(permno, date_start, illi_1m, illi_3m, illi_6m, illi_12m)
illi_monthly <- read_dta("illiquidity.dta")
illi_monthly <- mutate(illi_monthly,illi_1m=illi_1m/100,illi_3m=illi_3m/100,illi_6m=illi_6m/100,illi_12m=illi_12m/100)
write_dta(illi_monthly,"illiquidity.dta")











































