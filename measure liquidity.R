Many thanks for the R code provided by Dr. Christoph Scheuch (web: https://christophscheuch.github.io/). 
I employ the functions he defined and make some changes.

##set the footpath
setwd("D:/replication of Bali/chapter 13 liquidity/compute illiquidity/")
remove(list = ls())
gc()
library(sandwich)
library(tidyverse)
library(lubridate)
library(data.table)
library(stats)
library(haven)
library(readxl)
library(broom)
#remotes::install_github("DavisVaughan/slider")
memory.limit(size = 56000)
library(slider)     # for rolling window operations (https://github.com/DavisVaughan/slider)
library(kableExtra)
##read crsp monthly data
crsp <- read_dta("D:/replication of Bali/Part I The CRSP Sample in R/crsp daily data incluidng price and volume.dta")
##filter by share code
crsp <- filter(crsp,shrcd %in% c(10, 11))
##compute excess return and equity premium
crsp <- crsp %>%
  mutate(ret_excess = ret - rf_ff,
         mkt_ff_excess = mkt_ff - rf_ff) %>%
  select(-c("shrcd","exchcd","siccd"))
##keep only prc and vol are positive
crsp <- crsp%>%filter(prc>0&vol>0)
##compute daily Amihud illiquidity
crsp <- mutate(crsp,illi=abs(ret)*1000000/(abs(prc)*(vol)))


#####define function to compute illiquidity
illiquidity_computation <- function(x, window, freq) {
  # drop missing values
  x <- na.omit(x)
  
  # determine minimum number of observations depending on window size and data frequency
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
###define rolling-window function to calculate illiquidity for each stock
rolling_illiquidity_computation <- function(data, window, freq = "daily", col_name, ...) {
  # prepare data
  x <- data %>%
    select(date = date_start,  illi) %>%
    arrange(date)
  
  # slide across dates (rolling window)
  illis <- slide_index(x, x$date, ~illiquidity_computation(.x, window, freq ),
                       .before = months(window-1), .complete = FALSE) %>%
    bind_rows()
  
  # collect output
  col_name <- quo_name(col_name)
  mutate(data, !! col_name := illis$illi)
}
####apply above defined functions
illi_monthly <- crsp %>%
  group_by(permno)%>%
  group_modify(~rolling_illiquidity_computation(.x, window = 1, freq = "daily", col_name = "illi_1m"))%>%
  group_modify(~rolling_illiquidity_computation(.x, window = 3, freq = "daily", col_name = "illi_3m")) %>%
  group_modify(~rolling_illiquidity_computation(.x, window = 6, freq = "daily", col_name = "illi_6m")) %>%
  group_modify(~rolling_illiquidity_computation(.x, window = 12, freq = "daily", col_name = "illi_12m")) %>%
  distinct(permno, date_start, illi_1m, illi_3m, illi_6m, illi_12m)
##export the data in stata format
write_dta(illi_monthly,"illiquidity.dta")











































