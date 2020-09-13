setwd("F:/replication of Bali/chapter 13 liquidity/portfolios analysis/")
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
crsp <- read_dta("D:/replication of Bali/Part I The CRSP Sample in R/crsp monthly data.dta")

illiqidity <- read_dta("D:/replication of Bali/chapter 13 liquidity/empirical part/illiquidity.dta")%>%as.data.frame()
colnames(illiqidity) <- tolower(colnames(illiqidity))

df <- illiqidity %>%
  transmute(permno = as.numeric(permno),     # security identifier
            date_start = ymd(date_start),                # month identifier
            illi_1m = as.numeric(illi_1m),     # return (convert to percent)
            illi_3m = as.numeric(illi_3m),     # shares outstanding (in thousands)
            illi_6m = as.numeric(illi_6m),     # last traded price in a month
            illi_12m = as.numeric(illi_12m))
df <- df%>%
  mutate(lnilli_1m=log(1+illi_1m),lnilli_3m=log(1+illi_3m),lnilli_6m=log(1+illi_6m),lnilli_12m=log(1+illi_12m))

df <- df %>%
  left_join(crsp, by = c("permno", "date_start"))

#FF <- read_dta("FF factors.dta")

construct_portfolios <- function(data, var) {
  var <- enquo(var)
  
  # keep only observations where the sorting variable is defined
  data <- data %>%
    filter(!is.na(!!var))
  
  # calculate quantiles
  percentiles <- seq(0.1, 0.9, 0.1)
  percentiles_names <- map_chr(percentiles, ~paste0("q", .x*100))
  percentiles_funs <- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = percentiles_names)
  
  quantiles <- data %>%
    group_by(date) %>%
    summarize_at(vars(!!var), lst(!!!percentiles_funs)) 
  
  # sort all stocks into decile portfolios
  portfolios <- data %>%
    left_join(quantiles, by = "date") %>%
    mutate(portfolio = case_when(illi_1m <= q10 ~ 1L,
                                 illi_1m > q10 & illi_1m <= q20 ~ 2L,
                                 illi_1m > q20 & illi_1m <= q30 ~ 3L,
                                 illi_1m > q30 & illi_1m <= q40 ~ 4L,
                                 illi_1m > q40 & illi_1m <= q50 ~ 5L,
                                 illi_1m > q50 & illi_1m <= q60 ~ 6L,
                                 illi_1m > q60 & illi_1m <= q70 ~ 7L,
                                 illi_1m > q70 & illi_1m <= q80 ~ 8L,
                                 illi_1m > q80 & illi_1m <= q90 ~ 9L,
                                 illi_1m > q90 ~ 10L))
  
  portfolios_ts <- portfolios %>%
    mutate(portfolio = as.character(portfolio)) %>%
    group_by(portfolio, date) %>%
    summarize(ret_ew_adj = mean(ret_adj_excess_f1, na.rm = TRUE),
              ret_vw_adj = weighted.mean(ret_adj_excess_f1, mktcap, na.rm = TRUE),
              ret_mkt = mean(mkt_ff_excess_f1, na.rm = TRUE),
              mkt_ff = mean(mkt_ff, na.rm = TRUE),
              smb_ff = mean(smb_ff, na.rm = TRUE),
              hml_ff = mean(hml_ff, na.rm = TRUE),
              mom = mean(mom, na.rm = TRUE),
              ltrev = mean(ltrev, na.rm = TRUE),
              strev = mean(strev, na.rm = TRUE),
              ret_ew = mean(ret_excess_f1, na.rm = TRUE),
              ret_vw = weighted.mean(ret_excess_f1, mktcap, na.rm = TRUE)) %>%
    na.omit() %>%ungroup()
  
  portfolios_ts <- portfolios_ts%>%group_by(portfolio)%>%arrange(date)%>%
     mutate(ret_ew=lead(ret_ew,n = 1),ret_vw = lead(ret_vw,n = 1),ret_ew_adj=lead(ret_ew_adj,n = 1),ret_vw_adj = lead(ret_vw_adj,n = 1))%>%
    na.omit()%>%ungroup()
  
  
  # 10-1 portfolio undelisted adjust
  portfolios_ts_101_unadj<- portfolios_ts %>%
    select(-c(ret_ew_adj,ret_vw_adj))%>%
    filter(portfolio %in% c("1", "10")) %>%
    pivot_wider(names_from = portfolio, values_from = c(ret_ew, ret_vw)) %>%
    mutate(ret_ew = ret_ew_10 - ret_ew_1,
           ret_vw = ret_vw_10 - ret_vw_1,
           portfolio = "10-1") %>%
    select(portfolio, date, ret_ew, ret_vw, ret_mkt, mkt_ff,
           smb_ff, hml_ff, mom, ltrev, strev)
  
  # 10-1 portfolio delisted adjust
  portfolios_ts_101_adj<- portfolios_ts %>%
    select(-c(ret_ew,ret_vw))%>%
    filter(portfolio %in% c("1", "10")) %>%
    pivot_wider(names_from = portfolio, values_from = c(ret_ew_adj, ret_vw_adj)) %>%
    mutate(ret_ew_adj = ret_ew_adj_10 - ret_ew_adj_1,
           ret_vw_adj = ret_vw_adj_10 - ret_vw_adj_1,
           portfolio = "10-1") %>%
    select(portfolio, date, ret_ew_adj, ret_vw_adj, ret_mkt, mkt_ff,
           smb_ff, hml_ff, mom, ltrev, strev)
  # combine everything
  out <- bind_rows(portfolios_ts, portfolios_ts_101_unadj,portfolios_ts_101_adj) %>%
    mutate(portfolio = factor(portfolio, levels = c(as.character(seq(1, 10, 1)), "10-1")))
  return(out)
}

estimate_portfolio_returns <- function(data, ret) {
  #ret <- "ret_ew"
  # estimate average returns per portfolio
  average_ret <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>% nest()%>%
    mutate(model = map(data, ~lm(paste0(ret," ~ 1 "), data = .)))%>%
    mutate(nw_stderror = as.numeric(map(data, ~sqrt(diag(sandwich::NeweyWest(lm(paste0(ret," ~ 1 "), data = .), lag = 6))))))%>%
    mutate(estimate=map(data, ~tidy(lm(paste0(ret," ~ 1 "), data = .))$estimate)) %>%
    select(portfolio,nw_stderror,estimate)%>%
    ungroup() %>%
    mutate(nw_tstat = as.numeric(estimate)/nw_stderror) %>%
    mutate(estimate=as.numeric(estimate))%>%arrange(portfolio)%>%
    select(estimate, nw_tstat) %>%ungroup()%>%
    t()
  
  # estimate capm alpha per portfolio
  average_capm_alpha <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>% nest()%>%
    mutate(model = map(data, ~lm(paste0(ret," ~ 1 + ret_mkt "), data = .)))%>%
    mutate(nw_stderror = as.numeric(map(data, ~sqrt(diag(sandwich::NeweyWest(lm(paste0(ret," ~ 1 + ret_mkt "), data = .), lag = 6))[1]))))%>%
    mutate(estimate=map(data,~tidy(coef(lm(paste0(ret," ~ 1 + ret_mkt "), data = .)))[1,2]))%>%
    select(portfolio,nw_stderror,estimate)%>%
    unnest() %>%
    mutate(nw_tstat = as.numeric(x)/nw_stderror) %>%
    mutate(estimate=as.numeric(x))%>%arrange(portfolio)%>%ungroup()%>%
    select(estimate, nw_tstat) %>%
    t()
  # estimate FF 3 factors alpha per portfolio

  
  average_FF3_alpha <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>% nest()%>%
    mutate(model = map(data, ~lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff "), data = .)))%>%
    mutate(nw_stderror = as.numeric(map(data, ~sqrt(diag(sandwich::NeweyWest(lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff "), data = .), lag = 6))[1]))))%>%
    mutate(estimate=map(data,~tidy(coef(lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff"), data = .)))[1,2]))%>%
    select(portfolio,nw_stderror,estimate)%>%
    unnest() %>%
    mutate(nw_tstat = as.numeric(x)/nw_stderror) %>%
    mutate(estimate=as.numeric(x))%>%arrange(portfolio)%>%ungroup()%>%
    select(estimate, nw_tstat) %>%
    t()
  # estimate FF 4 factors alpha per portfolio
  average_FF4_alpha <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>% nest()%>%
    mutate(model = map(data, ~lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff + mom"), data = .)))%>%
    mutate(nw_stderror = as.numeric(map(data, ~sqrt(diag(sandwich::NeweyWest(lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff + mom"), data = .), lag = 6))[1]))))%>%
    mutate(estimate=map(data,~tidy(coef(lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff +mom"), data = .)))[1,2]))%>%
    select(portfolio,nw_stderror,estimate)%>%
    unnest() %>%
    mutate(nw_tstat = as.numeric(x)/nw_stderror) %>%
    mutate(estimate=as.numeric(x))%>%arrange(portfolio)%>%ungroup()%>%
    select(estimate, nw_tstat) %>%
    t()
  # estimate FF 5 factors alpha per portfolio
  average_FF5_alpha <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>% nest()%>%
    mutate(model = map(data, ~lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff + mom +strev"), data = .)))%>%
    mutate(nw_stderror = as.numeric(map(data, ~sqrt(diag(sandwich::NeweyWest(lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff + mom +strev"), data = .), lag = 6))[1]))))%>%
    mutate(estimate=map(data,~tidy(coef(lm(paste0(ret," ~ 1 + mkt_ff + smb_ff + hml_ff +mom + strev"), data = .)))[1,2]))%>%
    select(portfolio,nw_stderror,estimate)%>%
    unnest() %>%
    mutate(nw_tstat = as.numeric(x)/nw_stderror) %>%
    mutate(estimate=as.numeric(x))%>%arrange(portfolio)%>%ungroup()%>%
    select(estimate, nw_tstat) %>%
    t()
  
  # construct output table
  out <- rbind(average_ret, average_capm_alpha,average_FF3_alpha,average_FF4_alpha,average_FF5_alpha)%>%as.data.frame()
  colnames(out) <-c(as.character(seq(1, 10, 1)), "10-1")
  row.names(out) <- c("Excess Return", "t-Stat1" , "CAPM Alpha", "t-Stat2","FF3 Alpha", "t-Stat3","FF4 Alpha", "t-Stat4", "FF5 Alpha","t-Stat5")
  
  return(out)
}
portfolios_illi_1m <- construct_portfolios(df, illi_1m)
portfolios_illi_3m <- construct_portfolios(df, illi_3m)
portfolios_illi_6m <- construct_portfolios(df, illi_6m)
portfolios_illi_12m <- construct_portfolios(df, illi_12m)
portfolios_lnilli_1m <- construct_portfolios(df, lnilli_1m)
portfolios_lnilli_3m <- construct_portfolios(df, lnilli_3m)
portfolios_lnilli_6m <- construct_portfolios(df, lnilli_6m)
portfolios_lnilli_12m <- construct_portfolios(df, lnilli_12m)

a <- rbind(estimate_portfolio_returns(portfolios_illi_1m, "ret_ew"),
      estimate_portfolio_returns(portfolios_illi_3m, "ret_ew"),
      estimate_portfolio_returns(portfolios_illi_6m, "ret_ew"),
      estimate_portfolio_returns(portfolios_illi_12m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_1m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_3m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_6m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_12m, "ret_ew"))

b <- rbind(estimate_portfolio_returns(portfolios_illi_1m, "ret_vw"),
      estimate_portfolio_returns(portfolios_illi_3m, "ret_vw"),
      estimate_portfolio_returns(portfolios_illi_6m, "ret_vw"),
      estimate_portfolio_returns(portfolios_illi_12m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_1m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_3m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_6m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_12m, "ret_vw"))

c <- rbind(estimate_portfolio_returns(portfolios_illi_1m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_illi_3m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_illi_6m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_illi_12m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_lnilli_1m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_lnilli_3m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_lnilli_6m, "ret_ew_adj"),
      estimate_portfolio_returns(portfolios_lnilli_12m, "ret_ew_adj"))

d <- rbind(estimate_portfolio_returns(portfolios_illi_1m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_illi_3m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_illi_6m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_illi_12m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_lnilli_1m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_lnilli_3m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_lnilli_6m, "ret_vw_adj"),
      estimate_portfolio_returns(portfolios_lnilli_12m, "ret_vw_adj"))


















