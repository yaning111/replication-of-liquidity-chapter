setwd("D:/replication of Bali/chapter 13 liquidity/empirical part/")
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

illiqidity <- read_dta("illiquidity.dta")%>%as.data.frame()
colnames(illiqidity) <- tolower(colnames(illiqidity))
df <- illiqidity %>%
  transmute(permno = as.numeric(permno),     # security identifier
            date_start = ymd(date_start),                # month identifier
            illi_1m = as.numeric(illi_1m),     # return (convert to percent)
            illi_3m = as.numeric(illi_3m),     # shares outstanding (in thousands)
            illi_6m = as.numeric(illi_6m),     # last traded price in a month
            illi_12m = as.numeric(illi_12m))

#df <- na.omit(df)
crsp <- crsp %>%
  mutate(ret_excess = ret_adj - rf_ff,
         mkt_ff_excess = mkt_ff - rf_ff) %>%
  select(permno,date, date_start, ret_excess, mkt_ff_excess)

df <- crsp %>%
  left_join(df, by = c("permno", "date_start"))
df <- df%>%
  mutate(lnilli_1m=log(1+illi_1m),lnilli_3m=log(1+illi_3m),lnilli_6m=log(1+illi_6m),lnilli_12m=log(1+illi_12m))
#df <- na.omit(df)

df_ts <- df %>%
  select(date, illi_1m:lnilli_12m)%>%
  pivot_longer(cols = illi_1m:lnilli_12m, names_to = "measure", values_drop_na = TRUE) %>%
  group_by(measure, date) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            skew = skewness(value),
            kurt = kurtosis(value),
            min = min(value),
            q05 = quantile(value, 0.05),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q95 = quantile(value, 0.95),
            max = max(value),
            n = n()) %>%
  ungroup()

df_ts %>%
  select(-date) %>%
  group_by(measure) %>%
  summarize_all(list(mean)) %>%
  mutate(measure = factor(measure, 
                          levels = c("illi_1m","illi_3m","illi_6m","illi_12m","lnilli_1m","lnilli_3m","lnilli_6m","lnilli_12m"))) %>%
  arrange(measure) %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

cor_matrix <- df %>%
  select(contains("illi")) %>%
  cor(use = "complete.obs", method = "pearson")

cor_matrix[upper.tri(cor_matrix, diag = FALSE)] <- NA

cor_matrix %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

compute_persistence <- function(data, col, tau) {
  dates <- data %>%
    distinct(date) %>%
    arrange(date) %>%
    mutate(date_lag = lag(date, tau))
  
  col <- enquo(col)
  correlation <- data %>%
    select(permno, date, !!col) %>%
    left_join(dates, by = "date") %>%
    left_join(data %>% select(permno, date, !! col),
              by = c("permno", "date_lag"="date")) %>%
    select(-c(permno, date, date_lag)) %>%
    cor(use = "pairwise.complete.obs", method = "pearson")
  
  return(correlation[1, 2])
}
tau <- c(1, 3, 6, 12, 24, 36, 48, 60, 120)
illi_1m <- map_dbl(tau, ~compute_persistence(df, illi_1m, .x))
illi_3m <- map_dbl(tau, ~compute_persistence(df, illi_3m, .x))
illi_6m <- map_dbl(tau, ~compute_persistence(df, illi_6m, .x))
illi_12m <- map_dbl(tau, ~compute_persistence(df, illi_12m, .x))
lnilli_1m <- map_dbl(tau, ~compute_persistence(df, lnilli_1m, .x))
lnilli_3m <- map_dbl(tau, ~compute_persistence(df, lnilli_3m, .x))
lnilli_6m <- map_dbl(tau, ~compute_persistence(df, lnilli_6m, .x))
lnilli_12m <- map_dbl(tau, ~compute_persistence(df, lnilli_12m, .x))
tibble(tau = tau, 
       illi_1m = illi_1m, illi_3m = illi_3m, illi_6m = illi_6m, illi_12m = illi_12m, 
  lnilli_1m = lnilli_1m, lnilli_3m = lnilli_3m, lnilli_6m = lnilli_6m, lnilli_12m = lnilli_12m) %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


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
    mutate(portfolio = case_when(!!var <= q10 ~ 1L,
                                 !!var > q10 & !!var <= q20 ~ 2L,
                                 !!var > q20 & !!var <= q30 ~ 3L,
                                 !!var > q30 & !!var <= q40 ~ 4L,
                                 !!var > q40 & !!var <= q50 ~ 5L,
                                 !!var > q50 & !!var <= q60 ~ 6L,
                                 !!var > q60 & !!var <= q70 ~ 7L,
                                 !!var > q70 & !!var <= q80 ~ 8L,
                                 !!var > q80 & !!var <= q90 ~ 9L,
                                 !!var > q90 ~ 10L))
  
  portfolios_ts <- portfolios %>%
    mutate(portfolio = as.character(portfolio)) %>%
    group_by(portfolio, date) %>%
    summarize(ret_ew = mean(ret_adj_excess_f1, na.rm = TRUE),
              ret_vw = weighted.mean(ret_adj_excess_f1, mktcap, na.rm = TRUE),
              ret_mkt = mean(mkt_ff_excess_f1, na.rm = TRUE)) %>%
    na.omit() %>%
    ungroup()
  
  # 10-1 portfolio
  portfolios_ts_101 <- portfolios_ts %>%
    filter(portfolio %in% c("1", "10")) %>%
    pivot_wider(names_from = portfolio, values_from = c(ret_ew, ret_vw)) %>%
    mutate(ret_ew = ret_ew_10 - ret_ew_1,
           ret_vw = ret_vw_10 - ret_vw_1,
           portfolio = "10-1") %>%
    select(portfolio, date, ret_ew, ret_vw, ret_mkt)
  
  # combine everything
  out <- bind_rows(portfolios_ts, portfolios_ts_101) %>%
    mutate(portfolio = factor(portfolio, levels = c(as.character(seq(1, 10, 1)), "10-1")))
  
  return(out)
}
crsp <- read_dta("D:/replication of Bali/Part I The CRSP Sample in R/crsp monthly data.dta")
illiqidity <- illiqidity%>%
  mutate(lnilli_1m=log(1+illi_1m),lnilli_3m=log(1+illi_3m),lnilli_6m=log(1+illi_6m),lnilli_12m=log(1+illi_12m))
crsp <- crsp %>%
  left_join(illiqidity, by = c("permno", "date_start"))

portfolios_illi_1m <- construct_portfolios(crsp, illi_1m)
portfolios_illi_3m <- construct_portfolios(crsp, illi_3m)
portfolios_illi_6m <- construct_portfolios(crsp, illi_6m)
portfolios_illi_12m <- construct_portfolios(crsp, illi_12m)
portfolios_lnilli_1m <- construct_portfolios(crsp, lnilli_1m)
portfolios_lnilli_3m <- construct_portfolios(crsp, lnilli_3m)
portfolios_lnilli_6m <- construct_portfolios(crsp, lnilli_6m)
portfolios_lnilli_12m <- construct_portfolios(crsp, lnilli_12m)

estimate_portfolio_returns <- function(data, ret) {
  # estimate average returns per portfolio
  average_ret <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>%
    do(model = lm(paste0(ret," ~ 1"), data = .)) %>% 
    mutate(nw_stderror = sqrt(diag(sandwich::NeweyWest(model, lag = 6)))) %>%
    broom::tidy(model) %>%
    ungroup() %>%
    mutate(nw_tstat = estimate / nw_stderror) %>%
    select(estimate, nw_tstat) %>%
    t()
  
  # estimate capm alpha per portfolio
  average_capm_alpha <- data %>%
    group_by(portfolio) %>%
    arrange(date) %>%
    do(model = lm(paste0(ret," ~ 1 + ret_mkt"), data = .)) %>% 
    mutate(nw_stderror = sqrt(diag(sandwich::NeweyWest(model, lag = 6))[1])) %>%
    broom::tidy(model)%>%
    ungroup() %>%
    filter(term == "(Intercept)") %>%
    mutate(nw_tstat = estimate / nw_stderror) %>%
    select(estimate, nw_tstat) %>%
    t()
  
  # construct output table
  out <- rbind(average_ret, average_capm_alpha)
  colnames(out) <-c(as.character(seq(1, 10, 1)), "10-1")
  rownames(out) <- c("Excess Return", "t-Stat" , "CAPM Alpha", "t-Stat")
  
  return(out)
}

rbind(estimate_portfolio_returns(portfolios_illi_1m, "ret_ew"),
      estimate_portfolio_returns(portfolios_illi_3m, "ret_ew"),
      estimate_portfolio_returns(portfolios_illi_6m, "ret_ew"),
      estimate_portfolio_returns(portfolios_illi_12m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_1m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_3m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_6m, "ret_ew"),
      estimate_portfolio_returns(portfolios_lnilli_12m, "ret_ew")) %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  pack_rows("Estimation Period: 1 Month - Equal-Weighted Portfolio Returns", 1, 4) %>%
  pack_rows("Estimation Period: 3 Months - Equal-Weighted Portfolio Returns", 5, 8) %>%
  pack_rows("Estimation Period: 6 Months - Equal-Weighted Portfolio Returns", 9, 12) %>%
  pack_rows("Estimation Period: 12 Months - Equal-Weighted Portfolio Returns", 13, 16) %>%
  pack_rows("Estimation Period: 1 Month (log value) - Equal-Weighted Portfolio Returns", 17, 20) %>%
  pack_rows("Estimation Period: 3 Months (log value) - Equal-Weighted Portfolio Returns", 21, 24) %>%
  pack_rows("Estimation Period: 6 Months (log value) - Equal-Weighted Portfolio Returns", 25, 28) %>%
  pack_rows("Estimation Period: 12 Months (log value)- Equal-Weighted Portfolio Returns", 29, 32) 

rbind(estimate_portfolio_returns(portfolios_illi_1m, "ret_vw"),
      estimate_portfolio_returns(portfolios_illi_3m, "ret_vw"),
      estimate_portfolio_returns(portfolios_illi_6m, "ret_vw"),
      estimate_portfolio_returns(portfolios_illi_12m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_1m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_3m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_6m, "ret_vw"),
      estimate_portfolio_returns(portfolios_lnilli_12m, "ret_vw")) %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  pack_rows("Estimation Period: 1 Month - Value-Weighted Portfolio Returns", 1, 4) %>%
  pack_rows("Estimation Period: 3 Months - Value-Weighted Portfolio Returns", 5, 8) %>%
  pack_rows("Estimation Period: 6 Months - Value-Weighted Portfolio Returns", 9, 12) %>%
  pack_rows("Estimation Period: 12 Months - Value-Weighted Portfolio Returns", 13, 16) %>%
  pack_rows("Estimation Period: 1 Month (log value) - Value-Weighted Portfolio Returns", 17, 20) %>%
  pack_rows("Estimation Period: 3 Months (log value) - Value-Weighted Portfolio Returns", 21, 24) %>%
  pack_rows("Estimation Period: 6 Months (log value) - Value-Weighted Portfolio Returns", 25, 28) %>%
  pack_rows("Estimation Period: 12 Months (log value)- Value-Weighted Portfolio Returns", 29, 32) 
###Regression Analysis
winsorize <- function(x, cut = 0.005){
  cut_point_top <- quantile(x, 1 - cut, na.rm = TRUE)
  cut_point_bottom <- quantile(x, cut, na.rm = TRUE)
  i <- which(x >= cut_point_top) 
  x[i] <- cut_point_top
  j <- which(x <= cut_point_bottom) 
  x[j] <- cut_point_bottom
  return(x)
}

fama_macbeth_regression <- function(data, variable, cut = 0.005) {
  variable <- enquo(variable)
  
  # prepare and winsorize data
  data_nested <- data %>%
    filter(!is.na(ret_adj_f1) & !is.na(!!variable)) %>%
    group_by(date) %>%
    mutate(beta = winsorize(!!variable, cut = cut)) %>%
    nest() 
  
  # perform cross-sectional regressions for each month
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_adj_f1 ~ beta, data = .x))) %>%
    mutate(tidy = map(model, broom::tidy),
           glance = map(model, broom::glance),
           n = map(model, stats::nobs))
  
  # extract average coefficient estimates
  fama_macbeth_coefs <- cross_sectional_regs %>%
    unnest(tidy) %>%
    group_by(term) %>%
    summarize(coefficient = mean(estimate))
  
  # compute newey-west standard errors of average coefficient estimates
  newey_west_std_errors <- cross_sectional_regs %>%
    unnest(tidy) %>%
    group_by(term) %>%
    arrange(date) %>%
    group_modify(~enframe(sqrt(diag(sandwich::NeweyWest(lm(estimate ~ 1, data = .x), lag = 6))))) %>%
    select(term, nw_std_error = value)
  
  # put coefficient estimates and standard errors together and compute t-statistics
  fama_macbeth_coefs <- fama_macbeth_coefs %>%
    left_join(newey_west_std_errors, by = "term") %>%
    mutate(nw_t_stat = coefficient / nw_std_error) %>%
    select(term, coefficient, nw_t_stat) %>%
    pivot_longer(cols = c(coefficient, nw_t_stat), names_to = "statistic") %>%
    mutate(statistic = paste(term, statistic, sep = " ")) %>%
    select(-term)
  
  # extract average r-squared and average number of observations
  fama_macbeth_stats <- cross_sectional_regs %>% 
    unnest(c(glance, n)) %>%
    ungroup() %>%
    summarize(adj_r_squared = mean(adj.r.squared),
              n = mean(n)) %>%
    pivot_longer(c(adj_r_squared, n), names_to = "statistic")
  
  # combine desired output and return results
  out <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  return(out)
}

illi_1m_reg <- fama_macbeth_regression(crsp, illi_1m) %>% rename(illi_1m = value)
illi_3m_reg <- fama_macbeth_regression(crsp, illi_3m) %>% rename(illi_3m = value)
illi_6m_reg <- fama_macbeth_regression(crsp, illi_6m) %>% rename(illi_6m = value)
illi_12m_reg <- fama_macbeth_regression(crsp, illi_12m) %>% rename(illi_12m = value)
lnilli_1m_reg <- fama_macbeth_regression(crsp, lnilli_1m) %>% rename(lnilli_1m = value)
lnilli_3m_reg <- fama_macbeth_regression(crsp, lnilli_3m) %>% rename(lnilli_3m = value)
lnilli_6m_reg <- fama_macbeth_regression(crsp, lnilli_6m) %>% rename(lnilli_6m = value)
lnilli_12m_reg <- fama_macbeth_regression(crsp, lnilli_12m) %>% rename(lnilli_12m = value)

regression_table <- illi_1m_reg %>%
  left_join(illi_3m_reg, by = "statistic") %>%
  left_join(illi_6m_reg, by = "statistic") %>%
  left_join(illi_12m_reg, by = "statistic") %>%
  left_join(lnilli_1m_reg, by = "statistic") %>%
  left_join(lnilli_3m_reg, by = "statistic") %>%
  left_join(lnilli_6m_reg, by = "statistic") %>%
  left_join(lnilli_12m_reg, by = "statistic") 

regression_table$statistic <- c("intercept", "t-stat", "beta", "t-stat", "adj. R-squared", "n")
regression_table %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


