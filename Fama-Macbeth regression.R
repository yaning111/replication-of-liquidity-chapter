###Many thanks for dr. Christoph Scheuch who kindly provide the functions I am using. For more details, please visit https://christophscheuch.github.io/. Based on the functions, 
##I add some codes to preceed the Fama-Macbeth regression controlling for more than one independent variables.

##set footpath
setwd("D:/replication of Bali/chapter 13 liquidity/fama-macbeth regression/")
remove(list = ls())
gc()
##load required packages
library(sandwich)
library(tidyverse)
library(lubridate)
library(data.table)
library(stats)
library(haven)
library(readxl)
library(broom)
library(slider)     # for rolling window operations (https://github.com/DavisVaughan/slider)
library(kableExtra)
memory.limit(size = 56000)

##load CRSP data
crsp <- read_dta("D:/replication of Bali/Part I The CRSP Sample in R/crsp monthly data.dta")
##load illiquidity data
illiqidity <- read_dta("D:/replication of Bali/chapter 13 liquidity/summary statistics/illiquidity.dta")%>%as.data.frame()
colnames(illiqidity) <- tolower(colnames(illiqidity))
##select required variables from dataset
data <- select(crsp,permno,date,date_start,ret_excess_f1,mktcap,ret)
##load beta data which is the coefficient of the CAPM model
beta <- read_dta("D:/replication of Bali/Part II Beta and Stock Returns/betas_daily.dta")
beta <- select(beta,permno,date_start,beta_1m)
##merge data
data <- left_join(data,beta,by=c("permno","date_start"))
##load Momentum data
MOM <- read_dta("D:/replication of Bali/MOM chapter/compute MOM/mom monthly.dta")%>%select(-c(ret,MOM6,date))
#merge data
data <- left_join(data,MOM,by=c("permno","date_start"))
##calculate size
data <- mutate(data,size = log(abs(mktcap)))
##rename variables
data <- rename(data,ret_excess=ret_excess_f1,rev=ret,beta=beta_1m,mom=MOM12)%>%select(-c(mktcap))
df <- illiqidity %>%
  transmute(permno = as.numeric(permno),       
            date_start = ymd(date_start),      
            illi_1m = as.numeric(illi_1m),    
            illi_3m = as.numeric(illi_3m),     
            illi_6m = as.numeric(illi_6m),    
            illi_12m = as.numeric(illi_12m))
##display illiquidity in log-transformation
df <- df%>%
  mutate(lnilli_1m=log(1+illi_1m),lnilli_3m=log(1+illi_3m),lnilli_6m=log(1+illi_6m),lnilli_12m=log(1+illi_12m))

df <- df %>%
  left_join(data, by = c("permno", "date_start"))

df <- df%>%
  mutate(mom=mom*100)


##winsorization at 99.5%
winsorize <- function(x, cut = 0.005){
  cut_point_top <- quantile(x, 1 - cut, na.rm = TRUE)
  cut_point_bottom <- quantile(x, cut, na.rm = TRUE)
  i <- which(x >= cut_point_top) 
  x[i] <- cut_point_top
  j <- which(x <= cut_point_bottom) 
  x[j] <- cut_point_bottom
  return(x)
}

###fama-macbeth regression
fama_macbeth_regression <- function(data, variable, cut = 0.005) {
  variable <- enquo(variable)
  tf <- select(data,permno,date,!!variable,ret_excess,beta,rev,size,mom)%>%
    group_by(permno)%>%  arrange(date)%>%
    mutate(ret_excess=lead(ret_excess,n = 1))%>%na.omit()
  # prepare and winsorize data
  data_nested <- tf  %>%
    group_by(date) %>%
    mutate(ind = winsorize(!!variable, cut = cut)) %>%
    nest() 
  
  # perform cross-sectional regressions for each month including only one independent variable illiquidity
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_excess ~ ind, data = .x))) %>%
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
  out1 <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  
  # perform cross-sectional regressions for each month including independent variables illiquidity and beta
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_excess ~ ind + beta, data = .x))) %>%
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
  out2 <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  
  # perform cross-sectional regressions for each month including independent variables illiquidity and size
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_excess ~ ind+size, data = .x))) %>%
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
  out3 <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  
  # perform cross-sectional regressions for each month including independent variables illiquidity and rev
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_excess ~ ind+rev, data = .x))) %>%
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
  
  # combine desired output and return results including independent variables illiquidity and MOM
  out4 <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_excess ~ ind+mom, data = .x))) %>%
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
  out5 <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  
  # perform cross-sectional regressions for each month including independent variables illiquidity, beta, size, rev, and, MOM
  cross_sectional_regs <- data_nested %>%
    mutate(model = map(data, ~lm(ret_excess ~ ind+beta+size+rev+mom, data = .x))) %>%
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
  out6 <- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  out <- left_join(out6,out1,by= "statistic")%>%
    left_join(out2,by= "statistic")%>%
    left_join(out3,by= "statistic")%>%
    left_join(out4,by= "statistic")%>%
    left_join(out5,by= "statistic")
  ##rename columns
  names(out) <- c("statistic","1","2","3","4","5","6")
  return(out)
}


##results using illi_1m as independent variable
illi_1m_reg <- fama_macbeth_regression(df, illi_1m)
illi_1m_reg[5:6,1] <- c("illi_1m coefficient","illi_1m nw_t_stat")
illi_1m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

##results using illi_3m as independent variable
illi_3m_reg <- fama_macbeth_regression(df, illi_3m)
illi_3m_reg[5:6,1] <- c("illi_3m coefficient","illi_3m nw_t_stat")
##present results
illi_3m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

##results using illi_6m as independent variable
illi_6m_reg <- fama_macbeth_regression(df, illi_6m)
illi_6m_reg[5:6,1] <- c("illi_6m coefficient","illi_6m nw_t_stat")
##present results
illi_6m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

##results using illi_12m as independent variable
illi_12m_reg <- fama_macbeth_regression(df, illi_12m)
illi_12m_reg[5:6,1] <- c("illi_12m coefficient","illi_12m nw_t_stat")
##present results
illi_12m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

##results using lnilli_1m as independent variable
lnilli_1m_reg <- fama_macbeth_regression(df, lnilli_1m)
lnilli_1m_reg[5:6,1] <- c("lnilli_1m coefficient","lnilli_1m nw_t_stat")
##present results
lnilli_1m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

##results using lnilli_3m as independent variable
lnilli_3m_reg <- fama_macbeth_regression(df, lnilli_3m)
lnilli_3m_reg[5:6,1] <- c("lnilli_3m coefficient","lnilli_3m nw_t_stat")
##present results
lnilli_3m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

##results using lnilli_6m as independent variable
lnilli_6m_reg <- fama_macbeth_regression(df, lnilli_6m)
lnilli_6m_reg[5:6,1] <- c("lnilli_6m coefficient","lnilli_6m nw_t_stat")
##present results
lnilli_6m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

##results using lnilli_12m as independent variable
lnilli_12m_reg <- fama_macbeth_regression(df, lnilli_12m) 
lnilli_12m_reg[5:6,1] <- c("lnilli_12m coefficient","lnilli_12m nw_t_stat")
##present results
lnilli_12m_reg%>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 
  

##save results in CSV format
write.csv(illi_1m_reg,"illi_1m_reg.csv",row.names = FALSE)
write.csv(illi_3m_reg,"illi_3m_reg.csv",row.names = FALSE)
write.csv(illi_6m_reg,"illi_6m_reg.csv",row.names = FALSE)
write.csv(illi_12m_reg,"illi_12m_reg.csv",row.names = FALSE)

write.csv(lnilli_1m_reg,"lnilli_1m_reg.csv",row.names = FALSE)
write.csv(lnilli_3m_reg,"lnilli_3m_reg.csv",row.names = FALSE)
write.csv(lnilli_6m_reg,"lnilli_6m_reg.csv",row.names = FALSE)
write.csv(lnilli_12m_reg,"lnilli_12m_reg.csv",row.names = FALSE)





















