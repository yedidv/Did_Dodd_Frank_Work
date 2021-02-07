setwd("~/Desktop/econometrics_final_project")
library(tidyverse) 
library(readxl) 
library(lubridate) 
library(fGarch)
library(rugarch) 
library(dplyr)
library(plm) 

Arch_1 <- function(log_returns, ticker_names_unique) {
  ## Create the Arch(1) table 
  
  arch_1 <- tibble(
    Date = log_returns$Date
  )
  
  archspecs <- ugarchspec(mean.model = list(armaOrder = c(1,0)))
  for(tic in ticker_names_unique){
    
    volatility <- ugarchfit(spec = archspecs, data = log_returns[[tic]]) 
    arch_1[[tic]] <- volatility@fit$sigma
    
  }
  return(arch_1) 
  
}


Garch_1_1 <- function(log_returns, ticker_names_unique){
  ## Create the Garch(1,1) table 
  
  garch_1_1 <- tibble(
    date = log_returns$Date
  )
  
  for (tic in ticker_names_unique){
    garch_1_1[[tic]] <- garchFit(formula = ~garch(1,1), 
                                 data = log_returns[[tic]])@sigma.t
  }
  
  return(garch_1_1) 
}


ArchGarch <- function(log_returns, ticker_names_unique){
  ## Create the AR + GARCH model 
  
  archgarch_1_1 <- tibble(
    Date = log_returns$Date
  )
  
  archgarchspecs <- ugarchspec(
    variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), 
    mean.model = list(armaOrder = c(1,0), archm = T, archpow = 2), 
    distribution.model = 'std')
  
  for (tic in ticker_names_unique){
    archgarchfit <- ugarchfit(spec = archgarchspecs, 
                              data = log_returns[[tic]])
    
    archgarch_1_1[[tic]] <- archgarchfit@fit$sigma
  }
  
  return(archgarch_1_1) 
  
}




Format <- function(volatility) {
  ## Format the data 
  ## Annualize the volatility 
  ## average volatility for each quarter. 
  
  a <- volatility %>% 
    group_by(Quarter = quarter(ymd(Date), with_year = T)) %>% 
    summarize_at(vars(-Date), mean)
  
  return(a) 
  
}





##Read in the banks data 
bank_returns <- read_excel('Returns.xlsx', sheet = 'Banks_Data') %>%
  ## We want to see all the distinct tickers for each year, 
  ## along with their banks mean assets. 
  distinct(ticker, bank_meanAssets) %>%
  
  ## Create a dummy variable that outputs 1 if the bank is Large Cap 
  ## (Greater than 50Billion) and 0 if it is regional 
  ## (Greater than 10Billion, Less than 50Billion) 
  group_by(LargeBankDummy = ifelse(bank_meanAssets >= 50, 1, 0) ) %>% 
  
  ## Keep just the ticker and the dummy variable
  select(ticker, LargeBankDummy) %>% 
  
  ## Remove duplicates
  distinct(ticker)

## Read in the log returns 
log_returns <- read_excel('Returns.xlsx', 
                          sheet = 'No_Nulls_Log_Returns')[-c(1,2), ] %>% 
  
  ##Convert to date format
  mutate(Date = ymd(as.character((as.Date(as.numeric(Date), 
                                          origin = '1899-12-30') )))) %>% 
  
  ## We are trying to find monthly log returns given
  ## daily log returns. In order to do this, we just need to 
  ## find the sum of the log returns within each month. 
  mutate_at(vars(-Date), as.numeric)

## Select Randomly 3 models to find the order of the test. 
## WFC, AIG, FBP were arbitrarily chosen to determine the order 
## for this test. 
acf(log_returns$FBP, main = 'FBP ACF') 
acf((log_returns$FBP) ^2, main = 'FBP^2 ACF')
pacf(log_returns$FBP, main = 'FBP PACF') 
pacf((log_returns$FBP)^2, main = 'FBP^2 PACF')

## The FBP determines a (1,2) model 

acf(log_returns$AIG, main = 'AIG ACF') 
acf((log_returns$AIG)^2, main = 'AIG^2 ACF') 
pacf(log_returns$AIG, main = 'AIG PACF') 
pacf((log_returns$AIG)^2, main = 'AIG^2 PACF') 

## AIG determines a (1,2) model 

acf(log_returns$FULT, main = 'FULT ACF') 
acf((log_returns$FULT)^2, main = 'FULT^2 ACF') 
pacf(log_returns$FULT, main = 'FULT PACF') 
pacf((log_returns$FULT)^2, main = 'FULT^2 PACF') 

## FULT determines a (1,1) model

## We are going to generate an ARCH(1) model, 
## A Garch(1,2) model, 
## And a AR(1) + Garch(1,1) model 
ticker_names_unique <- unique(names(log_returns[-c(1)]))




## Create the Arch(1) table
arch1 <- Arch_1(log_returns, ticker_names_unique)

## Create the Garch(1,1) table 
garch11 <- Garch_1_1(log_returns, ticker_names_unique) %>% 
  rename(Date = date) 

garch11 %>% group_by(year = year(ymd(Date))) 
  


## Create a Arch(1) + Garch(1,1) 
archgarch11 <- ArchGarch(log_returns, ticker_names_unique)




saveRDS(arch1, file = 'arch1.rds') 
saveRDS(garch11, file = 'garch11.rds') 
saveRDS(archgarch11, file = 'archgarch11.rds') 



arch_1 <- Format(arch1)
garch_1_1 <- Format(garch11)
archgarch_1_1 <- Format(archgarch11) 




Panel <- function(volatility_model, ticker_names_unique, bank_returns){
  
  
  
  
  
  panel <- tibble(Quarter = numeric(), 
                  Bank = character(), 
                  volatility = numeric(), 
                  LargeDummy = numeric())
  
  
  for (tic in ticker_names_unique){
    
    large_bank_dummy <- subset.data.frame(bank_returns, ticker == tic) %>% 
      pull(LargeBankDummy) 
    
    subset_df <- volatility_model[,c(tic, 'Quarter')] %>% 
      add_column(LargeDummy = large_bank_dummy)
    
    panel <- panel %>% 
      add_row(
        Quarter = subset_df$Quarter, 
        volatility = subset_df[[tic]], 
        LargeDummy = subset_df$LargeDummy, 
        Bank = tic
      )
    
  }
  return(panel %>% mutate(date = yq(Quarter)) %>% 
           select(-Quarter))
}

## Panel data 
arch_1_panel <- Panel(arch_1, ticker_names_unique, bank_returns)

arch_panel <- pdata.frame(arch_1_panel, index = c('date', 'Bank'))
pdim(arch_panel) 


summary(plm(volatility ~ LargeDummy, 
    data = arch_panel, model = 'within') )




glimpse(arch_1_panel)

