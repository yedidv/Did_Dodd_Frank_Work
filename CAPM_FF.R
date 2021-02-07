setwd("~/Desktop/econometrics_final_project")
library(tidyverse) 
library(readxl) 
library(lubridate) 
library(writexl)  
library(ggplot2) 
library(dplyr) 



##Read in the banks data 
banks_returns <- read_excel('Returns.xlsx', sheet = 'Banks_Data') %>%
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
  mutate_at(vars(-Date), as.numeric) %>% 
  mutate(YM = format(Date, '%Y-%m')) %>% 
  group_by(YM) %>% 
  summarize_at(vars(-Date), sum) 
  



## Read in the Fama French Factors 
fama_french <- read_excel('fama_factors.xlsx') %>% 
  ## Convert to numeric
  mutate_all(as.numeric) %>% 
  
  ## Extract year
  mutate(year = year(ym(Date))) %>% 
  
  ##Create a Dodd Frank Variable that outputs 1 if 2011 or later
  ## and 0 if earlier than 2011.
  group_by(DoddFrankDummy = ifelse(year >= 2011, 1, 0) ) %>%


  ## Convert to date format for joining 
  mutate(YM = format(ym(Date), '%Y-%m')) %>% 
  
  
  ## Convert the Fama French Factors from percent to decimal
  mutate_at(vars(`Mkt-RF`, SMB, HML, RF), 
            function(x){return(x/100) }) %>% 
  
  ## Rename the risk free column
  rename(risk_free = RF, 
         mkt_rf = `Mkt-RF`, 
         smb = SMB, 
         hml = HML) %>% 
  
  
  
  ## inner join the log returns by date. this ensures that the fama factors 
  ## only appear on dates where there are log returns (no holidays, etc) 
  inner_join(log_returns, by = 'YM')
  
 
  
  


names(banks_returns)




## Create empty dataframes for the CAPM, CAPM with dummies 
## Along with the Fama French Factors, Fama French Factors with dummies 
panel <- tibble(
  Name = character(), 
  CAPM_Intercept = numeric(), 
  CAPM_Intercept_Dummies = numeric(), 
  FF_Intercept = numeric(), 
  FF_Intercept_Dummies = numeric(), 
  CAPM_Excess_Rets = numeric(), 
  CAPM_Excess_Rets_Dummies = numeric(), 
  FF_Excess_Rets = numeric(), 
  FF_Excess_Rets_Dummies = numeric(), 
  CAPM_Market_Rets = numeric(), 
  CAPM_Market_Rets_Dummies = numeric(), 
  FF_Market_Rets = numeric(), 
  FF_Market_Rets_Dummies = numeric(), 
  SMB = numeric(), 
  SMB_Dummies = numeric(), 
  HML = numeric(), 
  HML_Dummies = numeric(), 
  CAPM_LargeDodd_Dummies = numeric(), 
  FF_LargeDodd_Dummies = numeric(), 
  CAPM_Large_Dummies = numeric(), 
  FF_Large_Dummies = numeric(), 
  CAPM_Dodd_Dummies = numeric(), 
  FF_Dodd_Dummies = numeric(),
  IsLargeBanks = numeric()
  
  
) 

panel_p <- tibble( ## An empty dataframe with 
  ## the p values for the regression 
  Name = character(), 
  CAPM_Intercept = numeric(), 
  CAPM_Intercept_Dummies = numeric(), 
  FF_Intercept = numeric(), 
  FF_Intercept_Dummies = numeric(), 
  CAPM_Market_Rets = numeric(), 
  CAPM_Market_Rets_Dummies = numeric(), 
  FF_Market_Rets = numeric(), 
  FF_Market_Rets_Dummies = numeric(),
  CAPM_LargeDodd_Dummies = numeric(), 
  FF_LargeDodd_Dummies = numeric(), 
  IsLargeBank = numeric()
  
  
)
  

## All the unique ticker names 
ticker_names_unique <- unique(names(log_returns[-c(1)]))

for (tic in ticker_names_unique){
  
  
  ## Iterate through the tickers to append the dataframe 
  ## Subset the dataframe and return 1 if the ticker is a large bank, 
  ## 0 if it is a small bank
  large_bank_dummy <-subset.data.frame(banks_returns, ticker == tic) %>% pull(LargeBankDummy)
  
  ## only keep the columns for the fama factors, the dummy variables, and the ticker
  subset_ticker <- fama_french[,c(tic, 'mkt_rf', 'smb', 'hml', 
                                   'risk_free', 'DoddFrankDummy')] %>%
    add_column(LargeDummy = large_bank_dummy) 
  
  
  ## Determine the dummy Dit * Tit
  subset_ticker$LargeDoddDummy <- large_bank_dummy * subset_ticker$DoddFrankDummy
   
  
  subset_ticker$ticker <- subset_ticker[[tic]]
  
  
  ## Find the excess returns 
  subset_ticker$Excess_Rets <- subset_ticker$ticker - subset_ticker$risk_free 
  
  ## Total excess returns for the time period 
  Excess_Returns = last(subset_ticker$Excess_Rets) - first(subset_ticker$Excess_Rets) 
  
  
  ## Regression capm no dummies 
  capm <- as.numeric(
    lm(Excess_Rets ~ mkt_rf, data = subset_ticker)[['coefficients']]
  )
  
  
  
  ## Regression with the capm with dummies 
  capm_dummies <- as.numeric(
    lm(Excess_Rets ~ mkt_rf + LargeDoddDummy + LargeDummy + DoddFrankDummy, 
       data = subset_ticker)[['coefficients']]
  )
  
  
  
  ## Regression with the fama french factors 
  ff <- as.numeric(
    lm(Excess_Rets ~ mkt_rf + smb + hml, 
       data = subset_ticker)[['coefficients']]
  )
  

  
  
  ## Regression Fama French Factors dummies 
  ff_dummies <- as.numeric(
    lm(Excess_Rets ~ mkt_rf+ smb + hml + LargeDoddDummy + LargeDummy + DoddFrankDummy, 
       data = subset_ticker)[['coefficients']] 
  )
  


  ## Append the panel data- add the coefficients for all the regressions 
  panel <- panel %>% 
    add_row(
      
      Name = tic, 
      CAPM_Intercept = capm[1], 
      CAPM_Intercept_Dummies = capm_dummies[1], 
      FF_Intercept = ff[1], 
      FF_Intercept_Dummies = ff_dummies[1], 
      CAPM_Excess_Rets =Excess_Returns, 
      CAPM_Excess_Rets_Dummies = Excess_Returns, 
      FF_Excess_Rets = Excess_Returns, 
      FF_Excess_Rets_Dummies = Excess_Returns, 
      CAPM_Market_Rets = capm[2], 
      CAPM_Market_Rets_Dummies = capm_dummies[2], 
      FF_Market_Rets = ff[2], 
      FF_Market_Rets_Dummies = ff_dummies[2], 
      SMB = ff[3], 
      SMB_Dummies = ff_dummies[3], 
      HML = ff[4], 
      HML_Dummies = ff_dummies[4], 
      CAPM_LargeDodd_Dummies = capm_dummies[3], 
      FF_LargeDodd_Dummies = ff_dummies[5], 
      IsLargeBanks = large_bank_dummy, 
      CAPM_Large_Dummies = capm_dummies[4], 
      FF_Large_Dummies = ff_dummies[6], 
      CAPM_Dodd_Dummies = capm_dummies[5], 
      FF_Dodd_Dummies = ff_dummies[7], 
    )
  
  
  ## Find the p values for all the regressions
  ff_dummy_intercept_p <- as.numeric(summary(lm(
    Excess_Rets ~ mkt_rf + smb + hml + LargeDoddDummy+
    LargeDummy + DoddFrankDummy, 
    data = subset_ticker))$coefficients[,4][1])
  
  ff_dummy_market_return_p <- as.numeric(summary(lm(
    Excess_Rets ~ mkt_rf + smb + hml + LargeDoddDummy+
     LargeDummy + DoddFrankDummy, 
    data = subset_ticker))$coefficients[,4][2])
  
  ff_dummy_dummy_p <- as.numeric(summary(lm(
    Excess_Rets ~ mkt_rf + smb + hml + LargeDoddDummy + LargeDummy + DoddFrankDummy,
    
    data = subset_ticker))$coefficients[,4][5])
  
  print(ff_dummy_dummy_p) 
  
  ff_intercept_p <- as.numeric(summary(lm(
    Excess_Rets ~ mkt_rf + smb + hml, 
    data = subset_ticker))$coefficients[,4][1])
  
  ff_market_return_p <- as.numeric(summary(lm(
    Excess_Rets ~ mkt_rf + smb + hml, 
    data = subset_ticker))$coefficients[,4][2])
  
  
  capm_intercept_p <- as.numeric(summary(lm(Excess_Rets - risk_free ~ mkt_rf,
                                            data = subset_ticker) )$coefficients[,4][1])
  capm_market_returns_p <- as.numeric(summary(lm(Excess_Rets - risk_free ~ mkt_rf,
                                              data = subset_ticker) )$coefficients[,4][2])
  
  
  capm_dummy_intercept_p <- as.numeric(summary(lm(Excess_Rets - risk_free ~ 
                                                    mkt_rf + LargeDoddDummy,
                                                  #+ LargeDummy + DoddFrankDummy,
                                                  data = subset_ticker) )$coefficients[,4][1])
  capm_dummy_market_returns_p <- as.numeric(summary(lm(Excess_Rets - risk_free ~ mkt_rf + 
                                                         LargeDoddDummy + LargeDummy +  DoddFrankDummy,
                                                       data = subset_ticker) )$coefficients[,4][2])
  
  capm_dummy_dummy_p <-as.numeric(summary(lm(Excess_Rets - risk_free ~ mkt_rf  + 
                                               LargeDoddDummy + LargeDummy + 
                                               DoddFrankDummy,
                                             data = subset_ticker) )$coefficients[,4][3])
  
    
  panel_p <- panel_p %>% add_row(
    
  Name = tic, 
  CAPM_Intercept = capm_intercept_p, 
  CAPM_Intercept_Dummies = capm_dummy_intercept_p, 
  FF_Intercept = ff_intercept_p, 
  FF_Intercept_Dummies = ff_dummy_intercept_p, 
  CAPM_Market_Rets = capm_market_returns_p, 
  CAPM_Market_Rets_Dummies = capm_dummy_market_returns_p, 
  FF_Market_Rets = ff_market_return_p, 
  FF_Market_Rets_Dummies = ff_dummy_market_return_p,
  CAPM_LargeDodd_Dummies = capm_dummy_dummy_p, 
  FF_LargeDodd_Dummies = ff_dummy_dummy_p, 
  IsLargeBank = large_bank_dummy
  )
  
  
  
  
}





## Plot the intercepts for the CAPM, dummies and no dummies 
Model_Intercept <- ggplot(data = panel, guide = T) + 
  geom_line(aes(x = Name, y = CAPM_Intercept, group = 1,color = 'Blue')) + 
  geom_line(aes(x = Name, y = CAPM_Intercept_Dummies, color = 'Green', group = 1)) + 
  geom_line(aes(x = Name, y = FF_Intercept, color = 'Red', group = 1) )+ 
  geom_line(aes(x = Name, y = FF_Intercept_Dummies, group = 1, color = 'Purple')) +
  scale_color_discrete(name = 'Pricing Models', labels = c('CAPM', 'CAPM Dummies', 
                                                           'FF', 'FF Dummies')) +
  ggtitle('Intercepts For Pricing Models') + 
  labs(y = 'Pricing Model Intercept', x = 'Bank Holding Company (Large Caps Shown)')  + 
  scale_x_discrete(guide = guide_axis(n.dodge = 10)) 
Model_Intercept



## plot the Excess Market Returns 
Model_Excess_Market <-ggplot(data = panel, guide = T) + 
  geom_line(aes(x = Name, y = CAPM_Market_Rets, group = 1,color = 'Blue')) + 
  geom_line(aes(x = Name, y = CAPM_Market_Rets_Dummies, color = 'Green', group = 1)) + 
  geom_line(aes(x = Name, y = FF_Market_Rets, color = 'Red', group = 1) )+ 
  geom_line(aes(x = Name, y = FF_Market_Rets_Dummies, group = 1, color = 'Purple')) +
  scale_color_discrete(name = 'Pricing Models', labels = c('CAPM', 'CAPM Dummies', 
                                                           'FF', 'FF Dummies')) + 
  ggtitle('Excess Market Returns For Pricing Models') + 
  labs(y = 'Pricing Model Excess Market Returns Coefficient', x = 'Bank Holding Company') + 
  scale_x_discrete(guide = guide_axis(n.dodge = 10)) 
Model_Excess_Market

##Plot the SMB 
Model_SMB <-ggplot(data = panel, guide = T) + 
  geom_line(aes(x = Name, y = SMB, color = 'Red', group = 1) )+ 
  geom_line(aes(x = Name, y = SMB_Dummies, group = 1, color = 'Purple')) +
  scale_color_discrete(name = 'Pricing Models', 
                       labels = c('FF', 'FF Dummies')) + 
  ggtitle('SMB For Pricing Models') + 
  labs(y = 'Pricing Model SMB Coefficient', x = 'Bank Holding Company') + 
  scale_x_discrete(guide = guide_axis(n.dodge = 10)) 
Model_SMB

## Plot the HML 
Model_HML <-ggplot(data = panel, guide = T) + 
  geom_line(aes(x = Name, y = HML, color = 'Red', group = 1) )+ 
  geom_line(aes(x = Name, y = HML_Dummies, group = 1, color = 'Purple')) +
  scale_color_discrete(name = 'Pricing Models', labels = c(
    'FF', 'FF Dummies')) + 
  theme(axis.text.x = element_blank()) + ggtitle('HML For Pricing Models') + 
  labs(y = 'Pricing Model HML Coefficient', x = 'Bank Holding Company') + 
  scale_x_discrete(labels = c(' ', 'SC'))
Model_HML



## Plot the Dummies, delete regeional banks where this doesn't apply
Model_Dummies <-ggplot(data = panel, guide = T) + 
  geom_point(aes(x = Name, y = CAPM_LargeDodd_Dummies, color = 'Green', group = 1)) + 
  geom_point(aes(x = Name, y = FF_LargeDodd_Dummies, group = 1, color = 'Purple')) +
  scale_color_discrete(name = 'Pricing Models', labels = c('CAPM Dummies', 'FF Dummies')) + 
   ggtitle('Dodd Frank * Large Cap Dummy Variable For Pricing Models') + 
  labs(y = 'Dodd Frank * Large Cap Dummy Variable Coefficient', 
       x = 'Bank Holding Company') + 
  scale_x_discrete(guide = guide_axis(n.dodge = 10))
Model_Dummies

## Plot the Large Cap Frank Dummies 
Model_Dodd_Dummies <- ggplot(data = panel, guide = T) + 
  geom_point(aes(x = Name, y = CAPM_Large_Dummies, group = 1, color= 'Green')) + 
  geom_point(aes(x = Name, y = FF_Large_Dummies, group = 1, color = 'Purple')) + 
  scale_color_discrete(name = 'Pricing Models', labels = c('CAPM Dummies', 
                                                            'FF Dummies')) + 
  ggtitle('Large Cap Dummy Variable') + 
  labs(y = 'Large Cap Coefficient', x = 'Bank Holding Company') + 
  scale_x_discrete(guide = guide_axis(n.dodge = 10)) 
Model_Dodd_Dummies



## Find the mean of the p values for the regression. 
## Comparing if the coefficients are significantly different 
## from 0
mean_panel_p <- panel_p %>% 
  group_by(groups = 1) %>% 
  summarize_at(vars(-Name), mean) 

## Mean p values, split by large and small banks
mean_small_large_panel_p <- panel_p %>% 
  group_by(groups = IsLargeBank) %>% 
  summarize_at(vars(-Name), mean) 
mean_panel_p

