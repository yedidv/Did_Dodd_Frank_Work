library(fGarch)
library(fUnitRoots)
library(rugarch)
library(rmgarch)
library(forecast)
library(plm)
library(readxl) 
library(tidyverse) 

## Error Doing Arch Model at column 25
archT <- function(df, store) {
  for (i in 2:55) {
    print(i)
    model_specify <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
    
    model_fit = ugarchfit(spec = model_specify, data = df[[i]])
    store[i] <- model_fit@fit$sigma
  }
  names(store) <- names(Returns_Sheet)
  return(store)
}

## Quick run time
garchT <- function(df, store) {
  for (i in 2:55) {
    print(i)
    gar <- garchFit(formula = ~garch(1,1), data = Returns_Sheet[[i]])
    store[i] <- gar@sigma.t
  }
  names(store) <- names(Returns_Sheet)
  return(store)
}

## Long time to run
argarchT <- function(df, store) {
  for (i in 2:55) {
    print(i)
    specR <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(1,0), archm=T, archpow=2),
                        distribution.model="std")
    argar<-ugarchfit(spec=specR,data= df[[i]])
    store[i] <- argar@fit$sigma
  }
  names(store) <- names(Returns_Sheet)
  return(store)
}

annual <- function(df, store) {
  for (i in 2:55) {
    store[i] = df[i]*sqrt(252)
  }
  names(store) <- names(Returns_Sheet)
  return(store)
}

restructure <- function(df) {
  storea <- data.frame(matrix(ncol=5, nrow=0))
  colnames(storea) <- c("Ticker", "Date", "Volatility", "Large", "Dodd")
  checkReg <- c("BOH", "SCHW", "ZION", "SIVB", "MTB", "JPM", "VLY", "CBSH", "UMBF", "KEY", "HBAN", "PNC", 
                "FITB", "FCNCA", "SNV", "TRMK", "FHN", "BXS", "CFR", "IBOC", "PB", "STT", "FULT", "USB", 
                "WFC", "BPOP", "WBS", "ASB", "NTRS", "CMA", "BEN", "CATY", "BOKF", "NYCB", "MS", "WTFC", 
                "GS", "EWBC", "FBP", "UMPQ", "FNB", "WAFD", "PBCT", "RJF", "FBC", "PFG")
  dodd <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
  for (i in 1:54) {
    if (colnames(df)[i] %in% checkReg) {
      dftest = data.frame(Ticker = colnames(garch)[i], 
                          Date = garch$Date, Volatility = garch[i],
                          Large = 0, Dodd = dodd)
      names(dftest) <- c("Ticker", "Date", "Volatility", "Large", "Dodd")
      storea <- rbind(storea, dftest)
    }
    else {
      dftest = data.frame(Ticker = colnames(garch)[i], 
                          Date = garch$Date, Volatility = garch[i],
                          Large = 1, Dodd = dodd)
      names(dftest) <- c("Ticker", "Date", "Volatility", "Large", "Dodd")
      storea <- rbind(storea, dftest)
    }
    
  } 
  return(storea)
}

## For Hard Environment Reset (To Clear Clutter)
rm(list = ls())

## Generate columns for fit
sample(1:55, 3, replace = TRUE)

Returns_Sheet <- read_excel('Returns.xlsx', sheet = 'No_Nulls_Log_Returns')

## Visual ACF tests - Numbers I Generated
acf(Returns_Sheet[[20]])
pacf(Returns_Sheet[[20]])
acf(Returns_Sheet[[36]])
pacf(Returns_Sheet[[36]])
acf(Returns_Sheet[[51]])
pacf(Returns_Sheet[[51]])

## Squared tests
acf(Returns_Sheet[[20]]^2)
acf(Returns_Sheet[[36]]^2)
acf(Returns_Sheet[[51]]^2)

## Looking at P-Values
Box.test(Returns_Sheet[[20]], lag = 1, type = "Ljung") # Best was lag of 1
Box.test(Returns_Sheet[[36]], lag = 1, type = "Ljung") # PACF of 1****
Box.test(Returns_Sheet[[51]], lag = 1, type = "Ljung") # Best was lag of 1

## Find AR Lag Order and MA Order That Fits Each Column
fit_arima<-auto.arima(Returns_Sheet[[15]],d=1,D=1,stepwise=FALSE,
                      approximation = FALSE, trace = TRUE)

fit_arima<-auto.arima(Returns_Sheet[[49]],d=1,D=1,stepwise=FALSE,
                      approximation = FALSE, trace = TRUE)

fit_arima<-auto.arima(Returns_Sheet[[51]],d=1,D=1,stepwise=FALSE,
                      approximation = FALSE, trace = TRUE)

## ARIMA(5,1,0) Best Fit
store <- data.frame(date = Returns_Sheet[[1]])
arch <- archT(Returns_Sheet, store)
arch <- annual(arch, store)
test <- read.zoo(arch)
testa <- aggregate(test, as.yearqtr, mean)
arch <- data.frame(testa)

## Using Garch(1,1) Model
garch <- garchT(Returns_Sheet, store)
garch <- annual(garch, store)
test <- read.zoo(garch)
testa <- aggregate(test, as.yearqtr, mean)
garch <- data.frame(testa)

## Arch + Garch Model
argarch <- argarchT(Returns_Sheet, store)
argarch <- annual(argarch, store)
test <- read.zoo(argarch)
testa <- aggregate(test, as.yearqtr, mean)
argarch <- data.frame(testa)

## Prepare for Analysis (And add in Dummy Vars)
arch$Date <- rownames(arch)
garch$Date <- rownames(garch)
argarch$Date <- rownames(argarch)

rownames(arch) <- 1:nrow(arch)
rownames(garch) <- 1:nrow(garch)
rownames(argarch) <- 1:nrow(argarch)


arch1 <- restructure(arch)
garch1 <- restructure(garch)
argarch1 <- restructure(argarch)

test <- summary(lm(formula = arch1$Volatility ~ arch1$Large))
test1 <- plm(formula = arch1$Volatility ~ arch1$Large, data = arch1, index = c("Ticker", "Date"), model = "within")
