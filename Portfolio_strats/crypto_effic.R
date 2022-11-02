library(MFDFA)
library(readxl)
library(plotly)
library(nonlinearTseries)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(RiskPortfolios)
library(covRobust)
library(quantmod)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)


calcEfficiency <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  #effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  mdm <- (abs(b[["Hq"]][1] - 0.5) + abs(b[["Hq"]][9]-0.5))/2
  
  return(mdm)
  
}


# Cripto - data

crypto_prices <- read.csv("Crypto_prices.csv")

crypto_prices$Date <- as.POSIXct(crypto_prices$Date)

crypto_prices_xts <- xts(as.data.frame(crypto_prices[,-1]),order.by=crypto_prices$Date)

crypto_returns <- na.omit(Return.calculate(crypto_prices_xts))
#-------------------------------------------------------------------------------------------------------

#Analise_effisc 


#crypto_select <- crypto_returns[,c("BNB","ADA","ETH","LTC","TRX","BTC","XRP","DOGE")]

# crypto_effic_60d <- rollapply(crypto_select,60,calcEfficiency)
# crypto_effic_90d <- rollapply(crypto_select,90,calcEfficiency)
# crypto_effic_120d <- rollapply(crypto_select,120,calcEfficiency)

crypto_effic_60d <- rollapply(crypto_returns,60,calcEfficiency)
crypto_effic_90d <- rollapply(crypto_returns,90,calcEfficiency)
crypto_effic_120d <- rollapply(crypto_returns,120,calcEfficiency)

crypto_effic_60d %>%  as.data.frame() %>% write.csv('Rolling_efficiency60du.csv',row.names = TRUE)

crypto_effic_90d %>%  as.data.frame() %>% write.csv('Rolling_efficiency90du.csv',row.names = TRUE)

crypto_effic_120d %>%  as.data.frame() %>% write.csv('Rolling_efficiency120du.csv',row.names = TRUE)








