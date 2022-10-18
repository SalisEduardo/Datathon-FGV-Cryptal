
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(readxl)
library(tidyverse)

ada <- read_excel("Crypto Data/ADA.xlsx")
names(ada)[2:dim(ada)[2]] <- paste("ADA",names(ada)[2:dim(ada)[2]],sep=".")

bnb <- read_excel("Crypto Data/BNB.xlsx")
names(bnb)[2:dim(bnb)[2]] <- paste("BNB",names(bnb)[2:dim(bnb)[2]],sep=".")

btc <- read_excel("Crypto Data/BTC.xlsx")
names(btc)[2:dim(btc)[2]] <- paste("BTC",names(btc)[2:dim(btc)[2]],sep=".")

doge <- read_excel("Crypto Data/DOGE.xlsx")
names(doge)[2:dim(doge)[2]] <- paste("DOGE",names(doge)[2:dim(doge)[2]],sep=".")

eth <- read_excel("Crypto Data/ETH.xlsx")
names(eth)[2:dim(eth)[2]] <- paste("ETH",names(eth)[2:dim(eth)[2]],sep=".")

link <- read_excel("Crypto Data/LINK.xlsx")
names(link)[2:dim(link)[2]] <- paste("LINK",names(link)[2:dim(link)[2]],sep=".")

ltc <- read_excel("Crypto Data/LTC.xlsx")
names(ltc)[2:dim(ltc)[2]] <- paste("LTC",names(ltc)[2:dim(ltc)[2]],sep=".")

trx <- read_excel("Crypto Data/TRX.xlsx")
names(trx)[2:dim(trx)[2]] <- paste("TRX",names(trx)[2:dim(trx)[2]],sep=".")

xlm <- read_excel("Crypto Data/XLM.xlsx")
names(xlm)[2:dim(xlm)[2]] <- paste("XLM",names(xlm)[2:dim(xlm)[2]],sep=".")

xrp <- read_excel("Crypto Data/XRP.xlsx")
names(xrp)[2:dim(xrp)[2]] <- paste("XRP",names(xrp)[2:dim(xrp)[2]],sep=".")

crypto_list <- list(ada,bnb,btc,doge,eth,link,ltc,trx,xlm,xrp)

crypto_dataframe <- crypto_list %>% reduce(full_join,by='Date')

crypto_prices <- crypto_dataframe %>% 
  dplyr::select(Date,ends_with("Close")) %>% 
  arrange(Date) %>% 
  rename_with(~str_remove(., '.Close'))

crypto_prices$Date <- as.POSIXct(crypto_prices$Date)

crypto_prices_xts <- xts(as.data.frame(crypto_prices[,-1]),order.by=crypto_prices$Date)

crypto_prices_xts <- xts(as.data.frame(crypto_prices[,-1]),order.by=crypto_prices$Date)

crypto_returns <- na.omit(Return.calculate(crypto_prices_xts) )


#calc efficiency


##MFDFA
library(MFDFA)


calc_mdm <- function(x,asset_name){

  N <- length(x)
  
  scale <-10:(N/4) # literature suggestion
  q <- -4:4
  m <- 1 # degree polinomial  
  
  calc_mfdfa <- MFDFA(x, scale, m, q)
  MDM = (abs(calc_mfdfa[["Hq"]][1] - 0.5) + abs(calc_mfdfa[["Hq"]][9]-0.9))/2
  
  df_results = data.frame(MDM = MDM , asset=asset_name)
 
  
  return(df_results)
  
}

calc_mdm(crypto_returns$BTC,"BTC")

mdm_values <- list()

for(name in colnames(crypto_returns)){
  mdm <- calc_mdm(crypto_returns[,name],name)
  mdm_values <- append(mdm_values,mdm)
  
}





