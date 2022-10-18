#Packages

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
library(treasuryTR)

# Data

########################################################################################################################## 
# Cripto - data
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

crypto_returns <- na.omit(Return.calculate(crypto_prices_xts))

# Alocation Rules



## Minimum variance portfolio

mvp.spec <- function(p){
  p_MVP <- p %>% 
    add.constraint(type = "full_investment") %>% 
    add.constraint(type = "long_only") %>% 
    add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'StdDev', risk_aversion=9999)
  return(p_MVP)
}

## Tangency portfolio - (add maxSR=TRUE in the optimization to get max Sharp)

tp.sepc <- function(p){
  tp <- p %>%
      add.constraint(type = "full_investment") %>%
      add.constraint(type = "long_only") %>% 
      add.objective(type = "risk", name = "StdDev") %>%
      add.objective(type = "return", name = "mean")
  return(tp)
}


## Max return portfolio
maxRet.spec <- function(p){
  p_maxRet <- p %>%
    add.constraint(type = "full_investment") %>%
    add.constraint(type = "long_only") %>% 
    add.constraint(type = "box", min=0, max=1) %>%
    add.objective(type = "return", name = "mean")
  return(p_maxRet)
}




  
#######################################################################################################
#Subsets

# MF-DFA 2019-2020 >>> Rank Efficiency >>> Strats 2021

tags_efficients_2021 <- c("BNB","ADA","ETH","LTC")
tags_inefficients_2021 <- c("TRX","BTC","XRP","DOGE")

# MF-DFA  2020-2021 >>> Rank Efficiency >>> Strats 2022


tags_efficients_2022 <- c("BNB","TRX","ETH","BTC")
tags_inefficients_2022 <- c("LTC","ADA","XRP","DOGE")


#######################################################################################################

### Portfolio Specs

## 2021

effic2021_MVP <- portfolio.spec(assets = tags_efficients_2021) %>% mvp.spec()
ineffic2021_MVP <- portfolio.spec(assets = tags_inefficients_2021) %>% mvp.spec()

effic2021_TP <- portfolio.spec(assets = tags_efficients_2021) %>% tp.sepc()
ineffic2021_TP <- portfolio.spec(assets = tags_inefficients_2021) %>% tp.sepc()

effic2021_maxRet <- portfolio.spec(assets = tags_efficients_2021) %>% maxRet.spec()
ineffic2021_maxRet <- portfolio.spec(assets = tags_inefficients_2021) %>% maxRet.spec()

## 2022

effic2022_MVP <- portfolio.spec(assets = tags_efficients_2022) %>% mvp.spec()
ineffic2022_MVP  <- portfolio.spec(assets = tags_inefficients_2022) %>% mvp.spec()

effic2022_TP <- portfolio.spec(assets = tags_efficients_2022) %>% tp.sepc()
ineffic2022_TP <- portfolio.spec(assets = tags_inefficients_2022) %>% tp.sepc()

effic2022_maxRet <- portfolio.spec(assets = tags_efficients_2022) %>% maxRet.spec()
ineffic2022_maxRet <- portfolio.spec(assets = tags_inefficients_2022) %>% maxRet.spec()


#######################################################################################################


# Efficient Frontier Analysis


date_range_I <- "2021-01/2021-12"
date_range_II <- "2022-01/2022-12"

Crypto2021 <- crypto_returns[date_range_I]
Crypto2022 <- crypto_returns[date_range_II]


### In-sample 2021

Effic_inSample_MVP2021 <- optimize.portfolio(Crypto2021, effic2021_MVP, optimize_method = 'random',trace=TRUE)

Ineffic_inSample_MVP2021 <- optimize.portfolio(Crypto2021, ineffic2021_MVP, optimize_method = 'random',trace=TRUE)



chart.RiskReward(Effic_inSample_MVP2021 ,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE,
                 main='Cryptos Efficients MVP 2021 - Risk Reward')

chart.RiskReward(Ineffic_inSample_MVP2021,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE,
                 main='Cryptos Inefficients 2021')


chart.EfficientFrontier(Effic_inSample_MVP2021 ,
                        match.col = "StdDev", 
                        type='l',
                        n.portfolios = 200,
                        tangent.line = TRUE,main = 'Cryptos Efficients MVP 2021 - EfficientFrontier',
                        chart.assets = TRUE, labels.assets = TRUE)


chart.EfficientFrontier(Ineffic_inSample_MVP2021,
                        match.col = "StdDev", 
                        n.portfolios = 200,
                        tangent.line = TRUE,main = 'Cryptos Inefficients MVP 2021 - EfficientFrontier')


### Ovarlaing EFs 2021



Efficents_EF_2021 <- extractEfficientFrontier(Effic_inSample_MVP2021,match.col = "StdDev",n.portfolios = 250)

Efficents_EF_2021$frontier %>% ggplot(aes(x='StdDev',y='mean'))


## In-sample 2022

Effic_inSample_MVP2022 <- optimize.portfolio(Crypto2022, effic2022_MVP, optimize_method = 'random',trace=TRUE)
Ineffic_inSample_MVP2022 <- optimize.portfolio(Crypto2022, ineffic2022_MVP, optimize_method = 'random',trace=TRUE)



chart.RiskReward(Effic_inSample_MVP2022 ,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE,
                 main='Cryptos Efficients MVP 2022 - Risk Reward')

chart.EfficientFrontier(Effic_inSample_MVP2022 ,
                        match.col = "StdDev", 
                        n.portfolios = 200,
                        tangent.line = TRUE,main = 'Cryptos Efficients MVP 2022 - EfficientFrontier')

chart.RiskReward(Ineffic_inSample_MVP2022,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE,
                 main='Cryptos Inefficients 2022')


chart.EfficientFrontier(Ineffic_inSample_MVP2022,
                        match.col = "StdDev", 
                        n.portfolios = 200,
                        tangent.line = TRUE,main = 'Cryptos Inefficients MVP 2022 - EfficientFrontier')



#######################################################################################################
RiskFree_US = quantmod::getSymbols("DGS10",src = "FRED",auto.assign = FALSE) 

RiskFree_US = na.omit(Return.calculate(RiskFree_US['2019-01/2022-04']))



rfUS_I <- Return.annualized(RiskFree_US['2021'])


yield_10y <- treasuryTR::get_yields("DGS10")

tr_10y <- treasuryTR::total_return(yield_10y, maturity = 10, scale = 251)

#######################################################################################################


# Performance 2021



## Train and test split
train_dataI <- crypto_returns['2019-01/2020-12']
test_dataI <- crypto_returns['2021-01/2021-12']

## Optimization  (weights obtained with train data)

# optimizor <-'random'  

optimizor <-'ROI'  

### efficients

MVP_Effic_I <-  optimize.portfolio(train_dataI ,
                                   effic2021_MVP,
                                   optimize_method = optimizor,
                                   trace=TRUE)

MaxSharp_Effic_I <-  optimize.portfolio(train_dataI , 
                                   effic2021_TP, 
                                    maxSR=TRUE, #  max Sharp
                                   optimize_method = optimizor,
                                   trace=TRUE)

maxRet_Effic_I <-  optimize.portfolio(train_dataI ,
                                      effic2021_maxRet,
                                      optimize_method = optimizor,
                                      trace=TRUE)
### inefficients

MVP_Ineffic_I <-  optimize.portfolio(train_dataI,
                                     ineffic2021_MVP, 
                                     optimize_method = optimizor,
                                     trace=TRUE)


MaxSharp_Ineffic_I <-  optimize.portfolio(train_dataI, 
                                    ineffic2021_TP, 
                                    maxSR=TRUE, #  max Sharp
                                    optimize_method = optimizor,
                                    trace=TRUE)

maxRet_Ineffic_I <-  optimize.portfolio(train_dataI , 
                                        ineffic2021_maxRet,
                                        optimize_method = optimizor,
                                        trace=TRUE)



## Weights

### Minimum Variance
weights_effic_MVP_I <- extractWeights(MVP_Effic_I)
weights_ineffic_MVP_I <- extractWeights(MVP_Ineffic_I)

### Max Sharp
weights_effic_MaxSharp_I <- extractWeights(MaxSharp_Effic_I)
weights_ineffic_MaxSharp_I <- extractWeights(MaxSharp_Ineffic_I)

### Max Return
weights_effic_maxRet_I <- extractWeights(maxRet_Effic_I)
weights_ineffic_maxRet_I <- extractWeights(maxRet_Ineffic_I)

### Equal weights
weights_effic_EW_I <- rep(1/length(tags_efficients_2021),length(tags_efficients_2021))
weights_ineffic_EW_I <- rep(1/length(tags_inefficients_2021),length(tags_inefficients_2021))


## Strats - applyed on Test data

### MVP
Rets_Effic_MVP_I <- Return.portfolio(test_dataI[,tags_efficients_2021],
                                          weights = weights_effic_MVP_I)

Rets_Ineffic_MVP_I <- Return.portfolio(test_dataI[,tags_inefficients_2021],
                                            weights = weights_ineffic_MVP_I)

### Max Sharp
Rets_Effic_MaxSharp_I <- Return.portfolio(test_dataI[,tags_efficients_2021],
                                     weights = weights_effic_MaxSharp_I)

Rets_Ineffic_MaxSharp_I <- Return.portfolio(test_dataI[,tags_inefficients_2021],
                                       weights = weights_ineffic_MaxSharp_I)

### Max Return
Rets_Effic_maxRet_I <- Return.portfolio(test_dataI[,tags_efficients_2021],
                                     weights = weights_effic_maxRet_I)

Rets_Ineffic_maxRet_I <- Return.portfolio(test_dataI[,tags_inefficients_2021],
                                       weights = weights_ineffic_maxRet_I)
### Equal Weights
EW_Effic_I <- Return.portfolio(test_dataI[,tags_efficients_2021],
                               weights = weights_effic_EW_I)

EW_Ineffic_I <- Return.portfolio(test_dataI[,tags_inefficients_2021],
                                 weights = weights_ineffic_EW_I)

## Results
tableResults_I <- cbind(

  table.AnnualizedReturns(Rets_Effic_MVP_I),
  table.AnnualizedReturns(Rets_Ineffic_MVP_I),
  
  table.AnnualizedReturns(Rets_Effic_MaxSharp_I),
  table.AnnualizedReturns(Rets_Ineffic_MaxSharp_I),
  
  table.AnnualizedReturns(Rets_Effic_maxRet_I),
  table.AnnualizedReturns(Rets_Ineffic_maxRet_I),

  table.AnnualizedReturns(EW_Effic_I),
  table.AnnualizedReturns(EW_Ineffic_I)
)
colnames(tableResults_I) <- c("Effec-MVP","Ineffec-MVP", 
                              "Effec-MaxSharp","Ineffec-MaxSharp",
                              "Effec-maxRet","Ineffec-maxRet",
                              "Effec-EW","Ineffec-EW")


# charts.PerformanceSummary(Rets_Effic_MVP_I)
# charts.PerformanceSummary(Rets_Ineffic_MVP_I)
# charts.PerformanceSummary(EW_Effic_I)
# charts.PerformanceSummary(EW_Ineffic_I)


#######################################################################################################


# Performance 2022

## Train and test split
train_dataII <- crypto_returns['2020-01/2021-12']
test_dataII <- crypto_returns['2022-01/2022-12']

## Optimization  (weights obtained with train data)

optimizor <-'random'  

### efficients

MVP_Effic_II <-  optimize.portfolio(train_dataII ,
                                   effic2022_MVP,
                                   optimize_method = optimizor,
                                   trace=TRUE)

MaxSharp_Effic_II <-  optimize.portfolio(train_dataII , 
                                        effic2022_TP, 
                                        maxSR=TRUE, #  max Sharp
                                        optimize_method = optimizor,
                                        trace=TRUE)

maxRet_Effic_II <-  optimize.portfolio(train_dataII ,
                                      effic2022_maxRet,
                                      optimize_method = optimizor,
                                      trace=TRUE)
### inefficients

MVP_Ineffic_II <-  optimize.portfolio(train_dataII,
                                     ineffic2022_MVP, 
                                     optimize_method = optimizor,
                                     trace=TRUE)


MaxSharp_Ineffic_II <-  optimize.portfolio(train_dataII, 
                                          ineffic2022_TP, 
                                          maxSR=TRUE, #  max Sharp
                                          optimize_method = optimizor,
                                          trace=TRUE)

maxRet_Ineffic_II <-  optimize.portfolio(train_dataII , 
                                        ineffic2022_maxRet,
                                        optimize_method = optimizor,
                                        trace=TRUE)



## Weights

### Minimum Variance
weights_effic_MVP_II <- extractWeights(MVP_Effic_II)
weights_ineffic_MVP_II <- extractWeights(MVP_Ineffic_II)

### Max Sharp
weights_effic_MaxSharp_II <- extractWeights(MaxSharp_Effic_II)
weights_ineffic_MaxSharp_II <- extractWeights(MaxSharp_Ineffic_II)

### Max Return
weights_effic_maxRet_II <- extractWeights(maxRet_Effic_II)
weights_ineffic_maxRet_II <- extractWeights(maxRet_Ineffic_II)

### Equal weights
weights_effic_EW_II <- rep(1/length(tags_efficients_2022),length(tags_efficients_2022))
weights_ineffic_EW_II <- rep(1/length(tags_inefficients_2022),length(tags_inefficients_2022))


## Strats - applyed on Test data

### MVP
Rets_Effic_MVP_II <- Return.portfolio(test_dataII[,tags_efficients_2022],
                                     weights = weights_effic_MVP_II)

Rets_Ineffic_MVP_II <- Return.portfolio(test_dataII[,tags_inefficients_2022],
                                       weights = weights_ineffic_MVP_II)

### Max Sharp
Rets_Effic_MaxSharp_II <- Return.portfolio(test_dataII[,tags_efficients_2022],
                                          weights = weights_effic_MaxSharp_II)

Rets_Ineffic_MaxSharp_II <- Return.portfolio(test_dataII[,tags_inefficients_2022],
                                            weights = weights_ineffic_MaxSharp_II)

### Max Return
Rets_Effic_maxRet_II <- Return.portfolio(test_dataII[,tags_efficients_2022],
                                        weights = weights_effic_maxRet_II)

Rets_Ineffic_maxRet_II <- Return.portfolio(test_dataII[,tags_inefficients_2022],
                                          weights = weights_ineffic_maxRet_II)
### Equal Weights
EW_Effic_II <- Return.portfolio(test_dataII[,tags_efficients_2022],
                               weights = weights_effic_EW_II)

EW_Ineffic_II <- Return.portfolio(test_dataII[,tags_inefficients_2022],
                                 weights = weights_ineffic_EW_II)


## Results
tableResults_II <- cbind(
  
  table.AnnualizedReturns(Rets_Effic_MVP_II),
  table.AnnualizedReturns(Rets_Ineffic_MVP_II),
  
  table.AnnualizedReturns(Rets_Effic_MaxSharp_II),
  table.AnnualizedReturns(Rets_Ineffic_MaxSharp_II),
  
  table.AnnualizedReturns(Rets_Effic_maxRet_II),
  table.AnnualizedReturns(Rets_Ineffic_maxRet_II),
  
  table.AnnualizedReturns(EW_Effic_II),
  table.AnnualizedReturns(EW_Ineffic_II)
)
colnames(tableResults_II) <- c("Effec-MVP","Ineffec-MVP", 
                              "Effec-MaxSharp","Ineffec-MaxSharp",
                              "Effec-maxRet","Ineffec-maxRet",
                              "Effec-EW","Ineffec-EW")

# sp500 <- quantmod::getSymbols("^GSPC", auto.assign = FALSE, from = as.Date("2019-01-01"), to = as.Date("2022-09-29")) %>% 
#   Ad() %>% 
#   Return.calculate() %>% 
#   na.omit()
# 
# colnames(sp500) <- c("Returns")


nasdaq100 <- quantmod::getSymbols("^NDX", auto.assign = FALSE, from = as.Date("2019-01-01"), to = as.Date("2022-09-29"))%>% 
  Ad() %>% 
  Return.calculate() %>% 
  na.omit()  

colnames(nasdaq100) <- c("Returns")







