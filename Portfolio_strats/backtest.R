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
library(MFDFA)
library(readxl)
library(plotly)
library(nonlinearTseries)
library(bizdays)
library(plyr)
library(ggdark)




########################################################################################################################## 
# Cripto - data

crypto_prices <- read.csv("Crypto_prices.csv")

crypto_prices$Date <- as.POSIXct(crypto_prices$Date)

crypto_prices_xts <- xts(as.data.frame(crypto_prices[,-1]),order.by=crypto_prices$Date)

crypto_returns <- na.omit(Return.calculate(crypto_prices_xts))


## Risk Free

RiskFree_US = quantmod::getSymbols("DGS10",src = "FRED",auto.assign = FALSE) 

RiskFree_US = na.omit(Return.calculate(RiskFree_US['2019-01/2022-09-29']))
index(RiskFree_US) <- as.POSIXct(index(RiskFree_US)) 


rfUS_2021 <- RiskFree_US['2021'] 

ann_rfUS_2021 <- Return.annualized(rfUS_2021,scale = 252)[1] 

daily_rfUS_2021 <- ((ann_rfUS_2021 + 1) ^ (1/252) - 1) %>%  round(digits = 5)


rfUS_2022 <- RiskFree_US['2022'] 

ann_rfUS_2022 <- Return.annualized(rfUS_2022,scale = 252)[1]

daily_rfUS_2022 <- ((ann_rfUS_2022 + 1) ^ (1/252) - 1) %>%  round(digits = 5)

#######################################################################################################
#Subsets

# MF-DFA 2019-2020 >>> Rank Efficiency >>> Strats 2021

tags_efficients_2021 <- c("BNB","ADA","ETH","LTC")
tags_inefficients_2021 <- c("TRX","BTC","XRP","DOGE")

# MF-DFA  2020-2021 >>> Rank Efficiency >>> Strats 2022

tags_efficients_2022 <- c("BNB","TRX","ETH","BTC")
tags_inefficients_2022 <- c("LTC","ADA","XRP","DOGE")




######################################################################################################################################################

#Allocation Specs

## Base

# Long only , full investment
pspec.lo.full <- function(assets_names){
  p_spec <- portfolio.spec(assets = assets_names) %>% 
    add.constraint(type = "full_investment") %>% 
    add.constraint(type = "long_only")
  return(p_spec)
}


pspec.box.full <- function(assets_names, min_box=0,max_box=1){
  p_spec <- portfolio.spec(assets = assets_names) %>% 
    add.constraint(type = "full_investment") %>%
    add.constraint(type = "box", min=min_box, max=max_box)
}



## Strategies
  
### Minimum variance portfolio

mvp.spec <- function(p){
  p_MVP <- p %>% 
    #add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'StdDev', risk_aversion=9999)
  return(p_MVP)
}

### Tangency portfolio - (add maxSR=TRUE in the optimization to get max Sharp)

tp.sepc <- function(p){
  tp <- p %>%
    add.objective(type = "risk", name = "StdDev") %>%
    add.objective(type = "return", name = "mean")
  return(tp)
}


### Max return portfolio
maxRet.spec <- function(p){
  p_maxRet <- p %>%
    add.objective(type = "return", name = "mean")
  return(p_maxRet)
}

maxMV.util.specs <- function(p,lambda){
  p_maxMV <-p %>% 
    add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'var',risk_aversion=lambda)
  return(p_maxMV)
}
  
  
minCVaR.spec <- function(p,args=list(p=0.95,clean="boudt",method='historical'),cteCOV= FALSE){
  p_CVaR <- p %>%
    add.objective(type = "risk",name = "ES",arguments = args,enabled=TRUE)
  return(p_CVaR)
    
}


build.portfolio.strats <- function(strats_name,assets_names,return_series,train_period,test_period,base_specs,strats_specs,optimizor='ROI',maxSharp=FALSE,neg_to_zero=FALSE){
  portfolio_specs <- assets_names %>% base_specs %>% strats_specs
  
  if(maxSharp){
    optimize_train <- optimize.portfolio(return_series[train_period,assets_names] , 
                                         portfolio_specs, 
                                         maxSR=TRUE,
                                         optimize_method = optimizor,
                                         trace=TRUE)
  }else{
    optimize_train <- optimize.portfolio(return_series[train_period,assets_names] , 
                                         portfolio_specs,
                                         optimize_method = optimizor,
                                         trace=TRUE)
  }
  
  
  
  portfolio_weights <- extractWeights(optimize_train)
  
  if(neg_to_zero){
    portfolio_weights <- ifelse(portfolio_weights < 0, 0, portfolio_weights)
  }
  
  Returns_strategy <- Return.portfolio(return_series[test_period,assets_names],
                                       weights = portfolio_weights)
  
  return(list(
    name = strats_name,
    p = portfolio_specs,
    train = train_period,
    test = test_period,
    opt = optimize_train,
    w  =  portfolio_weights,
    R = Returns_strategy
  ))
}
  
build.EW.portfolio <- function(strats_name,assets_names,return_series,train_period,test_period){
  portfolio_weights <- rep(1/length(assets_names),length(assets_names))
  Returns_strategy <- Return.portfolio(return_series[test_period,assets_names],
                                        weights = portfolio_weights)
  return(list(
    name = strats_name,
    train = train_period,
    test = test_period,
    w  =  portfolio_weights,
    R = Returns_strategy
  ))
  
}




############################################################################################################

# Strategies

## 2021

### efficients

MVP_Effic_2021 <-  build.portfolio.strats("MVP_Effic_2021" ,
                                   tags_efficients_2021,
                                   crypto_returns,
                                   "2019/2020",
                                   "2021",
                                   pspec.lo.full,
                                   mvp.spec,
                                   neg_to_zero = TRUE)


maxSR_Effic_2021 <-  build.portfolio.strats("maxSR_Effic_2021" ,
                                            tags_efficients_2021,
                                            crypto_returns,
                                            "2019/2020",
                                            "2021",
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


maxRet_Effic_2021 <-  build.portfolio.strats("maxRet_Effic_2021" ,
                                             tags_efficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021",
                                             pspec.lo.full,
                                             maxRet.spec,
                                             neg_to_zero = TRUE)


minCVaR_Effic_2021 <- build.portfolio.strats("minCVaR_Effic_2021" ,
                                             tags_efficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021",
                                             pspec.lo.full,
                                             minCVaR.spec,
                                             neg_to_zero = TRUE)



EW_Effic_2021 <-  build.EW.portfolio("EW_Effic_2021" ,
                                             tags_efficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021")





### Inefficients


MVP_Ineffic_2021 <-  build.portfolio.strats("MVP_Ineffic_2021" ,
                                          tags_inefficients_2021,
                                          crypto_returns,
                                          "2019/2020",
                                          "2021",
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)

maxSR_Ineffic_2021 <-  build.portfolio.strats("maxSR_Ineffic_2021" ,
                                            tags_inefficients_2021,
                                            crypto_returns,
                                            "2019/2020",
                                            "2021",
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


maxRet_Ineffic_2021 <-  build.portfolio.strats("maxRet_Ineffic_2021" ,
                                             tags_inefficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021",
                                             pspec.lo.full,
                                             maxRet.spec,
                                             neg_to_zero = TRUE)


minCVaR_Ineffic_2021 <- build.portfolio.strats("minCVaR_Ineffic_2021" ,
                                             tags_inefficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021",
                                             pspec.lo.full,
                                             minCVaR.spec,
                                             neg_to_zero = TRUE)



EW_Ineffic_2021 <-  build.EW.portfolio("EW_Ineffic_2021" ,
                                     tags_inefficients_2021,
                                     crypto_returns,
                                     "2019/2020",
                                     "2021")


## 2022

### efficients

MVP_Effic_2022 <-  build.portfolio.strats("MVP_Effic_2022" ,
                                          tags_efficients_2022,
                                          crypto_returns,
                                          "2020/2021",
                                          "2022",
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)


maxSR_Effic_2022 <-  build.portfolio.strats("maxSR_Effic_2022" ,
                                            tags_efficients_2022,
                                            crypto_returns,
                                            "2020/2021",
                                            "2022",
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


maxRet_Effic_2022 <-  build.portfolio.strats("maxRet_Effic_2022" ,
                                             tags_efficients_2022,
                                             crypto_returns,
                                             "2020/2021",
                                             "2022",
                                             pspec.lo.full,
                                             maxRet.spec,
                                             neg_to_zero = TRUE)


minCVaR_Effic_2022 <- build.portfolio.strats("minCVaR_Effic_2022" ,
                                             tags_efficients_2022,
                                             crypto_returns,
                                             "2020/2021",
                                             "2022",
                                             pspec.lo.full,
                                             minCVaR.spec,
                                             neg_to_zero = TRUE)


EW_Effic_2022 <-  build.EW.portfolio("EW_Effic_2022" ,
                                       tags_efficients_2022,
                                       crypto_returns,
                                       "2020/2021",
                                       "2022")



### Inefficients

MVP_Ineffic_2022 <-  build.portfolio.strats("MVP_Ineffic_2022" ,
                                            tags_inefficients_2022,
                                            crypto_returns,
                                            "2020/2021",
                                            "2022",
                                            pspec.lo.full,
                                            mvp.spec,
                                            neg_to_zero = TRUE)


maxSR_Ineffic_2022 <-  build.portfolio.strats("maxSR_Ineffic_2022" ,
                                              tags_inefficients_2022,
                                              crypto_returns,
                                              "2020/2021",
                                              "2022",
                                              pspec.lo.full,
                                              tp.sepc,
                                              maxSharp = TRUE,
                                              neg_to_zero = TRUE)


maxRet_Ineffic_2022 <-  build.portfolio.strats("maxRet_Ineffic_2022" ,
                                               tags_inefficients_2022,
                                               crypto_returns,
                                               "2020/2021",
                                               "2022",
                                               pspec.lo.full,
                                               maxRet.spec,
                                               neg_to_zero = TRUE)


minCVaR_Ineffic_2022 <- build.portfolio.strats("minCVaR_Ineffic_2022" ,
                                             tags_inefficients_2022,
                                             crypto_returns,
                                             "2020/2021",
                                             "2022",
                                             pspec.lo.full,
                                             minCVaR.spec,
                                             neg_to_zero = TRUE)


EW_Ineffic_2022 <-  build.EW.portfolio("EW_Ineffic_2022" ,
                                       tags_inefficients_2022,
                                       crypto_returns,
                                       "2020/2021",
                                       "2022")


############################################################################################################

# Results




# Outras KPIs



table.modigliani <- function(R,period,riskfree,start_date = "2019-01-01",end_date = "2022-09-29"){
  sp500 <- quantmod::getSymbols("^GSPC", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)) %>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(sp500) <- c("Returns")
  
  nasdaq100 <- quantmod::getSymbols("^NDX", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))%>% 
    Ad() %>% 
    Return.calculate() %>% 
    na.omit()  
  
  colnames(nasdaq100) <- c("Returns")
  
  russel1000 <- quantmod::getSymbols("^RUI", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))%>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(russel1000) <- c("Returns")
  
  
  ibrx50 <- quantmod::getSymbols("^IBX50", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)) %>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(ibrx50) <- c("Returns")
  
  
  ibov <- quantmod::getSymbols("^BVSP", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))%>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(ibov) <- c("Returns")
  
  modig_nasdaq100 <- data.frame(portfolio.returns = Modigliani(R,nasdaq100[period],Rf=riskfree))
  rownames(modig_nasdaq100) <- paste("Modigliani-Nasdaq100 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_sp500 <- data.frame(portfolio.returns = Modigliani(R,sp500[period],Rf=riskfree))
  rownames(modig_sp500) <- paste("Modigliani-SP500 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_russel1000 <- data.frame(portfolio.returns = Modigliani(R,russel1000[period],Rf=riskfree))
  rownames(modig_russel1000) <- paste("Modigliani-Russel1000 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_ibrx50 <- data.frame(portfolio.returns = Modigliani(R,ibrx50[period],Rf=riskfree))
  rownames(modig_ibrx50) <- paste("Modigliani-IBrX50 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_ibov <- data.frame(portfolio.returns = Modigliani(R,ibov[period],Rf=riskfree))
  rownames(modig_ibov) <- paste("Modigliani-Ibovespa (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  df<- rbind(modig_sp500,
             modig_nasdaq100,
             modig_russel1000,
             modig_ibrx50,
             modig_ibov)
  return(df)
  
}


# Concating Results in a table



all_strategies_2021 = list(MVP_Effic_2021,
                      MVP_Ineffic_2021,
                      maxRet_Effic_2021,
                      maxRet_Ineffic_2021,
                      maxSR_Effic_2021,
                      maxSR_Ineffic_2021,
                      minCVaR_Effic_2021,
                      minCVaR_Ineffic_2021,
                      EW_Effic_2021,
                      EW_Ineffic_2021)

get.KPIs <- function(strategies_list,RF, year_file,folder_name='KPIs',mar= 0,prob=0.95){
  df_returns_all <- data.frame()
  df_dist_all <- data.frame()
  df_DR_all <- data.frame()
  df_DR_ratio_all <- data.frame()
  df_DD_all <- data.frame()
  df_DD_ratio_all <- data.frame()
  modig_all <- data.frame()
  
  for (s in strategies_list){
    print(s$name)
    
    df_returns <- table.AnnualizedReturns(s$R,Rf = RF)
    colnames(df_returns) <- s$name
    df_returns <- t(df_returns)
    df_returns_all <- rbind(df_returns_all,df_returns)
    
    df_dist <- table.Distributions(s$R)
    colnames(df_dist) <- s$name
    df_dist <- t(df_dist)
    df_dist_all <- rbind(df_dist_all,df_dist)
    
    df_DR <- table.DownsideRisk(s$R,MAR = 0, p=0.95)
    colnames(df_DR) <- s$name
    df_DR <- t(df_DR)
    df_DR_all <- rbind(df_DR_all,df_DR)
    
    df_DR_ratio <- table.DownsideRiskRatio(s$R,MAR = 0)
    colnames(df_DR_ratio) <- s$name
    df_DR_ratio <- t(df_DR_ratio)
    df_DR_ratio_all <- rbind(df_DR_ratio_all,df_DR_ratio)
    
    df_DD <- table.Drawdowns(s$R)
    #colnames(df_DD) <- s$name
    df_DD$strategy <- s$name
    #df_DD <- t(df_DD)
    df_DD_all <- rbind(df_DD_all,df_DD)
    
    df_DD_ratio <- table.DrawdownsRatio(s$R)
    colnames(df_DD_ratio) <- s$name
    df_DD_ratio <- t(df_DD_ratio)
    df_DD_ratio_all <- rbind(df_DD_ratio_all,df_DD_ratio)
    
    df_modig <- table.modigliani(s$R,period = s$test,riskfree = RF)
    colnames(df_modig) <- s$name
    df_modig <- t(df_modig)
    modig_all <- rbind(modig_all,df_modig)  
  }
  
  
  df_returns_all <-t(df_returns_all) 
  df_returns_all %>% write.csv(file  = paste(folder_name,'/Returns',year_file,".csv",sep=''),row.names = TRUE)
  
  df_returns_all %>% write.csv(file  = paste(folder_name,'/Returns',year_file,".csv",sep=''),row.names = TRUE)
  
  
    
  df_dist_all <- t(df_dist_all) 
  df_dist_all %>% write.csv(file  = paste(folder_name,'/Distributions',year_file,".csv",sep=''),row.names = TRUE)
  
  df_DR_all <- t(df_DR_all) 
  df_DR_all %>% write.csv(file  = paste(folder_name,'/DownsideRisk',year_file,".csv",sep=''),row.names = TRUE)
  
  df_DR_ratio_all <- t(df_DR_ratio_all) 
  df_DR_ratio_all %>% write.csv(file  = paste(folder_name,'/DownsideRiskRatio',year_file,".csv",sep=''),row.names = TRUE)
  
  #df_DD_all <- t(df_DD_all)
  df_DD_all %>% write.csv(file  = paste(folder_name,'/Drawdowns',year_file,".csv",sep=''),row.names = TRUE)
  
  df_DD_ratio_all <- t(df_DD_ratio_all) 
  df_DR_ratio_all %>% write.csv(file  = paste(folder_name,'/DrawdownsRatio',year_file,".csv",sep=''),row.names = TRUE)
  
  modig_all <- t(modig_all)
  modig_all %>% write.csv(file  = paste(folder_name,'/Modigliani',year_file,".csv",sep=''),row.names = TRUE)
  
  return(list(df_returns_all,df_dist_all,df_DR_all,df_DR_ratio_all,df_DD_all,df_DD_ratio_all,modig_all))
  
}

results2021 <- get.KPIs(all_strategies_2021,RF = rfUS_2021,year_file = '2021')
#results2021[[1]]

## 2022

all_strategies_2022 = list(MVP_Effic_2022,
                           MVP_Ineffic_2022,
                           maxRet_Effic_2022,
                           maxRet_Ineffic_2022,
                           maxSR_Effic_2022,
                           maxSR_Ineffic_2022,
                           minCVaR_Effic_2022,
                           minCVaR_Ineffic_2022,
                           EW_Effic_2022,
                           EW_Ineffic_2022)


results2022 <- get.KPIs(all_strategies_2022,RF = rfUS_2022,year_file = '2022')
results2022[[1]]


#Export Results


for(i in all_strategies_2021){
  i$R %>%  write.csv(paste("Returns/",i$name,".csv",sep=''))
}

for(i in all_strategies_2022){
  i$R %>%  write.csv(paste("Returns/",i$name,".csv",sep=''))
}

#Exporting charts of Performance
for(s in all_strategies_2021){
  file_name <-paste("Imagens/",s$name,'.png',sep = '')
  png(file_name)
  charts.PerformanceSummary(s$R,main = s$name)
  dev.off()
  
  
}


for(s in all_strategies_2022){
  file_name <-paste("Imagens/",s$name,'.png',sep = '')
  png(file_name)
  charts.PerformanceSummary(s$R,main = s$name)
  dev.off()
  
}

#chart.RelativePerformance


#########################################################################################

# Allocation by efficency


rolling60d_effic <- read.csv("Rolling_efficiency60d.csv")

rolling60d_effic <- xts(rolling60d_effic[,-1],order.by = as.POSIXct(rolling60d_effic[,1])) 


rolling90d_effic <- read.csv("Rolling_efficiency90d.csv")

rolling90d_effic <- xts(rolling90d_effic[,-1],order.by = as.POSIXct(rolling90d_effic[,1])) 


rolling120d_effic <- read.csv("Rolling_efficiency120d.csv")

rolling120d_effic <- xts(rolling120d_effic[,-1],order.by = as.POSIXct(rolling120d_effic[,1])) 


Month_effic60d <- rolling90d_effic[endpoints(rolling60d_effic,on = 'months')] %>% na.omit()
lag_Month_effic60d <- Month_effic60d %>% lag.xts() %>% na.omit()
tformat(lag_Month_effic60d) <- "%Y-%m"

Month_effic90d <- rolling90d_effic[endpoints(rolling90d_effic,on = 'months')] %>% na.omit()
lag_Month_effic90d <- Month_effic90d %>% lag.xts() %>% na.omit()
tformat(lag_Month_effic90d) <- "%Y-%m"


Month_effic120d <- rolling120d_effic[endpoints(rolling120d_effic,on = 'months')] %>% na.omit()
lag_Month_effic120d <- Month_effic120d %>% lag.xts() %>% na.omit()
tformat(lag_Month_effic120d) <- "%Y-%m"

Quarter_effic60d <- rolling90d_effic[endpoints(rolling60d_effic,on = 'quarter')]%>% na.omit()
lag_Quarter_effic60d <- Quarter_effic60d %>% lag.xts() %>% na.omit()
tformat(lag_Quarter_effic60d ) <- "%Y-%m"


Quarter_effic90d <- rolling90d_effic[endpoints(rolling90d_effic,on = 'quarter')]%>% na.omit()
lag_Quarter_effic90d <- Quarter_effic90d %>% lag.xts() %>% na.omit()
tformat(lag_Quarter_effic90d) <- "%Y-%m"


Quarter_effic120d <- rolling120d_effic[endpoints(rolling120d_effic,on = 'quarter')] %>% na.omit()
lag_Quarter_effic120d <- Quarter_effic120d %>% lag.xts() %>% na.omit()
tformat(lag_Quarter_effic90d) <- "%Y-%m"


rankrows <- function(x){
  ranks <- t(apply(x, 1, order,decreasing=FALSE)) # The more inefficient the asset , more the MDM will be . So the order need to be decreasing
  colnames(ranks) <- colnames(x)
  return(ranks)
}

get_effic_names <-function(x,nlargest=4){
    tickers <- c()
    
    for(i in 1:dim(x)[1]){
      
      row = x[i,] %>% as.data.frame() 
      colnames(row) <- "Rank"
      selected = row %>% filter(Rank <= 4)
      #print(selected)
      
      tickers[[length(tickers)+1]] <- list(rownames(selected))
      
    }
    
    return(tickers)
}
  

#get_effic_names(rankrows(Quarter_effic120d))[1] %>%  unlist()

  

## Apply all strategies by changing the portfolio constituents when the efficiency changes
## Train in the same period that the ranks were obtained
build_effifc_strategy() <- function(x,lag_effic, window_effic,split_period='months'){
  
  iteration_start <- format(index(lag_roll_effic)[1],"%Y-%m") # first date of series of testing periods 
  iteration_start_str <- paste(as.character(iteration_start),"/",sep='')
  
  x.tests <- split(x[iteration_start_str,], f= split_period)
  
 # discontinued ....
}
  

#-------------------------------------------------------------------------------------------------------------------
# Heuristic Strategies - similar to inverse volatility


inverse.inefficiency.weights <- function(x){
  inverse <- (1/x)
  sum_inverse <- rowSums(inverse)
  weights <- inverse/sum_inverse
  
  return(weights)
  
}


calcEfficiency <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  #effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  mdm <- (abs(b[["Hq"]][1] - 0.5) + abs(b[["Hq"]][9]-0.5))/2
  
  return(mdm)
  
}

#calcEfficiency(crypto_returns['2020/2021']$BNB)

#lapply(crypto_returns['2019/2020'],calcEfficiency)




##

mdm1 <- data.frame(BNB = 0.1239,
          TRX = 0.1560,
          ETH = 0.1230,
          BTC = 0.1893,
          LTC = 0.1485,
          ADA = 0.0778,
          XRP = 0.2530,
          DOGE = 0.2669)


weights_inverse_ineffic2021 <- inverse.inefficiency.weights(mdm1)


mdm2 <- data.frame(BNB = 0.0883,
                   TRX = 0.13105,
                   ETH = 0.1423,
                   BTC = 0.1535,
                   LTC = 0.1786,
                   ADA = 0.1857,
                   XRP = 0.2488,
                   DOGE = 0.2601)

weights_inverse_ineffic2022 <- inverse.inefficiency.weights(mdm2)


build.inverse.inefficency.strategy <- function(strats_name,initial_weights,return_series,train_period,test_period){
  
  R <- return_series[test_period,colnames(initial_weights)] #Ordering the columns and selecting the test period
  weights_vec <- as.numeric(initial_weights)
  
  InvInef_returns <- Return.portfolio(R,weights = weights_vec)
  
  return(list(
    name = strats_name,
    train = train_period,
    test = test_period,
    w  =  initial_weights,
    R = InvInef_returns
  ))
  
}

InvInef2021 <- build.inverse.inefficency.strategy("InvInef2021",weights_inverse_ineffic2021,crypto_returns,"2019/2020","2021")
InvInef2022 <- build.inverse.inefficency.strategy("InvInef2022",weights_inverse_ineffic2022,crypto_returns,"2021/2022","2022")




strategies_InvInef <- list(InvInef2021,InvInef2022)

for(i in strategies_InvInef){
  i$R %>%  write.csv(paste("Returns/",i$name,".csv",sep=''))
}


KPIs_InvInef2021 <- get.KPIs(list(InvInef2021),RF = rfUS_2021,year_file = '2021',folder_name='Heuristic KPIs')

KPIs_InvInef2022 <- get.KPIs(list(InvInef2022),RF = rfUS_2022,year_file = '2022',folder_name='Heuristic KPIs')


for(s in strategies_InvInef){
  file_name <-paste("Imagens/",s$name,'.png',sep = '')
  png(file_name)
  charts.PerformanceSummary(s$R,main = s$name)
  dev.off()
  
}


InvInef2021$w %>% write.csv("Pesos/InvInef2021.csv")

InvInef2022$w %>% write.csv("Pesos/InvInef2022.csv")


all_effic2022 = list(MVP_Effic_2022,
                     maxRet_Effic_2022,
                     maxSR_Effic_2022,
                     minCVaR_Effic_2022)

all_effic2021 = list(MVP_Effic_2021,
                     maxRet_Effic_2021,
                     maxSR_Effic_2021,
                     minCVaR_Effic_2021)

all_ineffic2021 = list(MVP_Ineffic_2021,
                     maxRet_Ineffic_2021,
                     maxSR_Ineffic_2021,
                     minCVaR_Ineffic_2021)

all_ineffic2022 = list(MVP_Ineffic_2022,
                       maxRet_Ineffic_2022,
                       maxSR_Ineffic_2022,
                       minCVaR_Ineffic_2022)



effic2021weights <- NULL
for(s in all_effic2021){
  w_df <- as.data.frame(s$w)
  colnames(w_df) <- c(s$name)
  w_df[,1] <- round(w_df[,1],4)
  w_df <- w_df %>% t()
  effic2021weights <- rbind(effic2021weights,w_df)
 
}

effic2021weights %>%  write.csv("Pesos/effic2021weights.csv")

effic2022weights <- NULL
for(s in all_effic2022){
  w_df <- as.data.frame(s$w)
  colnames(w_df) <- c(s$name)
  w_df[,1] <- round(w_df[,1],4)
  w_df <- w_df %>% t()
  effic2022weights <- rbind(effic2022weights,w_df)
  
}

effic2022weights %>%  write.csv("Pesos/effic2022weights.csv")


ineffic2022weights <- NULL
for(s in all_ineffic2022){
  w_df <- as.data.frame(s$w)
  colnames(w_df) <- c(s$name)
  w_df[,1] <- round(w_df[,1],4)
  w_df <- w_df %>% t()
  ineffic2022weights <- rbind(ineffic2022weights,w_df)
  
}

ineffic2022weights %>%  write.csv("Pesos/ineffic2022weights.csv")


ineffic2021weights <- NULL
for(s in all_ineffic2021){
  w_df <- as.data.frame(s$w)
  colnames(w_df) <- c(s$name)
  w_df[,1] <- round(w_df[,1],4)
  w_df <- w_df %>% t()
  ineffic2021weights <- rbind(ineffic2021weights,w_df)
  
}
ineffic2021weights %>%  write.csv("Pesos/ineffic2021weights.csv")


#################################################################################################

#Teste Retornos.

ibov <- quantmod::getSymbols("^BVSP", auto.assign = FALSE, from = as.Date("2021-01-01"), to = as.Date("2021-12-31"))%>%
  Ad() %>%
  Return.calculate() %>%
  na.omit()

colnames(ibov) <- c("Returns")

ibov$Returns


result1 = prod(1 + ibov$Returns)^(252/length(ibov$Return)) - 1
print(result1)
ra1 = Return.annualized(ibov$Returns)
print(ra1)

result2 = prod(1 + MVP_Effic_2021$R)^(252/length(MVP_Effic_2021$R)) - 1
print(result1)
ra2 = Return.annualized(MVP_Effic_2021$R)
print(ra2)


result3 = prod(1 + MVP_Effic_2022$R)^(252/length(MVP_Effic_2022$R)) - 1
print(result3)
ra3 = Return.annualized(MVP_Effic_2022$R)
print(ra3)


Return.annualized(crypto_returns['2021'])


#################################################################################################



series_effics2021 <- merge(MVP_Effic_2021$R,
                           maxRet_Effic_2021$R,
                           maxSR_Effic_2021$R,
                           minCVaR_Effic_2021$R,
                           EW_Effic_2021$R, all = TRUE)

colnames(series_effics2021) <- c(MVP_Effic_2021$name,
                                 maxRet_Effic_2021$name,
                                 maxSR_Effic_2021$name,
                                 minCVaR_Effic_2021$name,
                                 EW_Effic_2021$name)

series_effics2021 <- series_effics2021 %>% fortify.zoo %>% as.tibble()

# series_effics2021$Type <- "Efficient"

series_ineffic2021 <- merge(MVP_Ineffic_2021$R,
                           maxRet_Ineffic_2021$R,
                           maxSR_Ineffic_2021$R,
                           minCVaR_Ineffic_2021$R,
                           EW_Ineffic_2021$R, all = TRUE)

colnames(series_ineffic2021) <- c(MVP_Ineffic_2021$name,
                                 maxRet_Ineffic_2021$name,
                                 maxSR_Ineffic_2021$name,
                                 minCVaR_Ineffic_2021$name,
                                 EW_Ineffic_2021$name)

series_ineffic2021 <- series_ineffic2021 %>% fortify.zoo %>% as.tibble()
# series_ineffic2021$Type <- "Inefficient"



series_invinef2021 <- InvInef2021$R
colnames(series_invinef2021) <- c(InvInef2021$name)
series_invinef2021 <- series_invinef2021 %>% fortify.zoo %>% as.tibble()

# series_invinef2021$Type <- "Inverse Inefficiency"


series_returns2021 <- join_all(list(series_effics2021,series_ineffic2021,series_invinef2021), by='Index', type='left') 


series_returns2021 <- series_returns2021 %>%  dplyr::rename("Data" = Index )

series_returns2021[,-1] <- ((series_returns2021[,-1] + 1) %>%  cumprod()) -1


series_returns2021 %>%
  gather(key = "Strat", value = "return", -Data) %>% 
  ggplot(aes(x=Data,y=return)) +
  geom_line(aes(color = Strat), size = 1) +
  labs(x="Data",y='Retorno Acumulado',title = 'Desempenho das carteiras em 2021')+
  ggdark::dark_theme_gray() 
  

### MVP pairs

series_returns2021 %>%
  gather(key = "Strat", value = "return", -Data) %>% 
  filter(Strat == MVP_Effic_2021$name | Strat == MVP_Ineffic_2021$name ) %>% 
  ggplot(aes(x=Data,y=return)) +
  geom_line(aes(color = Strat), size = 1) +
  labs(x="Data",y='Retorno Acumulado',title = 'Desempenho das carteiras em 2021')+
  ggdark::dark_theme_gray() 

series_returns2021 %>%
  gather(key = "Strat", value = "return", -Data) %>% 
  filter(Strat == maxRet_Effic_2021$name | Strat == maxRet_Ineffic_2021$name ) %>% 
  ggplot(aes(x=Data,y=return)) +
  geom_line(aes(color = Strat), size = 1) +
  labs(x="Data",y='Retorno Acumulado',title = 'Desempenho das carteiras em 2021')+
  ggdark::dark_theme_gray() 


series_returns2021 %>%
  gather(key = "Strat", value = "return", -Data) %>% 
  filter(Strat == minCVaR_Effic_2021$name | Strat == minCVaR_Ineffic_2021$name ) %>% 
  ggplot(aes(x=Data,y=return)) +
  geom_line(aes(color = Strat), size = 1) +
  labs(x="Data",y='Retorno Acumulado',title = 'Desempenho das carteiras em 2021')+
  ggdark::dark_theme_gray() 



#2022
series_effics2022 <- merge(MVP_Effic_2022$R,
                           maxRet_Effic_2022$R,
                           maxSR_Effic_2022$R,
                           minCVaR_Effic_2022$R,
                           EW_Effic_2022$R, all = TRUE)

colnames(series_effics2022) <- c(MVP_Effic_2022$name,
                                 maxRet_Effic_2022$name,
                                 maxSR_Effic_2022$name,
                                 minCVaR_Effic_2022$name,
                                 EW_Effic_2022$name)

series_effics2022 <- series_effics2022 %>% fortify.zoo %>% as.tibble()
# series_effics2022$Type <- "Efficient"



series_ineffic2022 <- merge(MVP_Ineffic_2022$R,
                            maxRet_Ineffic_2022$R,
                            maxSR_Ineffic_2022$R,
                            minCVaR_Ineffic_2022$R,
                            EW_Ineffic_2022$R, all = TRUE)



colnames(series_ineffic2022) <- c(MVP_Ineffic_2022$name,
                                  maxRet_Ineffic_2022$name,
                                  maxSR_Ineffic_2022$name,
                                  minCVaR_Ineffic_2022$name,
                                  EW_Ineffic_2022$name)

series_ineffic2022 <- series_ineffic2022 %>% fortify.zoo %>% as.tibble()
# series_ineffic2022$Type <- "Inefficient"


series_invinef2022 <- InvInef2022$R
colnames(series_invinef2022) <- c(InvInef2022$name)
series_invinef2022 <- series_invinef2022 %>% fortify.zoo %>% as.tibble()

# series_invinef2022$Type <- "Inverse Inefficiency"

series_returns2022 <- join_all(list(series_effics2022,series_ineffic2022,series_invinef2022), by='Index', type='left') 


series_returns2022 <- series_returns2022 %>%  dplyr::rename("Data" = Index )

series_returns2022[,-1] <- ((series_returns2022[,-1] + 1) %>%  cumprod()) -1


series_returns2022 %>%
  gather(key = "Strat", value = "return", -Data) %>% 
  ggplot(aes(x=Data,y=return)) +
  geom_line(aes(color = Strat), size = 1) +
  labs(x="Data",y='Retorno Acumulado',title = 'Desempenho das carteiras em 2022')+
  ggdark::dark_theme_gray() 












