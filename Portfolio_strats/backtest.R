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


rfUS_2021<- RiskFree_US['2021'] 

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
    add.objective(type = "return",name = "mean") %>% 
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
                                            mvp.spec,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


maxRet_Effic_2021 <-  build.portfolio.strats("maxRet_Effic_2021" ,
                                             tags_efficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021",
                                             pspec.lo.full,
                                             mvp.spec,
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
                                            mvp.spec,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


maxRet_Ineffic_2021 <-  build.portfolio.strats("maxRet_Ineffic_2021" ,
                                             tags_inefficients_2021,
                                             crypto_returns,
                                             "2019/2020",
                                             "2021",
                                             pspec.lo.full,
                                             mvp.spec,
                                             neg_to_zero = TRUE)


minCVaR_Ineffic_2021 <- build.portfolio.strats("minCVaR_Ineffic_2021" ,
                                             tags_inefficients_2021,
                                             crypto_returns,
                                             "2020/2021",
                                             "2022",
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
                                            mvp.spec,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


maxRet_Effic_2022 <-  build.portfolio.strats("maxRet_Effic_2022" ,
                                             tags_efficients_2022,
                                             crypto_returns,
                                             "2020/2021",
                                             "2022",
                                             pspec.lo.full,
                                             mvp.spec,
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
                                              mvp.spec,
                                              maxSharp = TRUE,
                                              neg_to_zero = TRUE)


maxRet_Ineffic_2022 <-  build.portfolio.strats("maxRet_Ineffic_2022" ,
                                               tags_inefficients_2022,
                                               crypto_returns,
                                               "2020/2021",
                                               "2022",
                                               pspec.lo.full,
                                               mvp.spec,
                                               neg_to_zero = TRUE)


minCVaR_Ineffic_2022 <- build.portfolio.strats("minCVaR_Ineffic_2022" ,
                                             tags_efficients_2022,
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

#PainIndex
#PainRatio
#OmegaSharpeRatio
#OmegaExcessReturn
#RachevRatio
#Probabilistic Sharpe Ratio

table.Distributions(MVP_Effic_2021$R)

table.DownsideRisk(MVP_Effic_2021$R, MAR = 0, p=0.95) # riskfree diverge do tamanho

table.DownsideRiskRatio(MVP_Effic_2021$R, MAR = 0)

table.Drawdowns(MVP_Effic_2021$R)

table.DrawdownsRatio(MVP_Effic_2021$R)

table.InformationRatio

#table.CaptureRatios e table.InformationRatio - benchmarks diferem do tamanho da série
  # Necessario achar um bench para crypto


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

get.KPIs <- function(strategies_list,riskfree, year_file,folder_name='KPI',mar= 0,prob=0.95){
  df_returns_all <- data.frame()
  df_dist_all <- data.frame()
  df_DR_all <- data.frame()
  df_DR_ratio_all <- data.frame()
  df_DD_all <- data.frame()
  df_DD_ratio_all <- data.frame()
  modig_all <- data.frame()
  
  for (s in strategies_list){
    print(s$name)
    
    df_returns <- table.AnnualizedReturns(s$R,Rf = rfUS_2021)
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
    
    df_modig <- table.modigliani(s$R,period = s$test,riskfree = rfUS_2021)
    colnames(df_modig) <- s$name
    df_modig <- t(df_modig)
    modig_all <- rbind(modig_all,df_modig)  
  }
  
  
  df_returns_all <-t(df_returns_all) 
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

results2021 <- get.KPIs(all_strategies_2021,riskfree = rfUS_2021,year_file = '2021')
results2021[[1]]

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


results2022 <- get.KPIs(all_strategies_2022,riskfree = rfUS_2022)



#Export Results


#df_results2021 %>% as.data.frame() %>%   write.csv(file  = "Results2021.csv",row.names = TRUE)

#df_results2022 %>% as.data.frame() %>%   write.csv(file  = "Results2022.csv",row.names = TRUE)


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
  ranks <- t(apply(x, 1, order))
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
  

get_effic_names(rankrows(Quarter_effic120d))[1] %>%  unlist()

  
#as.difftime
#seq.date
  

library(bizdays)

build_effifc_strategy() <- function(x,lag_effic, window_effic,split_period='months'){
  
  iteration_start <- format(index(lag_roll_effic)[1],"%Y-%m") # first date of series of testing periods 
  iteration_start_str <- paste(as.character(iteration_start),"/",sep='')
  
  x.tests <- split(x[iteration_start_str,], f= split_period)
  
 
}
  

dt <- index(lag_Month_effic60d)[1] 

dt<- format(dt,"%Y-%m")

cryp_teste <- crypto_returns[dt,]

dt2 <- index(cryp_teste)[1]


dtx <- offset(dt2,-60) %>%  as.character()

dty <- offset(dt2,-1) %>%  as.character()

paste(dtx,dty,sep = '/')



for(dt in 1:length(index(lag_Month_effic90d))){
  print(index(lag_Month_effic90d)[dt] %>%  as.character())
}


