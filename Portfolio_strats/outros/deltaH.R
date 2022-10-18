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

precos <- read_excel("Data.xlsx")

precos_xts <- as.xts(precos[-1],order.by = as.POSIXct(precos$Data))

returns <- na.omit(Return.calculate(precos_xts))



calcEfficiency <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  
  return(effic)
  
}


#calcEfficiency(returns$PETR3)

rollapply(returns$PETR3[1:100],60,calcEfficiency)

