
# Load packages:

library(MFDFA)
library(readxl)
library(plotly)
library(nonlinearTseries)

# Set work space and load data:

#setwd("C:/Users/Leandro/Desktop/Livre-Docencia/Codigos/MF-DFA")
precos <- read_excel("Data.xlsx")

N <- dim(precos)[1] 
n <- dim(precos)[2] 

Mprices = matrix(0,N,n-1)
for(i in 1:N){
  for(j in 2:n){
    Mprices[i,j-1]=as.numeric(precos[i,j])
  }
}

# Compute returns:

retornos <- diff(log(Mprices))

# ---------------------------------------

# Set MF-DFA parameters:

scale <- 10:(N/4) # literature suggestion
q <- -4:4
m <- 1 # degree polinomial (avoid overfitting)

# ---------------------------------------

# RUN MF-DFA:


Resultados = matrix(0,n-1,1)

for(i in 1:(n-1)){
  
  b <- MFDFA(retornos[,i], scale, m, q)

  Resultados[i,1] = max(b[["Hq"]]) - min(b[["Hq"]]) #Delta h
}




