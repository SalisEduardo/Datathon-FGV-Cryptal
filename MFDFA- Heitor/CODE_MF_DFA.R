

# Load packages:

library(MFDFA)
library(readxl)


# Load data (daily price):

#setwd("C:/Users/Leandro/Desktop/MaterialMFDFA")

precos <- read_excel("Data.xlsx")

# Compute returns:

retornos <- diff(log(precos$ABEV3))

plot(retornos,type = "l")

# Generate random walk serie:

N <- length(retornos)

p <- 40 # exemple of initial price

for(i in 2:(N+1)){
  p[i] = p[i-1] + rnorm(1,0,0.1)
}

plot(p,type = "l")
RW_ret = diff(log(p))
plot(RW_ret,type = "l")

# ---------------------------------------

# Set MF-DFA parameters:

scale <-10:(N/4) # literature suggestion
q <- -4:4
m <- 1 # degree polinomial  

# ---------------------------------------

# RUN MF-DFA:

# For random walk:

b <- MFDFA(RW_ret, scale, m, q)

reset <- function(){
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)}
poly_fit<-function(x,y,n){
  formule<-lm(as.formula(paste('y~',paste('I(x^',1:n,')', sep='',collapse='+'))))
  res1<-coef(formule)
  poly.res<-res1[length(res1):1]
  allres<-list(polyfit=poly.res, model1=formule)
  return(allres)}

dev.new()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),heights=c(4, 4))
## b : mfdfa output
par(mai=rep(0.8, 4))
## 1st plot: Scaling function order Fq (q-order RMS)
p1<-c(1,which(q==0),which(q==q[length(q)]))
plot(log2(scale),log2(b$Fqi[,1]), pch=16, col=1, axes = F, xlab = "s (days)",
     ylab=expression('log'[2]*'(F'[q]*')'), cex=1, cex.lab=1.6, cex.axis=1.6,
     main= "Fluctuation function Fq",
     ylim=c(min(log2(b$Fqi[,c(p1)])),max(log2(b$Fqi[,c(p1)]))))
lines(log2(scale),b$line[,1], type="l", col=1, lwd=2)
grid(col="midnightblue")
axis(2)
lbl<-scale[c(1,floor(length(scale)/8),floor(length(scale)/4),
             floor(length(scale)/2),length(scale))]
att<-log2(lbl)
axis(1, at=att, labels=lbl)
for (i in 2:3){
  k<-p1[i]
  points(log2(scale), log2(b$Fqi[,k]), col=i,pch=16)
  lines(log2(scale),b$line[,k], type="l", col=i, lwd=2)
}
legend("bottomright", c(paste('q','=',q[p1] , sep=' ' )),cex=1,lwd=c(2,2,2),
       bty="n", col=1:3)
## 2nd plot: q-order Hurst exponent
plot(q, b$Hq, col=1, axes= F, ylab=expression('h'[q]), pch=16, cex.lab=1.8,
     cex.axis=1.8, main="Hurst exponent", ylim=c(min(b$Hq),max(b$Hq)))
grid(col="midnightblue")
axis(1, cex=4)
axis(2, cex=4)
## 3rd plot: q-order Mass exponent
plot(q, b$tau_q, col=1, axes=F, cex.lab=1.8, cex.axis=1.8,
     main="Mass exponent",
     pch=16,ylab=expression(tau[q]))
grid(col="midnightblue")
axis(1, cex=4)
axis(2, cex=4)
## 4th plot: Multifractal spectrum
plot(b$spec$hq, b$spec$Dq, col=1, axes=F, pch=16, main="Multifractal spectrum",
     ylab=bquote("f ("~alpha~")"),cex.lab=1.8, cex.axis=1.8,
     xlab=bquote(~alpha))
grid(col="midnightblue")
axis(1, cex=4)
axis(2, cex=4)
x1=b$spec$hq
y1=b$spec$Dq
rr<-poly_fit(x1,y1,4)
mm1<-rr$model1
mm<-rr$polyfit

x2<-seq(0,max(x1)+1,0.01)
curv<-mm[1]*x2^4+mm[2]*x2^3+mm[3]*x2^2+mm[4]*x2+mm[5]
lines(x2,curv, col="red", lwd=2)
reset()
legend("top", legend=" ", bty="n", cex=2)
## End(Not run)


# ----------------------------------------------------------

MDM_RW = (abs(b[["Hq"]][1] - 0.5) + abs(b[["Hq"]][9]-0.9))/2
MDM_RW

# ----------------------------------------------------------


# RUN MF-DFA:

# For AMBEV:

c <- MFDFA(retornos, scale, m, q)

reset <- function(){
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)}
poly_fit<-function(x,y,n){
  formule<-lm(as.formula(paste('y~',paste('I(x^',1:n,')', sep='',collapse='+'))))
  res1<-coef(formule)
  poly.res<-res1[length(res1):1]
  allres<-list(polyfit=poly.res, model1=formule)
  return(allres)}

dev.new()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),heights=c(4, 4))
## b : mfdfa output
par(mai=rep(0.8, 4))
## 1st plot: Scaling function order Fq (q-order RMS)
p1<-c(1,which(q==0),which(q==q[length(q)]))
plot(log2(scale),log2(c$Fqi[,1]), pch=16, col=1, axes = F, xlab = "s (days)",
     ylab=expression('log'[2]*'(F'[q]*')'), cex=1, cex.lab=1.6, cex.axis=1.6,
     main= "Fluctuation function Fq",
     ylim=c(min(log2(c$Fqi[,c(p1)])),max(log2(c$Fqi[,c(p1)]))))
lines(log2(scale),c$line[,1], type="l", col=1, lwd=2)
grid(col="midnightblue")
axis(2)
lbl<-scale[c(1,floor(length(scale)/8),floor(length(scale)/4),
             floor(length(scale)/2),length(scale))]
att<-log2(lbl)
axis(1, at=att, labels=lbl)
for (i in 2:3){
  k<-p1[i]
  points(log2(scale), log2(c$Fqi[,k]), col=i,pch=16)
  lines(log2(scale),c$line[,k], type="l", col=i, lwd=2)
}
legend("bottomright", c(paste('q','=',q[p1] , sep=' ' )),cex=1,lwd=c(2,2,2),
       bty="n", col=1:3)
## 2nd plot: q-order Hurst exponent
plot(q, c$Hq, col=1, axes= F, ylab=expression('h'[q]), pch=16, cex.lab=1.8,
     cex.axis=1.8, main="Hurst exponent", ylim=c(min(c$Hq),max(c$Hq)))
grid(col="midnightblue")
axis(1, cex=4)
axis(2, cex=4)
## 3rd plot: q-order Mass exponent
plot(q, c$tau_q, col=1, axes=F, cex.lab=1.8, cex.axis=1.8,
     main="Mass exponent",
     pch=16,ylab=expression(tau[q]))
grid(col="midnightblue")
axis(1, cex=4)
axis(2, cex=4)
## 4th plot: Multifractal spectrum
plot(c$spec$hq, c$spec$Dq, col=1, axes=F, pch=16, main="Multifractal spectrum",
     ylab=bquote("f ("~alpha~")"),cex.lab=1.8, cex.axis=1.8,
     xlab=bquote(~alpha))
grid(col="midnightblue")
axis(1, cex=4)
axis(2, cex=4)
x1=c$spec$hq
y1=c$spec$Dq
rr<-poly_fit(x1,y1,4)
mm1<-rr$model1
mm<-rr$polyfit

x2<-seq(0,max(x1)+1,0.01)
curv<-mm[1]*x2^4+mm[2]*x2^3+mm[3]*x2^2+mm[4]*x2+mm[5]
lines(x2,curv, col="red", lwd=2)
reset()
legend("top", legend=" ", bty="n", cex=2)
## End(Not run)

# ----------------------------------------------------------

MDM_AMBEV = (abs(c[["Hq"]][1] - 0.5) + abs(c[["Hq"]][9]-0.9))/2
MDM_AMBEV

# ----------------------------------------------------------


