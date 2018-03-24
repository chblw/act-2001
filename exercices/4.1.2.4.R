#Solution 4.1.2.4

p <- 0.2
beta <- 1/2500

#a)

EM <- p
EB <- 1/beta
VM <- p*(1-p)
VB <- (1/beta)^2

EX <-EM*EB
VX <- EM*VB+VM*EB^2


#b)

Fx <- function(x){
  (1-p) + p * pexp(x,beta)
}

VaRX <- function(k){
  if (k <= (1-p))
    0
  else
    optimize(function(x) abs(Fx(x) - k),c(0,10000))$minimum
}

TVaRX <- function(k){
  V <- VaRX(k)
  p*((exp(-beta*V)/beta) + V*exp(-beta*V))*(1/(1-k))
  
}

VaRX(0.5);TVaRX(0.5)
VaRX(0.99);TVaRX(0.99)


#c) n = 3 donc M suit une binomiale(n=3,p)

n<-3
p<-0.2

FS <- function(x){ 
  dbinom(0,n,p) + sum(sapply(1:3, function(i) dbinom(i,n,p) * pgamma(x,i,beta)))
}

VaR <- function(k){
  if (k <= dbinom(0,n,p))
    0
  else
    optimize(function(x) abs(FS(x) - k), c(0,20000))$minimum
}

TVaR <- function(k){ 
  V <- VaR(k)
  sum(sapply(1:3, function(i) (i/beta) * (1-pgamma(V,i+1,beta)) * dbinom(i,n,p))) * (1/(1-k))
}








