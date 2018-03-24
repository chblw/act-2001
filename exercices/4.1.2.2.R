#Numero 4.1.2.2

lambda <- 0.005
beta <- 1/1000

#a)
Fx <- function(x){
  dpois(0,lambda) + sum(sapply(1:1000, function(i) dpois(i,lambda) * pgamma(x,i,beta)))
}

Fx(5)
Fx(0);Fx(10)

VaRX <- function(k){
  if (k <= dpois(0,lambda))
    0
  else
    optimize(function(x) abs(Fx(x) - k), c(0,10000))$minimum
}

VaR99X <- VaRX(0.99)


TVaRX <- function(k){ 
  V <- VaRX(k)
  sum(sapply(1:1000, function(i) (i/beta) * (1-pgamma(V,i+1,beta)) * dpois(i,lambda))) * (1/(1-k))
}

TVaR99X <- TVaRX(0.99)


#b)

n<-1000
#S_n suit une PoisComp(n*lambda)

Fsn <- function(x){
  dpois(0,n*lambda) + sum(sapply(1:1000,function(i) dpois(i,n*lambda) * pgamma(x,i,beta)))
}

#P(Wn < x) = P(Sn < n*x)

Fsn(n*5)
Fsn(n*0);Fsn(n*10)


VaRSn <- function(k){
  if (k <= dpois(0,n*lambda))
    0
  else
    optimize(function(x) abs(Fsn(x) - k), c(0,100000))$minimum
}

#VaRWn = (1/n)VaRSn

VaR99Wn <- (1/n)*VaRSn(0.99)

TVaRSn <- function(k){
  V <- VaRSn(k)
  sum(sapply(1:1000, function(i) (i/beta) * (1-pgamma(V,i+1,beta)) * dpois(i,lambda*n))) * (1/(1-k))
}

#TVaRWn = (1/n)*TVaRSn

TVaR99Wn <- (1/n)*TVaRSn(0.99)

#c)

BEVaR <- VaR99X - VaR99Wn #Aucun benefice a mutualiser 
BETVaR <- TVaR99X - TVaR99Wn #Oui, benefice a mutualiser



