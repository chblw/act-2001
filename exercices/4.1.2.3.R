#Solution 4.1.2.3

r <- 2
q <- 0.997
alpha <- 2
beta <- 1/5000

EM <- (r*(1-q))/q
EB <- alpha/beta

#a)
EX <- EM*EB

#b)

Fx <- function(x){ #Fonction cumulative de X
  dnbinom(0,r,q) + sum(sapply(1:1000, function(k) dnbinom(k,r,q) * pgamma(x,alpha*k,beta)))
}

VaRX <- function(k){
  if (k <= dnbinom(0,r,q))
    0
  else
    optimize(function(x) abs(Fx(x) - k), c(0,10000))$minimum
}
VaR99X <- VaRX(0.99)

TVaR <- function(k){ 
  V <- VaRX(k)
  sum(sapply(1:1000, function(i) i*(alpha/beta) * (1-pgamma(V,alpha*i+1,beta)) * dnbinom(i,r,q))) * (1/(1-k))
}

TVaR99X <- TVaRX(0.99)
#c) Le problème est le meme, sauf que Sn suit une BinNegComp(r*n,q;FB).

Esperance.Sn <- function(n){
  (n*r*(1-q))/q
}

Esperance.Sn(10);Esperance.Sn(100);Esperance.Sn(1000)

FSn <- function(x,n){ #Fonction cumulative de Sn
  dnbinom(0,r*n,q) + sum(sapply(1:1000, function(k) dnbinom(k,r*n,q) * pgamma(x,alpha*k,beta)))
}

VaRSn <- function(k,n,borne){
  if (k <= dnbinom(0,r*n,q))
    0
  else
    optimize(function(x) abs(FSn(x,n) - k), c(0,borne))$minimum
}

VaR99Sn10 <- VaRSn(0.99,10,100000) #borne à 100000
VaR99Sn100 <- VaRSn(0.99,100,100000) #borne à 100000
VaR99Sn1000 <- VaRSn(0.99,1000,1000000) #borne à 1000000

TVaRSn <- function(k,n,borne){ 
  V <- VaRSn(k,n,borne)
  sum(sapply(1:1000, function(i) i*(alpha/beta) * (1-pgamma(V,alpha*i+1,beta)) * dnbinom(i,r*n,q))) * (1/(1-k))
}

TVaR99Sn10 <- TVaRSn(0.99,10,100000)
TVaR99Sn100 <- TVaRSn(0.99,100,100000)
TVaR99Sn1000 <- TVaRSn(0.99,1000,1000000)

#c) 

BEVaR10 <- 10 * VaR99X - VaR99Sn10
BEVaR100 <- 100 * VaR99X - VaR99Sn100
BEVaR1000 <- 1000 * VaR99X - VaR99Sn1000

#d)

BETVaR10 <- 10 * TVaR99X - TVaR99Sn10
BETVaR100 <- 100 * TVaR99X - TVaR99Sn100
BETVaR1000 <- 1000 * TVaR99X - TVaR99Sn1000


#e)

prime.a10 <- (1/10)*VaR99Sn10
prime.a100 <- (1/100)*VaR99Sn100
prime.a1000 <- (1/1000)*VaR99Sn1000

prime.b10 <- (1/10)*TVaR99Sn10
prime.b100 <- (1/100)*TVaR99Sn100
prime.b1000 <- (1/1000)*TVaR99Sn1000


