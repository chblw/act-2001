#Numero 4.1.2.1

r <- 0.5
q <- 5/6
alpha <- 0.5
beta <- 1/10000
#a) EX <- EM * EB
EM <- (r*(1-q))/q
EB <- alpha/beta
EX <- EM*EB

#b)
Fx <- function(x){
  dnbinom(0,size=r,prob=q) + sum(sapply(1:1000, function(i) dnbinom(i,size=r,prob=q) * pgamma(x,i * alpha, beta)))
}

Fx(10000)

#c)
Fs <- function(x){
  dnbinom(0,r*200,prob=q) + sum(sapply(1:1000, function(i) dnbinom(i,size=r*200,prob=q) * pgamma(x,i * alpha, beta)))
}
Fs(10000*200)

#d)
VaRX <- function(k){
  if (k <= dnbinom(0,r,q))
    0
  else
    optimize(function(x) abs(Fx(x) - k),c(0,80000))$minimum
}
VaRX(0.5);VaRX(0.99)

#e)
VaRS <- function(k){
  if (k <= dnbinom(0,r*200,q))
    0
  else
    optimize(function(x) abs(Fs(x) - k),c(0,800000))$minimum
}
VaRS(0.5);VaRS(0.99)

#f)

TVaRX05 <- (1/(1-0.5)) * EX #VaRX0.5 = 0
V <- VaRX(0.99)
TVaRX99 <- (1/(1-0.99)) * sum(sapply(1:1000, function(i) dnbinom(i,size=0.5,prob=5/6) * 0.5*i*10000*(1-pgamma(V,0.5*i+1,1/10000))))

#g)
VS <- VaRS(0.5)
TVaRS05 <- (1/(1-0.5)) * sum(sapply(1:1000, function(i) dnbinom(i,size=0.5*200,prob=5/6) * 0.5*i*10000*(1-pgamma(VS,0.5*i+1,1/10000))))
VSS <- VaRS(0.99)
TVaRS99 <- (1/(1-0.99)) * sum(sapply(1:1000, function(i) dnbinom(i,size=0.5*200,prob=5/6) * 0.5*i*10000*(1-pgamma(VSS,0.5*i+1,1/10000))))

#h) Bénéfice de mutualisation selon la VaR : sum_ VaR(X_i) - VaR(S)

(BEVaR <- 200 * VaRX(0.5) - VaRS(0.5)) 

#VaRS est plus grand : Benefice a mutualiser: Non

#i)

(BEEVaR <- 200 * VaRX(0.99) - VaRS(0.99))
#Benefice a mutualiser: oui


#j)

(BETVaR <- 200 * TVaRX05 - TVaRS05)
#Benefice a mutualiser: oui
  
#k)

(BEETVaR <- 200 * TVaRX99 - TVaRS99)
#Benefice a mutualiser: oui

#test
