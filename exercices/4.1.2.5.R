n <- 2
p <- 1/2
alpha <- 1.2
beta <- 1/10
EM <- n*p
EB <- alpha/beta


#a) S suit une BinomComp(n*3,1/2)

#b)

ES <- (n*3*p)*(alpha/beta)

VS <- (n*3*p)*(alpha/(beta^2)) + (n*3*p*(1-p)) * (alpha/beta)^2

#c)
Fs <- function(x){
    dbinom(0,3*n,p) + sum(sapply(1:6, function(i) dbinom(i,3*n,p) * pgamma(x,alpha*i,beta)))
}

Fs(0);Fs(10);Fs(50);Fs(100)

#d)

VaR <- function(k){ ##Il faut jouer avec les bornes
  if (k <= dbinom(0,3*n,p))
    0
  else
    optimize(function(x) abs(Fs(x) - k), c(0,1000))$minimum
}


TVaR <- function(k){ 
  V <- VaR(k)
  sum(sapply(1:1000, function(i) i*(alpha/beta) * (1-pgamma(V,alpha*i+1,beta)) * dbinom(i,3*n,p))) * (1/(1-k))
}


