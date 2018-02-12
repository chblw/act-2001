#Dépannage 15 février
#Auteur: Julien Bessette

#Valeur des paramètres
lambda <- 2
alpha <- 1/2
beta <- 1/200

#a)

EM <- lambda #Espérance de M
VarM <- lambda #Variance de M
EB <- alpha/beta  #Espérance de B
VarB <- alpha/(beta^2) #Variance de B
EX <- EM * EB #Espérance de X
VarX <- EM * VarB + VarM * EB^2 #Variance de X

k <- 100 * 0:10 #Vecteur de k, sera utile pour les lettres de b) a e)
#b)

Fx <- function(x){ #Fonction cumulative de X
  dpois(0,lambda) + sum(sapply(1:1000, function(k) dpois(k,2) * pgamma(x,alpha*k,beta)))
}
sapply(k, function(k) 1 - Fx(k)) #Valeurs exactes de 1-FX

#c) 

stop_loss <- function(x){
  term1 <- sum(sapply(1:1000, function(k) k*(alpha/beta) * (1-pgamma(x,alpha*k+1,beta)) * dpois(k,lambda)))
  term2 <- x * (1 - Fx(x))
  term1 - term2
}
sapply(k,stop_loss) #Valeurs exactes de la stop loss

#d)

TVaRFx <- function(x){ #On donne en argument les valeurs de k=0,100,...,1000
  kappa <- Fx(x)
  sum(sapply(1:1000, function(i) i*(alpha/beta) * (1-pgamma(x,alpha*i+1,beta)) * dpois(i,lambda))) * (1/(1-kappa))
}
sapply(k, function(x) TVaRFx(x)) #Valeurs exactes de la TVaR

#e)

#voir term1 du c)

#f)

VaR <- function(k){
  if (k <= dpois(0,lambda))
    0
  else
    optimize(function(x) abs(Fx(x) - k), c(0,10000))$minimum
}

kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
sapply(kappa,function(x) VaR(x))

#g)

TVaR <- function(x){ #Cette fois ci on donne en argument la valeur de kappa
  V <- VaR(x)
  sum(sapply(1:1000, function(k) k*(alpha/beta) * (1-pgamma(V,alpha*k+1,beta)) * dpois(k,lambda))) * (1/(1-x))
}
sapply(kappa, function(x) TVaR(x))

#h)

set.seed(2018)

nsim <- 1000000
u <- runif(nsim * qpois(0.99,lambda))
M <- numeric(nsim)
x <- numeric(nsim)
count <- 1

for(i in 1:nsim){
  M[i] <- qpois(u[count], lambda)
  
  if(M[i] > 0)
  {
      x[i] <- sum(sapply(1:M[i], function(k) qgamma(u[count+k],alpha,beta)))
  }
  
  count <- count + 1 + M[i] 
}

##a)
EX_approx <- mean(x) #Approximation de l'espérance
Var_approx <- var(x) #Approximation de la variance

##b)

Fxapp <- function(k){ #Approximation de la fonction cumulative
  mean(x<=k)
}

sapply(k, function(x) 1 - Fxapp(x)) #Approximation de la survie



##c)

stop_loss_approx <- function(d){ #Approximation de la stop loss
  mean(x * (x > d)) - d * (1-Fxapp(d))
}
sapply(k, function(x) stop_loss_approx(x))

##d)

TVaR_appFx <- function(z){ #On donne comme argument à la fonction les valeurs de k = 0,100,...,1000
  sum(sort(x)[(Fxapp(z)*nsim+1):nsim]) * (1/(1-Fxapp(z))) * (1/nsim)
}
sapply(k, function(x) TVaR_appFx(x))

##e)

#voir c)

##f)

VaR_app <- function(kappa){ #Approximation de la Value at Risk
  sort(x)[kappa * nsim]
}
sapply(kappa, function(x) VaR_app(x))

##g)

TVaR_app <- function(kappa){ #Approximation de la TVaR, mais cette fois ci avec les kappa
  sum(x[x > VaR_app(kappa)]) * (1/nsim) * (1/(1-kappa))
}
sapply(kappa, function(x) TVaR_app(x))
