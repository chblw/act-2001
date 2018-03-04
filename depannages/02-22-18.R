# Dépannage 02-22-18
# Auteur: Deniz Alic

#### 3.2.6 ####
rm(list=ls())

mu <- -0.5
sd <- 1

# Méthode 1
set.seed(20160419)
m <- 1000000

V <- matrix(runif(2*m), nrow=m, byrow=T)
V[c(1,2,m), ]

theta <- qlnorm(V[, 1], mu, sd)
M <- qpois(V[, 2], 2*theta)

# Méthode 2 
set.seed(20160419)

U <- runif(2*m)
M <- sapply(1:m, function(t) qpois(U[2*t], 2 * qlnorm(U[2*t - 1], mu, sd)))

max(M)

kk <- 0:max(M)

fmx <- sapply(kk, function(t) mean(M == t))
sum(fmx)

data.frame(kk[1:10], fmx[1:10])
em_test <- sum(kk * fmx)

em_sim <- mean(M)
vam_sim <- var(M)

et <- exp(mu + sd^2 / 2)
vat <- exp(2*mu + sd^2) *(exp(sd^2) - 1)

em <- 2*et
vam <- 2*et + 4*vat

em_sim ; em ; em_test
vam_sim ; vam


#### 4.1.2.2 ####
rm(list=ls())

beta <- 1 / 1000

# Fonction de masse de M
fNx <- function(x, n=1)
  fNx <- dpois(x, 0.005*n)

k <- 1:100
sum(fNx(k)) + fNx(0)

# Fonction de répartition de X
F.x <- function(x, n=1)
  F.x <- fNx(0,n) + sum(fNx(k,n) * pgamma(x, k, beta))

# Fonction à optimiser pour la VaR
VaR <- function(x, n = 1, kappa)
  VaR <- abs(F.x(x,n) - kappa)

# TVaR avec formules de l'annexe
TVaR <- function(n=1,kappa,d)
  TVaR <- sum(fNx(k,n) * k / beta * (1-(pgamma(d, k + 1, beta)))) / (1-kappa)

(F.x(0))
(F.x(5))
(F.x(10))

(VaR.X.99 <- (optimize(function(x) VaR(x, n=1, 0.99), c(0,100))$minimum))
(fNx(0))

# Kappa < prob à 0
VaR.X.99 <- 0
(TVaR.X.99 <- TVaR(n=1 ,0.99, d=VaR.X.99))

# Pour Sn/Wn
n <- 1000
k <- 1:1000

# Vérif que k est assez grand
sum(fNx(k,n)) + fNx(0,n)

#F.Wn(x) = F.Sn(nx)

(F.x(0*n, n))
(F.x(5*n, n))
(F.x(10*n, n))

#VaR de Wn = 1/n VaR de Sn
(VaR.Wn.99 <- (optimize(function(x) VaR(x,n,0.99), c(0,100000))$minimum) / n)
(TVaR.Wn.99 <- TVaR(n, 0.99, d=VaR.Wn.99*n) / n)


