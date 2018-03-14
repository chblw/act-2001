# Dépannage 03-15-18
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

#### 4.4.1 ####
rm(list=ls())

mu <- 2
sd <- 0.9
al <- 2
be <- 18

eb <- exp(mu + sd^2 / 2)
vab <- exp(2*mu + sd^2) *(exp(sd^2) - 1)

et <- al / (al + be)
vat <- al * be / (al + be)^2 / (al + be + 1)

ei <- et
vai <- et - et^2

ex <- eb * et
vax <- et * vab + eb^2 * (et - vat - et^2) + eb^2 * vat

cov_x1x2 <- eb^2 * vat

# Simulation
m <- 100000
set.seed(2^31 - 1)
TT <- rbeta(m, al, be)
I <- rbinom(m, 1, TT)
X <- I * rlnorm(m, mu, sd)
X2 <- rbinom(m, 1, TT) * rlnorm(m, mu, sd)

data.frame(ei, vai, ex, vax)
data.frame(mean(I), var(I), mean(X), var(X))
cov_x1x2 ; cov(X, X2)
