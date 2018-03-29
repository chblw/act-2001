# Dépannage 02-08-18
# Auteur: Deniz Alic

## Chapitre 5 - Euler ##

#### 5.1.3 ####
rm(list=ls())

p <- c(0.75, 0.25)

be1 <- c(1/10, 1/40)
be2 <- c(1/20, 1/80)
be3 <- c(1/40, 1/10)

ex1 <- sum(p * 1/be1)
ex2 <-sum(p * 1/be2)
ex3 <- sum(p * 1/be3)

vax1 <- sum (p * 2 / be1^2) - ex1^2
vax2 <- sum (p * 2 / be2^2) - ex2^2
vax3 <- sum (p * 2 / be3^2) - ex3^2

cov_x1x2 <- sum(p * 1/be1 * 1/be2) - ex1 * ex2
cov_x1x3 <- sum(p * 1/be1 * 1/be3) - ex1 * ex3
cov_x2x3 <- sum(p * 1/be2 * 1/be3) - ex2 * ex3

es <- ex1 + ex2 + ex3
vas <- vax1 + vax2 + vax3 +
  2 * (cov_x1x2 + cov_x1x3 + cov_x2x3)


kappa <- 0.99

c1 <- ex1 + qnorm(kappa) * (vax1 + cov_x1x2 + cov_x1x3) / sqrt(vas)
c2 <- ex2 + qnorm(kappa) * (vax2 + cov_x1x2 + cov_x2x3) / sqrt(vas)
c3 <- ex3 + qnorm(kappa) * (vax3 + cov_x1x3 + cov_x2x3) / sqrt(vas)
ctot <- es + qnorm(kappa) * sqrt(vas)

data.frame(c1, c2, c3, ctot, test = c1 + c2 + c3)

# Vérification avec simulation
m <- 100000
x1 <- numeric(m)
x2 <- numeric(m)
x3 <- numeric(m)

for(i in 1:m){
  u <- runif(1)
  
  if(u <= p[1]){
    x1[i] <- rexp(1, be1[1])
    x2[i] <- rexp(1, be2[1])
    x3[i] <- rexp(1, be3[1])
  }
  else{
    x1[i] <- rexp(1, be1[2])
    x2[i] <- rexp(1, be2[2])
    x3[i] <- rexp(1, be3[2])
  }
}

s <- x1 + x2 + x3

data.frame(ex1, mean(x1), ex2, mean(x2), ex3, mean(x3), es, mean(s))
data.frame(cov_x1x2, cov(x1,x2), cov_x1x3, cov(x1,x3), cov_x2x3, cov(x2,x3))
data.frame(vax1, var(x1), vax2, var(x2), vax3, var(x3), vas, var(s))

c1_sim <- mean(x1) + qnorm(kappa) * cov(x1, s) / sqrt(var(s))
c2_sim <- mean(x2) + qnorm(kappa) * cov(x2, s) / sqrt(var(s))
c3_sim <- mean(x3) + qnorm(kappa) * cov(x3, s) / sqrt(var(s))
ctot_sim <- mean(s) + qnorm(kappa) * sqrt(var(s))

data.frame(c1, c1_sim, c2, c2_sim, c3, c3_sim, ctot, ctot_sim)

#### 5.2.1 ####
rm(list=ls())

set.seed(2018)
m = 100000

U = matrix(runif(3*m), nrow=m, byrow=TRUE)

x1 <- qgamma(U[, 1], 10, 1/300)
x2 <- qgamma(U[, 2], 4, 1/500)
x3 <- qgamma(U[, 3], 1, 1/1000)

s <- x1 + x2 + x3

x1[c(1, m)]
x2[c(1, m)]
x3[c(1, m)]
s[c(1, m)]

kappa <- c(0.9 ,0.99, 0.999)
vars <- sort(s)[kappa*m]
tvars <- sapply(vars, function(t) mean(s[s>t]))

cv1 <- sapply(vars, function(t) sum(x1 * (s == t)))
cv2 <- sapply(vars, function(t) sum(x2 * (s == t)))
cv3 <- sapply(vars, function(t) sum(x3 * (s == t)))

data.frame(kappa, cv1, cv2, cv3, vars, test = cv1 + cv2 + cv3)

ct1 <- sapply(vars, function(t) sum(x1 * (s > t))) / (1-kappa) / m
ct2 <- sapply(vars, function(t) sum(x2 * (s > t))) / (1-kappa) / m
ct3 <- sapply(vars, function(t) sum(x3 * (s > t))) / (1-kappa) / m

data.frame(kappa, ct1, ct2, ct3, tvars, test = ct1 + ct2 + ct3)

#### 5.2.2 ####
rm(list=ls())

ex <- 100
vax <- 300^2

al_pa <- 2 * vax / ex^2 / (vax / ex^2 - 1)
lam <- ex * (al_pa - 1)

al_gam <- ex^2 / vax
be <- al_gam / ex

sd <- sqrt(log((1 + vax / ex^2)))
mu <- log(ex) - sd^2 / 2

set.seed(2018)
m = 100000

U = matrix(runif(3*m), nrow=m, byrow=TRUE)

library(actuar)
x1 <- qpareto(U[, 1], al_pa, lam)
x2 <- qgamma(U[, 2], al_gam, be)
x3 <- qlnorm(U[, 3], mu, sd)

s <- x1 + x2 + x3

kappa <- c(0.9, 0.99, 0.999)
vars <- sort(s)[kappa*m]
tvars <- sapply(vars, function(t) mean(s[s>t]))

cv1 <- sapply(vars, function(t) sum(x1 * (s == t)))
cv2 <- sapply(vars, function(t) sum(x2 * (s == t)))
cv3 <- sapply(vars, function(t) sum(x3 * (s == t)))

data.frame(kappa, cv1, cv2, cv3, vars, test = cv1 + cv2 + cv3)

ct1 <- sapply(vars, function(t) sum(x1 * (s > t))) / (1-kappa) / m
ct2 <- sapply(vars, function(t) sum(x2 * (s > t))) / (1-kappa) / m
ct3 <- sapply(vars, function(t) sum(x3 * (s > t))) / (1-kappa) / m

data.frame(kappa, ct1, ct2, ct3, tvars, test = ct1 + ct2 + ct3)

