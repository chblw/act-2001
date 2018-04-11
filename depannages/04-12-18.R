# Dépannage 04-12-18
# Auteur: Deniz Alic

# Document pour les exercices modèles de survie

#### 4 ####
rm(list=ls())
be <- 0.00006148
ga <- log(1.09159)
w <- 130

stx <- function(x,t)
  exp(-be/ga*exp(ga*x) * (exp(ga*t) - 1))

x <- 40
hh <- c(1, 1/2, 1/4, 1/12)

ex.approx <- sapply(hh, function(h) 
  h/2 + h*sum(sapply(1:((w-x)/h -1), function(y) stx(x,y*h))))

data.frame(hh, ex.approx)

#### 5 ####
be <- 0.00006148
ga <- log(1.09159)
w <- 130
al <- 9.111 * 10^-4 # Valeur random (mais dans le bon range)
# En examen le alpha sera donné

stx <- function(x,t)
  exp(-al*t - be/ga*exp(ga*x) * (exp(ga*t) - 1))

x <- 40
hh <- c(1, 1/2, 1/4, 1/12)

ex.approx <- sapply(hh, function(h) 
  h/2 + h*sum(sapply(1:((w-x)/h -1), function(y) stx(x,y*h))))

data.frame(hh, ex.approx)
#### 8 ####

be <-  1.694 * 10^(-5)
ga <- log(1.10960)

stx <- function(x,t)
  exp(-be/ga * exp(ga*x) * (exp(ga*t) - 1))

ftx <- function(x,t)
  stx(x,t) * be * exp(ga*(x+t))

qtx <- function(x,u)
  1/ga*log(1 - ga/be * exp(-ga*x) * log(1-u))


x <- 0

#a
e_tx <- integrate(function(t) t * ftx(x,t),0,130)$value
e_tx_test <- integrate(function(t) stx(x,t),0,130)$value
e2_tx <- integrate(function(t) t^2 * ftx(x,t),0,130)$value
var_tx <- e2_tx - e_tx^2
cv_tx <- sqrt(var_tx) / e_tx

data.frame(e_tx,e_tx_test,cv_tx)

#b
(mod_tx <- 1/ga * log(ga/be))

#c
(med_tx <- qtx(x,0.5))

#d
kappa <- c(0.1, 0.25, 0.75, 0.9)
vark_tx <- sapply(kappa, function(y) qtx(x,y))
data.frame(kappa=kappa, VaR=vark_tx)

#e
(eiq_tx <- vark_tx[3]-vark_tx[2])
(eid_tx <- vark_tx[4]-vark_tx[1])

