## Solution à la question 1.4.4 du document d'exercices (version 2018-01-31)
## Auteur : Christopher Blier-Wong

Fx <- function(x) 0.8 * pnorm(x, 0.1, 0.2) + 0.2 * pnorm(x, -0.3, 0.1)
fx <- function(x) 0.8 * dnorm(x, 0.1, 0.2) + 0.2 * dnorm(x, -0.3, 0.1)

# a)

(mu <- 0.8 * 0.1 + 0.2 * (-0.3))
integrate(function(t) t * fx(t), -10, 10)$value # verif
0.8 * (0.2 ^ 2 + 0.1 ^ 2) + 0.2 * (0.1 ^ 2 + (-0.3) ^ 2)
integrate(function(t) t ^ 2 * fx(t), -10, 10)$value # verif
(sig <- sqrt(0.06 - 0.02 ^ 2))

# b)

Fx(0)

# c)

kap <- c(0.0001, 0.01, 0.5, 0.99, 0.9999)
(VaR <- sapply(kap, function(k) optimize( function(t) abs( Fx(t) - k), c(-5, 5), 
                                          tol = .Machine$double.eps)$minimum))
round(Fx(VaR), 6)  # verif

# TVaR
(TVaR <- 1 / (1 - kap) * 0.8 * ( 0.1 * (1 - pnorm( VaR, 0.1, 0.2 )) + 0.2 ** 2 * dnorm(VaR, 0.1, 0.2 ) ) + 
  1 / (1 - kap) * 0.2 * ( -0.3 * (1 - pnorm( VaR, -0.3, 0.1 )) + 0.1 ** 2 * dnorm(VaR, -0.3, 0.1 ) ))
# verif
sapply(1:5, function(k) 1 / (1 - kap[k]) * 
         integrate(function(x) x * fx(x), lower = VaR[k], upper = 10, rel.tol = .Machine$double.eps ^ 0.5)$value)

# LTVaR
mu / kap - (1 - kap) / kap * TVaR
# Verif
sapply(1:5, function(k) 1 / kap[k] * 
         integrate(function(x) x * fx(x), lower = -10, upper = VaR[k], rel.tol = .Machine$double.eps ^ 0.5)$value)
