# Solution de la question 2.1.5, version 2018-02-11
# Christopher Blier-Wong

# b)

mu <- 1000

CalculTVaRParetoSelonAlpha <- function(alpha, kappa = 0.995) {
  mu * alpha * ((1 - kappa) ^ (-1 / alpha) - 1) + mu
}

CalculStopLossParetoSelonAlpha <- function(alpha, d = 10000) {
  mu * ((mu * (alpha - 1)) / (mu * (alpha - 1) + d)) ^ (alpha - 1)
}

alpha.vec <- c(1.1, 2.5, 5, 100)

sapply(alpha.vec, CalculStopLossParetoSelonAlpha)
sapply(alpha.vec, CalculTVaRParetoSelonAlpha)

# c)

mu - mu * log(1 - 0.995)  # TVaR0.995
mu * exp(-10000 / mu)
