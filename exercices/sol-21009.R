source("somme-gamma.R")

alpha.params <- c(2.5, 5)
beta.params <- c(0.1, 0.2)
k.max <- 100

psommegamma(100, alpha.params, beta.params, k.max)

(VaR0.99 <- optimize(function(x) abs(psommegamma(x, alpha.params, beta.params, k.max) - 0.99), c(0, 200))$minimum)
psommegamma(VaR0.99, alpha.params, beta.params, k.max) # verification
