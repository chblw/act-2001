source("somme-gamma.R")

alpha.params <- 0.5 * 1:3
beta.params <- 1 / 10 * 1:3
k.max <- 100

d <- c(30, 40, 50)

sapply(d, function(x) psommegamma(x, alpha.params, beta.params, k.max))

p.params <- GetGammaWeights(alpha.params, beta.params, k.max)
alpha.param <- sum(alpha.params)
beta.param <- max(beta.params)

sapply(d, function(x) 
  sum( p.params * (alpha.param + 0:k.max) / beta.param * 
         (1 - pgamma(x, alpha.param + 0:k.max + 1, beta.param))) - 
    x * (1 - psommegamma(x, alpha.params, beta.params, k.max)))
