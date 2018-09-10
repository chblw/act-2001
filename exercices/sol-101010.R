library(RCurl)
dataset <- read.csv(text=getURL("https://cdn.rawgit.com/actrisk/actrisk-data/4929e130/hurricane.csv"))

loss_data <- dataset[, 5]

neglogvrais <- function(params) {
  - sum(log(dlnorm(loss_data, params[1], params[2])))
}

parametres_mle <- constrOptim(c(2, 0.8), neglogvrais, grad = NULL, ui = c(0, 1), ci = 0)

parametres_mle$par


# VaR ---------------------------------------------------------------------

alph <- 0.05
kappa <- 0.995
g1 <- exp(parametres_mle$par[1] + qnorm(kappa) * parametres_mle$par[2])

g1 + c(-1, 1) * qt(1 - alph / 2, length(loss_data) - 1) * parametres_mle$par[2] * g1 * sqrt(1 + qnorm(kappa) ^ 2 / 2)

I_inv <- matrix(c(parametres_mle$par[2] ^ 2, 0, 0, parametres_mle$par[2] ^ 2 / 2), ncol = 2)

g1prime <- c(g1, g1 * qnorm(kappa))
Var_g1 <- g1prime %*% I_inv %*% g1prime

g1 + c(-1, 1) * qt(1 - alph / 2, length(loss_data) - 1) * sqrt(Var_g1[1])

# TVaR --------------------------------------------------------------------

alph <- 0.05
kappa <- 0.995

g2 <- 1 / (1 - kappa) * exp(parametres_mle$par[1] + parametres_mle$par[2]^2/2) * 
  (1 - pnorm( qnorm(kappa) - parametres_mle$par[2]))

g2prime <- c(g2, parametres_mle$par[2] * g2 + 
               1 / (1 - kappa) * exp(parametres_mle$par[1] + parametres_mle$par[2] ^ 2 / 2) * 
               dnorm(qnorm(kappa) - parametres_mle$par[2]))

Var_g2 <- g2prime %*% I_inv %*% g2prime

g2 + c(-1, 1) * qt(1 - alph / 2, length(loss_data) - 1) * sqrt(Var_g2[1])
