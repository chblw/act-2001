nb_sinistres <- 0:6
nb_contrats <- c(190, 681, 692, 345, 77, 13, 2)

x <- rep(nb_sinistres, nb_contrats)

# H1 ----------------------------------------------------------------------

param_poisson <- mean(x)

logvrais_poisson <- sum(log(dpois(x, param_poisson)))

plot.ecdf(x, main = "Exponentielle")
points(nb_sinistres, ppois(nb_sinistres, param_poisson), pch = 19, col = 2, type = "l")
legend(2, 0.2, c("Empirique", "Exponentielle"), lty = c(1, 1), col = 1:2)

# H2 ----------------------------------------------------------------------

dNt <- function(k, t, alpha_beta) {
  pgamma(t, k * alpha_beta[1], alpha_beta[2]) - pgamma(t, (k + 1) * alpha_beta[1], alpha_beta[2])
}

neglogvrais <- function(alpha_beta) {
  -sum(nb_contrats * log( dNt(nb_sinistres, 1, alpha_beta)) )
}

params_alpha_beta <- constrOptim(c(1, 1), neglogvrais, NULL, diag(2), c(0, 0))

params_alpha_beta$par
logvrais_gamma <- -params_alpha_beta$value
cdf <- cumsum(dNt(0:6, 1, params_alpha_beta$par))

plot.ecdf(x, main = "Gamma")
points(nb_sinistres, cdf, pch = 19, col = 2, type = "l")
legend(2, 0.2, c("Empirique", "Gamma"), lty = c(1, 1), col = 1:2)

# c) ----------------------------------------------------------------------

R <- 2 * (logvrais_gamma - logvrais_poisson  )
qchisq(0.95, 1)
