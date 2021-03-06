# Donnees -----------------------------------------------------------------

k <- c(2, 1, 2, 1, 3, 3, 1, 1, 5, 8, 5, 9, 8, 8, 3, 5, 5, 6, 7, 5, 9, 4, 4, 4, 7, 8, 10, 11, 14, 10, 9, 7, 5, 14)
t <- 0:(length(k) - 1)

# Estimation des parametres -----------------------------------------------

neglogvrais <- function(params) {
  a <- params[1]
  b <- params[2]
  Lambda <- a + b / 2 * (2 * t + 1)
  - sum(k * log(Lambda) - Lambda)
}

poisson_non_homogene_params <- constrOptim(c(0.1, 0.01), neglogvrais, grad = NULL, ui = diag(2), ci = c(0, 0))

poisson_non_homogene_params$par
poisson_non_homogene_params$value

# Estimation des parametres power law -------------------------------------

neglogvrais <- function(params) {
  a <- params[1]
  b <- params[2]
  Lambda <- a * (t + 1) ^ b - a * t ^ b
  - sum(k * log(Lambda) - Lambda)
}

poisson_power_law_params <- constrOptim(c(1, 1), neglogvrais, grad = NULL, ui = diag(2), ci = c(0, 0))

poisson_power_law_params$par
poisson_power_law_params$value

# # Estimation des parametres log-lineaire ----------------------------------
# 
# neglogvrais <- function(params) {
#   a <- params[1]
#   b <- params[2]
#   Lambda <- exp(a) / b * (exp(b * t) - exp(b * (t - 1)))
#   print(Lambda)
#   - sum(k * log(Lambda) - Lambda)
# }
# 
# poisson_log_lineaire_params <- constrOptim(c(1, 0.1), neglogvrais, grad = NULL, ui = c(0, 1), ci = 0)
# 
# poisson_log_lineaire_params$par
# poisson_log_lineaire_params$value

# Test ratio vraisemblance ------------------------------------------------

neglogvrais <- function(params) {
  a <- params[1]
  - sum(k * log(a) - a)
}

poisson_homogene_params <- constrOptim(0.1, neglogvrais, grad = NULL, ui = 1, ci = 0)
mean(k)  # Verification, numeriquement egal

R <- - 2 * (poisson_non_homogene_params$value - poisson_homogene_params$value)

pchisq(R, 1)  # On rejete H0


# Prediction --------------------------------------------------------------

t_futur <- 34:38

k_futur <- poisson_non_homogene_params$par[1] + 
  poisson_non_homogene_params$par[2] * (2 * t_futur + 1) / 2

# Graphique observations cumulees vs esperence ----------------------------

plot(c(0, 35), c(0, 220), type = "n", ylab = expression(Lambda(t)))
points(t, cumsum(k))
curve(poisson_non_homogene_params$par[1] * x + poisson_non_homogene_params$par[2] * x ^ 2 / 2, c(0, max(t)), add = TRUE, col = 2)
curve(poisson_homogene_params$par * x, c(0, max(t)), add = TRUE, col = 3)
curve(poisson_power_law_params$par[1] * x ^ poisson_power_law_params$par[2], c(0, max(t)), add = TRUE, col = 4)
# curve(exp(poisson_log_lineaire_params$par[1]) / poisson_log_lineaire_params$par[2] * exp(poisson_log_lineaire_params$par[2] * (x - 1)), 
#       c(0, max(t)), add = TRUE, col = 5)

legend(x = 0, y = 200, legend = c("Empirique", "Esperance lineaire", "Esperance homogene", "Esperance Power Law"),
       col = 1:4, lty = rep(1, 4))
