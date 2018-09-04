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

# Test adequation ---------------------------------------------------------



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

legend(x = 0, y = 200, legend = c("Empirique", "Esperence non homogene", "Esperence homogene"), col = c(1, 2, 3), lty = c(1, 1, 1))
