# Declaration des parametres ----------------------------------------------

alph <- 4
bet <- 0.2
lam <- 1.8
max_k <- 100

# Fonction de repartition -------------------------------------------------

Fs <- function(x, t) {
  dpois(0, lam * t) + sum(sapply(1:max_k, function(k) dpois(k, lam * t) * pgamma(x, alph * k, bet)))
} 

1 - Fs(0, 1.5)
1 - Fs(300, 1.5)
1 - Fs(600, 1.5)

# VaR ---------------------------------------------------------------------

VaR_k <- function(kappa, t) {
  optimize(function(x) abs(Fs(x, t) - kappa), c(0, 1000))
}

VaR_k(0.1, 1.5)$minimum
VaR_k(0.999, 1.5)$minimum

# TVaR --------------------------------------------------------------------

TVaR_k <- function(kappa, t) {
  (1 - kappa) ^ (-1) * sum(dpois(1:max_k, lam * t) * 1:max_k * alph / bet * (1 - pgamma(VaR_k(kappa, 1.5)$minimum, alph * 1:max_k + 1, bet)))
}

TVaR_k(0.1, 1.5)
TVaR_k(0.999, 1.5)
