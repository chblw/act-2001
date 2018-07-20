qpareto <- function(p, alpha, lambda) {
  lambda * ( (1 - p) ^ (- 1 / alpha) - 1)
}

VaRS <- function(kap) {
  qpareto(kap, 1.5, 50) + qexp(kap, 1 / 100) + qlnorm(kap, log(100) - 0.5, 1)
}

VaRS(0.95)  # Vérification
VaRS(0.99)

Fs <- function(x) optimize(function(t) abs(VaRS(t) - x), c(0, 1))$minimum

Fs(1000)  # Vérification
Fs(2000)
