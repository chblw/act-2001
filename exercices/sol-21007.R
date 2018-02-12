lambda <- 10000
alpha <- 2
tau <- 2

r.proba <- c(0.5, 0.4, 0.1)
r.valeurs <- c(1, 1.3, 2)

Fx <- function(x) {
  1 - (lambda / (lambda + x ^ tau)) ^ alpha
}

x <- 200 / r.valeurs

# Pr(X < 200)
sum(sapply(x, Fx) * r.proba)

# E[max(X - 200; 0)]
sum(r.valeurs * sapply(r.valeurs, function(t) {
  integrate(function(x) {
    (lambda / (lambda + x^tau)) ^ alpha
  }, lower = 200 / t, upper = 99999)$value
}) * r.proba)


