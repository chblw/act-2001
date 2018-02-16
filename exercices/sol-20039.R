## Solution à la question 2.2.1 du document d'exercices (version 2018-02-05)
## Auteur : Christopher Blier-Wong

## c

rate <- 1 / 20
n <- c(1, 2, 5, 10, 20)
len.n <- length(n)

plot(0:30, seq(0, 0.12, length.out = 31), type = "n"
     , ylab = expression(italic(f)[italic(X)](italic(x)))
     , xlab = expression(italic(x)))
for(i in 1:len.n) {
  curve(dgamma(x, n[i], n[i] * rate), 0, 30, col = i, add = TRUE)
}
legend(25, 0.1, paste("n =", n), lty = rep(1, len.n), col = 1:len.n)


## d
n <- c(1, 2, 5, 10, 20, 100)
len.n <- length(n)

plot(0:50, seq(0, 1, length.out = 51), type = "n"
     , ylab = expression(italic(F)[italic(X)](italic(x)))
     , xlab = expression(italic(x)))
for(i in 1:len.n) {
  curve(pgamma(x, n[i], n[i] * rate), 0, 50, col = i, add = TRUE)
}
legend(40, 0.5, paste("n =", n), lty = rep(1, len.n), col = 1:len.n)

plot(seq(0, 1, length.out = 91), 0:90, type = "n"
     , ylab = expression(italic(VaR)[kappa](italic(x)))
     , xlab = expression(kappa))
## e
for(i in 1:len.n) {
  curve(qgamma(x, n[i], n[i] * rate), 0, 1, col = i, add = TRUE)
}
legend(0.1, 80, paste("n =", n), lty = rep(1, len.n), col = 1:len.n)

## f

TVaRgamma <- function(x, params) {
  VaR <- qgamma(x, params[1], params[2])
  1 / (1 - x) * params[1] / params[2] *
    (1 - pgamma( VaR, params[1] + 1, params[2]))
}

plot(seq(0, 1, length.out = 121), 0:120, type = "n", 
     ylab = expression(italic(TVaR)[kappa](italic(x)))
     , xlab = expression(kappa))

for(i in 1:len.n) {
  params.n <- c(n[i], n[i] * rate)
  curve(TVaRgamma(x, params.n), 0, 1, col = i, add = TRUE)
}
legend(0.1, 110, paste("n =", n), lty = rep(1, len.n), col = 1:len.n)
