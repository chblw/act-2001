
# a) ----------------------------------------------------------------------

set.seed(2018)
m <- 5

rPP1 <- function(t, lam) {
  Ti <- 0
  while(tail(Ti, 1) < t) {
    Ti <- c(Ti, tail(Ti, 1) + rexp(1, lam))
  }
  Ti[-c(1, length(Ti))]
}

N <- replicate(m, rPP1(10, 1))

# b) ----------------------------------------------------------------------

Z <- numeric(m)

for(i in seq_along(Z)) {
  Ni <- N[[i]]
  Xi <- rlnorm(length(Ni), log(10) - 0.5, 1)
  Z[i] <- sum(exp(-0.03 * Ni) * Xi)
}

Z

# c) ----------------------------------------------------------------------

set.seed(2018)
m <- 1000000
N <- replicate(m, rPP1(10, 1))
Z <- numeric(m)

for(i in seq_along(Z)) {
  Ni <- N[[i]]
  Xi <- rlnorm(length(Ni), log(10) - 0.5, 1)
  Z[i] <- sum(exp(-0.03 * Ni) * Xi)
}

sort(Z)[0.99 * m]
