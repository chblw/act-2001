
# a) ----------------------------------------------------------------------

set.seed(2018)
m <- 5

rPP1 <- function(t, lam) {
  Ni <- 0
  while(tail(Ni, 1) < t) {
    Ni <- c(Ni, tail(Ni, 1) + rexp(1, lam))
  }
  Ni[-c(1, length(Ni))]
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
