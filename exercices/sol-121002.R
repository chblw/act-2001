
# a) ----------------------------------------------------------------------

set.seed(2018)
m <- 5

rPP2 <- function(t, lam) {
  Nt <- rpois(1, lam * t)
  if(Nt > 0) {
    Ui <- runif(Nt)
    t * Ui
  } else {
    numeric()
  }
}

N <- replicate(m, rPP2(10, 1))

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
N <- replicate(m, rPP2(10, 1))
Z <- numeric(m)

for(i in seq_along(Z)) {
  Ni <- N[[i]]
  Xi <- rlnorm(length(Ni), log(10) - 0.5, 1)
  Z[i] <- sum(exp(-0.03 * Ni) * Xi)
}

sort(Z)[0.99 * m]
