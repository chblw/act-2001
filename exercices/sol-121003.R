
m <- 5

# a) ----------------------------------------------------------------------

set.seed(2018)
rNHHP2 <- function(t, Fs_inv) {
  N0 <- 0
  U <- runif(1)
  while(tail(N0, 1) + Fs_inv(U, tail(N0, 1)) < t) {
    N0 <- c(N0, tail(N0, 1) + Fs_inv(U, tail(N0, 1)))
    U <- runif(1)
  } 
  N0[-1]
}

Fs_inv <- function(q, s) 1/0.05 * (-0.5 + sqrt(0.5**2 + 2 * 0.05 * ( 0.5 * s + 0.05 * s ** 2 / 2 - log(1 - q)))) - s

N <- replicate(m, rNHHP2(10, Fs_inv))

# b) ----------------------------------------------------------------------

set.seed(2018)

rNHHP3 <- function(t, Intensite) {
  N <- rpois(1, Intensite(t))
  V <- 1/0.05 * (-0.5 + sqrt( 0.5^2 + 2 * 0.05 * runif(N) * Intensite(t)))
  sort(V)
}

Intensite1 <- function(t) 0.5 * t + 0.05 * t ^ 2 / 2

N <- replicate(m, rNHHP3(10, Intensite1))
