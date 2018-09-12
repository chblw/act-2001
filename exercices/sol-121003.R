
m <- 5

# a) ----------------------------------------------------------------------

set.seed(2018)

rNHHP1 <- function(t, intensite) {
  N0 <- 0
  Tstar <- 0
  lambda_barre <- intensite(t)
  step2 <- function() {
    E <- rexp(1, lambda_barre)
    if(Tstar + E < t) {
      Tstar <<- Tstar + E
      U <- runif(1)
      if( U < intensite(Tstar) / lambda_barre) {
        N0 <<- c(N0, Tstar)
      }
      step2()
    } else {
      N0[-1]
    }
  }
  step2()
}

intensite1 <- function(t) 0.5 + 0.05 * t

N <- replicate(m, rNHHP1(10, intensite1))

# b) ----------------------------------------------------------------------

set.seed(2018)
rNHHP2 <- function(t, Fs_inv) {
  N0 <- 0
  U <- runif(1)
  while(tail(N0, 1) + Fs_inv(U, tail(N0, 1)) < t) {
    N0 <- c(N0, tail(N0, 1) + Fs_inv(U, tail(N0, 1)))
    U <- runif(1)
  } 
  N0[-c(1, length(N0))]
  
}

Intensite1 <- function(t) 0.5 * t + 0.05 * t ^ 2 / 2
Fs <- function(t, s) exp(-(Intensite1(s + t) - Intensite1(s)))

Fs_inv <- function(q, s) optimize(function(t) abs(Fs(t, s) - q), c(0, 10))$minimum

N <- replicate(m, rNHHP2(10, Fs_inv))

# c) ----------------------------------------------------------------------

set.seed(2018)

rNHHP3 <- function(t, Intensite) {
  N <- rpois(1, Intensite(t))
  V <- sapply(runif(N), function(u) optimize(function(v) abs(Intensite(v) / Intensite(t) - u), c(0,  1000))$minimum)
  sort(V)
}

N <- replicate(m, rNHHP3(10, Intensite1))

## Validation graphique des méthodes 

N1 <- replicate(100, rNHHP1(100, intensite1))
N2 <- replicate(100, rNHHP2(100, Fs_inv))
N3 <- replicate(100, rNHHP3(100, Intensite1))

plot(c(0, 100), c(0, 400), type = "n")

for(i in seq_along(N1)) {
  points(N1[[i]], 1:length(N1[[i]]), col = 1, pch = ".")
  points(N2[[i]], 1:length(N2[[i]]), col = 2, pch = ".")
  points(N3[[i]], 1:length(N3[[i]]), col = 3, pch = ".")
}
