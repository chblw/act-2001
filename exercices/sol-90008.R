
# Constant definitions ----------------------------------------------------

max_n <- 100
range_x <- 0:max_n

# Function definitions ----------------------------------------------------

Fm <- function(lam) {
  function(m) ppois(m, lam)
}

generate_Fm1m2 <- function(lam1, lam2, Fcopule) {
  
  Fm1 <- Fm(lam1)
  Fm2 <- Fm(lam2)
  
  function(m1, m2) {
    if(m1 < 0 || m2 < 0) {
      0
    } else {
      Fcopule(Fm1(m1), Fm2(m2))
    }
  }
}

generate_fm1m2 <- function(lam1, lam2, Fcopule) {
  
  Fm1m2 <- generate_Fm1m2(lam1, lam2, Fcopule)
  
  Vectorize(function(m1, m2) {
    Fm1m2(m1, m2) - Fm1m2(m1 - 1, m2) - 
      Fm1m2(m1, m2 - 1) + Fm1m2(m1 - 1, m2 - 1)
  })
}

generate_summand <- function(lam1, lam2, Fcopule) {
  
  fm1m2 <- generate_fm1m2(lam1, lam2, Fcopule)
  
  function(m1, m2) {
    m1 * m2 * fm1m2(m1, m2)
  }
  
}

calculate_Em1m2 <- function(lam1, lam2, Fcopule, ...) {
  
  summand <- generate_summand(lam1, lam2, Fcopule)
  
  sum(outer(0:max_n, 0:max_n, "summand"))
}

# a -----------------------------------------------------------------------

Fcomonotone <- function(u1, u2) {
  min(u1, u2)
}

Em1m2 <- sapply(1:5, function(i) sapply(1:5, function(j) calculate_Em1m2(i, j, Fcopule = Fcomonotone)))

(Em1m2 - outer(1:5, 1:5)) / sqrt(outer(1:5, 1:5))

# b -----------------------------------------------------------------------

Fantimonotone <- function(u1, u2) {
  max(u1 + u2 - 1, 0)
}

Em1m2 <- sapply(1:5, function(i) sapply(1:5, function(j) calculate_Em1m2(i, j, Fcopule = Fantimonotone)))

(Em1m2 - outer(1:5, 1:5)) / sqrt(outer(1:5, 1:5))

# c -----------------------------------------------------------------------

Ffrechet <- function(u1, u2, theta) {
  theta * Fcomonotone(u1, u2) + (1 - theta) * Fantimonotone(u1, u2)
} 

find_theta <- function(lam1, lam2) {
  optimize(function(t) abs(calculate_Em1m2(lam1, lam2, Fcopule = function(u1, u2) t * Fcomonotone(u1, u2) + (1 - t) * Fantimonotone(u1, u2)) - lam1 * lam2), c(0, 1))$minimum
}

zero_rho_theta <- sapply(1:5, function(i) sapply(1:5, function(j) find_theta(i, j)))

generate_summand <- function(k, lam1, lam2, Fcopule) {
  
  fm1m2 <- generate_fm1m2(lam1, lam2, Fcopule)
  
  function(k, m1, m2) {
    max(m1 + m2 - k, 0) * fm1m2(m1, m2)
  }
}

calculate_expectation <- function(k, lam1, lam2, Fcopule) {
  
  summand <- Vectorize(generate_summand(k, lam1, lam2, Fcopule))
  sum(outer(0:max_n, 0:max_n, "summand", k))
}

expectation_k <- function(k) calculate_expectation(k = k, lam1 = 1, lam2 = 5, Fcopule = function(u1, u2) zero_rho_theta[1, 5] * min(u1, u2) + (1 - zero_rho_theta[1, 5]) * max(u1 + u2 - 1, 0))
expectation_k <- Vectorize(expectation_k)
sapply(0:20, expectation_k)

expectation_k <- function(k) calculate_expectation(k = k, lam1 = 1, lam2 = 5, Fcopule = function(u1, u2) u1 * u2)
expectation_k <- Vectorize(expectation_k)
sapply(0:20, expectation_k)

expectation_k <- function(k) calculate_expectation(k = k, lam1 = 2, lam2 = 3, Fcopule = function(u1, u2) zero_rho_theta[2, 3] * min(u1, u2) + (1 - zero_rho_theta[2, 3]) * max(u1 + u2 - 1, 0))
expectation_k <- Vectorize(expectation_k)
sapply(0:20, expectation_k)

expectation_k <- function(k) calculate_expectation(k = k, lam1 = 1, lam2 = 5, Fcopule = function(u1, u2) u1 * u2)
expectation_k <- Vectorize(expectation_k)
sapply(0:20, expectation_k)
