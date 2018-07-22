qx1 <- function(u) {
  - 1000 * log(1 - u)
}

qx2 <- function(u) {
  800 * ((1 - u) ^ (- 1 / 1.8) - 1)
}

y0 <- 20141111

a <- 41358
m <- 2 ^ 31 - 1
nsim <- 10000
kap <- c(0.001, 0.01, 0.1, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999)
d <- 500 * 0:10

y <- numeric(nsim * 3 + 1)
y[1] <- y0

for(i in 1:(nsim * 3)) {
  y[i + 1] <- (a * y[i]) %% m
}

u <- matrix(y[-1] / m, nsim, 3, byrow = TRUE)

Z1 <- qexp(u[, 1])
Z2 <- qexp(u[, 2])

for(alpha in c(1, 5, 10)) {
  THETA <- qgamma(u[, 3], 1 / alpha)
  
  U1 <- (1 + Z1 / THETA) ^ (-1 / alpha)
  U2 <- (1 + Z2 / THETA) ^ (-1 / alpha)
  
  X1 <- qx1(U1)
  X2 <- qx2(U2)
  
  S <- sort(X1 + X2)
  
  VaRS <- S[kap * nsim + 1]
  TVaRS <- sapply(VaRS, function(t) mean(S[S > t]))
  
  Emax <- sapply(d, function(t) mean(pmax(S - t, 0)))
  EXind <- sapply(d, function(t) mean(X1 * (S > t)))
  
  print(paste0("Pour alpha = ", alpha, ", on obtient"))
  print(paste0("VaR", kap, "(S) = ", VaRS))
  print(paste0("TVaR", kap, "(S) = ", TVaRS))
  print(paste0("E[max(S - ", d, ")] = ", Emax))
  print(paste0("E[X1 x 1{S>", d, "}] = ", EXind))
}
