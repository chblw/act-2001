nsim <- 1000000

# H1 ----------------------------------------------------------------------

tau_beta_H1 <- c(1, 2)
1 - pweibull(1, tau_beta_H1[1], 1 / tau_beta_H1[2])

# H2 ----------------------------------------------------------------------

tau_beta_H2 <- c(0.5, 2 * gamma(1 + 1 / 0.5))
1 - pweibull(1, tau_beta_H2[1], 1 / tau_beta_H2[2])
set.seed(2018)

U_j <- matrix(runif(nsim * 6), ncol = 6)

W_j_H2 <- qweibull(U_j, tau_beta_H2[1], 1 / tau_beta_H2[2])

T_j_H2 <- t(apply(W_j_H2, 1, cumsum))
FTk_H2 <- sapply(1:6, function(t) mean(T_j_H2[, t] > 1))

dN1_H2 <- diff(c(0, FTk_H2))

sum(dN1_H2 * pmin(0:5, 5))


# H3 ----------------------------------------------------------------------

tau_beta_H3 <- c(1.5, 2 * gamma(1 + 1 / 1.5))
1 - pweibull(1, tau_beta_H3[1], 1 / tau_beta_H2[2])
set.seed(2018)

U_j <- matrix(runif(nsim * 6), ncol = 6)

W_j_H2 <- qweibull(U_j, tau_beta_H2[1], 1 / tau_beta_H2[2])

T_j_H2 <- t(apply(W_j_H2, 1, cumsum))
FTk_H2 <- sapply(1:6, function(t) mean(T_j_H2[, t] > 1))

dN1_H2 <- diff(c(0, FTk_H2))

sum(dN1_H2 * pmin(0:5, 5))
