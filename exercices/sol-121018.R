U <- c(0.65, 0.24, 0.98, 0.76, 0.34, 0.92, 0.03, 0.07, 0.35, 0.51)

mt <- function(t, alpha_beta) {
  k0 <- 1000
  sum(pgamma(t, 1:k0 * alpha_beta[1], alpha_beta[2]))
}

dNt <- function(k, t, alpha_beta) {
  pgamma(t, k * alpha_beta[1], alpha_beta[2]) - pgamma(t, (k + 1) * alpha_beta[1], alpha_beta[2])
}

calculer_valeurs <- function(alpha_beta) {
  
  print(paste0("m(1) = ", mt(1, alpha_beta)))
  
  dN10 <- sapply(0:100, function(k) dNt(k, 1, alpha_beta))
  
  EN1 <- sum(0:100 * dN10)
  EN2 <- sum((0:100) ^ 2 * dN10)
  VarN1 <- EN2 - EN1 ^ 2
  
  print(paste0("E[N(1)] = ", EN1))
  print(paste0("Var(N(1)) = ", VarN1))
  
  print(paste0("VaR_", c(0.9, 0.99), "(N(1)) = ", sapply(c(0.9, 0.99), function(kap) min(which(cumsum(dN10) > kap)) - 1)))
  
  T_1 <- cumsum(qgamma(U, alpha_beta[1], alpha_beta[2]))
  N_1 <- sum(T_1 < 1)
  
  print(paste0("T_", 1:10, " = ", T_1))
  print(paste0("N = ", N_1))
  
}

# H1 ----------------------------------------------------------------------

alpha_beta_H1 <- c(1, 5)
calculer_valeurs(alpha_beta_H1)

# H2 ----------------------------------------------------------------------

alpha_beta_H2 <- c(0.1, 0.5)
calculer_valeurs(alpha_beta_H2)

# H3 ----------------------------------------------------------------------

alpha_beta_H3 <- c(5, 25)
calculer_valeurs(alpha_beta_H3)
