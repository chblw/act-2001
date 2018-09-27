EN1H2 <- sum(pgamma(1, 0.1 * 1:1000, 0.5))
EN1H3 <- sum(pgamma(1, 5 * 1:1000, 25))

# b) ----------------------------------------------------------------------

#  H2
dN1H2 <- c(pgamma(1, 0.1 * 0:999, 0.5) - pgamma(1, 0.1 * 1:1000, 0.5), pgamma(1, 0.1 * 1000, 0.5))
sum(dN1H2 * (0:1000) ^ 2) - EN1H2 ^ 2

#  H3
dN1H3 <- c(pgamma(1, 5 * 0:999, 25) - pgamma(1, 5 * 1:1000, 25), pgamma(1, 5 * 1000, 25))
sum(dN1H3 * (0:1000) ^ 2) - EN1H3 ^ 2

# d) ----------------------------------------------------------------------

dN1H1 <- dpois(0:1000, 5)

FS1 <- function(x, dN1) sum(dN1 * pgamma(x, 0:1000 * 1.5, 1.5)) 

sapply(10 * 1:2, function(x) FS1(x, dN1H1))
sapply(10 * 1:2, function(x) FS1(x, dN1H2))
sapply(10 * 1:2, function(x) FS1(x, dN1H3))

# e) ----------------------------------------------------------------------

VaRH1 <- optimize(function(VaR) abs(FS1(VaR, dN1H1) - 0.99), c(0, 50))$minimum
VaRH2 <- optimize(function(VaR) abs(FS1(VaR, dN1H2) - 0.99), c(0, 50))$minimum
VaRH3 <- optimize(function(VaR) abs(FS1(VaR, dN1H3) - 0.99), c(0, 50))$minimum

# f) ----------------------------------------------------------------------

1 / (1 - 0.99) * sum(dN1H1[-1] * 1:1000 * (1 - pgamma(VaRH1, 1:1000 * 1.5 + 1, 1.5)))
1 / (1 - 0.99) * sum(dN1H2[-1] * 1:1000 * (1 - pgamma(VaRH2, 1:1000 * 1.5 + 1, 1.5)))
1 / (1 - 0.99) * sum(dN1H3[-1] * 1:1000 * (1 - pgamma(VaRH3, 1:1000 * 1.5 + 1, 1.5)))
