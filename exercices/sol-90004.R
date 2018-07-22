C_theta <- function(u1, u2) {
  u1 + u2 - 1 + ((1 - u1) ^ (-2) + (1 - u2) ^ (-2) - 1) ^ (-1 / 2)
}

C_hat_theta <- function(u1, u2) {
  C_theta(1 - u1, 1 - u2) + u1 + u2 - 1
}

Fx1 <- function(x1) 1 - (20 / (20 + x1)) ^ 3
Fx2 <- function(x2) pgamma(x2, 2, 1/10)

Fx1x2 <- function(x1, x2) {
  C_theta(Fx1(x1), Fx2(x2))
}

Fbarrex1x2 <- function(x1, x2) {
  C_hat_theta(1 - Fx1(x1), 1 - Fx2(x2))
}

# i -----------------------------------------------------------------------

Fx1x2(15, 11) - Fx1x2(5, 11) - Fx1x2(15, 6) + Fx1x2(5, 6)

# ii ----------------------------------------------------------------------

Fbarrex1x2(50, 30) / (1 - Fx2(30))
