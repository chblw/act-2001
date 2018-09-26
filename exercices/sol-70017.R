x <- seq(0, 500000, 1000)

f_X1 <- c(diff(c(0, plnorm(x, 8, 1))), rep(0, 1024 - 501))
f_X2 <- c(diff(c(0, pgamma(x, 4, 1/1000))), rep(0, 1024 - 501))

phi_X1 <- fft(f_X1)
phi_X2 <- fft(f_X2)

phi_S <- phi_X1 * phi_X2

f_S <- Re(fft(phi_S, inverse = TRUE)) / 1024

sum(f_S)  # ok somme à 1
F_S <- cumsum(f_S)

round(f_S[10 * 1:3 + 1], 6)