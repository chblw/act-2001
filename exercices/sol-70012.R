fft_length <- 32

fx1 <- dbinom(0:10, 10, 0.2)
fx2 <- dbinom(0:20, 20, 0.3)

phix1 <- fft(c(fx1, rep(0, fft_length - length(fx1))))
phix2 <- fft(c(fx2, rep(0, fft_length - length(fx2))))

phis <- phix1 * phix2

fs <- Re(fft(phis, inverse = TRUE)) / fft_length

sum(fs)  # Verification
