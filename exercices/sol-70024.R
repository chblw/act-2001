n_contrats <- 1000

supp_x <- seq(0, 40000*1000, 1000)

ppareto <- function(x, alpha, lambda) {
  1 - (lambda / (lambda + x)) ** alpha
}

fB <- c(diff(c(0, ppareto(supp_x, 2.5, 15000))), rep(0, 2**16 - length(supp_x)))

phiB <- fft(fB)
phiS <- ((0.25 / (1 - 0.75 * phiB)) ** 0.01) ** n_contrats

fS <- Re(fft(phiS, inverse = TRUE)) / (2 ** 16)
Fs <- cumsum(fS)

sum(fS[1:40001] * supp_x)
sum(fS[1:40001] * pmax(supp_x - 2000000, 0))
Fs[500 + 1]
Fs[1000 + 1]
Fs[2000 + 1]

