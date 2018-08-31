count <- c(141781, 2892, 535, 152, 57, 12, 3, 3, 0)
x <- rep(0:8, count)

# a) ----------------------------------------------------------------------

mle_parametres_poisson <- mean(x) 

# b) ----------------------------------------------------------------------

negvraisemblance <- function(param) {
  - sum(log(dnbinom(x, size = param[1], prob = param[2])))
}

mle_parametres_nbinom <- optim(c(0.05, 0.5), fn = negvraisemblance)$par

# c) ----------------------------------------------------------------------

# Poisson

count_binned <- c(count[1:2], sum(count[3:9]))

fi <- dpois(0:1, mle_parametres_poisson)
fi <- c(fi, 1 - sum(fi))
Ei <- sum(count) * fi

Q_poisson <- sum((count_binned - Ei)^2 / Ei)
pchisq(Q_poisson, df = length(Ei) - length(mle_parametres_poisson) - 1)

# NBinom

count_binned <- c(count[1:6], sum(count[7:9]))

fi <- c(dnbinom(0:5, mle_parametres_nbinom[1], mle_parametres_nbinom[2]))

fi <- c(fi, 1 - sum(fi))
Ei <- sum(count) * fi
Q_nbinom <- sum((count_binned - Ei)^2 / Ei)
pchisq(Q_nbinom, df = length(Ei) -length(mle_parametres_nbinom) - 1)
