count <- c(103704, 14075, 1834, 255, 34, 4, 1)
x <- rep(0:6, count)

# a) ----------------------------------------------------------------------

mle_parametres_poisson <- mean(x) 

# b) ----------------------------------------------------------------------

negvraisemblance <- function(param) {
  - sum(log(dnbinom(x, size = param[1], prob = param[2])))
}

mle_parametres_nbinom <- optim(c(0.05, 0.5), fn = negvraisemblance)$par

# c) ----------------------------------------------------------------------

count_binned <- c(count[1:4], sum(count[5:7]), 0)

# Poisson

fi <- c(dpois(0:3, mle_parametres_poisson), sum(dpois(4:6, mle_parametres_poisson)))
fi <- c(fi, 1 - sum(fi))
Ei <- sum(count) * fi


Q_poisson <- sum((count_binned - Ei)^2 / Ei)
pchisq(Q_poisson, df = length(Ei) - 1)

# NBinom

fi <- c(dnbinom(0:3, mle_parametres_nbinom[1], mle_parametres_nbinom[2]), 
        sum(dnbinom(4:6, mle_parametres_nbinom[1], mle_parametres_nbinom[2])))
fi <- c(fi, 1 - sum(fi))
Ei <- sum(count) * fi
Q_nbinom <- sum((count_binned - Ei)^2 / Ei)
pchisq(Q_nbinom, df = length(Ei) - 1)
