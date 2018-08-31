count <- c(120, 106, 60, 15, 6, 0)
x <- rep(0:5, count)

# a) ----------------------------------------------------------------------

mle_parametres_poisson <- mean(x) 

# c) ----------------------------------------------------------------------

# Poisson

count_binned <- c(count[1:4], sum(count[5:6]))

fi <- dpois(0:3, mle_parametres_poisson)
fi <- c(fi, 1 - sum(fi))
Ei <- sum(count) * fi

Q_poisson <- sum((count_binned - Ei)^2 / Ei)
pchisq(Q_poisson, df = length(Ei) - length(mle_parametres_poisson) - 1)


