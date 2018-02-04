## Solution à la question 1.4.3 du document d'exercices (version 2018-01-31)
## Auteur : Christopher Blier-Wong

# a)

(sig <- sqrt(log(5)))
mu <- -sig^2 / 2

# b)

qnorm(0.9995)
(VaR <- exp( -log(5) / 2 + sqrt(log(5)) * qnorm(0.9995)))

# c)

(sl <- exp(mu + sig^2 / 2) * (1 - pnorm( (log(VaR) - mu - sig^2) / sig  )) -
  VaR * (1 - pnorm( (log(VaR) - mu) / sig )))

# Une bonne manière de se vérifier, c'est de simuler.
set.seed(20180202)
mean(pmax(rlnorm(1000000, mu, sig) - VaR, 0))

# d)

VaR + 1 / (1 - 0.9995) * sl
