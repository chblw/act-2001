## Solution à la question 1.4.2 du document d'exercices (version 2018-01-31)
## Auteur : Christopher Blier-Wong

alph <- 1 / 4
bet <- 1 / 4

# b)

(VaR <- qgamma(0.9995, alph, bet))

# c)

(sl <- alph / bet * (1 - pgamma(VaR, alph + 1, bet)) - VaR * (1 - pgamma(VaR, alph, bet)))

# Une bonne manière de se vérifier, c'est de simuler.
set.seed(20180202)
mean(pmax(rgamma(1000000, alph, bet) - VaR, 0))

# d)

VaR + 1 / (1 - 0.9995) * sl
