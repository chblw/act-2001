#Solution au numéro 2.4.2.2 (version du 2018-01-28)
#Auteur: Julien Bessette

#kappa
k <- c(0.9,0.95,0.99,0.995)

VaRS <- numeric(4)

#trouver la Value at Risk pour les différentes valeurs de kappa
for (i in 1:4){
  VaRS[i] <- qbinom(k[i],size=400,prob=0.008)
}

#Paramètres
n <- 400
b <- 1000
q <- 0.008

#a)
#Espérance de la variable aléatoire Sn = X1 + ... + Xn
ESN <- b * n * q

#Capital economique selon la Value at Risk
CEVAR <- numeric(4)

for (i in 1:4){
  CEVAR[i] <- b * VaRS[i] - ESN
}

#b)
TVaRS <- numeric(4)

#Fonction pour trouver la Tail value at Risk
for (i in 1:4){
  vk <- 1:1000 
  pdf <- dbinom(vk,size=400,prob=0.008)
  d <- 1/(1-k[i])
  y <- vk[vk > VaRS[i]] * pdf[vk > VaRS[i]]
  Fx <- pbinom(VaRS[i],size=400,prob=0.008)
  TVaRS[i] <- ((d)*(sum(y)+(VaRS[i])*(Fx-k[i])))
}

#Capital Economique selon la Tail value at risk
CETVAR <- numeric(4)

for (i in 1:4){
  CETVAR[i] <- b * TVaRS[i] - ESN
}

#c)
#Valeurs des Value at Risk
VaRXI <- numeric(4)
VaRX <- numeric(4)
for (i in 1:4){
  VaRXI[i] <- qbinom(k[i],size=1,prob=0.008)
  VaRX[i] <- b * qbinom(k[i],size=1,prob=0.008)
}

#Somme des Value at Risk
sumVaRX <- numeric(4)
for (i in 1:4){
  sumVaRX[i] <- n * VaRX[i]
}

#Benefice de mutualisation selon la Value at Risk
BEVAR <- numeric(4)
for (i in 1:4){
  BEVAR[i] <- sumVaRX[i] - b * VaRS[i]
}

#Il est souhaitable que le bénéfiace de mutualisation soit postif. Pour une valeur de kappa de
# 0.995, le benefice est postif, sinon il est négatif. Ainsi, pour des valeurs de kappa sous
# 0.995, il n'est pas avantageux de mettre en commun les risques

#d)
#Tail Value at risk
TVaRX <- numeric(4)

for (i in 1:4){
  vk <- 1:1000
  Fx <- pbinom(VaRXI[i],size=1,prob=0.008)
  pdf <- dbinom(vk,size=1,prob=0.008)
  d <- 1/(1-k[i])
  y <- vk[vk > VaRXI[i]] * pdf[vk > VaRXI[i]]
  TVaRX[i] <- b * ((d)*(sum(y)+(VaRXI[i])*(Fx-k[i])))
}

#Benefice de mutualisation selon la Tail value at risk
BETVAR <- numeric(4)
for (i in 1:4){
  BETVAR[i] <- n * TVaRX[i] - b * TVaRS[i]
}

#Pour toutes les valeurs de kappa, les valeurs des bénéfices de mutualisation sont positives. Ainsi,
#il est intéressant de mutualiser les risques.
