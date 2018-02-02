#####Problème####


#Solution au numéro 2.4.2.2 (version du 2018-01-28)
#Auteur: Julien Bessette
#Dépannage 1er février

#Valeurs des kappa
k <- c(0.9,0.95,0.99,0.995)

#Paramètres
n <- 400
b <- 1000
q <- 0.008

VaRS <- numeric(4)
#Trouver la Value at Risk pour les différentes valeurs de kappa
for (i in 1:4){
  VaRS[i] <- qbinom(k[i],size=400,prob=0.008) #S suit une binomial n=400, prob=0.008
}

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

#Fonction pour trouver la Tail value at Risk de Sn
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

#Il est souhaitable que le bénéfice de mutualisation soit postif. Pour une valeur de kappa de
# 0.995, le benefice est postif, sinon il est négatif. Ainsi, pour des valeurs de kappa sous
# 0.995, il n'est pas avantageux de mettre en commun les risques selon la Value at Risk

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
#il est intéressant de mutualiser les risques selon la Tail Value at Risk




#####Graphiques####
# c)

# i) #FONCTIONS DE DENSITÉ
plot(0:30, seq(0, 1, length.out = 31), type = "n", ylab = expression(f[X](x)))
for(i in 1:10) {
  curve(dexp(x, 1/i), 0, 30, col = i, add = TRUE)
}

legend(25, 0.9, paste("n =", 1:10), lty = rep(1, 10), col = 1:10)

# ii) #FONCTIONS CUMULATIVES

plot(0:30, seq(0, 1, length.out = 31), type = "n", ylab = expression(f[X](x)))
for(i in 1:10) {
  curve(pexp(x, 1/i), 0, 30, col = i, add = TRUE)
}

legend(23, 0.85, paste("n =", 1:10), lty = rep(1, 10), col = 1:10)

# iii) #VALUE AT RISK

plot(seq(0, 1, length.out = 31), 0:30, type = "n", ylab = expression(f[X](x)))
for(i in 1:10) {
  curve(qexp(x, 1/i), 0, 1, col = i, add = TRUE)
}

legend(0.05, 30, paste("n =", 1:10), lty = rep(1, 10), col = 1:10)






# viii
n <- 10

f.erlang.generalise <- function(x) {
  sum(sapply(1:n, function(t) {
    vec <- 1:n
    vec <- vec[-t]
    vec <- 1 / vec
    prod( vec / (vec - (1 / t))) / t * exp(- x / t)
  })) 
}

f.erlang.generalise <- Vectorize(f.erlang.generalise)

curve(f.erlang.generalise, 0, 150, main = paste("n =", n))
curve(dnorm(x, sum(1:n), sqrt(sum((1:n)**(2)))), 0, 150, add = TRUE, col = 2)
legend(80, 0.02, c("Erlang généralisée", "Normale"), lty = rep(1, 2), col = 1:2)

n <- 20

curve(f.erlang.generalise, 0, 500, main = paste("n =", n))
curve(dnorm(x, sum(1:n), sqrt(sum((1:n)**(2)))), 0, 500, add = TRUE, col = 2)
legend(270, 0.007, c("Erlang généralisée", "Normale"), lty = rep(1, 2), col = 1:2)

# ix

n <- 10

F.erlang.generalise <- function(x) {
  1 - sum(sapply(1:n, function(t) {
    vec <- 1:n
    vec <- vec[-t]
    vec <- 1 / vec
    prod( vec / (vec - (1 / t))) * exp(- x / t)
  })) 
}

F.erlang.generalise <- Vectorize(F.erlang.generalise)

curve(F.erlang.generalise, 0, 150, main = paste("n =", n))
curve(pnorm(x, sum(1:n), sqrt(sum((1:n)**(2)))), 0, 150, add = TRUE, col = 2)
legend(80, 0.6, c("Erlang généralisée", "Normale"), lty = rep(1, 2), col = 1:2)

n <- 20

curve(F.erlang.generalise, 0, 500, main = paste("n =", n))
curve(pnorm(x, sum(1:n), sqrt(sum((1:n)**(2)))), 0, 500, add = TRUE, col = 2)
legend(250, 0.6, c("Erlang généralisée", "Normale"), lty = rep(1, 2), col = 1:2)

