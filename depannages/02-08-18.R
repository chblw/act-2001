# Dépannage 02-08-18
# Auteur: Deniz Alic

#### 2.3.3 ####
rm(list=ls())

# Paramètres
mu <- log(10) - 0.18
sd <- 0.6
be_exp <- 1/10
al_ga <- 2
be_ga <- 1/5

# a)
kappa <- c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)

# Espérances théoriques
ex1 <- exp(mu + sd^2 / 2)
ex2 <- 1 / be_exp
ex3 <- al_ga / be_ga

# VaR théoriques
var_x1 <- qlnorm(kappa, mu, sd)
var_x2 <- qexp(kappa, be_exp)
var_x3 <- qgamma(kappa, al_ga, be_ga)

# TVaR rhéoriques (formules dans annexe)
tvar_x1 <- 1/(1-kappa) * exp(mu + 0.5 * sd^2) * (1 - pnorm(qnorm(kappa) - sd))
tvar_x2 <- ex2 + var_x2
tvar_x3 <- 1/(1-kappa) * al_ga /be_ga * (1 - pgamma(var_x3, al_ga + 1, be_ga))

(ex_theo <- data.frame(ex1, ex2, ex3))
(res_theo <- data.frame(kappa, var_x1, var_x2, var_x3, tvar_x1, tvar_x2, tvar_x3))

# b)
# Espérance opérateur linéaire
es <- ex1 + ex2 + ex3

# Variance des Xi
vax1 <- exp(2*mu + sd^2) * (exp(sd^2) - 1)
vax2 <- 1 / be_exp^2
vax3 <- al_ga / be_ga^2

(va_theo <- data.frame(vax1, vax2, vax3))

# Les Xi sont indépdendants
vas <- vax1 + vax2 + vax3

es ; vas

# c) - d)

m <- 100000
set.seed(2018)

# 1er choix pour simuler les Uniformes et les Xi
u <- runif(3*m)
u[1:6]
tail(u, 3)

al1 <- 0.5
be1 <- al1/10
al2 <- 1.5
be2 <- al2/10
al3 <- 2.5
be3 <- al3/10
x1 <- sapply(1:m, function(y) qgamma(u[3 * y - 2], al1, be1))
x2 <- sapply(1:m, function(y) qgamma(u[3 * y - 1], al2, be2))
x3 <- sapply(1:m, function(y) qgamma(u[3 * y], al3, be3))

s <- x1 + x2 + x3

# Autre option pour simuler les Uniformes et les Xi
set.seed(20160419) # Il faut toujours le remettre pour comparer
U <- matrix(runif(3*m), ncol=3, byrow=T)

# Les 2 premières rangées
U[c(1,2), ]

x1_autre <- sapply(U[,1], function(t) qlnorm(t, mu, sd))
x2_autre <- sapply(U[,2], function(t) qexp(t, be_exp))
x3_autre <- sapply(U[,3], function(t) qgamma(t, al_ga, be_ga))

s_autre <- x1_autre + x2_autre + x3_autre

# Vérification des 2 méthodes
s[1:3] ; s_autre[1:3]


x1[c(1,2,m)]
x2[c(1,2,m)]
x3[c(1,2,m)]
# e)
# Moyennes empiriques des simulations
ex1_sim <- mean(x1)
ex2_sim <- mean(x2)
ex3_sim <- mean(x3)

# Comparer avec valeurs théoriques
data.frame(ex_theo, ex1_sim, ex2_sim, ex3_sim)

# Variance empirique des simulations
vax1_sim <- var(x1)
vax2_sim <- var(x2)
vax3_sim <- var(x3)

# Comparer avec valeurs théoriques
data.frame(va_theo, vax1_sim, vax2_sim, vax3_sim)

# VaR Empiriques 
kappa <- 0.9
# Trouver l'indice des valeurs ordonnées qui fait en sorte que F(x) = kappa
j <- kappa * m

# VaR: La valeur ordonnée qui correspond au quantile de kappa
var_x1 <- sort(x1)[j]
var_x2 <- sort(x2)[j]
var_x3 <- sort(x3)[j]

# TVaR, 2 méthodes différentes
# On somme les valeurs plus grandes que la VaR
# Le 2è terme donne 0 car F(VaR) = kappa

tvar_x1_test <- sapply(j, function(y) sum(sort(x1)[(y + 1):m])) / m / (1 - kappa)
tvar_x1 <- sapply(var_x1, function(t) mean(x1[x1 > t]))
tvar_x2 <- sapply(var_x2, function(t) mean(x2[x2 > t]))
tvar_x3 <- sapply(var_x3, function(t) mean(x3[x3 > t]))

# Vérifier les 2 méthodes
tvar_x1_test ==  tvar_x1
tvar_x1_test - tvar_x1

res_simul <- data.frame(kappa, var_x1, var_x2, var_x3, tvar_x1, tvar_x2, tvar_x3)

# Comparer avec les valeurs théoriques
res_theo ; res_simul

# f)

# Espérance / Variance empirique de S
es_sim <- mean(s)
vas_sim <- var(s)

# Comparer avec valeurs théoriques
es; es_sim
vas ; vas_sim

# VaR / TVaR de S
var_s <- sort(s)[j]
tvar_s <- sapply(j, function(y) sum(sort(s)[(y+1):m])) / m / (1 - kappa) 

data.frame(kappa, var_s, tvar_s)

# g)
# Béméfices de mutualisation
bm_var <- var_x1 + var_x2 + var_x3 - var_s
bm_tvar <- tvar_x1 + tvar_x2 + tvar_x3 - tvar_s

data.frame(kappa, bm_var, bm_tvar)


#### 3.3.1 ####
rm(list=ls())

# On peut avoir des valeurs théoriques pour tout ce qui est demandé
# Car Bi ~ Gamma(a ,b) avec le même b, donc la somme reste gamma

# Paramètres
lam <- 1
alpha <- 1.5
beta <- alpha / 10

# Fonction de masse pour la variable M
fmx <- function(x, n = 1) # n = ... va être utile plus tard (Chap 4?)
  dpois(x, lam * n)

# k est le vecteur des valeurs possibles pour M
k <- 1:100
sum(fmx(k)) + fmx(0) # Verif somme = 1 
# Il faut aussi vérifier plus tard quand on change le n = ...

# Fonction de répartition
# On conditionne sur chaque valeur de M et on pondère par les probabilités
Fx_fun <- function(x, n = 1)
  fmx(0, n) + sum(fmx(k, n) * pgamma(x, alpha*k, beta))

# Fonction pour optimiser (mettre 2 valeurs dans interval)
# Parfois il faut gosser avec l'intervalle pour optimiser
VaR_fun <- function(x, n = 1, kappa, interval){
  optimize(function(x) abs(Fx_fun(x, n) - kappa), interval)$minimum
}

# Avec uniroot
#VaR_fun <- function(x, n = 1, kappa, interval){
#  uniroot(function(x) Fx_fun(x , n) - kappa, lower = interval[1], upper = interval[2])$root
#}

# Fonction pour TVaR Gamma (Formule dans l'annexe)
TVaR_fun <- function(n = 1, kappa, d)
  sum(fmx(k, n) * alpha * k / beta * (1 - (pgamma(d, alpha*k + 1, beta)))) / (1 - kappa)

# Fonction pour Stop-loss Gamma (Formule dans l'annexe)
Stop_loss_fun <- function(n = 1, d){
  sum(fmx(k, n) * 
        (alpha * k / beta * (1 - (pgamma(d, alpha*k + 1, beta))) -
           d * (1 - pgamma(d, alpha*k, beta)))) 
}

# a)
# E(M) * E(B)
e_x <- nn * q * alpha / beta

# E(M) * Var(B) + Var(M) * E(B)^2
va_x <- nn*q * (alpha / beta^2) + nn*q*(1-q) * (alpha / beta)^2

data.frame(EX = e_x, VarX = va_x)

# b)
# Valeurs désirées
xx <- 100 * seq(0, 10)
Sx <- 1 - sapply(xx, function(x) Fx_fun(x))
round(data.frame(xx, Sx), 7)

# c)
stop_loss <- sapply(xx, function(x) Stop_loss_fun(n = 1, d = x))
data.frame(xx, stop_loss)

# d)
# Selon b
(kappa2 <- 1-Sx)

fmx(0) # Vérifier pour les VaR de 0

# Calculer la VaR
var <- sapply(kappa2, function(y) VaR_fun(kappa = y, interval = c(10,5000)))
var # Attention au premier élément
var[which(kappa2 <= fmx(0))] <- 0 # Selon la définition de la VaR

tvar <- sapply(1:length(kappa2), function(k) TVaR_fun(n = 1, kappa = kappa2[k], d = var[k]))

data.frame(kappa2, var, tvar)

# e)
# Déjà calculé au c)

# f)

kappa <- c(0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)
fmx(0)

# Fonction définie plus haut
var <- sapply(kappa, function(y) VaR_fun(kappa = y, interval = c(10,10000)))
var[which(kappa <= fmx(0))] <- 0 # Selon la définition de la VaR

tvar <- sapply(1:length(kappa), function(k) TVaR_fun(n = 1, kappa = kappa[k], d = var[k]))

#tvar <- numeric(length(kappa))
#for(i in 1:length(kappa))
#  tvar[i] <- TVaR_fun(n = 1, kappa = kappa[i], d = var[i])

# On va calculer les stop-loss aussi
stop_loss <- sapply(var, function(x) Stop_loss_fun(n = 1, d = x))

# Selon propsition des séances précédentes
tvar_test <- var + stop_loss / (1 - kappa)

data.frame(kappa, VaR = var, TVaR = tvar, StopLoss = stop_loss, TVaR_Test = tvar_test)

# h)
# Simulation

m <- 1000000
set.seed(2018)

# On ne sait pas combien d'uniformes on va avoir besoin, alors on en simule assez
u <- runif(m * qbinom(0.99, nn, q))

M <- numeric(m)
x <- numeric(m)

#Count sert à savoir où est-ce qu'on est rendu dans le vecteur des uniformes (u)
count <- 1
for (i in 1:m)
{
  # Simuler le M
  M[i] <- qbinom(u[count], nn, q)
  
  # Si M > 0, on simule les B, sinon rien (x est un vecteur de 0 par défaut)
  if(M[i] > 0) 
  {
    for(j in 1:M[i]) # Généralement on peut faire un sapply ici
      x[i] <- x[i] + qgamma(u[count + j], alpha, beta)
  }
  
  count <- count + M[i] + 1 # On ajuste le compteur
}

set.seed(2018)
#### Autre méthode sans count
for (i in 1:m)
{
  # Simuler le M
  M[i] <- qbinom(runif(1), nn, q)
  
  # Si M > 0, on simule les B, sinon rien (x est un vecteur de 0 par défaut)
  if(M[i] > 0) 
  {
    x[i] <- sum(qgamma(runif(M[i]), alpha, beta))
  }
}

# Espérance / Variance empirique
e_x_sim <- mean(x)
va_x_sim <- var(x)

data.frame(e_x, e_x_sim, va_x, va_x_sim)

# Fonction de répartition comme somme d'indicatrices
Fx_sim <- sapply(xx, function(y) sum(x <= y)) / m

data.frame(xx, 1-Sx, Fx_sim)

# Indice de la VaR (entier)
j <- kappa * m

# VaR par définition
var_x_sim <- sort(x)[j]

# TVaR
# Valide car kappa * m est un entier (Pas besoin du 2ème terme)
tvar_x_sim <- sapply(j, function(y) sum(sort(x)[(y+1):m])) / m / (1 - kappa) 

# Valider encore la TVaR avec stop loss
stp_ls <- sapply(var_x_sim, function(y) sum((x - y) * (x > y))) / m
tvar_x_2_sim <- var_x_sim + stp_ls / (1 - kappa)

# Triple vérification
data.frame(kappa, VaR = var, VaR_sim = var_x_sim, TVaR = tvar, 
           TVaR_sim = tvar_x_sim,TVaR2_sim = tvar_x_2_sim)

