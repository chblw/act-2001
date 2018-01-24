# Dépannage 01-25-18

## Package actuar 

#### 1.4.1 ####

rm(list=ls())

# Définition des paramètres
lam <- 5
r <- c(0.5, 5)
q <- c(10 / 11, 0.5)

kappa <- c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)

# Vecteur de valeurs possibles pour les variables 
k <- 0:50

# Élément i de la liste correspond à toutes valeurs possibles pour chaque hyp de M
pmf <- list()
pmf[[1]] <- dpois(k, lam)
pmf[[2]] <- dnbinom(k, r[1], q[1])
pmf[[3]] <- dnbinom(k, r[2], q[2])

# Validation que k est assez gros
sapply(1:3, function(t) sum(pmf[[t]]))

# Voir les valeurs/probs pur hyp 1
data.frame(k, pmf[[1]], cumsum(pmf[[1]]))

# Définition de la fonction pour la VaR
var_fun <- function(kappa, pmf)
  k[min(which(cumsum(pmf) >= kappa))]

# Définition de la fonction pour la TVaR
tvar_fun <- function(kappa, pmf){
  
  # Pour savoir à partir de quel indice qu'on atteint la VaR
  pos <- min(which(cumsum(pmf) >= kappa))
  
  # Espérance tronquée
  etronq <- sum(k[(pos+1):length(k)] * pmf[(pos+1):length(k)])  
  
  # Partie de droite dans l'équation
  autre <- var_fun(kappa, pmf) * (cumsum(pmf)[pos] - kappa)
  
  (etronq + autre) / (1 - kappa)
  
  
}

# Matrice pour les VaR
# Chaque colonne est une variable pour M et chaque ligne valeur pour kappa
var <- matrix(nrow = length(kappa), ncol = 3)

# Le sapply est "la version" efficace des bouches for en R
var <- 1000 * sapply(1:3, function(i) sapply(1:length(kappa), function(j) 
  var_fun(kappa[j], pmf[[i]])))

# Version inefficace
#for(i in 1:3){  ## Pour chaque hyp
#  for(j in 1:length(kappa))  ## Pour chaque kappa
#    var[j,i] <- 1000 * var_fun(kappa[j], pmf[[i]])
#}

# Matrice pour la TVaR
tvar <- matrix(nrow = length(kappa), ncol = 3)

tvar <- 1000 * sapply(1:3, function(i) sapply(1:length(kappa), function(j) 
  tvar_fun(kappa[j], pmf[[i]])))

# Dataframe pour les résultats
res <- data.frame(kappa, var, tvar)
colnames(res) <- c("Kappa", "VaR Pois", "VaR BN1", "VaR BN2", 
                   "TVaR Pois", "TVaR BN1", "TVaR BN2")
res

#### 1.4.4 ####

rm(list=ls())
# Pondérations des normales
w <- c(0.8, 0.2)

# Paramètres
mu <- c(0.1, -0.3)
sd <- c(0.2, 0.1)

# R fait des opérations vectorielles
ex <- sum(w * mu) ## Voici ce qu'il fait :  w[1] * mu[1] + [w2] + mu[2]
ex2 <- sum(w * (sd^2 + mu^2))
sdx <- sqrt(ex2 - ex^2)
FX <- sum(w * pnorm(0, mu, sd)) 

data.frame(EX = ex, VarX = sdx^2, FX = FX)

kappa <- c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)

# Définition de la fonction de répartition
FX_fun <- function(x)
  sum(w * pnorm(x, mu, sd))

# On veut Fx(x) = kappa et on utilise valeur absolue car on veut minimiser (optimize) 
# Si on utilise uniroot, pas besoin de valeur absolue

# Selon optimize - Potentiel de gossage pour trouver les bornes
var_optim <- sapply(kappa, function(t) 
  optimize(function(x) abs(FX_fun(x) - t), c(-2,2))$minimum)

# Selon uniroot 
var_uni <- sapply(kappa, function(t) 
  uniroot(function(x) FX_fun(x) - t, c(-2,2))$root)

# Devrait être = Kappa en théorie
FX <- sapply(var_optim, function(x) FX_fun(x))


# install.packages("pracma")
library(pracma)


# Quad est souvent plus efficace que integrate. À privilégier
tvar_quad <- sapply(1:length(kappa), function(k) quad(function(x) 
  x * (w[1] * dnorm(x, mu[1], sd[1]) + w[2] * dnorm(x, mu[2], sd[2])), 
  var_optim[k], 2)) / (1 - kappa)

# Voilà un exemple avec Integrate
tvar_integrate <- sapply(1:length(kappa), function(k) integrate(function(x) 
  x * (w[1] * dnorm(x, mu[1], sd[1]) + w[2] * dnorm(x, mu[2], sd[2])), 
  var_optim[k], 2)$value) / (1 - kappa)

es <- sapply(1:length(kappa), function(k) quad(function(x) 
  x * (w[1] * dnorm(x, mu[1], sd[1]) + w[2] * dnorm(x, mu[2], sd[2])), 
  -2, var_optim[k])) / kappa

# Vérification
data.frame(DevraitEtreEX = (1-kappa) * tvar_quad + kappa * es, EX = ex)

# Données
round(data.frame(kappa, FX_VaR = FX, VaR_Optim = var_optim, VaR_Uni = var_uni,
                 TVaR_Quad = tvar_quad, TVaR_Integrate = tvar_integrate, ES = es), 4)

