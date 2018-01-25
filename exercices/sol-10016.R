## Solution à la question 1.4.2 du document d'exercices (version 2018-01-25)
## Auteur : Deniz Alic

# Définition des paramètres
lam <- 5
r <- c(0.5, 5)
q <- c(10 / 11, 0.5)

kappa <- c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)

# Vecteur de valeurs possibles pour les variables 
k <- 0:500

# Élément i de la liste correspond à toutes valeurs possibles pour chaque hyp de M
pmf <- list()
pmf[[1]] <- dpois(k, lam)
pmf[[2]] <- dnbinom(k, r[1], q[1])
pmf[[3]] <- dnbinom(k, r[2], q[2])

# Validation que k est assez gros
sapply(1:3, function(t) sum(pmf[[t]]))

# Voir les valeurs/probs pour hyp 1
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
