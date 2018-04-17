#Numéro 3.9
#Auteur: Julien Bessette

x <- 40 #age
n <- 20 #différé de 20 ans
b <- 1000 #prestation

t2060 <- cbind(0:110, c(0.00533, 0.00029, 0.00022, 0.00018, 0.00015, 0.00013, 0.00011, 1e-04, 1e-04, 1e-04,
                        1e-04, 0.00011, 0.00013, 0.00016, 0.00022, 3e-04, 0.00042, 0.00054, 0.00064, 0.00071, 0.00078, 0.00082,
                        0.00083, 0.00082, 0.00079, 0.00075, 0.00073, 0.00072, 0.00073, 0.00074, 0.00077, 0.00081, 0.00086, 9e-04,
                        0.00096, 0.00101, 0.00108, 0.00115, 0.00123, 0.00131, 0.00141, 0.00151, 0.00163, 0.00176, 0.00191, 0.00207,
                        0.00225, 0.00245, 0.00268, 0.00293, 0.00322, 0.00354, 0.00389, 0.00427, 0.0047, 0.00516, 0.00568, 0.00624,
                        0.00686, 0.00755, 0.0083, 0.00914, 0.01005, 0.01106, 0.01217, 0.0134, 0.01475, 0.01624, 0.01788, 0.01969,
                        0.02168, 0.02388, 0.0263, 0.02898, 0.03193, 0.03518, 0.03877, 0.04273, 0.04711, 0.05193, 0.05726, 0.06314,
                        0.06963, 0.0768, 0.08471, 0.09345, 0.10311, 0.11378, 0.12556, 0.13858, 0.15297, 0.16852, 0.1849, 0.20204,
                        0.21988, 0.2344, 0.25251, 0.27115, 0.2902, 0.30953, 0.32903, 0.34856, 0.36799, 0.38718, 0.40601, 0.42436,
                        0.44214, 0.45925, 0.47562, 0.4912, 1)) #importation de la table


qx <- t2060[,2] #probabilités de décès
px <- 1-qx #probabilité de survie
FxBarre <- cumprod(px) #probabilité pour une personne d'age 0 de vivre un certain nombre d'années

FBarre <- function(k,x=40){ #probabilité qu'une personne d'age (40) vive passe l'age (40+k)
  FxBarre[x+k]/FxBarre[x]
}

F <- function(k,x=40){ #probabilité qu'une personne d'age (40) meurt d'ici k années
  1 - FBarre(k,x)
}


#Esperance de Z (premier moment)
vt <- function(t){ #fonction de PV
  exp(-delta*(t+1))
}

EZ <- b * sum(sapply(20:70, function(i) vt(i) * FBarre(i,x=40) * qx[40+1+i])) #Méthode 1
EZ <- b * sum(sapply(20:70, function(i) vt(i) * (F(i+1,x=40) - F(i,x=40)))) #Méthode 2 #Les deux méthode reviennent au meme, j'y reviendrai mercredi


#(deuxième moment)

EZ2 <- b^2 * sum(sapply(20:70, function(i) (vt(i)^2) * FBarre(i,x=40) * qx[40+1+i])) #Méthode 1
EZ2 <- b^2 * sum(sapply(20:70, function(i) (vt(i)^2) * (F(i+1,x=40) - F(i,x=40)))) #Méthode 2 #Les deux méthode reviennent au meme, j'y reviendrai mercredi

#Variance de Z

VZ <- EZ2 - EZ^2


#Valeurs possibles de la v.a Z

ValeursPos <- b * c(0,sapply(20:70, function(i) vt(i)))
ValeursPosSort <- sort(ValeursPos)


#Valeurs de la fonction de masse de probabilité de Z

ValeursMasse <- c(F(20,x=40),sapply(70:20, function(i) FBarre(i,x=40) * qx[40+1+i]))


#Valeurs de la fonction cumulative

ValeursCumul <- cumsum(ValeursMasse)


#Tableau
cbind(ValeursPosSort,ValeursMasse,ValeursCumul)


#Valeur de la VaR

kappa <- c(0.1,0.9)

VaR <- sapply(kappa, function(k) ValeursPosSort[min(which(ValeursCumul>= k))])


#Valeur de la TVaR

TVaR0.1 <- (1/(1-0.1)) * (sum(ValeursPosSort[ValeursPosSort > VaR[1]] * ValeursMasse[ValeursPosSort > VaR[1]]) + VaR[1]*(ValeursCumul[min(which(ValeursCumul>= 0.1))] - 0.1))
TVaR0.9 <- (1/(1-0.9)) * (sum(ValeursPosSort[ValeursPosSort > VaR[2]] * ValeursMasse[ValeursPosSort > VaR[2]]) + VaR[2]*(ValeursCumul[min(which(ValeursCumul>= 0.9))] - 0.9))







