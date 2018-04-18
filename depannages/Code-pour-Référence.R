#Code pour référence

#### Table de mortalité ####

t2060 <- cbind(0:110, c(0.00533, 0.00029, 0.00022, 0.00018, 0.00015, 0.00013, 0.00011, 1e-04, 1e-04, 1e-04,
                        1e-04, 0.00011, 0.00013, 0.00016, 0.00022, 3e-04, 0.00042, 0.00054, 0.00064, 0.00071, 0.00078, 0.00082,
                        0.00083, 0.00082, 0.00079, 0.00075, 0.00073, 0.00072, 0.00073, 0.00074, 0.00077, 0.00081, 0.00086, 9e-04,
                        0.00096, 0.00101, 0.00108, 0.00115, 0.00123, 0.00131, 0.00141, 0.00151, 0.00163, 0.00176, 0.00191, 0.00207,
                        0.00225, 0.00245, 0.00268, 0.00293, 0.00322, 0.00354, 0.00389, 0.00427, 0.0047, 0.00516, 0.00568, 0.00624,
                        0.00686, 0.00755, 0.0083, 0.00914, 0.01005, 0.01106, 0.01217, 0.0134, 0.01475, 0.01624, 0.01788, 0.01969,
                        0.02168, 0.02388, 0.0263, 0.02898, 0.03193, 0.03518, 0.03877, 0.04273, 0.04711, 0.05193, 0.05726, 0.06314,
                        0.06963, 0.0768, 0.08471, 0.09345, 0.10311, 0.11378, 0.12556, 0.13858, 0.15297, 0.16852, 0.1849, 0.20204,
                        0.21988, 0.2344, 0.25251, 0.27115, 0.2902, 0.30953, 0.32903, 0.34856, 0.36799, 0.38718, 0.40601, 0.42436,
                        0.44214, 0.45925, 0.47562, 0.4912, 1))

#### Algo pour simuler des uniformes ####
GNPA <- function(n, x0, m=2^31-1, a=41358){
  x <- numeric(n+1)
  x[1] <- x0
  for(i in 1:n)
    x[i+1] <- (x[i] * a) %% m
  x[-1] / m
}

#### Examen pratique Info qu'Etienne vous a envoyé ####

#### 1 ####

library(actuar)

al1 <- 4.784422
lam <- 623.945768

al2 <- 0.581976707
be <- 0.003529867

mu <- 4.60517 
sd <- 1


m <- 1000000
set.seed(20160419)

u <- runif(3 * m)

# u[1] va pour x1(1), u[2] va pour x2(1), ...
x1 <- sapply(1:m, function(x) qpareto(u[3*x - 2], al1, lam))
x2 <- sapply(1:m, function(x) qgamma(u[3*x - 1], al2, be))
x3 <- sapply(1:m, function(x) qlnorm(u[3*x], mu, sd))

s <- x1 + x2 + x3

u[1:3]
u[(3*m - 2):(3*m)]

x1[c(1, 2, m)]
x2[c(1, 2, m)]
x3[c(1, 2, m)]
s[c(1, 2, m)]


xx <- c(10,50,200)
sum((s<20)) / m

x <- 1000
FS <- sum(s > x) / m
FS

var <- sum(((s > x) - FS)^2) / (m-1)   / m

FS + c(-1,1) * sqrt(var) * qnorm(0.975)

var
(FS * (1 - FS)) / m

kappa <- 0.999
j <- kappa*m

var_s <- sort(s)[j]
tvar_s <- sum(sort(s)[(kappa*m+1):m]) / (1-kappa) / m

data.frame(kappa, var_s, tvar_s)

k <- 1:m
cVx1 <- sum(x1[k] * (s[k] == var_s))
cVx2 <- sum(x2[k] * (s[k] == var_s))
cVx3 <- sum(x3[k] * (s[k] == var_s))

data.frame(var_s, cVx1, cVx2, cVx3, test = cVx1 + cVx2 + cVx3)


(cTx1 <- sum(x1 * (s > var_s)) / (1-kappa) / m)
(cTx2 <- sum(x2 * (s > var_s)) / (1-kappa) / m)
(cTx3 <- sum(x3 * (s > var_s)) / (1-kappa) / m)

data.frame(tvar_s, cTx1, cTx2, cTx3, test = cTx1 + cTx2 + cTx3)



#### 2 ####

x <- 40 
n <- 60
b <- 100000

be <- 0.00003
ga <- log(1.1)

de <- 0.04
v <- exp(-de)

qtx <- function(x,u)
  1/ga*log(1 - ga/be * exp(-ga*x) * log(1-u))

m <- 1000
u <- GNPA(3*m, 20150418)
u[1:3]; u[2998:(3*m)]

tx1 <- sapply(1:m, function(y) qtx(x, u[3*y - 2]))
tx2 <- sapply(1:m, function(y) qtx(x, u[3*y - 1]))
tx3 <- sapply(1:m, function(y) qtx(x, u[3*y]))

tx1[c(1,2,1000)] ; tx2[c(1,2,1000)] ; tx3[c(1,2,1000)]

z1 <- b * v^tx1 * (tx1 <= n)
z2 <- b * v^tx2 * (tx2 <= n)
z3 <- b * v^tx3 * (tx3 <= n)

s <- z1 + z2 + z3

z1[c(1,1000)] ; z2[c(1,1000)] ; z3[c(1,1000)] ; s[c(1,1000)]

mean(s > b)

#### 3 ####

n <- 40
x <- 60

g <- 10000

delta <- 0.03
v <- exp(-delta)

qx <- t2060[,2]
px <- 1 - qx
  
#sx[i] = Pr(x0 > i)
sx <- cumprod(px)
  
#Pr(Tx > t)
stx <- function(x,t)
  sx[x+t] / sx[x]
  
#Fonction de valeur d'annuité discrète
ann <- function(t)
  (1 - v^t) / (1 - v)
  
k <- 0:(n-1)
  


#Vecteur de probabilités ordonné
fz <- sapply(k, function(y) stx(x, y) - stx(x, y + 1))
fz[n] <- fz[n] + stx(x, n)

fz_cum <- cumsum(fz)
  
#Valeurs possibles ordonnées
z <- g * sapply(1:n, function(y) ann(y))

data.frame(z, fz, fz_cum, Kx=0:39)

ez <- sum(fz * z)
ez2 <- sum(fz * z^2)
va <- ez2 - (ez)^2

data.frame(ez, ez2, va, EZ_Test = sum(v^(0:39) * stx(x,0:39)) * g)
  
s <- 100000 * c(1, 2, 3)
Fzz <- sapply(s, function(y) sum(fz * (z <= y)))

print(data.frame(s=s, Fz=Fzz))
  
kappa <- c(0.05, 0.99)
j <- sapply(kappa, function(y) which(fz_cum >= y)[1])
  
var <- z[j]
  
etronq <- sapply(j, function(y) sum(fz[(y+1):n] * z[(y+1):n]))
autre <- var * (fz_cum[j] - kappa)
tvar <- (etronq + autre) / (1-kappa)
  
#Si un des j = n+1, var=tvar
if(any(j == n)){
  i <- which(j == n)
  tvar[i] <- var[i]
}
  
print(data.frame(kappa=kappa, VaR=var, TVaR=tvar))


#### Divers codes comme référence ####


#### Assurance discrète cas plus général ####


#' Assurance de vie discrète avec vecteur qx
#'
#' @description Il faut seulement entrer les paramètres et tout ce qu'Étienne demande normalement est calculé.
#' Les fonctions ne sont pas idiotproof, il faut rentrer les paramètres correctement.
#' @param x âge de l'individu (0 par défaut)
#' @param n durée de contrat (1)
#' @param m années différée(0)
#' @param b prestation de décès (1)
#' @param delta taux d'intérêt (0)
#' @param qx vecteur des qx
#' @param s valeurs pour évaluer Fz (0)
#' @param kappa kappa comme d'habitude (0)
#' @export

ass_dis_qx <- function(x=0, n=1, m=0, b=1, delta=0, qx, s=0, kappa=0){
  
  px <- 1 - qx
  v <- exp(-delta)
  
  #sx[i] = Pr(x > i)
  sx <- cumprod(px)
  
  # Pr(Tx > t)
  stx <- function(x,t)
    sx[x+t] / sx[x]
  
  k <- m:(m+n-1)
  
  #Vecteur de probabilités ordonné
  fz <- sapply(k, function(y) stx(x,y) - stx(x,y+1))
  fz <- c(fz, 1 - stx(x,m) + stx(x, m+n))
  fz <- rev(fz)
  fz_cum <- cumsum(fz)
  
  #Valeurs possibles ordonnées
  z <- b * v^m * c(0, rev(v^(k+1)))
  
  ez <- sum(fz * z)
  ez2 <- sum(fz * z^2)
  va <- ez2 - (ez)^2
  
  Fzz <- sapply(s, function(y) sum(fz * (z <= y)))
  
  j <- sapply(kappa, function(y) which(fz_cum >= y)[1])
  
  var <- z[j]
  
  etronq <- sapply(j, function(y) sum(fz[(y+1):(n+1)] * z[(y+1):(n+1)]))
  autre <- var * (fz_cum[j] - kappa)
  tvar <- (etronq + autre) / (1-kappa)
  
  #Si un des j = n+1, var=tvar
  if(any(j == n+1)){
    i <- which(j == (n+1))
    tvar[i] <- var[i]
  }
  
  print(data.frame(z=z, fz=fz, Fz=fz_cum))
  print(data.frame(EZ=ez, var=va))
  print(data.frame(s=s, Fz=Fzz))
  print(data.frame(kappa=kappa, VaR=var, TVaR=tvar))
}

#' @rdname ass_dis_qx
#' @export



#### Rente discrète cas plus général ####

#' @description Il faut seulement entrer les paramètres et tout ce qu'Étienne demande normalement est calculé.
#' Les fonctions ne sont pas idiotproof, il faut rentrer les paramètres correctement.
#' @param x âge de l'individu (0 par défaut)
#' @param n durée de contrat (1)
#' @param m années différée(0)
#' @param g paiement de rente (1)
#' @param delta taux d'intérêt (0)
#' @param qx vecteur des qx
#' @param s valeurs pour évaluer Fz (0)
#' @param kappa kappa comme d'habitude (0)
#' @export

ren_dis_qx <- function(x=0, n=1, m=0, g=1, delta=0, qx, s=0, kappa=0){
  
  px <- 1 - qx
  v <- exp(-delta)
  
  #sx[i] = Pr(x > i)
  sx <- cumprod(px)
  
  #Pr(Tx > t)
  stx <- function(x,t)
    sx[x+t] / sx[x]
  
  #Fonction de valeur d'annuité discrète
  ann <- function(t)
    (1 - v^t) / (1 - v)
  
  k <- m:(m+n-1)
  
  #Vecteur de probabilités ordonné
  fz <- sapply(k, function(y) stx(x,y) - stx(x,y+1))
  fz[n] <- fz[n] + stx(x, m+n)
  fz <- c(1-stx(x,m), fz)
  fz_cum <- cumsum(fz)
  
  #Valeurs possibles ordonnées
  z <- g * v^m * sapply(1:n, function(y) ann(y))
  z <- c(0, z)
  
  ez <- sum(fz * z)
  ez2 <- sum(fz * z^2)
  va <- ez2 - (ez)^2
  
  Fzz <- sapply(s, function(y) sum(fz * (z <= y)))
  
  j <- sapply(kappa, function(y) which(fz_cum >= y)[1])
  
  var <- z[j]
  
  etronq <- sapply(j, function(y) sum(fz[(y+1):(n+1)] * z[(y+1):(n+1)]))
  autre <- var * (fz_cum[j] - kappa)
  tvar <- (etronq + autre) / (1-kappa)
  
  #Si un des j = n+1, var=tvar
  if(any(j == n+1)){
    i <- which(j == (n+1))
    tvar[i] <- var[i]
  }
  
  print(data.frame(z=z, fz=fz, Fz=fz_cum))
  print(data.frame(EZ=ez, var=va))
  print(data.frame(s=s, Fz=Fzz))
  print(data.frame(kappa=kappa, VaR=var, TVaR=tvar))
}

#' @rdname ren_dis_qx
#' @export




#### Assurance discrète paiements variables ####

x <- 60 ; n <- 10
be <- 0.00004 ; ga <- log(1.1)
de <- 0.03 ; v <- exp(-de)
b <- 1000 * c(1:5, 5:1)

stx <- function(x,t)
  exp(-be/ga * exp(ga*x) * (exp(ga*t) - 1))

vk <- 0:(n-1)
fz <- stx(x,vk) - stx(x,vk+1) ; fz <- c(fz,stx(x,n)) ; sum(fz)
z <- b * v^(vk+1) ; z <- c(z,0)

data.frame(z,fz)

sum(z*fz)
fz <- fz[order(z)] ; z <- z[order(z)]

e_z <- sum(z*fz)
e2_z <- sum(z^2 * fz)
sd_z <- sqrt(e2_z - e_z^2)
data.frame(e_z, e2_z, var=sd_z^2)

z_test <- b * exp(-0.04)^(vk+1) ; z_test <- c(z_test,0) ; z_test <- z_test[order(z_test)]
sum(z_test*fz)

z_test[min(which(cumsum(fz)>=0.9))]

kappa <- 0.95
j <- min(which(cumsum(fz) >= kappa))
var_z <- z[j]

etronq <- sum(z[(j+1):length(z)] * fz[(j+1):length(fz)])
autre <- var_z * (cumsum(fz)[j] - kappa)

tvar_z <- (etronq + autre) / (1 - kappa)

data.frame(kappa, var_z, tvar_z)
