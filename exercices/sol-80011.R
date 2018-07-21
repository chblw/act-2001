m <- c(4, 1000, 1000000)

genx <- function(m, i){
  if(i == 1){
    x <- -100 * log(1 - (1:m) / (m + 1))
  } else if(i == 2){
    x <- 100 * exp(- 0.04 / 2 + 0.2 * qnorm(1:m / (m + 1)))
  }
  return(x)
}

Varx <- function(x){ 
  # Je ne prend pas la fonction var car elle prend n-1 comme denominateur
  Var <- mean(x ** 2) - mean(x) ** 2
  return(Var)
}

quantile_key <- function(kap, x){
  m <- length(x)
  key <- ceiling(m * kap) 
  return(key)
}

VaRx <- function(kap, x){
  key <- quantile_key(kap, x)
  VaR <- x[key]
  return(VaR)
}

TVaRx <- function(kap, x){
  key <- quantile_key(kap, x)
  VaR <- VaRx(kap, x)
  TVaR <- 1  / (1 - kap) * (mean(x * (x > VaR)) + VaR * (key / length(x) - kap))
  return(TVaR)
}

Cov12 <- function(x1, x2, como){
  if(como == FALSE){
    x2 <- rev(x2)
  }
  Cov <- mean(x1 * x2) - mean(x1) * mean(x2)
  return(Cov)
}

Rho12 <- function(x1, x2, como){
  var1 <- Varx(x1)
  var2 <- Varx(x2)
  Cov <- Cov12(x1, x2, como)
  Rho <- Cov / sqrt(var1 * var2)
  return(Rho)
}

sloss <- function(d, s){
  sded <- pmax(s - d, 0)
  stoploss <- mean(sded)
  return(stoploss)
}

reponses <- function(m, como){
  x1 <- genx(m, 1)
  x2 <- genx(m, 2)
  
  esp1 <- mean(x1)
  esp2 <- mean(x2)
  
  var1 <- Varx(x1)
  var2 <- Varx(x2)
  
  kap <- c(0.5, 0.75, 0.95)
  
  VaR1 <- sapply(kap, VaRx, x = x1)
  VaR2 <- sapply(kap, VaRx, x = x2)
  
  TVaR1 <- sapply(kap, TVaRx, x = x1)
  TVaR2 <- sapply(kap, TVaRx, x = x2)
  
  cov12 <- Cov12(x1, x2, como)
  rho12 <- Rho12(x1, x2, como)
  
  if(como == 1){
    ss <- x1 + x2
  } else{
    ss <- x1 + rev(x2)
  }
  
  ss <- sort(ss)
  
  d <- c(200, 400, 600)
  
  sl <- sapply(d, sloss, s = ss)
  VaR12 <- sapply(kap, VaRx, x = ss)
  TVaR12 <- sapply(kap, TVaRx, x = ss)
  
  IBM <- 1 - TVaR12 / (TVaR1 + TVaR2) 
  
  print(paste0('Les espérances de X1 et X2 sont ', esp1, " et ", esp2))
  print(paste0('Les variances de X1 et X2 sont ', var1, " et ", var2))
  print(paste0('Les VaR pour X1 sont ', VaR1[1], ", ", VaR1[2], " et ", VaR1[3]))
  print(paste0('Les VaR pour X2 sont ', VaR2[1], ", ", VaR2[2], " et ", VaR2[3]))
  print(paste0('Les TVaR pour X1 sont ', TVaR1[1], ", ", TVaR1[2], " et ", TVaR1[3]))
  print(paste0('Les TVaR pour X2 sont ', TVaR2[1], ", ", TVaR2[2], " et ", TVaR2[3]))
  print(paste0('La covariance entre X1 et X2 est ', cov12))
  print(paste0('La coéfficient de corrélation de Pearson entre X1 et X2 est ', rho12))
  print(paste0('Les primes stop loss pour S sont ', sl[1], ", ", sl[2], " et ", sl[3]))
  print(paste0('Les VaR pour S sont ', VaR12[1], ", ", VaR12[2], " et ", VaR12[3]))
  print(paste0('Les TVaR pour S sont ', TVaR12[1], ", ", TVaR12[2], " et ", TVaR12[3]))
  print(paste0('Les indices des bénéfices de mutualisation sont ', IBM[1], ", ", IBM[2], " et ", IBM[3]))
}

# a)
reponses(4, 1)  

# b)
reponses(4, 0)

# c)
reponses(1000, 1)

# d)
reponses(1000, 0)

# e)
reponses(1000000, 1)

# f)
reponses(1000000, 0)

