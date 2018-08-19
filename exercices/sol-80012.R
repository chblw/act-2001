# a) ----------------------------------------------------------------------

kap <- c(0.5, 0.85, 0.999)
VaR1 <- function(kap){
  if((kap > 0.9) & (kap < 1)){
    VaR <- - 2 * log( 1 - (kap - 0.9) / (0.1))
  } else{
    VaR <- 0
  }
  return(VaR)
}

VaR2 <- function(kap){
  if((kap > 0.8) & (kap < 1)){
    VaR <- - log( 1 - (kap - 0.8) / (0.2))
  } else{
    VaR <- 0
  }
  return(VaR)
}

VaRcomo <- function(kap){
  VaR <- VaR1(kap) + VaR2(kap)
  return(VaR)
}

sapply(kap, VaRcomo)

# b) ----------------------------------------------------------------------

kap <- c(0.5, 0.85, 0.999)

Fx1 <- function(x){
  0.9 + 0.1 * (1 - exp( - x / 2))
}

Fx2 <- function(x){
  0.8 + 0.2 * (1 - exp( - x ))
}

VaR1 <- function(kap){
  if((kap > 0.9) & (kap < 1)){
    VaR <- - 2 * log( 1 - (kap - 0.9) / (0.1))
  } else{
    VaR <- 0
  }
  return(VaR)
}

VaR2 <- function(kap){
  if((kap > 0.8) & (kap < 1)){
    VaR <- - log( 1 - (kap - 0.8) / (0.2))
  } else{
    VaR <- 0
  }
  return(VaR)
}

TVaR1 <- function(kap){
  1 / (1 - kap) * ((1 - Fx1(VaR1(kap))) * (VaR1(kap) + 2) + VaR1(kap) * (Fx1(VaR1(kap)) - kap))
}

TVaR2 <- function(kap){
  1 / (1 - kap) * ((1 - Fx2(VaR2(kap))) * (VaR2(kap) + 1) + VaR2(kap) * (Fx2(VaR2(kap)) - kap))
}

TVaR12 <- function(kap){
  TVaR1(kap) + TVaR2(kap)
}

sapply(kap, TVaR12)