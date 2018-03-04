# Fonction de densite et de repartition pour une somme
# de distributions Gamma. Les fonctions proviennent de 
# Moschopoulos, P.G. Ann Inst Stat Math (1985) 37: 541. 
# https://doi-org.acces.bibl.ulaval.ca/10.1007/BF02481123

GetGammaWeights <- function(alpha.params, beta.params, k.max) {
  alpha.param <- sum(alpha.params)
  beta.param <- max(beta.params)
  sigma.param <- prod((beta.params / beta.param) ^ alpha.params)
  zeta.param <- sapply(1:k.max, function(k) 
    sum(alpha.params / k * (1 - beta.params / beta.param) ^ k))
  xi.param <- 1
  for(k in 1:k.max) {
    xi.param <- c(xi.param, 1 / k * sum(1:k * zeta.param[1:k] * rev(xi.param)))
  }
  p.param <- sigma.param * xi.param
  
  return(p.param)
}

dsommegamma <- function(x, alpha.params, beta.params, k.max) {
  p.param <- GetGammaWeights(alpha.params, beta.params, k.max)
  alpha.param <- sum(alpha.params)
  beta.param <- max(beta.params)
  sum(p.param * dgamma(x, alpha.param + 0:k.max, beta.param))
}

psommegamma <- function(x, alpha.params, beta.params, k.max) {
  p.param <- GetGammaWeights(alpha.params, beta.params, k.max)
  alpha.param <- sum(alpha.params)
  beta.param <- max(beta.params)
  sum(p.param * pgamma(x, alpha.param + 0:k.max, beta.param))
}
