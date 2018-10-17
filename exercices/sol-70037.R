panjer.poisson<-function(lam,ff,smax)
{
  aa<-0
  bb<-lam
  ll<-length(ff)
  ffs<-exp(lam*(ff[1]-1))
  ff<-c(ff,rep(0,smax-ll+1))
  for (i in 1 :smax)
  {
    j<-i+1
    ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2 :j]*ffs[i :1]*(bb*(1 :i)/i+aa)))
  }
  return(ffs)
}

lambda <- 0.6 - 0.1 * 1:5
lambda_n <- sum(lambda)

pj <- lambda / lambda_n

sapply(c(2000, 8000), function(x) sum(pj * pgamma(x, 1:5, 1 / 1000)))

gamma_k <- panjer.poisson(lambda_n * (1.25 - 0.25), c(0, pj), 1000)       

gamma_k[1:4]

Fs <- function(x) gamma_k[1] + sum(gamma_k[-1] * pgamma(x, 1:1000, 1 / 1000))
sapply(c(0, 2000, 8000, 20000), Fs)
