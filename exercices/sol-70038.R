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

library(actuar)

h <- 100
alpha <- 2.9 - 0:4 * 0.2
eta <- 1900 - 0:4 * 200

lambda <- 0.6 - 1:5 * 0.1

lambda_n <- sum(lambda)
pj <- lambda / lambda_n

calculate_fb <- function(index) c(0, diff(ppareto(h * 0:200, alpha[index], eta[index])))

fb <- sapply(1:5, calculate_fb)

fc <- sapply(0:200, function(k) sum(pj * fb[k + 1, ]))

fc[c(21, 81)]

sum(fc * 0:200) * h

lambda_n * (3.8 - 2.2)

sum(fc * 0:200) * h * lambda_n * (3.8 - 2.2)

fs <- panjer.poisson(lambda_n * (3.8 - 2.2), fc, 200 * 4)

fs[1 + c(0, 20, 80)]

Fs <- cumsum(fs)

Fs[1 + c(0, 20, 80, 100)]

j <- 0:(200 * 4)

pi_k <- function(k) sum(pmax(100 * j - 100 * k, 0) * fs)

sapply(c(0, 20, 80, 200), pi_k)

sapply(c(0, 20, 80, 100), function(k) 100 * k + 1 / (1 - Fs[k + 1]) * pi_k(k))