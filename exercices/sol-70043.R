recur.nrisks<-function(ff,nn=5,smax=100) {
  # convolution de n fns de masses de probabilité avec
  # elle-meme
  # premier algorihtme de DePril
  ll<-length(ff)
  ffs<-ff[1]^nn
  ff<-c(ff,rep(0,smax-ll+1))
  for (i in 1 :smax)
  {
    j<-i+1
    ffs<-c(ffs,(1/ff[1])*sum(ff[2 :j]*ffs[i :1]*((nn+1)*(1 :i)/i-1)))
  }
  return(ffs)
}


beta1 <- 0.1
beta2 <- 0.5

alpha <- 5/8
n <- 20

q <- beta1 / beta2

fk <- c(alpha * q + (1 - alpha), alpha * q * (1 - q) ** (1:999))

fl <- recur.nrisks(fk, n, 1000)

fl <- c(rep(0, 20), fl)

fl[1 + c(50, 60, 70) ]

Fs <- function(x) fl[1] + sum(fl[-1] * pgamma(x, 1:1020, beta2))

sapply(c(100, 140, 200, 300), Fs)
