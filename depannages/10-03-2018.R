
library(actuar)

#----- exemple LN -----
mu <- log(10) - 0.8 ** 2 / 2
sigma <- 0.8
vect_F_upper <- function(h) cumsum(discretise(plnorm(x,mu,sigma), from=0,to=1000,step=h,method="upper"))
VaR_k_upper<- function(k,h) (min(which(vect_F_upper(h)>k)) - 1) * h
sapply(10**(-(0:3)), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_k_upper(k, h)))

h <- c(1,0.1, 0.01)
curve(plnorm(x, mu, sigma), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_upper(h[i])[1:(100 / h[i])], type = "s", col = i+1)
legend("bottomright",col=c("black",2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"upper, h=1","upper, h=0.1","upper, h=0.01"),lty=1)

h <- c(4, 2, 1)
curve(plnorm(x, mu, sigma), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c("black",2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"upper, h=4","upper, h=2","upper, h=1"),lty=1)


#lower
vect_F_lower <- function(h) cumsum(discretise(plnorm(x,mu,sigma), from=0,to=1000,step=h,method="lower"))
VaR_k_lower <- function(k,h) (min(which(vect_F_lower(h)>k)) - 1) * h
sapply(10**(-(0:3)), function(h) sapply(c(0.9999, 0.999, 0.99, 0.9), function(k) VaR_k_lower(k, h)))

#valeur exacte 

qlnorm(c(0.9, 0.99, 0.999, 0.9999),mu,sigma)


h <- c(4, 2, 1)
curve(plnorm(x, mu, sigma), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=1)
legend("bottomright",col=c("black",2,3,4,2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"lower, h=4","lower, h=2","lower, h=1"),lty=1)

h <- c(4, 2, 1)
curve(plnorm(x, mu, sigma), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
legend("bottomright",col=c("black",2,3,4,2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))

#----- exemple Pareto-----
alpha <- 1.5
lambda <- 5 
#upper
vect_F_upper <- function(h) cumsum(discretise(ppareto(x,alpha,lambda), from=0,to=3000,step=h,method="upper"))
VaR_k_upper <- function(k,h) (min(which(vect_F_upper(h)>k)) - 1) * h
bb <- sapply(10**(-(0:3)), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_k_upper(k, h)))

#lower
vect_F_lower <- function(h) cumsum(discretise(ppareto(x,alpha,lambda), from=0,to=3000,step=h,method="lower"))
VaR_k_lower <- function(k,h) (min(which(vect_F_lower(h)>k)) - 1) * h
aa <- sapply(10**(-(3:0)), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_k_lower(k, h)))

#valeur exacte 

ee <- qpareto(c(0.9, 0.99, 0.999, 0.9999),alpha,lambda)

data.frame(bb,ee, aa)

#graphes

h <- c(4, 2, 1)
curve(ppareto(x, alpha, lambda), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c("black",2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"upper, h=4","upper, h=2","upper, h=1"),lty=1)

h <- c(4, 2, 1)
curve(ppareto(x, alpha, lambda), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=1)
legend("bottomright",col=c("black",2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"lower, h=4","lower, h=2","lower, h=1"),lty=1)

h <- c(4, 2, 1)
curve(ppareto(x, alpha, lambda), from = 0, to = 100,ylab="")
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
for(i in seq_along(h)) points(seq.int(0, 100 - h[i], h[i]), vect_F_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
legend("bottomright",col=c("black",2,3,4,2,3,4), cex=0.7,legend=c(expression(paste(F[Y])),"upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))


#-----exemple LN version 2-----

# FFT -- Somme de deux v.a. discrètes indépendantes

fft.directconvo <- function(fx, fy, m=16) {
  aa <- 2^m
  nx <- length(fx)
  ny <- length(fy)
  ftx <- fft(c(fx, rep(0, aa - nx))) 
  fty <- fft(c(fy, rep(0, aa - ny))) 
  fs <- Re(fft(ftx*fty, TRUE))/aa 
  return(fs)
}

mu <- log(10) - 0.8**2/2
sigma <- 0.8
vect_f_upper <- function(h) discretise(plnorm(x,mu,sigma), from=0,to=800,step=h,method="upper")
vect_fs_upper <- function(h)  fft.directconvo(vect_f_upper(h),vect_f_upper(h),m=18)
vect_Fs_upper <- function(h) cumsum(vect_fs_upper(h))
VaR_k_upper <- function(k,h) (min(which(vect_Fs_upper(h)>k)) - 1) * h
aa <- sapply(10**(-(0:2)), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_k_upper(k, h)))


vect_f_lower <- function(h) discretise(plnorm(x,mu,sigma), from=0,to=800,step=h,method="lower")
vect_fs_lower <- function(h)  fft.directconvo(vect_f_lower(h),vect_f_lower(h),m=18)
vect_Fs_lower <- function(h) cumsum(vect_fs_lower(h))
VaR_k_lower <- function(k,h) (min(which(vect_Fs_lower(h)>k)) - 1) * h
bb<- sapply(10**(-(2:0)), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_k_lower(k, h)))

data.frame(aa,bb)


h <- c(4, 2,1)
plot(0:100,vect_Fs_upper(1)[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("upper, h=1","upper, h=2","upper, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,vect_Fs_lower(1)[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("lower, h=1","lower, h=2","lower, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,vect_Fs_lower(1)[1:101],type="s",col="blue",ylab="",xlab="x",lty=2)
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
for(i in 1:3) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))


#-----exemple Pareto version 2-----


# FFT -- Somme de deux v.a. discrètes indépendantes

fft.directconvo <- function(fx, fy, m=16) {
  aa <- 2^m
  nx <- length(fx)
  ny <- length(fy)
  ftx <- fft(c(fx, rep(0, aa - nx))) 
  fty <- fft(c(fy, rep(0, aa - ny))) 
  fs <- Re(fft(ftx*fty, TRUE))/aa 
  return(fs)
}

#-----Pareto_prise2----

alpha <- 1.5
lambda <- 5
vect_fx_upper <- function(h) discretise(ppareto(x,alpha,lambda), from=0,to=4000,step=h,method="upper")
vect_fs_upper <- function(h)  fft.directconvo(vect_fx_upper(h),vect_fx_upper(h),m=22)
vect_Fs_upper <- function(h) cumsum(vect_fs_upper(h))
VaR_S_upper <- function(k,h) (min(which(vect_Fs_upper(h)>k)) - 1) * h
aa <- sapply(10**(-(0:2)), function(h) sapply(c(0.9, 0.99, 0.999,0.9999), function(k) VaR_S_upper(k, h)))


vect_fx_lower <- function(h) discretise(ppareto(x,alpha,lambda), from=0,to=4000,step=h,method="lower")
vect_fs_lower <- function(h)  fft.directconvo(vect_fx_lower(h),vect_fx_lower(h),m=22)
vect_Fs_lower <- function(h) cumsum(vect_fs_lower(h))
VaR_S_lower <- function(k,h) (min(which(vect_Fs_lower(h)>k)) - 1) * h
bb<- sapply(10**(-(2:0)), function(h) sapply(c(0.9, 0.99, 0.999,0.9999), function(k) VaR_S_lower(k, h)))

kappa <- c(0.9, 0.99, 0.999,0.9999)
data.frame(kappa,aa,bb)

h <- c(4, 2, 1)
plot(0:100,vect_Fs_lower(1)[1:101],type="s",col="blue",ylab="",xlab="x",lty=2)
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
for(i in 1:3) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=5","upper,  h=2", "upper,  h=1","lower, h=5","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))



h <- c(4, 2,1)
plot(0:100,vect_Fs_upper(1)[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("upper, h=1","upper, h=2","upper, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,vect_Fs_lower(1)[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("lower, h=1","lower, h=2","lower, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,vect_Fs_lower(1)[1:101],type="s",col="blue",ylab="",xlab="x",lty=2)
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_lower(h[i])[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
for(i in 1:3) points(seq.int(0, 100 - h[i], h[i]), vect_Fs_upper(h[i])[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))


#----- exemple LN panjer-----
panjer.poisson<-function(lam,ff,smax)
{
  # Algorithme de Panjer
  # Cas Poisson
  # Loi discrete pour B
  aa<-0
  bb<-lam
  ll<-length(ff)
  ffs<-exp(lam*(ff[1]-1))
  ff<-c(ff,rep(0,smax-ll+1))
  for (i in 1:smax)
  {
    j<-i+1
    ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2:j]*ffs[i:1]*(bb*(1:i)/i+aa)))
  }
  return(ffs)
}

mu <- log(10) - 0.8**2/2
sigma <- 0.8
vect_f <- function(h) discretise(plnorm(x,mu,sigma), from=0,to=700,step=h,method="upper")
vect_fs <- function(h) panjer.poisson(2,vect_f(h),1000/h)
VaR_S <- function(k,h) (min(which(cumsum(vect_fs(h))>k)) - 1) * h
aa <- sapply(c(1,0.5,0.1), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_S(k, h)))

vect_f_lower <- function(h) discretise(plnorm(x,mu,sigma), from=0,to=700,step=h,method="lower")
vect_fs_lower <- function(h) panjer.poisson(2,vect_f_lower(h),1000/h)
VaR_S_lower <- function(k,h) (min(which(cumsum(vect_fs_lower(h))>k)) - 1) * h
bb <- sapply(c(1,0.5,0.1), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_S_lower(k, h)))

data.frame(aa,bb)


h <- c(4, 2,1)
plot(0:100,cumsum(vect_fs(1))[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs(h[i]))[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("upper, h=1","upper, h=2","upper, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,cumsum(vect_fs_lower(1))[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs_lower(h[i]))[1:(100 / h[i])], type = "s", col = i + 1,lty=1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("lower, h=1","lower, h=4","lower, h=2"),lty=1)

h <- c(4, 2, 1)
plot(0:100,cumsum(vect_fs_lower(1))[1:101],type="s",col="blue",ylab="",xlab="x",lty=2)
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs_lower(h[i]))[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
for(i in 1:3) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs(h[i]))[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))

h <- c(1, 0.5, 0.1)
plot(0:100,cumsum(vect_fs_lower(1))[1:101],type="s",col="blue",ylab="",xlab="x",lty=2)
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs_lower(h[i]))[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
for(i in 1:3) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs(h[i]))[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))


#-------pareto_panjer------

vect_f <- function(h) discretise(ppareto(x,alpha,lambda), from=0,to=5000,step=h,method="upper")
vect_fs <- function(h) panjer.poisson(2,vect_f(h),15000)
VaR_S <- function(k,h) (min(which(cumsum(vect_fs(h))>k)) - 1) * h
aa <- sapply(c(4,2,1,0.1), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_S(k, h)))

vect_f_lower <- function(h) discretise(ppareto(x,alpha,lambda), from=0,to=5000,step=h,method="lower")
vect_fs_lower <- function(h) panjer.poisson(2,vect_f_lower(h),15000)
VaR_S_lower <- function(k,h) (min(which(cumsum(vect_fs_lower(h))>k)) - 1) * h
bb <- sapply(c(4,2,1,0.1), function(h) sapply(c(0.9, 0.99, 0.999, 0.9999), function(k) VaR_S_lower(k, h)))

kappa <- c(0.9, 0.99, 0.999, 0.9999)

data.frame(kappa,aa,bb)


h <- c(4, 2,1)
plot(0:100,cumsum(vect_fs(1))[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs(h[i]))[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("upper, h=1","upper, h=2","upper, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,cumsum(vect_fs_lower(1))[1:101],type="s",col="blue",ylab="",xlab="x")
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs_lower(h[i]))[1:(100 / h[i])], type = "s", col = i + 1,lty=1)
legend("bottomright",col=c("blue",3,2), cex=0.7,legend=c("lower, h=1","lower, h=2","lower, h=4"),lty=1)

h <- c(4, 2, 1)
plot(0:100,cumsum(vect_fs_lower(1))[1:101],type="s",col="blue",ylab="",xlab="x",lty=2)
for(i in 1:2) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs_lower(h[i]))[1:(100 / h[i])], type = "s", col = i + 1,lty=2)
for(i in 1:3) points(seq.int(0, 100 - h[i], h[i]), cumsum(vect_fs(h[i]))[1:(100 / h[i])], type = "s", col = i + 1)
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))
