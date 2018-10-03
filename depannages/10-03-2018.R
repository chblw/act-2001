
library(actuar)

# exemple: somme de deux pareto indep -------------------------------------

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
legend("bottomright",col=c(2,3,4,2,3,4), cex=0.7,legend=c("upper,  h=4","upper,  h=2", "upper,  h=1","lower, h=4","lower, h=2","lower, h=1"),lty=c(1,1,1,1,2,2,2))
