# Dépannage 02-08-18
# Auteur: Deniz Alic

    ## Document Euler ##

#### 4 ####
rm(list=ls())
gnpa = function(n, x0, a = 41358, m = 2^31 - 1){
  x = numeric(n+1)
  x[1] = x0
  for(i in 1:n)
    x[i + 1] = (a * x[i]) %% m
  x[-1] / m
}

m = 1000
U = matrix(gnpa(3*m, x0=20150418), nrow=m, byrow=TRUE)

x1 <- qgamma(U[, 1], 10, 1/300)
x2 <- qgamma(U[, 2], 4, 1/500)
x3 <- qgamma(U[, 3], 1, 1/1000)

s <- x1 + x2 + x3

kappa <- c(0.9 ,0.95)
vars <- sort(s)[kappa*m]
tvars <- sapply(vars, function(t) mean(s[s>t]))

cv1 <- sapply(vars, function(t) sum(x1 * (s == t)))
cv2 <- sapply(vars, function(t) sum(x2 * (s == t)))
cv3 <- sapply(vars, function(t) sum(x3 * (s == t)))

data.frame(kappa, cv1, cv2, cv3, vars, test = cv1 + cv2 + cv3)

ct1 <- sapply(vars, function(t) sum(x1 * (s > t))) / (1-kappa) / m
ct2 <- sapply(vars, function(t) sum(x2 * (s > t))) / (1-kappa) / m
ct3 <- sapply(vars, function(t) sum(x3 * (s > t))) / (1-kappa) / m

data.frame(kappa, ct1, ct2, ct3, tvars, test = ct1 + ct2 + ct3)

va_s <- var(s)
c1 <- cov(x1,s) / sqrt(va_s)
c2 <- cov(x2,s) / sqrt(va_s)
c3 <- cov(x3,s) / sqrt(va_s)

data.frame(c1, c2, c3, sqrt(va_s), test = c1 + c2 +c3)

