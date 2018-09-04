
# Source and library ------------------------------------------------------

library(actuar)
library(RCurl)

# Import dataset ----------------------------------------------------------

dataset <- read.csv(text=getURL("https://cdn.rawgit.com/chblw/actrisk-data/c53de681/sweden_data.csv"))
loss_data <- dataset$data
loss_data <- loss_data[loss_data > 0]  # Une perte de 0 n'est pas une perte

# b) ----------------------------------------------------------------------

hist(loss_data)

# d) ----------------------------------------------------------------------

mle_exp <- function(param){
  sum(log(dexp(loss_data, param)))
}

mle_gam <- function(param){
  sum(log(dgamma(loss_data, param[1], param[2])))
}

mle_ln <- function(param){
  sum(log(dlnorm(loss_data, param[1], param[2])))
}

mle_pa <- function(param){
  sum(log(dpareto(loss_data, param[1], param[2])))
}

(param_exp <- optimize(function(t) mle_exp(t), c(0,2), maximum=TRUE)$maximum)

(param_gam <-optim(c(9.9, 0.4), mle_gam, method="L-BFGS-B", control=list(fnscale = -1),
                   lower=c(0.01, 0.01), upper=c(20, 20))$par)

(param_ln <-optim(c(1, 1), mle_ln, method="L-BFGS-B", control=list(fnscale = -1),
                  lower=c(0.1, 0.1), upper=c(2, 1.5))$par)

(param_pa <- constrOptim(c(1.1, 1.1), mle_pa, grad = NULL, 
                         ui = c(0, 1), ci = 0, control=list(fnscale = -1))$par)

# c) ----------------------------------------------------------------------

plot()








d <- head(sort(fire), -1)

# Empirique
el_emp <- sapply(d, function(x) sum((fire-x) * (fire > x)) / sum(fire > x))

plot(d, el_emp, main='Excess Loss Empirique', 
     xlab='Montants', ylab='Perte', cex=0.4)

# Théoriques
el_exp <- rep(1/param_exp, length(d))

el_gam <- sapply(d, function(x) (1-pgamma(x, param_gam[1] + 1, param_gam[2])) / 
                   (1-pgamma(x, param_gam[1], param_gam[2])))
el_gam <- param_gam[1] / param_gam[2] * el_gam - d

el_ln <- sapply(d, function(x) 
  (1-pnorm((log(x) - param_ln[1] - param_ln[2]^2) / param_ln[2])) / 
    (1-pnorm((log(x) - param_ln[1]) / param_ln[2])))
el_ln <- exp(param_ln[1] + param_ln[2]^2 / 2) * el_ln - d

el_pa <- (param_pa[2] + d) / (param_pa[1] - 1)

plot(d, el_emp, main='Excess Loss', 
     xlab='Montants', ylab='Loss', cex=0.4)

lines(d, el_exp, col=2)
lines(d, el_gam, col=3)
lines(d, el_ln, col=4)
lines(d, el_pa, col=6)
legend('topleft', legend=c('Empirique', 'Exp', 'Gamma', 'LogNorm', 'Pareto'), 
       col=c(1,2,3,4,6), 
       lty=c(3,1,1,1,1), cex=0.6)

#### Probable Maximum Loss
kappa <- c(0.99, 0.995, 0.998, 0.999)

pml_exp <- qexp(kappa, param_exp)
pml_gam <- qgamma(kappa, param_gam[1], param_gam[2])
pml_ln <- qlnorm(kappa, param_ln[1], param_ln[2])
pml_pa <- qpareto(kappa, param_pa[1], param_pa[2])

pml_res <- cbind(pml_exp, pml_gam, pml_ln, pml_pa)

#### Goodness of fit

p1 <- hist(fire)

n <- length(fire)

# Prob observée dans chaque intervalle
p_obs <- p1$counts / n

# Prob espérée dans chaque intervalle
p_expected_exp <- sapply(1:length(p1$counts), function(t) 
  pexp(p1$breaks[t+1], param_exp) - pexp(p1$breaks[t], param_exp))

p_expected_gam <- sapply(1:length(p1$counts), function(t) 
  pgamma(p1$breaks[t+1], param_gam[1], param_gam[2]) - 
    pgamma(p1$breaks[t], param_gam[1], param_gam[2]))

p_expected_ln <- sapply(1:length(p1$counts), function(t) 
  plnorm(p1$breaks[t+1], param_ln[1], param_ln[2])- 
    plnorm(p1$breaks[t], param_ln[1], param_ln[2])) 

p_expected_pa <- sapply(1:length(p1$counts), function(t) 
  ppareto(p1$breaks[t+1], param_pa[1], param_pa[2]) - 
    ppareto(p1$breaks[t], param_pa[1], param_pa[2]))

sum(p_expected_exp)
sum(p_expected_gam)
sum(p_expected_ln)
sum(p_expected_pa)

# La dernière valeure ajustée pour avoir somme prob = 1
p_expected_exp[length(p_expected_exp)] <- p_expected_exp[length(p_expected_exp)] + 
  1 - sum(p_expected_exp)

p_expected_gam[length(p_expected_gam)] <- p_expected_gam[length(p_expected_gam)] + 
  1 - sum(p_expected_gam)

p_expected_ln[length(p_expected_ln)] <- p_expected_ln[length(p_expected_ln)] + 
  1 - sum(p_expected_ln)

p_expected_pa[length(p_expected_pa)] <- p_expected_pa[length(p_expected_pa)] + 
  1 - sum(p_expected_pa)

# Test Khi-carré
khi_exp <- chisq.test(p_obs, p=p_expected_exp)$p.value
khi_gam <- chisq.test(p_obs, p=p_expected_gam)$p.value
khi_ln <- chisq.test(p_obs, p=p_expected_ln)$p.value
khi_pa <- chisq.test(p_obs, p=p_expected_pa)$p.value

khi_res <- c(khi_exp, khi_gam, khi_ln, khi_pa)

# Kolmogorov-Smirnov
fn <- ecdf(fire)
kappa <- fn(fire)

ks_exp <- max(sapply(1:n, function(x) 
  abs(x/n - pexp(sort(fire)[x], param_exp))))

ks_exp <- max(ks_exp, sapply(1:n, function(x) 
  abs((x-1) / n - pexp(sort(fire)[x], param_exp))))

ks_gam <- max(sapply(1:n, function(x) 
  abs(x/n - pgamma(sort(fire)[x], param_gam[1], param_gam[2]))))

ks_gam <- max(ks_gam, sapply(1:n, function(x) 
  abs((x-1) / n - pgamma(sort(fire)[x], param_gam[1], param_gam[2]))))

ks_ln <- max(sapply(1:n, function(x) 
  abs(x/n - plnorm(sort(fire)[x], param_ln[1], param_ln[2]))))

ks_ln <- max(ks_ln, sapply(1:n, function(x) 
  abs((x-1) / n - plnorm(sort(fire)[x], param_ln[1], param_ln[2]))))

ks_pa <- max(sapply(1:n, function(x) 
  abs(x/n - ppareto(sort(fire)[x], param_pa[1], param_pa[2]))))

ks_pa <- max(ks_pa, sapply(1:n, function(x) 
  abs((x-1) / n - ppareto(sort(fire)[x], param_pa[1], param_pa[2]))))

ks_res <- c(ks_exp, ks_gam, ks_ln, ks_pa)

# AIC
aic_exp <- 2*mle_exp(param_exp) - 2 * length(param_exp)
aic_gam <- 2*mle_gam(param_gam) - 2 * length(param_gam)
aic_ln <- 2*mle_ln(param_ln) - 2 * length(param_ln)
aic_pa <- 2*mle_pa(param_pa) - 2 * length(param_pa)
aic_res <- c(aic_exp, aic_gam, aic_ln, aic_pa)

# Résultats
adequation <- round(data.frame(khi_res, ks_res, aic_res), 5)
colnames(adequation) <- c('p Value (khi2)', 'Kolmogorov-Smirnov', 'AIC')
rownames(adequation) <- c('Exp', 'Gamma', 'Lor Norm', 'Pareto')
adequation

# Probable Maximum loss
pml_df <- data.frame(pml_res)
rownames(pml_df) <- paste(c('0.99', '0.995', '0.998', '0.999'))
pml_df

#### QQplot

# Exponentielle
qqplot(qexp(ppoints(500), param_exp), fire, 
       xlab = "Quantiles théoriques", ylab='Quantiles empiriques', main='Exp')
qqline(fire, distribution = function(p) qexp(p, param_exp), col=2)

# Gamma
qqplot(qgamma(ppoints(500), param_gam[1], param_gam[2]), fire, 
       xlab = "Quantiles théoriques", ylab='Quantiles empiriques', main='Gamma')
qqline(fire, distribution = function(p) qgamma(p, param_gam[1], param_gam[2]), col=2)

# Log Normale
qqplot(qlnorm(ppoints(500), param_ln[1], param_ln[2]), fire, 
       xlab = "Quantiles théoriques", ylab='Quantiles empiriques', main='Log Norm')
qqline(fire, distribution = function(p) qlnorm(p, param_ln[1], param_ln[2]), col=2)

# Pareto
qqplot(qpareto(ppoints(500), param_pa[1], param_pa[2]), fire, 
       xlab = "Quantiles théoriques", ylab='Quantiles empiriques', main='Pareto')
qqline(fire, distribution = function(p) qpareto(p, param_pa[1], param_pa[2]), col=2)
