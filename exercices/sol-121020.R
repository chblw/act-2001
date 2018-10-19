T_i <- c(0.3055285, 0.7095883, 0.7968152,
0.9986248, 1.411262, 1.690603, 2.21957, 2.368388, 3.567566, 4.041499,
4.39206, 4.979485, 6.380634, 6.98289, 7.753156, 8.309093, 8.5659,
9.045187, 9.718129, 10.5263, 10.7752, 11.26835, 11.43449, 11.55362,
12.25831, 12.72269, 13.02203, 14.06437, 14.58528, 14.67648, 14.83452)

W_i <- diff(c(0, T_i))
par(mfrow = c(1, 2))

# a) ----------------------------------------------------------------------

param_poisson <- length(W_i) / 15

neglogvrais <- function(param) {
  - sum(log(dexp(W_i, param))) - log(1-pexp(15 - tail(T_i, 1), param))
}

mle_poisson <- optimize(neglogvrais, c(0, 10))

param_poisson <- mle_poisson$minimum
logvrais_poisson <- - mle_poisson$objective

plot.ecdf(W_i, main = "Exponentielle")
curve(pexp(x, param_poisson), add = TRUE, col = 2)
legend(0.5, 0.2, c("Empirique", "Exponentielle"), lty = c(1, 1), col = c(1, 2))

# b) ----------------------------------------------------------------------

neglogvrais <- function(params) {
  - sum(log(dweibull(W_i, params[1], params[2]))) - log(1-pweibull(15 - tail(T_i, 1), params[1], params[2]))
}

param_weibull <- constrOptim(c(1, 1), neglogvrais, NULL, diag(2), c(0, 0))
param_weibull$par

plot.ecdf(W_i, main = "Weibull")
curve(pweibull(x, param_weibull$par[1], param_weibull$par[2]), add = TRUE, col = 2)
legend(0.5, 0.2, c("Empirique", "Weibull"), lty = c(1, 1), col = c(1, 2))

# c) ----------------------------------------------------------------------

R <- 2 * (-param_weibull$value - logvrais_poisson)
qchisq(0.95, 1)
1 - pchisq(R, 1)

# d) ----------------------------------------------------------------------

1 - pexp(1, param_poisson)
1 - pweibull(1, param_weibull$par[1], param_weibull$par[2])
