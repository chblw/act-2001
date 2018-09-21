T_i <- c(0.3055285, 0.7095883, 0.7968152,
0.9986248, 1.411262, 1.690603, 2.21957, 2.368388, 3.567566, 4.041499,
4.39206, 4.979485, 6.380634, 6.98289, 7.753156, 8.309093, 8.5659,
9.045187, 9.718129, 10.5263, 10.7752, 11.26835, 11.43449, 11.55362,
12.25831, 12.72269, 13.02203, 14.06437, 14.58528, 14.67648, 14.83452)

W_i <- diff(c(0, T_i))
par(mfrow = c(1, 2))

# a) ----------------------------------------------------------------------

param_poisson <- mean(W_i)
logvrais_poisson <- sum(log(dpois(W_i, param_poisson)))

plot.ecdf(W_i, main = "Exponentiel")
curve(pexp(x, 1/param_poisson), add = TRUE)

# b) ----------------------------------------------------------------------

neglogvrais <- function(params) {
  - sum(log(dweibull(W_i, params[1], params[2])))
}

param_weibull <- constrOptim(c(1, 1), neglogvrais, NULL, diag(2), c(0, 0))
param_weibull$par

plot.ecdf(W_i)
curve(pweibull(x, param_weibull$par[1], param_weibull$par[2]), add = TRUE)

# c) ----------------------------------------------------------------------

R <- 2 * (- param_weibull$value + param_poisson$value)
