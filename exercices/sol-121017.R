
library(RCurl)

# Import dataset ----------------------------------------------------------

dataset <- read.csv(text=getURL("https://cdn.rawgit.com/actrisk/actrisk-data/b69662ae/flood_canada.csv"))
years <- 1909:2008

# a) ----------------------------------------------------------------------

count_data <- sapply(years, function(t) sum(dataset$Year == t))

# b) ----------------------------------------------------------------------

plot(years, cumsum(count_data), type = "l")

# c) ----------------------------------------------------------------------

curve(nrow(dataset) / 100 * (x - 1909), from = 1909, to = 2008, add = TRUE, col = 2)

# d) ----------------------------------------------------------------------

neglogvrais <- function(params) {
  deltaLambda <- params[1] + params[2] * (2 * seq_along(count_data) + 1) / 2
  - sum(count_data * log(deltaLambda) - deltaLambda)
}

mle_params <- constrOptim(c(1, 1), neglogvrais, grad = NULL, ui = diag(2), ci = c(0, 0))

curve(mle_params$par[1] * (x - 1909) + 0.5 * mle_params$par[2] * (x - 1909) ^ 2 , from = 1909, to = 2008, add = TRUE, col = 3)

R <- 2 * (- mle_params$value - sum(log(dpois(count_data, 0.28))))
qchisq(0.95, 1)
