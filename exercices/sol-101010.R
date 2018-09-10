library(RCurl)
dataset <- read.csv(text=getURL("https://cdn.rawgit.com/actrisk/actrisk-data/4929e130/hurricane.csv"))

loss_data <- dataset[, 5]

neglogvrais <- function(params) {
  - sum(log(dlnorm(loss_data, params[1], params[2])))
}

parametres_mle <- constrOptim(c(2, 0.8), neglogvrais, grad = NULL, ui = c(0, 1), ci = 0)

parametres_mle$par
