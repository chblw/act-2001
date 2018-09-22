nb_sinistres <- 0:6
nb_contrats <- c(190, 681, 692, 345, 77, 13, 2)

x <- rep(nb_sinistres, nb_contrats)

# H1 ----------------------------------------------------------------------

param_poisson <- mean(x)

plot.ecdf(x)
points(nb_sinistres, ppois(nb_sinistres, param_poisson), pch = 19, col = 2, type = "l")

legend(4, 0.2, c("Empirique", "Poisson"), lty = c(1, 1), col = 1:2)


# H2 ----------------------------------------------------------------------


