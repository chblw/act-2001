library(actuar)

borne_inf <<- c(0, 500, 1000, 1500, 2000, 3000, 5000, 10000)
borne_sup <<- c(borne_inf[-1], 100000)

nombre_par_classe <- c(905, 135, 56, 26, 38, 41, 23, 18)

generateur_neglogvrais <- function(loi) {
  function(params) {
    - sum(nombre_par_classe * log(loi(borne_sup, params[1], params[2]) - 
                                      loi(borne_inf, params[1], params[2])))
  }
}

test_adequation <- function(loi, theta_init) {
  
  neglogvrais <- generateur_neglogvrais(loi)
  
  if(identical(loi, plnorm)) {
    parametres_mle <- constrOptim(theta_init, neglogvrais, grad = NULL, 
                                  ui = c(0, 1), ci = 0)
  } else {
    parametres_mle <- constrOptim(theta_init, neglogvrais, grad = NULL, 
                                  ui = diag(2), ci = c(0, 0))
    
  }
  
  densite_groupe <- diff(loi(c(0, borne_sup), parametres_mle$par[1], 
                         parametres_mle$par[2]))
  
  esperence_groupe <- sum(nombre_par_classe) * densite_groupe
  
  Q <- sum((esperence_groupe - nombre_par_classe) ^ 2 / esperence_groupe)
  Q_critique <- qchisq(0.95, length(esperence_groupe) - length(parametres_mle$par) - 1)
  
  print(paste0("Parametre ", 1:2, " : ", parametres_mle$par))
  print(paste0("Log-vraisemblance : ", -parametres_mle$value))
  print(paste0("Valeur Q : ", Q))
  print(paste0("Valeur critique : ", Q_critique))
  ifelse(Q > Q_critique, print("On rejette"), print("On ne rejette pas"))
  print(paste0("BIC : ", 2 * parametres_mle$value - length(parametres_mle$par) * log(sum(nombre_par_classe))))
}

test_adequation(loi = pgamma, theta_init = c(100, 1/100))
test_adequation(loi = plnorm, theta_init = c(5, 2))
test_adequation(loi = ppareto, theta_init = c(2, 100))
