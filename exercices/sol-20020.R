## Solution a la question 2.4.2.1 du document d'exercices (version 2018-01-28)
## Auteur : Julien Bessette

B <- 1/2000000

#a) Les valeurs pour la VaR

VaRX1 <- qgamma(0.995,1,B)
VaRX2 <- qgamma(0.995,2,B)
VaRS <- qgamma(0.995,3,B)

#b) Les valeurs pour la TVaR

TVaRX1 <- (1/(1-0.995))*(1/B)*(1-pgamma(VaRX1,2,B))
TVaRX2 <-   (1/(1-0.995))*(2/B)*(1-pgamma(VaRX2,3,B))
TVaRS <- (1/(1-0.995))*(3/B)*(1-pgamma(VaRS,4,B))

#c) Pour kappa entre (0,1), la valeur de la TVaR_kappa sera toujours superieure 
#a la valeur de la VaR_kappa. La mesure de risque VaR correspond au montant a mettre 
#de cote de telle sorte que la probabilite que les couts excedent le capital 
#VaR_kappa est de (1-kappa)%.
#Ainsi, le capital permet de "couvrir" les couts de portefeuille avec une probabilite
#de kappa. Le defaut de la VaR est qu'elle ne tient pas en compte du comportement 
#de X au-dela de VaR_kappa. La  mesure TVaR a ete introduite pour remedier a cet inconvenient. 
#Cette mesure decrit l'esperance des couts dans la mesure que ceux-ci sont plus eleves que la valeur de la VaR.