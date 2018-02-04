#Solution 1.4.2.1
#Auteur: Julien Bessette

alpha <- 1/4
beta <- 1/4
k <- 0.9995
#a)

EX <- alpha/beta #esperance de X
VX <- alpha/(beta^2) #variance de X

#b)

VaR <- qgamma(0.9995,alpha,beta) #value at risk

#c)

stop_loss <- (alpha/beta) * (1-pgamma(VaR,alpha+1,beta)) - VaR * (1 - pgamma(VaR,alpha,beta))

#d) 

TVaR <- VaR + (1/(1-k)) * stop_loss


#Solution 1.4.2.2
#Auteur: Julien Bessette

#Note: Il faudrait plutot lire LN(µ=-sigma^2 / 2, sigma^2) --> le sigma dans la parenthèse est au carré
#Voir annexe

#a) 

sig <- sqrt(log(5)) #sigma
sigc <- log(5) #sigma au carré
µ <- -(sigc / 2)

EX <- exp(µ + (sigc)/2) #esperance de X

#b) 

VaR <- qlnorm(0.9995,µ,sig)

#c) 

stop_loss <- exp(µ + sigc/2) * (1 - pnorm((log(VaR) - µ - sigc)/sqrt(sigc))) - VaR * (1 - pnorm((log(VaR) - µ)/sqrt(sigc)))

#d)

TVaR <- VaR + (1/(1-k)) * stop_loss

