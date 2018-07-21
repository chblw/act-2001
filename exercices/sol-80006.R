q1 <- 0.3
q2 <- 0.2

n1 <- 0.5
n2 <- 0.6

alpha1 <- numeric(11)
alpha1[1] <- (1 - q1) + q1 * choose(10, 0) * n1 ^ 0 * (1 - n1) ^ (10 - 0)
alpha1[2:11] <- q1 * choose(10, 1:10) * n1 ^ (1:10) * (1 - n1) ^ (10 - 1:10)

alpha2 <- numeric(11)
alpha2[1] <- (1 - q2) + q2 * choose(10, 0) * n2 ^ 0 * (1 - n2) ^ (10 - 0)
alpha2[2:11] <- q2 * choose(10, 1:10) * n2 ^ (1:10) * (1 - n2) ^ (10 - 1:10)

Fx1 <- cumsum(alpha1)
Fx2 <- cumsum(alpha2)

antimono <- function(i, j) pmax(i + j - 1, 0)
Fx1x2 <- outer(Fx1, Fx2, "antimono")

# a) ----------------------------------------------------------------------

fx1x2 <- matrix(numeric(), 11, 11)

fx1x2[1, 1] <- Fx1x2[1, 1]
fx1x2[2:11, 1] <- Fx1x2[2:11, 1] - Fx1x2[1:10, 1]
fx1x2[1, 2:11] <- Fx1x2[1, 2:11] - Fx1x2[1, 1:10]
fx1x2[2:11, 2:11] <- Fx1x2[2:11, 2:11] - Fx1x2[2:11, 1:10] - Fx1x2[1:10, 2:11] + Fx1x2[1:10, 1:10]
fx1x2

# b) ----------------------------------------------------------------------

x1x2 <- 0:10 %o% 0:10
sum(x1x2 * fx1x2)
sum(x1x2 * fx1x2) - sum(alpha1 * 0:10) * sum(alpha2 * 0:10)

# c) ----------------------------------------------------------------------

convo_i <- function(x, y, i) ifelse(x + y == i, 1, 0)
fs1 <- numeric(21)
fs2 <- numeric(21)
for(i in 0:20) {
    ident_matrix <- outer(0:10, 0:10, "convo_i", i)
    fs1[i + 1] <- sum(fx1x2 * ident_matrix)
}

fs2[1] <- fx1x2[1, 1]
fs2[2:11] <- alpha1[2:11] + alpha2[2:11]

fs1 == fs2  # erreurs de conversion et d'arrondis, 
all.equal(fs1, fs2)  # Permet de contourner ces petites erreurs

# d -----------------------------------------------------------------------

EmaxSK1 <- numeric(11)

for(k in 0:10) {
  maxsk_foo <- function(x, y) pmax(x + y - k, 0)
  maxsk <- outer(0:10, 0:10, "maxsk_foo")
  EmaxSK1[k + 1] <- sum(maxsk * fx1x2)
}

EmaxX1k <- sapply(0:10, function(k) sum(pmax(0:10 - k, 0)* alpha1))
EmaxX2k <- sapply(0:10, function(k) sum(pmax(0:10 - k, 0)* alpha2))
EmaxSK2 <- EmaxX1k + EmaxX2k

all.equal(EmaxSK1, EmaxSK2)

# Cas Comonotone ----------------------------------------------------------

comono <- function(i, j) pmin(i, j)
Fx1x2 <- outer(Fx1, Fx2, "comono")

# a) ----------------------------------------------------------------------

fx1x2 <- matrix(numeric(), 11, 11)

fx1x2[1, 1] <- Fx1x2[1, 1]
fx1x2[2:11, 1] <- Fx1x2[2:11, 1] - Fx1x2[1:10, 1]
fx1x2[1, 2:11] <- Fx1x2[1, 2:11] - Fx1x2[1, 1:10]
fx1x2[2:11, 2:11] <- Fx1x2[2:11, 2:11] - Fx1x2[2:11, 1:10] - Fx1x2[1:10, 2:11] + Fx1x2[1:10, 1:10]
fx1x2

# b) ----------------------------------------------------------------------

x1x2 <- 0:10 %o% 0:10
sum(x1x2 * fx1x2)
sum(x1x2 * fx1x2) - sum(alpha1 * 0:10) * sum(alpha2 * 0:10)

# c) ----------------------------------------------------------------------

convo_i <- function(x, y, i) ifelse(x + y == i, 1, 0)
fs1 <- numeric(21)
fs2 <- numeric(21)
for(i in 0:20) {
  ident_matrix <- outer(0:10, 0:10, "convo_i", i)
  fs1[i + 1] <- sum(fx1x2 * ident_matrix)
}

fs2[1] <- fx1x2[1, 1]
fs2[2:11] <- alpha1[2:11] + alpha2[2:11]

fs1 == fs2  # Seulement fs(0) est pareil

# d -----------------------------------------------------------------------

EmaxSK1 <- numeric(11)

for(k in 0:10) {
  maxsk_foo <- function(x, y) pmax(x + y - k, 0)
  maxsk <- outer(0:10, 0:10, "maxsk_foo")
  EmaxSK1[k + 1] <- sum(maxsk * fx1x2)
}

EmaxX1k <- sapply(0:10, function(k) sum(pmax(0:10 - k, 0)* alpha1))
EmaxX2k <- sapply(0:10, function(k) sum(pmax(0:10 - k, 0)* alpha2))
EmaxSK2 <- EmaxX1k + EmaxX2k
