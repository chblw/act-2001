# 3b

lam <- 1 / 50
tau <- 1 / 2

qwei <- function(u) 1 / lam * (- 1 * log(1 - u))^(1 / tau)

sapply(c(0.001, 0.5, 0.999), qwei)

# 4b

lam <- 20
tau <- 2.5

qq <- function(u) lam / ( u^(-1) - 1 )^{1 / tau}

sapply(c(0.001, 0.5, 0.999), qq)


# 5b

alph <- 2.5
lam <- 100
tau <- 2

qpar2 <- function(u) (lam * ( (1 - u)^(- 1 / alph) - 1))^(1 / tau)
sapply(c(0.001, 0.5, 0.999), qpar2)


ppar2 <- function(x) 1 - (lam / (lam + x^tau))^alph

sapply(sapply(c(0.001, 0.5, 0.999), qpar2), ppar2)


# 6
## a

x <- c(0, 500, 1200, 2700, 5000)
fx <- c(0.4, 0.1, 0.3, 0.15, 0.05)

sum(x * fx)

## b

sum(pmax(x - 2500, 0) * fx)

## c

sum(pmin(x, 2500) * fx)

## d

sum((x * fx)[x > 2500])

## e

sum((x * fx)[x <= 2500])

## f

Fx <- cumsum(fx)

x[sapply(c(0.5, 0.6), function(u) min(which(Fx >= u)))]

sum((x * fx)[x > 500])
sum((x * fx)[x > 1200])

## g

sum((x * fx)[x <= 500])
sum((x * fx)[x <= 1200])

# 7

x <- c(0, 20, 50, 100, 300)
fx <- c(0.2, 0.05, 0.35, 0.3, 0.1)

## a

sum(x * fx)

## b

sum(pmax(x - 80, 0) * fx)

## c

sum(pmin(x, 80) * fx)

## d

sum((x * fx)[x > 80])

## e

sum((x * fx)[x <= 100])

# 9

## a

0.5 * exp(0.08 / 0.2)

## c

0.2 * log(2 * 0.001) + 0.08
- 0.2 * log(2 * (1 - 0.999))

## d

### ii

10 * exp(0.08) / (1 - 0.2 ** 2)
10 ** 2 * exp(2 * 0.08) * ( 1 / (1 - 4 * 0.2 ** 2) - 1 / (1 - 0.2 ** 2) ** 2)

### iv

1 - 1 / 2 * exp(- 0.08 / 0.2)

### v

0.5 * (1.5) ** (-5) * exp(0.08 * 5)

0.5 * exp(-(log(15/10) - 0.08) / 0.2) 



