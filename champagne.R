# Part I: Modeling

# Modeling the Champagne glass
# non-vectorized function:
g <- function(x) {
  if(x < 0) {
    return(0)
  } else if (x < .5) {
    return(15)
  } else if (x < 10) {
    return(2)
  } else if (x < 15) {
    return(8 * log2(x - 9) + 2)
  } else if (x <= 20) {
    return(8 * log2(6) + 2)
  } else {
    return(0)
  }
}

g(c(1,2))

# i. 
f1 <- function(x) {
  y <- double(length(x))
  for(i in seq_along(y)) {
    y[i] <- g(x[i])
  }
  y
}

# ii.
library(purrr)
f2 <- function(x) map_dbl(x, g)

# iii. using vapply
f3 <- function(x) vapply(X = x, FUN = g, FUN.VALUE = double(1))

# iv.
f4 <- Vectorize(g, "x")

# comparison
set.seed(123)
x <- runif(1000, 0, 20)
library(microbenchmark)
microbenchmark(f1(x), f2(x), f3(x), f4(x), times = 100)

# a vectorized version of f
f5 <- function(x) 15 * (0 <= x & x < .5) + 2 * (.5 <= x & x < 10) + 
  (8 * log2(abs(x - 9)) + 2) * (x >= 10 & x < 15) + (8 * log2(6) + 2) * (x >= 15 & x <= 20) 

# note: we use log2(abs(x - 9)) to avoid problems with evaluation of log2 with negative values.

microbenchmark(f1(x), f2(x), f3(x), f4(x), f5(x), times = 1000)

# Remark: the vectorized implementation is clearly the most efficient however not the easiest to read.

f <- function(x) {
  structure(15 * (0 <= x & x < .5) + 2 * (.5 <= x & x < 10) + 
              (8 * log2(abs(x - 9)) + 2) * (x >= 10 & x < 15) + (8 * log2(6) + 2) * (x >= 15 & x <= 20) ,
            class = "champagne")
}

plot.champagne <- function(x, from=0, to=20, ...) {
  curve(x, from=from, to=to, ...)
}

x <- f(1)
plot(x)

# Computing the volume of Champagne glass
I_approx <- integrate(f, lower = 10, upper = 20)$value
I <-  4 * (-10 + 22 * log(6) + log(32)) / log(2)
abs(I - I_approx)

V_approx <- pi * integrate(function(x) f(x)^2, lower = 10, upper = 20)$value
V <- pi * 8 * (80 + 5*log(2)^2 + 88*log(6)^2 + 4 * (11 * log(6) - 5) * log(2) - 96 * log(6)) / log(2)^2
abs(V - V_approx)

content <- V / 1000

# What is the value of such that 0.2 liters is poured in the glass? 
fit1 <- uniroot(function(y) pi * integrate(function(x) f(x)^2, lower = 10, upper = y)$value / 1000 - 0.2, lower = 10, upper = 20)
fit1$root

fit2 <- optimize(function(y) (pi * integrate(function(x) f(x)^2, lower = 10, upper = y)$value / 1000 - 0.2)^2, lower = 10, upper = 20)
fit2$minimum

# Organizing a party where Champagne is served
n <- 1e4 # number of party
set.seed(123L)
G <- rpois(n, lambda = 50) # number of guest per party
D <- sapply(G, function(x) sum(rpois(x, lambda = 1.5))) # number of glasses per party
simu_b <- function(x) {
  b <- rnorm(x, mean = 11.15, sd = 0.5)
  while(any(b<=10)) b[b<=10] <- rnorm(sum(b<=10), mean = 11.15, sd = 0.5)
  b
}
B <- lapply(D, simu_b) # quantity poured by the barman
vol <- function(b) pi * integrate(function(x) f(x)^2, lower = 10, upper = b)$value / 1000
volV <- Vectorize(vol, "b")
total_liters <- sapply(B, function(x) sum(volV(x)))
total_bottles <- sapply(total_liters, function(x) ceiling(x / .75))

boxplot(total_liters)
barplot(table(total_bottles))
quantile(total_bottles, probs = .99)
