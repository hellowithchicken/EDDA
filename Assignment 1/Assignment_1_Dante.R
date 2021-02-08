library(tidyverse)
library(rstudioapi) 

# Exercise 1
setwd(dirname(getActiveDocumentContext()$path))
getwd()
data<-read.table(file="data/birthweight.txt",header=TRUE)

# A
bw_data<-data$birthweight
hist(bw_data)
qqnorm(bw_data)

# Estimate +- 1.96 x std.error/N^1/2

m <- mean(bw_data)
s <- sd(bw_data)
n <- length(bw_data)
error <- qnorm(0.95)*s/sqrt(n);error
ci <- c(m-error, m+error);ci

# B
t.test(bw_data,mu=2800,alternative = "greater",conf.level = 0.95)
t.test(bw_data,mu=2800,alternative = "greater",conf.level = 0.99)

# C

# Exercise 2
# A
n <- m <- 30
mu <- 180
nu <- 175
sd <- 5
B <- 1000

p <- numeric(B)

grid <- seq(175,185, by=0.25)
G <- length(grid)

fractions <- numeric(G)

for (grid_nu in 1:G){
  p <- numeric(B)
  for (b in 1:B){
    x <- rnorm(n,mu,sd)
    y <- rnorm(m,grid[grid_nu],sd)
    p[b] <- t.test(x,y, var.equal = TRUE)[[3]]
  }
  fractions[grid_nu] <- mean(p<0.05)
}
plot(grid,fractions)

# B
n <- m <- 100
mu <- 180
sd <- 5

fractions <- numeric(G)

for (grid_nu in 1:G){
  p <- numeric(B)
  for (b in 1:B){
    x <- rnorm(n,mu,sd)
    y <- rnorm(m,grid[grid_nu],sd)
    p[b] <- t.test(x,y, var.equal = TRUE)[[3]]
  }
  fractions[grid_nu] <- mean(p<0.05)
}
plot(grid,fractions)

# C
n <- m <- 30
mu <- 180
sd <- 15

fractions <- numeric(G)

for (grid_nu in 1:G){
  p <- numeric(B)
  for (b in 1:B){
    x <- rnorm(n,mu,sd)
    y <- rnorm(m,grid[grid_nu],sd)
    p[b] <- t.test(x,y, var.equal = TRUE)[[3]]
  }
  fractions[grid_nu] <- mean(p<0.05)
}
plot(grid,fractions)

# D
# The more datapoints seems to have an influence
# on the narrowness of the plot. Furthermore,
# a bigger std gives a more wider distribution
# of fractions as presented in the plot of C.

# Exercise 3
# A

data<-read.table(file="data/telephone.txt",header=TRUE)
data_tele <- data$Bills
hist(data_tele)
qqnorm(data_tele)
boxplot(data_tele)

# The data seems oddly distributed with two
# subpeaks, would have expected a more normally
# distributed set. Therefore perhaps a good 
# idea if the manager arranges the prices better.

# B
B <- 1000
T1 <- median(data_tele)
Tstar <- numeric(B)
for (i in 1:B){
  Xstar <- sample(data_tele,replace=TRUE)
  Tstar[i] <- median(Xstar)
}
Tstar25 <- quantile(Tstar,0.025)
Tstar975 <- quantile(Tstar, 0.975)

c(2*T1-Tstar975, 2*T1-Tstar25)

hist(data_tele, prob=T, ylim=c(0,0.05))
x<-seq(0,max(data_tele),length=1000)
lines(x,dexp(0.08*x),type="l",col="blue",lwd=2)

# t <- max(data_tele)
# tstar <- numeric(B)
# 
# n <- length(data_tele)
# for (i in 1:B){
#   xstar <- rexp(n,1)
#   tstar[i] <- max(xstar)
# }
# hist(tstar, prob = T)
# 
# hist(tstar,prob=T,ylim=c(0,0.4),main="histogram of tstar & true density curve of T")
# densmaxexp=function(x,n) n*exp(-x)*(1-exp(-x))^(n-1)
# # lines(rep(t,2),seq(0,2*densmaxexp(t,n),length=2),type="l", col="red", lwd=3)
# # axis(1,t,expression(paste("t") ) )
# u=seq(0,max(tstar),length=1000)
# lines(u,densmaxexp(u,n),type="l",col="blue")

# C
B <- 1000
T1 <- median(data_tele)
Tstar <- numeric(B)
for (i in 1:B){
  Xstar <- sample(data_tele,replace=TRUE)
  Tstar[i] <- median(Xstar)
}
Tstar25 <- quantile(Tstar,0.025)
Tstar975 <- quantile(Tstar, 0.975)

T1
c(2*T1-Tstar975, 2*T1-Tstar25)
