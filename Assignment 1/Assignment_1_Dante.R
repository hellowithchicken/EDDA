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
X <- seq(0.01, 0.1, 0.0005)
pvalues <- c()
t <- median(data_tele)
for (x in X){
  B <- 1000
  tstar <- numeric(B)
  n <- length(data_tele)
  
  for (i in 1:B){
    xstar <- rexp(n,x)
    tstar[i] <- median(xstar)
  }
  pl<-sum(tstar<t)/B
  pr<-sum(tstar>t)/B
  p<-2*min(pl,pr)
  pl;pr;p
  pvalues <- c(pvalues,p)
}
pvalues
plot(X, pvalues)
# There exist a Exp function that fits the hypothesis

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

hist(data_tele, prob=T, ylim=c(0,0.05))
x<-seq(0,max(data_tele),length=1000)
lines(x,exp(x),type="l",col="blue",lwd=2)

# D 

max_index <- which.max(pvalues)

opt_X <- X[max_index]

# Opt_X represents the optimal Lambda

# E
bill_bigeq40 <- sum(data_tele>=40)
bill_smal40 <- sum(data_tele<40)

binom.test(bill_bigeq40, length(data_tele),p=0.5)
binom.test(bill_smal40, length(data_tele),p=0.5)

bill_less10 <- sum(data_tele < 10)
bill_less10/length(data_tele)

# Exercise 4
# A
data_run <- read.table(file="data/run.txt", header=TRUE)

plot(before~after,pch=drink,data=data_run)
abline(0,1)
boxplot(data_run[,1],data_run[,2])
boxplot(data_run[,1]-data_run[,2])

qqnorm(data_run[,1]-data_run[,2])
shapiro.test(data_run[,1]-data_run[,2])
# normality assumed

cor.test(data_run[,1],data_run[,2])
# significant correlation

# B

data_run_lemo <- data_run[data_run[,"drink" ]=="lemo",]
data_run_energy <- data_run[data_run[,"drink" ]=="energy",]

t.test(data_run_lemo[,1], data_run_lemo[,2],paired=TRUE)
# Is the same as
t.test(data_run_lemo[,1]-data_run_lemo[,2])
# Negative

t.test(data_run_energy[,1]-data_run_energy[,2])
# Positive

# C
data_run$dif <- data_run[,1]-data_run[,2]
data_run_lemo <- data_run[data_run[,"drink" ]=="lemo",]
data_run_energy <- data_run[data_run[,"drink" ]=="energy",]

t.test(data_run_lemo$dif, data_run_energy$dif)
qqnorm(data_run_lemo$dif)
qqnorm(data_run_energy$dif) # Not really normal so other test

wilcox.test(data_run_lemo$dif, data_run_energy$dif)
ks.test(data_run_lemo$dif, data_run_energy$dif)

# D
# For experiment B there is no comparison done between the
# two groups lemo and energy. The test will therefore not 
# give any usefull results. For experiment C it is perhaps 
# better to use another test?

# Exercise 5
# A

