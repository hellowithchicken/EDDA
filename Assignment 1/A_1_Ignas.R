library(tidyverse)
library(rstudioapi) 

bootstarp <- function(data, fun = mean) {
  results <- c()
  statistic = fun(data)
  n = length(data)
  for (i in 1:n){
    bs_sample <- sample(data, size = n, replace = T)
    results <- c(results, fun(bs_sample))
  }
  CI_25=quantile(results,0.025)
  CI_975=quantile(results,0.975)
  CI = c(2*statistic-CI_975,2*statistic-CI_25)
  return_list <- list(statistic, results, CI)
  return(return_list)
}


#----------Exercise 3----------------
setwd(dirname(getActiveDocumentContext()$path))
data <- read.table(file="data/telephone.txt",header=TRUE)

# a - create a histogram plot
ggplot(data, aes(x=Bills)) +
  geom_histogram()

# b - do bootstrap

p_values <- c()
lambdas <- seq(0.01, 0.1, 0.0001)
for (lambda in lambdas){
  B <- 1000
  tstar=numeric(B)
  n=length(data$Bills)
  for (i in 1:B){
    xstar=rexp(n,lambda)
    tstar[i]=median(xstar)}
  #hist(tstar,prob=T)
  
  t = median(data$Bills)
  pl=sum(tstar<t)/B 
  pr=sum(tstar>t)/B 
  p=2*min(pl,pr)
  p_values <- c(p_values, p)
}

df_lambdas <- tibble(lambdas, p_values)

ggplot(df_lambdas, aes(x = lambdas, y = p_values)) +
  geom_point()

# c - bootstrap confidence interval

bootstarp(data$Bills, median)
