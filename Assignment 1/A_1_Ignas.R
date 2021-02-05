library(tidyverse)
library(rstudioapi) 

bootstarp <- function(data, n, fun = mean) {
  results <- c()
  n = length(data)
  for (i in 1:n){
    bs_sample <- sample(data$Bills, size = n, replace = T)
  }
  results <- c(results, fun(bs_sample))
  return(results)
}


#----------Exercise 3----------------
setwd(dirname(getActiveDocumentContext()$path))
data <- read.table(file="data/telephone.txt",header=TRUE)

# a - create a histogram plot
ggplot(data, aes(x=Bills)) +
  geom_histogram()

# b - do bootstrap

p_values <- c()
lambdas <- seq(0.01, 0.1, 0.000001)
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
plot(lambdas, p_values)
