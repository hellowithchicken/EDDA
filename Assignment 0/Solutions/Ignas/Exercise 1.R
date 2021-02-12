library(tidyverse)
library(rstudioapi) 
## set wd as wource file location
setwd(dirname(getActiveDocumentContext()$path))
###------Exercise 1-------

# a

## create samples
norm_100 <- rnorm(100)
norm_10k <- rnorm(10000)
## create qq plots
qqnorm(norm_100)
qqnorm(norm_10k)
## create histograms
hist(norm_100)
hist(norm_10k)
## calculate mean and sd
sd_100 <- sd(norm_100)
sd_10k <- sd(norm_10k)
mean_100 <- mean(norm_100)
mean_10k <- mean(norm_10k)

# b

##
less_2 <- pnorm(2)
bigger_0.5 <- 1 - pnorm(-0.5)
between__1_2 <- pnorm(2) - pnorm(-1)

# c

## verify above with data
verify_less_2 <- sum(norm_10k < 2)/length(norm_10k)
# d

## generate new data
new_10k <- rnorm(10000, 3, 2)
## find value such that 95% are smaller than it
smaller_95 <- qnorm(0.95, 3, 2)
# e

## transform standard distribution
normal <- rnorm(1000)
transformed <- -10 + 5*normal
## confirm it works
mean(transformed)
sd(transformed)


###-------Exercise 2--------
## establish faceted grid
par(mfrow=c(1,3))
## generate data
log_normal <- rlnorm(10000, meanlog = 2, sdlog = 2)
binomial_data <- rbinom(50, prob = 0.25)
uniform <- runif(60, -2, 3)
poisson_data <- rpois(200, 9)
## generate plots
hist(log_normal)
boxplot(log_normal)
qqnorm(log_normal)

###-------Exercise 3-------

## set working directory to where data is
setwd("/Users/IggyMac/OneDrive - UvA/2020-2021/EDDA/GitHub/EDDA/Assignment 0/data")

## read data

data <- read.table("mortality.txt", header = TRUE)

#data <- data %>% 
#  gather(key = "type", value = "rate", 3:4)


## plots for birthrate
hist(data$teen)
boxplot(data$teen)
qqnorm(data$teen)

## plots of mortality

hist(data$mort)
boxplot(data$mort)
qqnorm(data$mort)

## see relationship between the two

cor(data$teen, data$mort) # positively correlated
ggplot(data, aes(x = teen, y = mort)) + # draw correlation plot
  geom_point() +
  geom_smooth(method = lm)

###-------Exercise 4-------
