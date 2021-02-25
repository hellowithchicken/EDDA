install.packages("tidyverse")
install.packages("rstudioapi")
install.packages("rstatix")
install.packages("ggpubr")
library(tidyverse)
library(rstudioapi) 
library(rstatix)
library(ggpubr)

##################################### Exercise 1 #####################################
#setwd("C:/Users/KGulp/oneDrive/Documenten/Computational Science/Experimental design and data analysis/R_data")
#setwd(dirname(getActiveDocumentContext()$path))
#getwd()

# Exercise 1
data=read.table(file="data/bread.txt",header=TRUE)
data
I=3; J=2; N=3
rbind(rep(1:I,each=N*J),rep(1:J,N*I),sample(1:(N*I*J)))

par(mfrow=c(1,2))
attach(data)
boxplot(hours~environment)
boxplot(hours~humidity)
interaction.plot(humidity,environment,hours)
interaction.plot(environment,humidity,hours)

data$environment=as.factor(data$environment);
data$humidity=as.factor(data$humidity)
dataaov=lm(hours~humidity*environment); anova(dataaov)

summary(dataaov)
