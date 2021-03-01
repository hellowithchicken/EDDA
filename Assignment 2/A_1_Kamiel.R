install.packages("tidyverse")
install.packages("rstudioapi")
install.packages("rstatix")
install.packages("ggpubr")
library(tidyverse)
library(rstudioapi) 
library(rstatix)
library(ggpubr)

##################################### Exercise 1 #####################################
setwd("C:/Users/KGulp/oneDrive/Documenten/Computational Science/Experimental design and data analysis/EDDA/Assignment 2/data")
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# Exercise 1
data=read.table(file="bread.txt",header=TRUE)
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
# We see thaat the humidity:environment column show a significant p-value which means that the there is evidence for the interaction effect between humidity and environment
summary(dataaov)

contrasts(data$humidity)=contr.sum; contrasts(data$environment)=contr.sum
dataaov2=lm(hours~humidity*environment,data=data); 
summary(dataaov2)

data$humidity=as.factor(data$humidity)
data$environment=as.factor(data$environment)
dataaov=lm(hours~humidity+environment,data=data)
anova(dataaov)



# Exercise 2
data=read.table(file="search2.txt",header=TRUE)
data

a = xtabs(time~skill+interface,data=data)
a[,3]
I = 5; B = 5; N =1

for (i in 1:B) print(sample(1:(N*I)))

data=read.table(file="search.txt",header=TRUE)
data
attach(data)
boxplot(time~interface)
boxplot(time~skill)

par(mfrow=c(1,2))
interaction.plot(skill,interface,time)
interaction.plot(interface,skill,time)

aovdata=lm(time~skill+interface)
aovdata2=lm(time~skill*interface,data=data); 
anova(aovdata2)
summary(aovdata)


friedman.test(time,interface,skill,data=data)

interfaceaov <- lm(time~interface, data = data)
# performing one-way ANOVA
anova(interfaceaov)

# Exercise 3
data=read.table(file="cow.txt",header=TRUE)
data
data$id=factor(data$id)
data$per=factor(data$per)
datalm=lm(milk~treatment+per+id,data=data)
anova(datalm)
summary(datalm)
# The Anova test does not show a significant difference for feeding stuffs
library("Matrix")
library("lme4")
attach(data)
datalmer=lmer(milk~treatment+order+per+(1|id),data=data,REML=FALSE)
summary(datalmer)
datalmer1=lmer(milk~order+per+(1|id),data=data,REML=FALSE)

anova(datalmer1, datalmer)
attach(data)
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE)


# Exercise 4
data=read.table(file="austen.txt",header=TRUE)
# Yes because we look at a a count of units in different categories of two factors which are words and story

austen = data[,1:3]
rowsum= apply(austen,1,sum)
rowsum
colsum = apply(austen,2,sum)
total = sum(austen)
expected = (rowsum%*%t(colsum))/total
round(expected, 0)

Te = sum((austen - expected)^2/expected)
1-pchisq(Te, 8)

z = chisq.test(austen)

# She is not inconsistent

residuals(z)
# main inconsistencies can be found in a, without and that

austen = data
rowsum= apply(austen,1,sum)
rowsum
colsum = apply(austen,2,sum)
total = sum(austen)
expected = (rowsum%*%t(colsum))/total
round(expected, 0)

Te = sum((austen - expected)^2/expected)
1-pchisq(Te, 8)

z = chisq.test(austen)
z

residuals(z)
# Main inconcistency is that and an 

# Exercise 5
data=read.table(file="expensescrime.txt",header=TRUE)
data

plot(data[,c(2,3,4, 5, 6, 7)]) 
par(mfrow=c(1,3))
for (i in c(2,3,4, 5, 6, 7)) hist(sat[,i],main=names(sat)[i])
                                    