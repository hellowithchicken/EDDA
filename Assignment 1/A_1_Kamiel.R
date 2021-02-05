library(tidyverse)
library(rstudioapi) 

##################################### Exercise 1 #####################################
#setwd("C:/Users/KGulp/oneDrive/Documenten/Computational Science/Experimental design and data analysis/R_data")
setwd(dirname(getActiveDocumentContext()$path))
getwd()
data=read.table(file="data/birthweight.txt",header=TRUE)

bw = data$birthweight
hist(bw)
qqnorm(bw)
boxplot(bw)
shapiro.test(bw)
m = mean(bw)
s = sd(bw)
n = length(bw)
error = qnorm(0.95)*s/sqrt(n)
error
ci = c(m-error, m+error)
ci

m_2800 = numeric(length(bw))

for (i in 1:length(bw)){m_2800[i] = 2800}
m_2800
t.test(bw,m_2800, alternative = "greater", conf.level = 0.95)
#################################### Exercise 2 #######################################

###################################2a############################################
n = 30
m = 30
mu = 180
nu = 175
sd = 5
x = rnorm(n,mu,sd)
y = rnorm(n,nu,sd)

t.test(x, y)

p = numeric(1000)

se = seq(175,185,by=0.25)
power = numeric(length(se))



for (n in  1:length(se)){
  for (i in 1:1000){
    nu = se[n]
    x=rnorm(n,mu,sd); y=rnorm(m,nu,sd)
    p[i] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  power[n] = mean(p<0.05)
}
p

length(power)
length(se)
plot(se,power, main="30 - 5")

########################################## 2b ###############################
n = 100
m = 100
mu = 180
nu = 175
sd = 5
x = rnorm(n,mu,sd)
y = rnorm(n,nu,sd)

t.test(x, y)

p = numeric(1000)

se = seq(175,185,by=0.25)
power = numeric(length(se))
help(seq)


for (n in  1:length(se)){
  for (i in 1:1000){
    nu = se[n]
    x=rnorm(n,mu,sd); y=rnorm(m,nu,sd)
    p[i] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  power[n] = mean(p<0.05)
}
p

length(power)
length(se)
plot(se,power, main = "100 - 5")

############################### 2c ################################
par(mfrow=c(1,3))
########################################## 2b ###############################
n = 30
m = 30
mu = 180
nu = 175
sd = 15
x = rnorm(n,mu,sd)
y = rnorm(n,nu,sd)

t.test(x, y)

p = numeric(1000)

se = seq(175,185,by=0.25)
power = numeric(length(se))


# We  now  study  thep-value  and  thepower functionof  thet-test.   
# The  power  function  gives,  for  everygiven set of parameters (n,m,??,??,sd), 
# the probability that thet-test rejects the null hypothesis. 
for (n in  1:length(se)){
  for (i in 1:1000){
    nu = se[n]
    x=rnorm(n,mu,sd); y=rnorm(m,nu,sd)
    p[i] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  power[n] = mean(p<0.05)
}
p

length(power)
length(se)
plot(se,power, main = "30 - 15")

######################## exercise 3 #############################################

data=read.table(file="telephone.txt",header=TRUE)
data
bills = data$Bills
bills
hist(bills)
qqnorm(bills)
boxplot(bills)