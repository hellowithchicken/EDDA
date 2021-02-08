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

?t.test

t.test(bw, mu=2800, alternative = "greater", conf.level = 0.95)

m_2800 = numeric(length(bw))

for (i in 1:length(bw)){m_2800[i] = 2800}
m_2800
t.test(bw,m_2800, alternative = "greater", conf.level = 0.95)
#################################### Exercise 2 #######################################

###################################2a############################################
n <- 30
m <- 30
mu <- 180
nu <- 175
sd <- 5
B <- 1000





se <- seq(175,185,by=0.25)
se
power <- numeric(length(se))



for (n in  1:length(se)){
  p =numeric(B)
  for (i in 1:B){
    y<-rnorm(n,mu,sd) 
    x<-rnorm(m,se[n],sd)
    p[i] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  power[n] <- mean(p<0.05)
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


t.test(x, y)



se = seq(175,185,by=0.25)
power = numeric(length(se))
help(seq)


for (n in  1:length(se)){
  p = numeric(1000)
  for (i in 1:1000){
    nu = se[n]
    x=rnorm(n,mu,sd) 
    y=rnorm(m,nu,sd)
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


# We  now  study  thep-value  and  thepower functionof  thet-test.   The  power  function  gives,  for  everygiven set of parameters (n,m,??,??,sd), the probability that thet-test rejects the null hypothesis. 
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

data=read.table(file="data/telephone.txt",header=TRUE)
data
bills = data$Bills
bills
hist(bills)
qqnorm(bills)
boxplot(bills)

hist(bills, probability = T, ylim=c(0,0.7))
x = seq(0,median(bills),length=1000)
print(x)

lines(x,dexp(x),type="l", col="blue", lwd=2)

t = median(bills)
t



X = seq(0.01, 0.1, 0.0005)
X
p_values = c()

par(mfrow=c(1,1))
length(X)

for (x in X){  
  B = 1000
  tstar = numeric(B)
  n = length(bills)
  for (j in 1:B){
    
    xstar = rexp(n,x)
    xstar
    tstar[j] = median(xstar)
  }
  pl=sum(tstar<t)/B
  pr=sum(tstar>t)/B
  p=2*min(pl,pr)

  p_values = c(p_values, p)
  }

p_values
bills
tstar
hist(tstar, prob= T)

p_values
X
p_values
plot(X, p_values)


B = 10000
Tstar = c()

for (i in  1:B){
  Xstar = sample(bills, replace=TRUE)
  Tstar= c(Tstar, median(Xstar))
}
Tstar
Tstar25 = quantile(Tstar, 0.025)
Tstar25
Tstar975 = quantile(Tstar, 0.975)
sum(Tstar<Tstar25)
T1 = median(bills)
T1
c(2*T1-Tstar975,2*T1-Tstar25)


######################### 3d ###############
b_40 = sum(bills>40)
binom.test(b_40, length(bills))
b_10 = sum(bills<10)
b_10/length(bills)


###########################4 ###########################
data=read.table(file="data/run.txt",header=TRUE)
# 4a
data

first_run = data$before
second_run = data$after

plot(first_run, second_run)
cor(first_run, second_run)


hist(first_run)
hist(second_run)
shapiro.test(first_run)
shapiro.test(second_run)

t.test(first_run, second_run)

time_diverence_lemo = c()
time_diverence_energy = c()

for (i in 1:(length(first_run)/2))
  time_diverence_lemo = c(time_diverence_lemo, abs(first_run[i] - second_run[i]))
  time_diverence_energy = c(time_diverence_energy, abs(first_run[i + (first_run)/2)] - second_run[i+ (first_run)/2)]))

  time_diverence = c(time_diverence, abs(first_run[i] - second_run[i]))

length(time_diverence)
length(data$before)
