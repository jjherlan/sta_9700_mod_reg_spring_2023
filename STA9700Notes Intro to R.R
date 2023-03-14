
#------------------------------------
# R basics
#------------------------------------

install.packages("MASS")
library(MASS)

# Commands

2+2 # expression
5*5+2; 5/5-1
2^3
pi; 1+pi/2

a=6 # assignment
x<-1
(x<-1)

ls()
rm(list=ls())

# Some standard functions

round(5.549,digits=1); round(5.550,digits=1)
abs(-5.55)
log(10)
sqrt(9)
exp(1)
sin(pi)
cos(pi)

# Vectors

mydata=c(10,3,9,1,6,2)
mydata[3]; mydata[3:5]; mydata[-1]; mydata[-(1:2)]
mydata[mydata>3]

# Matrices

matrix(mydata,2,3); matrix(mydata,3,2)
?matrix # display description of 'matrix'

# Exercises 

(M=matrix(c(5,13,2,2,26,3,87,19,68),nrow=3,ncol=3))
(V=M[3,])
sqrt(V)

# Generating sequences

(x<-1:20)
(s1=seq(-5,5,by=0.2))
(s2=seq(length=51,from=-5,by=0.2))
(s3=rep(s1,times=2))

# Input external data

setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer
Wind_power=read.csv("Wind_power.csv")

#------------------------------------
# Graphical output
#------------------------------------

plot(s1,exp(s1),pch=19) # 'pch' for  plotting character  & '19' means solid circle; see ?points
plot(s1,exp(s1),type="l",main="Exponential function") # 'l' means lines; see ?plot
hist(exp(s1))

# Multiple figure environments

par(mfrow=c(1,2))
plot(s1); plot(exp(s1))

#------------------------------------
# Exploratory data analysis
#------------------------------------

data(package="MASS")
Boston

dim(Boston)
head(Boston)
names(Boston)
str(Boston)

hist(Boston$medv,breaks=10,col="red",xlab="medv",xlim=c(0,50),main="median value of owner-occupied homes")
hist(Boston$medv,breaks=20,col="red",xlab="medv",xlim=c(0,50),main="median value of owner-occupied homes")

summary(Boston$medv)
sum(Boston$medv); mean(Boston$medv)
sd(Boston$medv)

sort(Boston$medv)


