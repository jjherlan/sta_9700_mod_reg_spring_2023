setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer
install.packages("nortest")
install.packages("car")

require(nortest)
require(car)

### Example: Aerobic fitness

Aerobic=read.csv("Aerobic.csv")
dim(Aerobic); head(Aerobic)

MaxOxysq=Aerobic$MaxOxy^2
(Aerobic2=cbind(Aerobic,MaxOxysq))
pairs(Aerobic2) # Scatter plot matrix
round(cor(Aerobic2),digits=2) # Correlation matrix

# Center predictor
MaxOxy.mc=Aerobic$MaxOxy-mean(Aerobic$MaxOxy)
cor(MaxOxy.mc,MaxOxy.mc^2) # collinearity reduced

m.a1<-lm(IgG~MaxOxy.mc,data=Aerobic); summary(m.a1)
plot(m.a1$residuals~m.a1$fitted.values,xlab="Predicted values",ylab="Residuals")

m.a2<-lm(IgG~MaxOxy.mc+I(MaxOxy.mc^2),data=Aerobic); summary(m.a2)
plot(m.a2$residuals~m.a2$fitted.values,xlab="Predicted values",ylab="Residuals")

plot(IgG~MaxOxy.mc,data=Aerobic,xlab="Centered MaxOxy",ylab="IgG")
abline(m.a1,col="red")
summary(MaxOxy.mc); newx=seq(-20,20,by=5)
lines(newx,predict(m.a2,newdata=data.frame(MaxOxy.mc=newx)),col="blue")


### Example: Power cell

Power=read.csv("Power_cell.csv")

#,fileEncoding="UTF-8-BOM")

dim(Power); head(Power)
X1sq=Power$X1^2; X2sq=Power$X2^2; X1X2=Power$X1*Power$X2
(Power2=cbind(Power,X1sq,X2sq,X1X2))

pairs(Power2) # Scatter plot matrix
round(cor(Power2),digits=2) # Correlation matrix

# Center and rescale X1 and X2
x1=(Power$X1-1)/0.4; x2=(Power$X2-20)/10 # not standardization
# mean(Power$X1); sd(Power$X1); mean(Power$X2); sd(Power$X2)
x1sq=x1^2; x2sq=x2^2; x1x2=x1*x2
round(cor(cbind(Power$Y,x1,x2,x1sq,x2sq,x1x2)),digits=2)

m2c<-lm(Power$Y~x1+x2+x1sq+x2sq+x1x2)
summary(m2c)
pairs(~m2c$residuals+m2c$fitted.values+x1+x2)
shapiro.test(m2c$residuals)
library(nortest)
lillie.test(m2c$residuals)

# Lack-of-fit test
x1f=factor(x1); x2f=factor(x2)
anova(m2c,lm(Power$Y~x1f*x2f))

# Partial F test
m1c=lm(Power$Y~x1+x2)
anova(m1c,m2c)

# Fitting 1st-order model
summary(m1c)
summary(lm(Y~X1+X2,Power))


### Example: Body fat

Body=read.csv("Body.csv",fileEncoding="UTF-8-BOM")
dim(Body); head(Body)

x1c=Body$X1-mean(Body$X1); x2c=Body$X2-mean(Body$X2); x3c=Body$X3-mean(Body$X3)
x1cx2c=x1c*x2c; x1cx3c=x1c*x3c; x2cx3c=x2c*x3c
(Body2=cbind(Body,x1c,x2c,x3c,x1cx2c,x1cx3c,x2cx3c))
m.i<-lm(Y~.,Body2[,-(2:4)])
summary(m.i)
anova(m.i) # Type I SS
anova(lm(Y~x1c+x2c+x3c,Body2),m.i)


### Insurance innovation

Insurance=read.csv("Insurance.csv",fileEncoding="UTF-8-BOM")
dim(Insurance); head(Insurance)

library(car)
scatterplot(Y~X1|X2,data=Insurance,smooth=FALSE,xlab="Firm size",ylab="Time to adoption")

m.ins=lm(Y~X1+X2,Insurance); summary(m.ins)
confint(m.ins,level=0.95)

m.ins2=lm(Y~X1*X2,Insurance); summary(m.ins2)
anova(lm(Y~X1,Insurance),m.ins2)


### Burger King
bk=read.csv("Burger King.csv")
dim(bk); head(bk)
plot(Calories~Carbs,data=bk,xlab="Carbs (g)",ylab="Calories")
library(car)
scatterplot(Calories~Carbs|Meat,data=bk,smooth=FALSE,xlab="Carbs (g)",ylab="Calories")
m.bk<-lm(Calories~Carbs*Meat,data=bk); summary(m.bk)


