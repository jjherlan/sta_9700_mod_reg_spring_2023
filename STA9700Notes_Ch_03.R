setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer
install.packages("ALSM")
install.packages("nortest")
install.packages("car")
install.packages("lmtest")


### Example: Westwood company

Westwood=read.csv("Westwood.csv")
plot(Man.Hours~Lot.Size,data=Westwood,xlab="lot sizes",ylab="man-hours")
m<-lm(Man.Hours~Lot.Size,data=Westwood); summary(m)

# Residuals
m$residuals # or residuals(m)

# Semistudentized residuals
m$residuals/summary(m)$sigma 

# Studentized residuals (internally studentized residuals)
rstandard(m) # or stdres() in R package 'MASS'

# Studentized deleted residuals (externally studentized residuals) 
rstudent(m) # or studres() in R package 'MASS'

# Plot of residuals against fitted values
plot(residuals(m)~fitted.values(m),xlab="Fitted value",ylab="Residual")

# Sequence plot of residuals
plot(m$residuals,type="l",xlab="Production run",ylab="Residual")
abline(h=0)

# Plot of semistudentized residuals against fitted values
plot(m$residuals/summary(m)$sigma~m$fitted.values,xlab="Fitted value",ylab="Semistudentized residual")

# Normal probability plot of residuals
qqnorm(m$residuals,main="Normal Q-Q plot of residuals"); qqline(m$residuals)

# Normal correlation test
library(ALSM)
normal.cor.test(residuals(m),sigma(m)^2) # obtain correlation coefficient between the ordered residuals and their expected values under normality; can use Table B.6 to obtain critical value to ascertain whether the plausibility of normality assumption. 

# Shapiro-Wilk test of normality
shapiro.test(m$residuals)

# Lilliefors test of normality (modified Kolmogorov-Smirnov test)
library(nortest)
lillie.test(m$residuals)


### Example: Transit

Riders=read.csv("Riders.csv",fileEncoding="UTF-8-BOM")
dim(Riders); head(Riders)
plot(Ridership~Maps,data=Riders,xlab="Maps distributed (thousands)",ylab="Increase in ridership (thousands)")
m.Riders<-lm(Ridership~Maps,data=Riders); summary(m.Riders)
plot(m.Riders$residuals~Riders$Maps,xlab="Maps distributed (thousands)",ylab="Residual")


### Example: Copier

Copier=read.csv("Copier.csv",fileEncoding="UTF-8-BOM")
dim(Copier); head(Copier)
plot(Y~X,data=Copier,xlab="Number of copiers serviced",ylab="Total number of minutes")
m.Copier<-lm(Y~X,data=Copier); summary(m.Copier)
plot(m.Copier$residuals~Copier$X,xlab="Number of copiers serviced",ylab="Residual")
abline(h=0)


### Example: GPA

GPA=read.csv("GPA.csv",fileEncoding="UTF-8-BOM")
dim(GPA); head(GPA)
plot(Y~X1,data=GPA,xlab="entrance test score",ylab="GPA")
m.GPA<-lm(Y~X1,data=GPA); summary(m.GPA)
plot(m.GPA$residuals/summary(m.GPA)$sigma~m.GPA$fitted.values,xlab="Fitted value",ylab="Semistudentized residual")

# Plot of residuals against omitted predictor variable
plot(m.GPA$residuals~GPA$X2,xlab="Intelligence score",ylab="Residual")
plot(m.GPA$residuals~GPA$X3,xlab="High school class rank percentile",ylab="Residual")


### Example: Productivity

Productivity=read.csv("Productivity.csv",fileEncoding="UTF-8-BOM")
dim(Productivity); head(Productivity)
plot(Output~Age,data=Productivity,xlab="Age",ylab="Output")
mp<-lm(Output~Age,data=Productivity); summary(mp)
semires=residuals(mp)/summary(mp)$sigma
plot(semires~Age,data=Productivity,xlab="Age",ylab="Semistudentized residual")
abline(h=0)

library(car)
scatterplot(semires~Age|Machine,data=Productivity,smooth=FALSE,regLine=FALSE,xlab="Age",ylab="Semistudentized residual")
boxplot(semires~Machine,data=Productivity,xlab="Machine",ylab="Semistudentized residual")


### Example: t(1) data

t.1=read.csv("t(1).csv",fileEncoding="UTF-8-BOM")
dim(t.1); head(t.1)
qqnorm(t.1$res,main="Normal Q-Q plot of residuals"); qqline(t.1$res)
shapiro.test(t.1$res)
lillie.test(t.1$res)


### Example: Toluca Company

# Brown-Forsythe test
library(ALSM)
data(package="ALSM")
g<-rep(1,25); g[TolucaCompany$x<=70]=0
bftest(lm(y~x,TolucaCompany),g)

# Breusch-Pagan test
library(lmtest)
bptest(lm(y~x,TolucaCompany),studentize=FALSE)


### Example: Bank

Bank=read.csv("Bank.csv",fileEncoding="UTF-8-BOM")
dim(Bank); head(Bank)
plot(Accounts~Deposit,data=Bank,xlab="Deposit",ylab="Accounts")
mb<-lm(Accounts~Deposit,data=Bank); summary(mb)
abline(mb,col="red")

# F test for lack of fit
anova(mb,lm(Accounts~factor(Deposit),Bank))
# Also see function 'anovaPE' in R package "EnvStats"
anova(mb)


### Transformations on X

X=seq(1,5,0.01)
X.log=log10(X); X.sqrt=sqrt(X); X.exp=exp(X); X.sq=X^2; X.inv=1/X; X.ne=exp(-X)
op=par(mfrow=c(3,2))
plot(X,X.log,type="l");plot(X,X.sqrt,type="l")
plot(X,X.exp,type="l");plot(X,X.sq,type="l")
plot(X,X.inv,type="l");plot(X,X.ne,type="l")
par(op)


### Example: Sales training

Sales=read.csv("Sales.csv",fileEncoding="UTF-8-BOM")
dim(Sales); head(Sales)
plot(Score~Days,data=Sales,xlab="Days of training",ylab="Performance score")
plot(Score~Sqrtx,data=Sales,xlab="Square root of days",ylab="Performance score")
m.Sales<-lm(Score~Sqrtx,data=Sales); summary(m.Sales)
plot(m.Sales$residuals~m.Sales$fitted.values,xlab="Fitted value",ylab="Residual")
abline(h=0)
qqnorm(m.Sales$residuals,main="Normal Q-Q plot of residuals")
qqline(m.Sales$residuals)
shapiro.test(m.Sales$residuals)


### Example: Plasma level

library(ALSM)
data(package="ALSM")
dim(Plasma); head(Plasma)

plot(y~x,data=Plasma,xlab="Age",ylab="Plasma level of polyamine")
m.Plasma0<-lm(y~x,data=Plasma); summary(m.Plasma0)
plot(m.Plasma0$residuals~Plasma$x,xlab="Age",ylab="Residual")
abline(h=0)

# Transformations on Y
plot(log.y~x,data=Plasma,xlab="Age",ylab=expression(paste(Log[10],"(Plasma level)")))
# See ?plotmath
m.Plasma<-lm(log.y~x,data=Plasma); summary(m.Plasma)
plot(m.Plasma$residuals~m.Plasma$fitted.values,xlab="Fitted value",ylab="Residual")
abline(h=0)
qqnorm(m.Plasma$residuals,main="Normal Q-Q plot of residuals")
qqline(m.Plasma$residuals)
shapiro.test(m.Plasma$residuals)

# Box-Cox transformation
boxcox.sse(Plasma[,1],Plasma[,2],l=seq(-2,1,0.1))


