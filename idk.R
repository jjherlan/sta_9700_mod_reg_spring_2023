985530-136366

33.17+11.55+98.40

2033565 + 6675 + 985530

setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer
install.packages("car")
install.packages("rsq")
install.packages("lm.beta")


### Example: Body fat

grocery <- read.csv("grocery.csv", fileEncoding = "UTF-8-BOM")
dim(grocery); head(grocery)

pairs(grocery) # Scatter plot matrix
round(cor(grocery),digits=2) # Correlation matrix

# SSR(X2|X1)
m1<-lm(Y~X1,grocery) # regression of Y on X1 only
m2<-update(m1,~.+X2) # regression of Y on X1 & X2
anova(m1,m2)

# Type I SS
m3<-update(m2,~.+X3) # regression of Y on X1, X2, & X3
anova(m3)  # Type I ANOVA; Sequential F-tests: use MSE from m3 to estimate error variance

# Type III SS
library(car)
Anova(m3,type=3)
# Anova(m3,type=2) # Type II ANOVA: intercept isn't listed in table

summary(m3)

anova(m2,m3) # Testing H0: \beta_[3}=0
anova(m1,m3) # Testing H0: \beta_[2}=\beta_[3}=0

# Coefficient of partial determination
library(rsq)
rsq.partial(m2, m1) # in R package 'rsq'
rsq.partial(m3, m1)

2033565/3025770

(2040240/2)

(985530/2)

1020120/492765

# Coefficient of partial correlation
pcor(m2,m1) # in R package 'rsq'

# Standardized multiple regression model
library(lm.beta)
m.s=lm.beta(m3); summary(m.s)

# Collinearity
summary(lm(X3~X1+X2,grocery))
summary(m2)
summary(m3)