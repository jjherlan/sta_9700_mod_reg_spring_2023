setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer
install.packages("nortest")

require(nortest)
require(tidyverse)

### Example: Dwaine Studios

Dwaine <- read.csv("Dwaine.csv", fileEncoding = "UTF-8-BOM") %>%
  as_tibble()

dim(Dwaine)
head(Dwaine)

pairs(Dwaine) # Scatter-plot matrix
cor(Dwaine)
# Correlation matrix
# cor.test(~Y+X1,data=Dwaine)

m <- lm(Y ~ X1 + X2, data = Dwaine)
summary(m)
anova(m)

confint(m)

pairs(~m$residuals+m$fitted.values+X1+X2+I(X1*X2),data=Dwaine)
plot(abs(m$residuals)~m$fitted.values,data=Dwaine)
qqnorm(m$residuals); qqline(m$residuals)
shapiro.test(m$residuals)

library(nortest)
lillie.test(m$residuals)


