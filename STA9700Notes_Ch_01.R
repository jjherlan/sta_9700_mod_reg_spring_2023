require(car)

# setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer

### Example: Westwood company

# Data
west <- read_csv("Westwood.csv")

west

summary(west)

# Scatter plot of man-hours against lot sizes
plot(Man.Hours ~ Lot.Size, data = west, xlab = "lot sizes", ylab = "man-hours")

# Adding fitted line

west.lm <- lm(Man.Hours ~ Lot.Size, data = west)

abline(west.lm, col = "red")

# Estimation of regression function

summary(west.lm)

# Slope = 2
# b1 and b0 are estimates
# y-intercept is 10
# Intercept is Ybar, std. error of Ybar
# t-value


west.lm$coefficients

Anova(west.lm, type = "III")

# Fitted values
west.lm$fitted.values

# Prediction
predict(west.lm, data.frame(Lot.Size = 55)) # m$coefficients%*%c(1,55)

# Residuals
west.lm$residuals

# Properties of fitted regression line

sum(west.lm$residuals)

sum(west.lm$residuals^2) # SSE

sum(west.lm$Man.Hours)
sum(west.lm$fitted.values)

sum(west$Lot.Size*m$residuals) # cor(m$residuals,Westwood$Lot.Size)

sum(west.lm$fitted.values*west.lm$residuals) # cor(m$residuals,m$fitted.values)

# cor(m$residuals,Westwood$Man.Hours)
west.lm$coefficients%*%c(1,mean(west$Lot.Size))
mean(west$Man.Hours)

# Estimation of error variance
summary(west.lm)$sigma^2 # MSE Residual standard error (^2) in the "lm" output


