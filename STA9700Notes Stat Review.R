require(tidyverse)

# getwd()
# setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer

# Input external data
getwd()

wind <- read.csv("Wind_power.csv") %>%
  as_tibble()

wind
summary(wind)

dim(wind)

head(wind)

names(wind)

speed = wind$WindSpeed

summary(speed)

hist(speed, main = "Wind Speed", xlab = "mph")

hist(WindSpeed, main="Wind Speed", breaks = 20, col= "red", xlab="mph", xlim =c (0, 21))

?hist
boxplot(WindSpeed,col="lightgray",main="Wind Speed",ylab="mph",ylim=c(0,21),horizontal=FALSE)

mean(speed)
sd(speed)

# confidence interval

wind_summ <-
wind %>%
#  group_by(vs) %>%
  summarise(mean.speed = mean(speed, na.rm = TRUE),
            sd.speed = sd(speed, na.rm = TRUE),
            n.speed = n()) %>%
  mutate(se.speed = sd.speed / sqrt(n.speed),
         lower.ci.speed = mean.speed - qt(1 - (0.05 / 2), n.speed - 1) * se.speed,
         upper.ci.speed = mean.speed + qt(1 - (0.05 / 2), n.speed - 1) * se.speed)

t.test(speed, conf.level = 0.95)

t.test(speed, conf.level = 0.95, mu = 8, alternative = "greater")

# t is 0.16716 (from t-test formula) 
# We lack significance evidence against H0, in favor of HA

# Because Type I is the probability of rejecting the null when the null is true
# Type II is failing to reject the null when the null is FALSE

# t-test provides result for corresponding confidence interval

# Mean is within the confidence interval

# if outside the confidence interval, if yes, 





