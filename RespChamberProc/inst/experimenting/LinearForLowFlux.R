#Author: Oscar Perez-Priego
#Propose: testing the appropiateness of linear and quadratic if  flux is low and thus total CO2 change during deployment<2 sd data (noise mostly mediated)
#Date: 20.02.2014

#reset R´s brain
rm(list=ls())

slope <- 0.02

noise <- rnorm(180, sd=1)
time <- seq(0:179)
plot(time, noise)
CO2.1 <- (slope*time+380+noise)
plot(time, CO2.1)
data <- data.frame(CO2.1, time)
my.poly <- lm(CO2.1 ~ poly(data$time, 2, raw = TRUE))
my.linear <- lm(CO2.1 ~ time)

summary(my.poly)
summary(my.linear)

CO2.1.poly <- predict(my.poly)
CO2.1.linear <- predict(my.linear)
lines(time, CO2.1.poly, col="red")
lines(time, CO2.1.linear, col='blue')



CO2.2 <- (390+noise+(slope*time)+0.00005*time*time)
plot(time, CO2.2)
data <- data.frame(CO2.2, time)
my.poly <- lm(CO2.2 ~ poly(data$time, 2, raw = TRUE))
my.linear <- lm(CO2.2 ~ time)

summary(my.poly)
summary(my.linear)

CO2.2.poly <- predict(my.poly)
CO2.2.linear <- predict(my.linear)
lines(time, CO2.2.poly, col="red")
lines(time, CO2.2.linear, col='blue')
