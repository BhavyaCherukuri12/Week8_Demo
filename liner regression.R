lm(weight ~ height, data = women)

var
summary(var)

View(women)

summary(lm(weight ~ height, data = women))


View(cars)

lm(speed ~ dist, data = cars)
summary(lm(speed ~ dist, data = cars))

scatter.smooth(x=cars$speed,
               y=cars$dist, main="Dist ~ Speed")
window(20,12)
par(mfrow=c(1,2))
boxplot(x=cars$dist, main="car distance")

boxplot(cars$speed,main="car of speed")



install.packages("e")

par(mfrow=c(1,2))

plot(density(cars$speed),
     main="Density plot:speed",
     ylab="Frequency")

polygon(density(cars$speed), col = "blue")

plot(density(cars$dist),
     main="Density plot:dist",
     ylab="Frequency")

polygon(density(cars$dist), col = "red")



cor(cars$speed, cars$dist)
cor(cars)

attach(cars)
linearMod<-lm(dist~speed)
linearMod

summary(linearMod)

AIC(linearMod)
BIC(linearMod)
