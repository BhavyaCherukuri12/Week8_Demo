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




no_of_record<-sample(1:nrow(cars),0.8*nrow(cars))
str(no_of_record)


Training_data<-cars[no_of_record,]
Training_data


Testing_Data<-cars[-no_of_record,]
Testing_Data


lr_model<-lm(dist ~speed,data=Training_data )
lr_model
summary(lr_model)
dist_predicted <- predict(lr_model, Testing_Data)
dist_predicted

actuals_pred<-data.frame(cbind(actuals=Testing_Data$dist,
                               predicted=dist_predicted))
actuals_pred



attach(actuals_pred)
correlation_acc<-cor(actuals,predicted)
correlation_acc


correlation_acc1<-cor(actuals_pred)
correlation_acc1


min_max_accuracy<-mean(apply(actuals_pred,1,min)/
                       apply(actuals_pred,1,max))

min_max_accuracy

attach(actuals_pred)
mape<-mean(abs(predicted-actuals)/actuals)
mape


#k-fold
install.packages("DAAG")

library(DAAG)


windows(20,10)
cvResult<-suppressWarnings((CVlm(data = cars,
                                 form.lm = dist~
                                 speed,m=5,dots=FALSE,seed = 29,
                                 legend.pos = "topleft",
                                 printit=FALSE,
                                 main = "small symbols are predicted values while bigger one 
                                 are actuals"
                                 
                                 )))
cvResult


saveRDS(lr_model,"./cars_model.rds")

