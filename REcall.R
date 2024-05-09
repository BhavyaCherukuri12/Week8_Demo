readRDS("./cars_model.rds")






insurance_data<-read.csv("insurance.csv")
insurance_data
View(insurance_data)

library(dplyr)
categorical_cols <- sapply(insurance_data, is.factor)
categorical_cols
# Factorize categorical variables
insurance_data <- insurance_data %>%
  mutate_if(categorical_cols, as.factor)


insurance_data

attach(insurance_data)
insurance_data$sex<-factor(sex,
                           levels = c("mail,female"),
                           ordered = FALSE)

insurance_data$smoker<-factor(smoker,
                              levels = c("yes,no"),
                              ordered = FALSE)

insurance_data$smoker


insurance_data$region<-factor(region,
                              levels = c("northeast","northwest","southeast","southwest" ),
                              ordered = FALSE)
str(insurance_data)

install.packages("psych")
library(psych)

windows(20,10)
pairs.panels(insurance_data,
             smooth = TRUE,
             scale=FALSE,
             density=TRUE,
             ellipses=TRUE,
             method="spearman",
             pch=21,
             lm=FALSE,
             cor=TRUE,
             jiggle=FALSE,
             factor=2,
             hist.col=4,
             stars=TRUE,
             ci=TRUE)


attach(insurance_data)
model_lms<-lm(charges~age+sex+bmi+children+smoker+region)
model_lms

summary(model_lms)

charges~ -256.