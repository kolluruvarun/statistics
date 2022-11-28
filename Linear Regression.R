#Varun Teja Kolluru
#Assignment 4

#PREPROCESSING

#remove all the variables in Environment window
rm(list=ls())

#Import the required libraries
library(rio)

#Load the data into R
my_data=import('6304 Module 4 Assignment Data.xlsx')
colnames(my_data)=tolower(make.names(colnames(my_data)))

#create sample data from the loaded data
set.seed(97)
my_sample=my_data[sample(1:nrow(my_data),200,
                     replace=FALSE),]
attach(my_sample)

#ANALYSIS

#1
plot(odometer,price,pch=19,
     main="Odometer & Price Raw Data Plot")

output=lm(price~odometer,data=my_sample)
#2
summary(output)
#3
confint(output)

#4
# Linearity
plot(my_sample$price,output$fitted.values,
     pch=19,main="Odometer&Price Actual v. Fitted Values")
abline(0,1,col="red",lwd=3)
plot(my_sample$price,output$fitted.values,
     pch=19,
     xlim=c(0,60000),ylim=c(0,30000),
     main="Odometer&Price Actual v. Fitted Values")
abline(0,1,col="red",lwd=3)

# Normality
qqnorm(output$residuals,pch=19,main="O&P Normality Plot")
qqline(output$residuals,col="red",lwd=3)

# Equality of Variances
plot(output$fitted.values,scale(output$residuals),
     pch=19,main="O&P Standardized Residuals",
     xlab=c("Fitted Values"),ylab=c("Residuals"))
abline(0,0,col="red",lwd=3)

#5
# predicting what the price of car would be if odometer reading was 78,521 miles
newdata=data.frame(odometer=78521)
# predict is a point in time
predict(output,newdata,interval="predict")
# confidence is a long term average
predict(output,newdata,interval="confidence")

# predicting what the price of car would be if odometer reading was 98,000 miles
newdata=data.frame(odometer=98000)
# predict is a point in time
predict(output,newdata,interval="predict")
# confidence is a long term average
predict(output,newdata,interval="confidence")
