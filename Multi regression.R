#Varun Teja Kolluru

#clear all the variable in environment window
rm(list=ls())

#get all the required libraries
library(rio)

#Preprocessing
#Import the data into R
my_data = import("6304 Module 5 Assignment Data.xlsx")
colnames(my_data)=tolower(make.names(colnames(my_data)))

my_split=subset(my_data, make=="cadillac" & 
                  (year>="2006" & year<="2011") & 
                  (condition=='excellent' | condition=='good') &
                  (cylinders==6 | cylinders==8) &
                  (paint.color!='black' & paint.color!='custom'))

#Get random sample from the split data
set.seed(97)
my_sample = my_split[sample(1:nrow(my_split),90,replace=FALSE),]

#Analysis
#1
str(my_sample)

#2
#change the categorical variables and do regression
my_sample$year=as.factor(my_sample$year)
my_sample$condition=as.factor(my_sample$condition)
my_sample$paint.color=as.factor(my_sample$paint.color)
my_sample$cylinders=as.factor(my_sample$cylinders)

output = lm(price~odometer+year+condition+paint.color+cylinders,data=my_sample)
coefficients(output)
summary(output)
confint(output)

output2 = lm(price~odometer+year+condition+cylinders,data=my_sample)
summary(output2)

#QUESTION5
#Linearity
plot(my_sample$price, output$fitted.values,pch=19,
     main="Price Actual and Fitted values")
abline(0,1,lwd=3,col="red")

#NORMALITY
qqnorm(output$residuals,pch=19,main="Odometer and Price Normality Plot")
qqline(output$residuals,col="red",lwd=3)

#EQUALITY OF VARIANCE
plot(output$fitted.values,output$residuals,pch=19,main="Odometer and Price Residuals")
abline(0,0,col="red",lwd=3)

#QUESTION6
pricing = data.frame(odometer = 215354,year="2011",condition='excellent',paint.color='red',make='cadilac',cylinders='8')
predict(output,pricing,interval = "predict")



