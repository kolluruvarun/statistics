#Varun Teja Kolluru

#Preprocessing

#remove all the variables in Environment window
rm(list=ls())

#Import the required libraries
library(rio)

#1
#Load the data into R
my_data=import('6304 Module 6 Assignment Data.xlsx')
colnames(my_data)=tolower(make.names(colnames(my_data)))

#2
#Create 2nd data set with selected Destinations
selected_data=subset(my_data,(destination=="IAD"|destination=="IAH"| 
                              destination=="LAS"|destination=="LAX"|
                              destination=="LGA"|destination=="MSP"| 
                              destination=="PHX"|destination=="SAN"|
                              destination=="SEA"|destination=="SFO"| 
                              destination=="STL"|destination=="TPA")) 

#3
str(selected_data)

selected_data$origin=as.factor(selected_data$origin)
selected_data$destination=as.factor(selected_data$destination)
selected_data$market.leading.airline=as.factor(selected_data$market.leading.airline)
selected_data$low.price.airline=as.factor(selected_data$low.price.airline)
attach(selected_data)

#4
#Get random sample from the split data
set.seed(97)
my_sample = selected_data[sample(1:nrow(selected_data),50,replace=FALSE),]

#Analysis
#1
# Copy the continuous variables to a new data object. 
my_sample_con=subset(my_sample,select=c("average.fare","distance","avg.weekly.passengers",
                                        "route.market.share","price")) 
attach(my_sample_con)

#Scatterplot Matrix
plot(my_sample_con,pch=19, 
     main="ScatterPlot Matrix for continuous variables") 
#Correlation analysis of the continuous variables. 
cor(my_sample_con) 
round(cor(my_sample_con),3) 

#2
# Presenting correlation graphically. 
# First put a correlation matrix into an object. 
library(corrplot) 
gcorr=cor(my_sample_con) 
gcorr 
corrplot(gcorr,method="ellipse") 
corrplot(gcorr,method="number") 

# Correlation matrix with p values. 
corr_with_p=Hmisc::rcorr(as.matrix(my_sample_con)) 
corr_with_p 

#3
# Conducting a Regression -- with Continuous Variables Only 
output=lm(price~.,data=my_sample_con) 
summary(output) 
output_3=lm(price~average.fare+distance ,data=my_sample_con) 
summary(output_3)
output_3=lm(price~average.fare+distance+avg.weekly.passengers ,data=my_sample_con) 
summary(output_3)
output_3=lm(price~average.fare+distance+route.market.share  ,data=my_sample_con) 
summary(output_3)


#4
# Assumptions of Regression 
par(mfrow=c(2,2)) 

# Linearity 
plot(my_sample_con$price,output$fitted.values, 
     pch=19,main="Actuals v. Fitted Values, Price") 
abline(0,1,col="red",lwd=3) 

# Normality 
qqnorm(output$residuals,pch=19, 
       main="Normality Plot, Price") 
qqline(output$residuals,lwd=3,col="red") 
hist(output$residuals,col="red", 
     main="Residuals, Price", 
     probability=TRUE) 

curve(dnorm(x,mean(output$residuals), 
            sd(output$residuals)), 
      from=min(output$residuals), 
      to=max(output$residuals), 
      lwd=3,col="blue",add=TRUE) 

# Equality of Variances 
plot(output$fitted.values,rstandard(output), 
     pch=19,main="Equality of Variances, Price") 
abline(0,0,lwd=3,col="red") 
par(mfrow=c(1,1)) 


#5
# Evidence of Multicollinearity. 
plot(my_sample_con,pch=19, 
     main="Continuous Variables") 
my_cor=cor(my_sample_con) 
corrplot(my_cor,method="number") 
corrplot(my_cor,method="ellipse") 

library(car) 
vif(output) 

# Let's look at the kitchen sink model. 
#summary(fullwage.out) 
#vif(fullwage.out) 


#6
# Adding Destination binary variables. 
output2=lm(price~average.fare+distance+avg.weekly.passengers+route.market.share
           +destination,data=my_sample) 
summary(output2)
output2=lm(price~average.fare+distance,data=my_sample) 
summary(output2)
output2=lm(price~average.fare+distance+destination,data=my_sample) 
summary(output2)

#7
#output2=lm(price~average.fare+distance
#           +relevel(destination,"MSP"),data=my_sample) 
#summary(output2)

#my_sample.relevel=my_sample 
#my_sample.relevel$destination=relevel(my_sample.relevel$destination,"MSP") 
my_sample$msp=NA 
for(i in 1:length(my_sample$destination)){ 
  if(my_sample$destination[i]=="MSP"){ 
    my_sample$msp[i]="MSP"} 
  else{ 
    my_sample$msp[i]="Other" 
  } 
} 

output2=lm(price~average.fare+distance+avg.weekly.passengers+route.market.share
           +msp,data=my_sample) 
summary(output2)
output2=lm(price~average.fare+distance
           +msp,data=my_sample) 
summary(output2)

