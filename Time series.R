#Varun Teja Kolluru
#preprocessing
#1
rm(list = ls())
library(rio)
library(moments)
library(car)
#install.packages('readxl')
library(readxl)
## Loading required package: carData
my_data=import('6304 Module 7 Assignment Data.xlsx')
colnames(my_data)=tolower(make.names(colnames(my_data)))

#2
my_data$index=seq(1:nrow(my_data))
names(my_data)
attach(my_data)

#Analysis
#1
plot(index,china.visitors,main = "No of China Visitors",pch=20,type = 'o')

#2
output=lm(china.visitors~index,data=my_data)
summary(output)
#3
plot(index,china.visitors,main = "Actual vs Fitted values",pch=20,type = 'o',lwd=2) 
abline(output,col="red",lwd=3)

#4
#Durbin-Watson test
library(robustHD)
durbinWatsonTest(output)


#5
SI=data.frame(quater=1:4,average=0,index=0) 

for(a in 1:4) {
  count=0
  for(b in 1:nrow(my_data)) {
    if(a==my_data$quarter[b]) {
      SI$average[a]=SI$average[a]+my_data$china.visitors[b]
      count=count+1 }
  }
  SI$average[a]=SI$average[a]/count
  SI$index[a]=SI$average[a]/mean(my_data$china.visitors)
}
#deseasonalize the visitors data
for (a in 1:4){
  for(b in 1:nrow(my_data)) {
    if(a==my_data$quarter[b]) {
      my_data$deseason[b]=my_data$china.visitors[b]/SI$index[a]
    } }
}
#plot(my_data$index,my_data$deseason,type='o',pch=19,lwd=1,col='blue')

#6
#1st order
output1=lm(my_data$deseason~my_data$index,data=my_data)
summary(output1)
plot(my_data$index,output1$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(output1)
plot(my_data$index,my_data$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot")
points(my_data$index,output1$fitted.values,
       type="l",col="red",lwd=3)


# Second order
my_data$index2=my_data$index^2
output2=lm(deseason~index+index2,data=my_data)
plot(my_data$index,output2$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(output2)
plot(my_data$index,my_data$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot 2")
points(my_data$index,output2$fitted.values,
       type="l",col="red",lwd=3)

#3rd order
my_data$index3=my_data$index^3
output3=lm(deseason~index+index2+index3,data=my_data)
plot(my_data$index,output3$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(output3)

plot(my_data$index,my_data$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot 3")
points(my_data$index,output3$fitted.values,
       type="l",col="red",lwd=3)

#4th order 
my_data$index4=my_data$index^4
output4=lm(deseason~index+index2+index3+index4,data=my_data)
plot(my_data$index,output4$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(output4)

plot(my_data$index,my_data$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot 4")
points(my_data$index,output4$fitted.values,
       type="l",col="red",lwd=3)



#7
#Reseasonalize the values for 4 models
#1st model
for(j in 1:nrow(my_data)) {
  xx=my_data$quarter[j]
  my_data$reseason.y.hat[j]=output1$fitted.values[j]*SI$index[xx]
  my_data$reseason.error[j]=my_data$production[j]-my_data$reseason.y.hat[j]
}
plot(my_data$index,my_data$reseason.y.hat,pch=19,type="o",lwd=3, main="Actual v. Reseasoned Values for model 1")
points(my_data$index,my_data$china.visitors,pch=19,
       type="o",lwd=2,col="red")
cor(my_data$index,my_data$reseason.y.hat)
cor(my_data$index,my_data$reseason.y.hat)^2

#second order
for(j in 1:nrow(my_data)) {
  xx=my_data$quarter[j]
  my_data$reseason.y.hat[j]=
    output2$fitted.values[j]*SI$index[xx]
  my_data$reseason.error[j]=my_data$production[j]-my_data$reseason.y.hat[j]
}
plot(my_data$index,my_data$reseason.y.hat,pch=19,type="o",lwd=3, main="Actual v. Reseasoned Values for model 2")
points(my_data$index,my_data$china.visitors,pch=19,
       type="o",lwd=2,col="red")
cor(my_data$index,my_data$reseason.y.hat)
cor(my_data$index,my_data$reseason.y.hat)^2

#third order
for(j in 1:nrow(my_data)) {
  xx=my_data$quarter[j]
  my_data$reseason.y.hat[j]=
    output3$fitted.values[j]*SI$index[xx]
  my_data$reseason.error[j]=my_data$production[j]-my_data$reseason.y.hat[j]
}
plot(my_data$index,my_data$reseason.y.hat,pch=19,type="o",lwd=3, main="Actual v. Reseasoned Values for model 3")
points(my_data$index,my_data$china.visitors,pch=19,
       type="o",lwd=2,col="red")
cor(my_data$index,my_data$reseason.y.hat)
cor(my_data$index,my_data$reseason.y.hat)^2


#fourth order
for(j in 1:nrow(my_data)) {
  xx=my_data$quarter[j]
  my_data$reseason.y.hat[j]=
    output4$fitted.values[j]*SI$index[xx]
  my_data$reseason.error[j]=my_data$production[j]-my_data$reseason.y.hat[j]
}
plot(my_data$index,my_data$reseason.y.hat,pch=19,type="o",lwd=3, main="Actual v. Reseasoned Values for model 4")
points(my_data$index,my_data$china.visitors,pch=19,
       type="o",lwd=2,col="red")
cor(my_data$index,my_data$reseason.y.hat)
cor(my_data$index,my_data$reseason.y.hat)^2

#8th
summary(output3)
summary(output4)

