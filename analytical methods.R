#Varun Teja Kolluru
#remove the list in environment window
rm(list=ls())

install.packages('rio')
library(rio)

#Load the file from the directory
my_data = import('6304 Module 1 Assignment Data.xlsx')

#change the column names to lower case
colnames(my_data) = tolower(make.names(colnames(my_data)))

#Creating a new subset for states with Washington and Georigia
sub_data = subset(my_data,state=='WA'|state=='GA')

#Creating a seed with my U number and create a sample from subset
set.seed('97')
my_sample = sub_data[sample(1:nrow(sub_data),60),]
attach(my_sample)
my_sample

#Start of analysis
#Structure of the data object
str(my_sample)

#Mean Median SD skewness kurtosis
mean(price)
median(price)
sd(price)
skewness(price)
kurtosis(price)

#Boxplot for age variable
boxplot(age,pch=19,main="Box Plot for the Age variable")

#qunatile for age variable
quantile(age,probs = seq(0,1,0.20))

#Histogram for the length variable
hist(length,col="blue",main="Histogram for Lenght varible with better look and feel",
     xlim = c(0,150))

#stem and leaf plot for age variable
plot(density(age),lwd=3,main="Plot for age variable")
plot(age,pch=19)

#Boxplot comparing prices for GA and WA state
my.ga = subset(my_data,state=='GA')
my.wa = subset(my_data,state=='WA')
boxplot(my.ga$price,my.wa$price,pch=19,
        main="GA state price vs WA state price",
        col=c("red","red"),names=c("Georgia","Washington"))
