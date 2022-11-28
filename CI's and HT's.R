#Varun Teja Kolluru
#Module 3 Assignment

rm(list=ls())

#import required libraries
library(rio)
library(moments)

#Preprocessing
#import the data from the excel
my_data=import('6304 Module 3 Assignment Data.xlsx')

#split the data based on facility
df<-split(my_data,f=my_data$facility)
df_hl<-rbind(df$Lapataganj,df$Hooterville)

#set seed and get 40 random samples for each facility
set.seed(97)
fd_c=df$Cecily[sample(1:nrow(df$Cecily),40,replace=FALSE),]
fd_h=df$Hooterville[sample(1:nrow(df$Hooterville),40,replace=FALSE),]
fd_l=df$Lapataganj[sample(1:nrow(df$Lapataganj),40,replace=FALSE),]
fd_p=df$Pixley[sample(1:nrow(df$Pixley),40,replace=FALSE),]


#Analysis

#1 90% CI
#mean+_z*(sd/sqrt(n))- to find the CI
#h_data=subset(my_data,facility=="Hooterville")
mn=mean(df$Hooterville$transaction.time)#Mean of the Hooterville sample
stn=sd(df$Hooterville$transaction.time)#Standard deviation of the sample
n=nrow(df$Hooterville)#number of rows in the sample

z=qnorm(.95)
me<-z*(stn/sqrt(n))  
mn-me#Lower Value
mn+me#Upper value

#qnorm(.975)*(15000/sqrt(10))
#2
t.test(df$Hooterville$transaction.time,conf.level=0.90,
       alternative=c("two.sided"))
t.test(df$Hooterville$transaction.time,conf.level=0.95,
       alternative=c("two.sided"))

#3
lapa=t.test(fd_l$transaction.time,mu=8,alternative = "greater")
lapa
lapa_gt_9=t.test(fd_l$transaction.time,mu=9.25,alternative = 'greater')
lapa_gt_9

#4
lapa_ts=t.test(fd_l$transaction,mu=9,alternative = "two.sided")
lapa_ts
lapa_ts=t.test(fd_l$transaction,mu=8,alternative = "two.sided")
lapa_ts
lapa_ts=t.test(fd_l$transaction,mu=8.5,alternative = "two.sided")
lapa_ts

#5
skewness(fd_h$transaction.time)
skewness(fd_l$transaction.time)
boxplot(fd_h$transaction.time,fd_l$transaction.time,
        col=c("red","blue"),names=c("Hooterville","Lapataganj"),
        main="Transaction time Notched Boxplot",
        notch = TRUE)

#6
results=t.test(fd_h$transaction.time,fd_l$transaction.time,mu=0,
               alternative=c("two.sided"),paired=TRUE)
results
