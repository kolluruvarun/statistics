#Varun Teja Kolluru

#clear all the variable in environment window
rm(list=ls())

#get all the required libraries
library(rio)
library(car)

#Preprocessing
#1
#Import the data into R
my_data = import("6304 Module 9 Assignment Data.xlsx")
colnames(my_data)=tolower(make.names(colnames(my_data)))

#2
my_subset=subset(my_data,
                (cylinders==4 | cylinders==6 | cylinders==8) &
                (fuel=="gas" | fuel=="diesel"))
my_texas=subset(my_subset,
(region=="amarillo, TX" |region=="austin, TX" |region=="brownsville, TX" |
region=="college station, TX" |region=="corpus christi, TX" |region=="dallas / fort worth" |
region=="el paso, TX" |region=="galveston, TX" |region=="houston, TX" |
region=="lubbock, TX" |region=="odessa / midland" |region=="tyler / east TX" |
region=="waco, TX" ) )
#add new state column
my_texas$state =rep("texas",nrow(my_texas))

#Get random sample from the texas subset data
set.seed(97)
my_tex_sample = my_texas[sample(1:nrow(my_texas),150,replace=FALSE),]

my_illi=subset(my_subset,
(region=="champaign urbana" |region=="chicago" |region=="danville" |
 region=="peoria, IL" |region=="quad cities, IA/IL" |region=="rockford, IL" |
 region=="southern illinois" |region=="springfield, IL") )
#add new state column
my_illi$state=rep('illinois',nrow(my_illi))
#Get random sample from the illinois subset data
set.seed(97)
my_ill_sample = my_illi[sample(1:nrow(my_illi),150,replace=FALSE),]

my_nc=subset(my_subset,
(region=="asheville, NC" |region=="boone, NC" |region=="charlotte, NC" |
 region=="eastern NC" |region=="fayetteville, NC" |region=="greensboro, NC" |
 region=="wilmington, NC" |region=="winston-salem, NC" ) )

#add new state column
my_nc$state =rep("northcarolina",nrow(my_nc))

#Get random sample from the NC subset data
set.seed(97)
my_nc_sample = my_nc[sample(1:nrow(my_nc),150,replace=FALSE),]

#creating 3 state 450 observation data set
my_sample = rbind(my_tex_sample,my_nc_sample,my_ill_sample)

#checking the type of each column
str(my_sample)

#change the state column to factor
my_sample$state=as.factor(my_sample$state)
my_sample$region=as.factor(my_sample$region)
my_sample$fuel=as.factor(my_sample$fuel)
my_sample$condition=as.factor(my_sample$condition)

#Analysis
#1
leveneTest(asking.price~state,data=my_sample)
boxplot(asking.price~state,
        main="Price, three state Flavors",
        col="red",
        data=my_sample)

#2
vari=aggregate(asking.price~state,my_sample,var)
vari
max(vari$asking.price)
min(vari$asking.price)
max(vari$asking.price)/min(vari$asking.price)
rm(vari)

#one way ANOVA
output=aov(asking.price~state,data=my_sample)
summary(output)
names(output)

my_mean=aggregate(asking.price~state,my_sample,mean)
my_mean

output$coefficients

my_tukey=TukeyHSD(output)
my_tukey

par(mar=c(5.1,8,4.1,2.1))
plot(my_tukey,las=2)
par(mar=c(5.1,4.1,4.1,2.1)) 

#3
leveneTest(odometer~state,data=my_sample)
boxplot(odometer~state,
        main="Odometer, three state Flavors",
        col="red",
        data=my_sample)

o_vari=aggregate(odometer~state,my_sample,var)
o_vari
max(o_vari$odometer)
min(o_vari$odometer)
max(o_vari$odometer)/min(o_vari$odometer)
rm(o_vari)

#one way ANOVA
output1=aov(odometer~state,data=my_sample)
summary(output1)
names(output1)

my_omean=aggregate(odometer~state,my_sample,mean)
my_omean

output1$coefficients

my_otukey=TukeyHSD(output1)
my_otukey
plot(my_otukey)

par(mar=c(5.1,8,4.1,2.1))
plot(my_otukey,las=2)
par(mar=c(5.1,4.1,4.1,2.1)) 


#4
texas_output= aov(asking.price~region,data=my_tex_sample) 
summary(texas_output)

my_tex_tukey=TukeyHSD(texas_output) 
my_tex_tukey
par(mar=c(2.1,12,4.1,2.1)) 
plot(my_tex_tukey,las=1,cex.axis=0.6) 
par(mar=c(5.1,4.1,4.1,2.1))

#5
output2=aov(asking.price~fuel+condition,data=my_sample)
summary(output2)

my_tot_tukey=TukeyHSD(output2) 
my_tot_tukey 

par(mfrow=c(1,2)) 
par(mar=c(5.1,8,4.1,2.1)) 
plot(my_tot_tukey,las=1.5,cex.axis=.8) 
par(mfrow=c(1,1)) 
par(mar=c(5.1,4.1,4.1,2.1))




