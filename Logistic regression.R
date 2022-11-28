#Varun Teja Kolluru
#preprocessing

#1
rm(list=ls())
library(rio)
library(moments)
my_data=import("6304 Module 8 Assignment Data Set.xlsx")
attach(my_data)

#2
set.seed(97)
my_sample = my_data[sample(1:nrow(my_data),750),]

#analysis
#1
output = glm(spam ~ to_multiple+ image+ dollar+ winner+ inherit+ 
              password +format+re_subj+ urgent_subj + exclaim_subj, 
              family=binomial, data = my_sample)

#2
summary(output)

#7
new_output=glm(spam~to_multiple+dollar+inherit+password+format, data = my_sample,family=binomial)
summary(new_output)
coefficients(new_output)

#8
pred = expand.grid(dollar=quantile(dollar,c(.25,.50,.75,1)),
                       password=quantile(password,c(.25,.50,.75,1)), to_multiple=unique(my_sample$to_multiple),
                       inherit=quantile(inherit,c(.25,.50,.75,1)), format=unique(my_sample$format))

reg_nf = glm(spam ~ to_multiple+dollar+inherit+password+format,data=my_sample,family='binomial')
summary(reg_nf)
coefficients(reg_nf)
confint(reg_nf)
prob_predict=round(predict(reg_nf,newdata=pred,type="response"),4)
spam_predict=cbind(pred,prob_predict)
spam_predict[1:5,]




