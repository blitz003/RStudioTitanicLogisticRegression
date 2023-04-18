titanic<-read.table(header=TRUE,file="D:/Master Folder/Fall 2022 Classes/QMST 3339/REGRESSION/RDataFiles/titanic.txt",sep=",")
head(titanic)
attach(titanic)
titanic$age
names(titanic)

table(survived)

table(survived,sex)

table(survived,pclass)

table(survived,pclass,sex)
plot(age~survived)
plot(age~factor(survived))
median(age[survived==0],na.rm=TRUE)

median(age[survived==1],na.rm=TRUE)

mean(age[survived==0],na.rm=TRUE)

mean(age[survived==1],na.rm=TRUE)
mean(age,na.rm=TRUE)
boxplot(age~factor(survived))

boxplot(survived~pclass+sex)
#######################################################
#Building Logistic Regression Models
o1<-glm(survived~age,family=binomial(link="logit"))
summary(o1)
#4.07
###
(869.54-865.47)/(632-631)
o2<-glm(survived~pclass,family=binomial(link="logit"))
summary(o2)
#87.25
(1686.8-1512.3)/(1312-1310)
o3<-glm(survived~sex,family=binomial(link="logit"))
summary(o3)
#328.1
(1686.8-1358.7)/(1312-1311)
#associated with o1  1-pchisq(869.5-865.5,632-631)
predict(o3,type="response")
o4<-glm(survived~sex+pclass,family=binomial(link="logit"))

anova(o3,o4,test="Chisq")

condition=!is.na(age) & !is.na(sex) & !is.na(pclass) 

cleantitanic=titanic[condition,]
attach(cleantitanic)
o1<-glm(survived~age,family=binomial(link="logit"))
summary(o1)

o2<-glm(survived~pclass,family=binomial(link="logit"))
summary(o2)

o3<-glm(survived~sex,family=binomial(link="logit"))
summary(o3)

o4<-glm(survived~sex+pclass,family=binomial(link="logit"))
summary(o4)
anova(o3,o4,test="Chisq")
o5<-glm(survived~sex+pclass+age,family=binomial(link="logit"))
summary(o5)

#Hypothesis Test 1
#Is my model doing a better job than the intercept alone
#The null hypothesis is that the intercept alone model (null model)
#is doing as good of a job as the model with the independent variables (covariates)
# so would you rather use sex,pclass, and age in order to predict the probability
# that a person survives or will you simply obtain the average value of the dependent 
# variable and use that as the predicted probability. The null hypothesis says there
# is no difference. The alternative says that the independent variables (covariates)
# you use is a better fit to the dependent variable than just the proportion of survivors.


#Hypothesis Test 2
#I have 2 models that have identical dependent variables. One of them uses 
#X1...Xk independent variables. The other one uses X_1....X_k....X_(k+j)
#The one with k independent variables is simpler, the one with k+j independent
#variables is the more complicated one.
#The null hypothesis is that 
#General Description

#Null Deviance= 2*Saturated Likelihood -2*Null Model Likelihood
#Residual Deviance=2*Saturated Likelihood -2*Model Likelihood 

#1-pchisq(Residual Deviance - Null Deviance,df_RD - df_ND)
#Computing based on summary(o5)
#Note:
#(null model which is not the 
#same thing as null DEVIANCE, a null model (intercept only) leads to the computation
#of null deviance)

#For logistic regression the saturated model has each Y data value probability evaluate to 1, 
# since each Y value is modeled with its own seperate p value. Which will either equal to
#1 or 0 depending on the value of Y. So if Y = 1 than p = 1, if Y=0 than p = 0. 
#Which means you are either evaluating 1^1 or 0^0 . The probability of observing 1 or 0 is 1. 

#Bernoulli Distribution Form P(Y=y)=(p^y)*((1-p)^(1-y))
#Assume Survived had the following 5 values
# 1 1 1 0 0
#The likelihood would be measured as 
# p*p*p*(1-p)*(1-p)

#Null Deviance
#2*log(Saturated Likelihood) -2*log(Null Model) (intercept only) Likelihood
#2*((281+(633-281))*log(1))-2*(281*log(0.4439179)+(633-281)*log(1-0.4439179))

#Residual Deviance
#2*(633*log(1))-2*log(Model Likelihood) where the covariates and their coefficients compute p through
#the link function (logit(p)=b0+b1*X1+...bk*Xk)
#
#associated o5
#p value for Hypothesis test 1
1-pchisq(869.54-539.71,632-628)

#p value for Hypothesis test 2
anova(o4,o5,test="Chisq")

summary(o5)
#####Predictions
predict(o5,type="response")
############################################################3

optimapred=(predict(o5,type="response")>0.5)*1
#Sensitivity P(Yhat=1|Y=1)
#P(Yhat=1,Y=1)/P(Y=1)
(Number of Cases where Yhat=1 and Y=1)/Total Number of Cases
(Number of Cases where Y=1)/Total Number of Cases
sum((optimapred==1 & survived==1))/sum(survived==1)
#Specifity P(Yhat=0|Y=0)
sum((optimapred==0 & survived==0))/sum(survived==0)
#Accuracy
(sum((optimapred==1 & survived==1))+sum((optimapred==0 & survived==0)))/length(survived)

#P(Y=1|Yhat=1)
sum((optimapred==1 & survived==1))/sum(optimapred==1)

#P(Y=0|Yhat=0)
sum((optimapred==0 & survived==0))/sum(optimapred==0)

library(InformationValue)
# The columns are actuals, while rows are predicteds.
optCutOff <- optimalCutoff(survived, predict(o5,type="response"))
predicted=predict(o5,type="response")
plotROC(survived, predicted)
Concordance(survived,predicted)
#sum(survived==0)*sum(survived==1)

# The columns are actuals, while rows are predicteds.
sensitivity(survived, predicted, threshold = 0.5)
sensitivity(survived, predicted, threshold =optCutOff )
specificity(survived, predicted, threshold = 0.5)
specificity(survived, predicted, threshold = optCutOff )
confusionMatrix(survived, predicted, threshold = 0.5)
confusionMatrix(survived, predicted, threshold = optCutOff)









