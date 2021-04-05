#load packages
library(psych)
library(lavaan)
library(semPlot)

#####################
#Simulate the data!
#####################
model1=
  'recidivism~.20*psychopathy+.15*impulsivity+.40*violent_crime
'
data1<-simulateData(model = model1, model.type = "sem", standardized=TRUE,sample.nobs = 500L)

#####################
#correlate variables
#####################
cor<-corr.test(data1)
cor$r
partial.r(data1,c("recidivism","psychopathy"),c("impulsivity","violent_crime"))
partial.r(data1,c(1:2),c(3:4))#or simply this

#####################
#correct for unreliability
#####################
cor<-corr.test(data1)
cor$r
correct.cor(cor$r,c(.70,.70,.70,.70))

######################################################################
#regression analysis (standardized variables, so coefficients are beta)
######################################################################
fit1<-lm(recidivism~impulsivity+violent_crime, data=data1)
summary(fit1)
fit2<-lm(recidivism~psychopathy+impulsivity+violent_crime, data=data1)
summary(fit2)
anova(fit1,fit2)

#####################
#the importance of controlling variables
#####################
#does being in a relationship make people happy?

model2=
  'happiness~.5*children+.5*money
  children~.5*relationship
  money~-.5*relationship
'
data2<-simulateData(model = model2, model.type = "sem", standardized=TRUE,sample.nobs = 500L)

#first, correlate and observe
corr.test(data2)

#now test if having children and being in a relationship make people happier
fit3<-lm(happiness~children+relationship, data=data2)
summary(fit3)#what is happening here?

