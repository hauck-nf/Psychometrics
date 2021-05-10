#Lecture on Path Analysis and SEM
##Prof. Dr. Nelson Hauck
##most of the syntax and examples come from: Patrick Mair, Modern psychometrics with R
#https://www.springer.com/gp/book/9783319931753

#PRELUDE
#Try to understand the matrix calculations underlying a multivariate regression model
#Y=XB+E
#model parameters: y = 1.15122X1 + 0.09756X2 - 1.21463
Y=cbind(Y1=c(1,3,5,4,6))
X=cbind(B0=c(1,1,1,1,1),X1=c(2,3,5,4,6),X2=c(2,4,3,7,5))
B=rbind(B0=-1.21,X1=(1.15),X2=(.10))

#calculate residuals
E=Y-X%*%B

#predict item responses from model matrices. Beautiful!
Ypred=X%*%B+E

#Linear models & MANOVA
# test a model of personality factors A and O predicting 
#ethnic prejudice and mental illness prejudice
library("MPsychoR")
data("Bergh")
fitmvreg <- lm(cbind(EP, DP) ~ A1 + A2 + O1 + O2, data = Bergh)
summary(fitmvreg)

library("car")
Manova(fitmvreg)

#Now using lavaan
library("lavaan")
mvreg.model <- '
EP ~ b11*A1 + b12*A2 + b13*O1 + b14*O2
DP ~ b21*A1 + b22*A2 + b23*O1 + b24*O2'
fitmvreg2 <- sem(mvreg.model, data = Bergh)
summary(fitmvreg2, standardized=TRUE,fit.measures=TRUE)
#look at the residual variances, compare with the R^2 from the multiple regressions

#construct a plot
library("semPlot")
semPaths(fitmvreg2, what = "est", edge.label.cex = 1,
         layout = "tree", residuals = FALSE, edge.color = 1,
         esize = 1, rotation = 3, sizeMan = 8, asize = 2.5,
         fade = FALSE, optimizeLatRes = TRUE)

#MODERATION
#Y=X+X*Z
#participative climate moderates the relationship between 
#work intensification and cognitive appraisal
library("MPsychoR")
data("Paskvan")
#first with no interaction effect
wintense.c <- scale(Paskvan$wintense, scale = FALSE) ## center
fit.YX <- lm(cogapp ~ wintense.c, data = Paskvan) ## Y on X
round(summary(fit.YX)$coefficients, 4)

#including interaction
library("QuantPsyc")
fit.mod <- moderate.lm(x = wintense, z = pclimate, y = cogapp,
                       data = Paskvan)
round(summary(fit.mod)$coefficients, 4)

#understanding interaction by slicing the Z varible
fit.ss <- sim.slopes(fit.mod, Paskvan$pclimate)
round(fit.ss, 4)

#MEDIATION
#Y=X+Z
#Z=X
#cognitive appraisal mediates the relationship between 
#work intensification and emotional exhaustion
#Potential outcome framework
library("mediation")
fit.MX <- lm(cogapp ~ wintense, data = Paskvan)
fit.YXM <- lm(emotion ~ wintense + cogapp, data = Paskvan)

set.seed(123)
fitmed <- mediation::mediate(fit.MX, fit.YXM,
                             treat = "wintense", mediator = "cogapp",
                             sims = 999, boot = TRUE, boot.ci.type = "bca")
summary(fitmed)

#now using lavaan
med.model <- '
emotion ~ c*wintense + b*cogapp
cogapp ~ a*wintense
ind := a*b
tot := ind+c
prop := ind/tot'
set.seed(123)
fitmedsem <- lavaan::sem(med.model, Paskvan, se = "bootstrap",
                         bootstrap = 999)
parameterEstimates(fitmedsem, zstat = FALSE, pvalue = FALSE,
                   boot.ci.type = "bca.simple")[c(7,1,8,9),]


#SEMODEL
#Openness and Agreeableness as predictors of a general factor or prejudice
library("MPsychoR")
library("lavaan")
data("Bergh")
Bergh.model <- 'GP =~ EP + HP + DP + SP
Agree =~ A1 + A2 + A3
Open =~ O1 + O2 + O3
GP ~ Agree + Open'
fitGP <- sem(Bergh.model, data = Bergh, estimator = "MLR")

semPaths(fitGP, what = "std", edge.label.cex = 0.7, esize = 1,
         intercepts = FALSE,rotation = 4, edge.color = 1, asize = 2.5,
         sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)

summary(fitGP, standardized = TRUE, fit.measures = TRUE)

#HOMEWORK ASSIGNEMENT
#Using the BFI data from psych package test an SEModel specifying
#a latent factor of Agreeableness predicted by the age variable.
#You can use three to five indicators (items A1, A2, A3, A4, and A5)
#to define the Agreebleness factor. Treat the indicators as ordinal variables
#and estimate model parameters using the WLSMV estimator
library(psych)
data("bfi")
View(bfi)
