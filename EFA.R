#FACTOR ANALYSIS
#load packages
library(psych)
library(lavaan)
library(semPlot)

#####################
#Simulate the data!
#####################
model1=
  'f1=~.50*y1+.60*y2+.70*y3
  f2=~.50*y4+.60*y5+.70*y6
'
data1<-simulateData(model = model1, model.type = "sem", standardized=TRUE,sample.nobs = 500L)
data1_cut<-apply(data1,2,function (x) cut(x,breaks=c(5), labels=c("1","2","3","4","5")))
data1_cut<-as.data.frame(apply(data1_cut,2,as.numeric))
#####################
#factor analysis
#####################
fit1<-fa(data1_cut,nfactors=2,cor="poly")
print(fit1,sort=FALSE)

#############################
#extract matrices of interest
#############################
psi<-round(fit1[["residual"]],2)#
lambda<-round(fit1[["Structure"]],2)
lambdat<-t(lambda)
phi<-round(fit1[["Phi"]],2)

#calculating the Sigma matrix from the factor model
#the model implied matrix is Sigma=L*Phi*Lt+Psi
Sigma=round(lambda%*%phi%*%lambdat+psi,2)
#The closest to this is the "model" matrix from the output
#model matrix is basically the Sigma matrix with communalities in the main diagonal
#check it
model<-round(fit1[["model"]],2)
print(model)
diag(model)
#if you want to sum the sommunalities
#tr(model)
fit1[["communalities"]]

#contrast model implied matrix and the 
S<-polychoric(data1_cut)
S<-S[["rho"]]
S<-round(S,2)
diff<-Sigma-model

#tetrachoric correlation
library("MPsychoR")
data("YouthDep")
item1 <- YouthDep[, 1]
levels(item1) <- c("0", "1", "1")
item2 <- YouthDep[, 14]
levels(item2) <- c("0", "1", "1")
table(item1, item2)
library("psych")
tetcor <- tetrachoric(cbind(item1, item2))
tetcor

#polichoric correlation
item1 <- YouthDep[, 1]
item2 <- YouthDep[, 14]
polcor <- polychoric(cbind(item1, item2))
polcor

#EFA using tetrachoric correlations
data("bfi")
names(bfi)
poly <- polychoric(bfi[,1:25])
fit_bfi <- fa(poly$rho, nfactors = 5, rotate = "none", fm = "ml")
print(fit_bfi$loadings, cutoff = 0.3)

#or simply
fit<-fa(bfi[,1:25],nfactors=5, rotate="none", fm="ml", cor="poly")
print(fit,sort=FALSE)

#rotations
fa(bfi[,1:25],nfactors=5, rotate="varimax", fm="ml", cor="poly")
print(fit,sort=FALSE)

fa(bfi[,1:25],nfactors=5, rotate="oblimin", fm="ml", cor="poly")
print(fit,sort=FALSE)

#number of factors
##screeplot
poly <- polychoric(bfi[,1:25])$rho
evals <- eigen(poly)$values
scree(poly, factors = FALSE)

##very simple structure
VSS(bfi[,1:25],cor="poly")

##parallel analysis
fa.parallel(bfi[,1:25],fa="fa",fm="ml",cor="poly")

#HOME ASSIGNEMENT
##now execute a factor analysis using the data1_cut data frame
##run a polychoric correlation matrix
##determine the number of factors
##examine the loadings pattern and the model fit
##rotate the solution
