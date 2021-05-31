#A MIRT APPROACH TO THE CONTROL OF RESPONSE STYLES
##Prof. Dr. Nelson Hauck
#This lectures emphasizes the control of response styles using lavaan and the mirt framework
#for testing multidimensional models
#load packages
library(lavaan)
library(mirt)
library(psych)
library(dplyr)

#Acquiescent and extreme responding styles in self-reported big five domains
#We are going to use items from C factor
bfi=data("bfi")#load the data
extreme<-apply(bfi,2,function (x) recode(x,`1`=1,`6`=1,`2`=0,`3`=0,`4`=0,`5`=0))
extreme<-as.data.frame(apply(extreme[,1:25],2,as.numeric))
ext <- grepl("[1-5]",names(extreme)); names(extreme)[ext] <- paste0(names(extreme)[ext],"_ext")
ext<-cbind(bfi[,1:25],extreme)

corr.test(bfi[,6:10])#check correlations to detect item keying

#Random intercept model to control for acquiescence
model_C_ARS='
C=~C1+C2+C3+C4+C5
ARS=~1*C1+1*C2+1*C3+1*C4+1*C5
ARS~~ARS
ARS~~0*C
'
fit_C_ARS=cfa(model_C_ARS, data = bfi,ordered = names(bfi))
summary(fit_C_ARS, standardized = TRUE, fit.measures = TRUE)

#multidimensional model to control for acquiescence and extreme responding
model_C_ARS_ERS='
C=~C1+C2+C3+C4+C5
ARS=~1*C1+1*C2+1*C3+1*C4+1*C5
ARS~~ARS
ARS~~0*C
ERS=~A1_ext+A2_ext+A3_ext+A4_ext+A5_ext+
E1_ext+E2_ext+E3_ext+E4_ext+E5_ext+
N1_ext+N2_ext+N3_ext+N4_ext+N5_ext+
O1_ext+O2_ext+O3_ext+O4_ext+O5_ext+
C1+C2+C3+C4+C5
ERS~~ERS
ERS~~0*ARS
ERS~~0*C
'

fit_C_ARS_ERS=cfa(model_C_ARS_ERS, data = ext,ordered = names(ext))
summary(fit_C_ARS_ERS, standardized = TRUE, fit.measures = TRUE)

#Now using scoring keys in mirt
#define scoring keys
scoring<-list()
for(i in 1:5){
  scoring[[i]]<-matrix(
    c(1,2,3,4,5,6, # trait
      0,0,0,0,1,2,#ARS
      2,1,0,0,1,2# ERS
    ), 6, 3)
}

# Define the model: all items load on the three dimensions according to constraints
model<-"
 C = 1-5
 ARS = 1-5
 ERS = 1-5
 COV = C*0ARS
 COV = C*0ERS
 COV = ARS*0ERS
 "
# Estimate the multidimensional GPCM
fit<-mirt(bfi[,6:10],model,itemtype="gpcm",gpcm_mats=scoring,TOL = .0001,technical=list(NCYCLES=1000))
summary(fit)
coef(fit,simplify=TRUE, irt.parms = TRUE)






#Let us simulate the data!
#This is a multidimensional model in which one trait-descriptive
#and one acquiescence random intercept factor
#explain responses to eight items rated on a five-point graded scale
#Trait and acquiescence factors are orthogonal one to another

model=
  'trait=~.7*y1+.7*y2+.7*y3+.7*y4+(-.7)*y5+(-.7)*y6+(-.7)*y7+(-.7)*y8
  acq=~.2*y1+.2*y2+.2*y3+.2*y4+.2*y5+.2*y6+.2*y7+.2*y8
  trait~~0*acq
  trait~~1*trait
'
data<-simulateData(model = model, model.type = "sem", standardized=TRUE,sample.nobs = 2000L)
data<-apply(data,2,function (x) cut(x,breaks=c(5), labels=c("1","2","3","4","5")))
data<-as.data.frame(apply(data,2,as.numeric))

#EXPLORATORY CATEGORICAL ITEM FACTOR ANALYSIS
ifa=mirt(data,2,verbose = FALSE)#positive and negative items separated because of acquiescence variance
print(ifa@Fit)
itemfit(ifa)

#BIFACTOR MODEL
bfactor=bfactor(data,model=c(1,1,1,1,1,1,1,1))#the same in a bifactor model
summary(bfactor)

#STANDARD GRADED RESPONSE MODEL (SAMEJIMA, 1969)
#item loadings on a main factor are consistent with a trait representation of the data
grm=mirt(data,1,itemtype = "graded",verbose = FALSE)
summary(grm)
coef(grm,simplify=TRUE, irt.parms = TRUE)

#RANDOM INTERCEPT GRADED RESPONSE MODEL (RI ITEM FACTOR ANALYSIS)
#Now let us try to control for acquiescence by modeling it as a
#random intercept factor!!
#Strategy 1: keep items 5:8 negatively-keyed (negative discriminations in the trait dimension will occur)
prs<-mirt(data,2, itemtype = 'graded', pars = 'values',TOL = .001)
prs[prs$name=="a2", ]$value <- 1 
prs[prs$name=="a2", ]$est <- FALSE 
prs[prs$name=="a1", ]$value <- abs(prs[prs$name=="a1", ]$value)
prs[prs$name=="COV_22", ]$est <- TRUE 
prs[prs$name=="COV_21", ]$est <- FALSE#this will work even you if set it  to TRUE
mirt_acq<-mirt(data,2, itemtype = 'graded', pars = prs,TOL = .001)
summary(mirt_acq)#standard factor analysis results
coef(mirt_acq,simplify=TRUE, irt.parms = TRUE)#standard IRT


#RANDOM INTERCEPT GRADED RESPONSE MODEL (RI ITEM FACTOR ANALYSIS)
#revsersing items 5:8 negatively-keyed (does not work so well)
prs <- data %>% mutate_at(vars(5:8),funs(6-.)) %>%
mirt(2, itemtype = 'graded', pars = 'values',TOL = .001)
key=c(1,1,1,1,-1,-1,-1,-1)
prs[prs$name=="a2", ]$value <- key
prs[prs$name=="a2", ]$est <- FALSE 
prs[prs$name=="a1", ]$value <- abs(prs[prs$name=="a1", ]$value)
prs[prs$name=="COV_11", ]$value <- 1 
prs[prs$name=="COV_22", ]$est <- TRUE 
prs[prs$name=="COV_21", ]$est <- FALSE
mirt_acq<-mirt(data,2, itemtype = 'graded', pars = prs,TOL = .0001,technical = list(NCYCLES = 2000))
summary(mirt_acq)
coef(mirt_acq,simplify=TRUE, irt.parms = TRUE)
  

#RANDOM INTERCEPT GENERALIZED PARTIAL CREDITS MODEL 
prs<-mirt(data,2, itemtype = 'gpcm', pars = 'values',TOL = .001)
prs[prs$name=="a2", ]$value <- 1 
prs[prs$name=="a2", ]$est <- FALSE 
prs[prs$name=="a1", ]$value <- abs(prs[prs$name=="a1", ]$value)
prs[prs$name=="COV_22", ]$est <- TRUE 
prs[prs$name=="COV_21", ]$est <- FALSE#this will work even you if set it  to TRUE
mirt_acq<-mirt(data,2, itemtype = 'gpcm', pars = prs,TOL = .001)
summary(mirt_acq)#standard factor analysis results
coef(mirt_acq,simplify=TRUE, irt.parms = TRUE)#standard IRT


#MULTIDIMENSIONAL GENERALIZED PARTIAL CREDITS MODEL WITH SCORING KEYS
#Define the response style dimension by using a scoring key matrix
# Custom scoring functions for trait and ARS factors
scoring<-list()
for(i in 1:8){#nuber of items
 scoring[[i]]<-matrix(
     c(1,2,3,4,5, # trait
       0,0,0,1,2 # ARS
       ), 5, 2) #number of response options, number of dimensions
   }

 # Define model: all items load on both dimensions
 # Estimate covariance between QOL and ERS
 model<-"
 trait = 1-8 #all items load on the trait factor
 ARS = 1-8 #all items load on the ARS factor
 COV = trait*0ARS #trait and ARS have 0 correlation
 COV = trait*1trait #trait variance is constrained to 1
 "
 # Estimate the trait and ARS model
 fit<-mirt(data,model,itemtype="gpcm",gpcm_mats=scoring,TOL = .0001,technical=list(NCYCLES=1000))
 summary(fit)#factor analysis parameterization
 coef(fit,simplify=TRUE, irt.parms = TRUE)#IRT parameterization
 


#HOMEWORK ASSIGNEMENT
#Now try and use some of the previous methods to control for acquiescent or/and extreme responding
#in the agreeableness items from the bfi data