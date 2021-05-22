#Lecture on IRT
##Prof. Dr. Nelson Hauck
##most of the syntax and examples come from: Patrick Mair, Modern psychometrics with R
#https://www.springer.com/gp/book/9783319931753
#an excellent presentation on polytomous IRT models
#is available at https://rpubs.com/Tarid/polyIRT_practice

#
library(mirt)
library(eRm)
library(ltm)
library(MPsychoR)

#load Bond & Fox Children Empathic Questionnaire items
data("CEAQ")
itceaq <- CEAQ[,1:16] - 1

#fit the Rating Scale model (Andrich, 1978) with the eRm package (classic Rasch parameterization)
fitrsm <- RSM(itceaq)
ppar <- person.parameter(fitrsm)
ifit0 <- eRm::itemfit(ppar)
ifit0

#eliminate the missfitting items 10 and 15
fitrsm1 <- RSM(itceaq[,c(-10,-15)])
ppar1 <- person.parameter(fitrsm1)
ifit1 <- eRm::itemfit(ppar1)
ifit1

#perform an invariance test across grades
library("mice")
set.seed(222)
imp <- mice(CEAQ)
gradevec <- complete(imp)$grade
levels(gradevec) <- c("grade56","grade56","grade78","grade78")
LRtest(fitrsm1, gradevec)

#perform a more detailed test of the fit of each item category
Waldtest(fitrsm1)

#inspect item parameters
thpar <- thresholds(fitrsm2)
thpar

#plot item-category characteristic curves
plotICC(fitrsm1, item.subset = "all", xlab = "Empathy",
             main = "ICCs Empathy Items")

#plot person-item (construct) map#this is great!!
plotPImap(fitrsm1, latdim = "Empathy",
          main = "Person-Item Map CEAQ")

#using mirt parameterization (theta M=0, SD=1)
rsm<-mirt(itceaq[,c(-10,-15)], 1, itemtype = 'rsm',verbose = FALSE, TOL = 0.001)
coef.rsm <- coef(rsm, IRTpars=TRUE, simplify=TRUE)
items.rsm <- as.data.frame(coef.rsm$items)
print(items.rsm)

#build beautiful plots!
plot(rsm,type="trace")
plot(rsm,type="infoSE")
plot(rsm, type = 'rxx', theta_lim = c(-3, 3), 
     main="" )#conditional reliability

#Partial Credits model
data("ASTI")
PGitems <- ASTI[ ,c(11,14,15,17,18,23)] ## extract PG items
fitpcm <- PCM(PGitems)
ppar <- person.parameter(fitpcm)
ifit <- eRm::itemfit(ppar)
ifit
thresholds(fitpcm)

#person-item map
plotPImap(fitpcm, latdim = "Presence/Growth",
          main = "Person-Item Map ASTI")

#plot ICCs
plotICC(fitpcm, item.subset = "all", xlab = "Empathy",
        main = "ICCs Empathy Items")

#run using mirt
pcm<-mirt(PGitems, 1, itemtype = 'Rasch',verbose = FALSE, TOL = 0.001)
coef.pcm <- coef(pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)
plot(pcm,type="trace")

#Generalized Partial Credits model (Muraki, 1992)
gpcm <- mirt(PGitems, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.gpcm <- coef(gpcm, IRTpars=TRUE, simplify=TRUE)
items.gpcm <- as.data.frame(coef.gpcm$items)
print(items.gpcm)
plot(gpcm,type="trace")


#compare the results from the PCM and the GPCM
anova(pcm,gpcm)

#Graded Response model (Samejima, 1969)
grm <- mirt(PGitems, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
coef.grm <- coef(grm, IRTpars=TRUE, simplify=TRUE)
items.grm <- as.data.frame(coef.grm$items)
print(items.grm)
plot(grm,type="trace")


#compare the results from the GPCM and the GRM
anova(gpcm,grm)
