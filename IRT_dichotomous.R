#Lecture on IRT
##Prof. Dr. Nelson Hauck
##most of the syntax and examples come from: Patrick Mair, Modern psychometrics with R
#https://www.springer.com/gp/book/9783319931753
#other interesting Rasch class: https://rpubs.com/Tarid/raschmirt

#load binary indicators dataset (subtraction items)
library("MPsychoR")
library("mirt")
data("zareki")
zarsub <- zareki[, grep("subtr", colnames(zareki))]

#descriptive analysis
library(ltm)
descript(zarsub)

#dimensionality assessment 
library("Gifi")
prinzar <- princals(zarsub)
plot(prinzar, main = "Zareki Loadings")

library(psych)
VSS(zarsub,cor="poly")
fa.parallel(zarsub,cor="poly")
iclust(zarsub)


fitifa1 <- mirt(zarsub, 1, verbose = FALSE)
fitifa2 <- mirt(zarsub, 2, verbose = FALSE, TOL = 0.001)
anova(fitifa1, fitifa2, verbose = FALSE)

#compare the IFA solution with a 1-factor ML solution using tetrachoric correlations
fitifa1@Fit
fa(zarsub,nfactors=1,cor="poly",fa="ml")

#Rasch model
library("eRm")
fitrasch1 <- RM(zarsub)
fitrasch1
round(fitrasch1$betapar, 3)
round(sort(-fitrasch1$betapar), 3)

#Rasch using alternative parameterization
rasch<-mirt(zarsub, 1, itemtype = 'Rasch',verbose = FALSE, TOL = 0.001)
coef.rasch <- coef(rasch, IRTpars=TRUE, simplify=TRUE)
items.rasch <- as.data.frame(coef.rasch$items)
print(items.rasch)

#test the invariance of the Rasch model over data partitions (p should be > .05)
timecat <- factor(zareki$time <= median(zareki$time),
                  labels = c("fast", "slow"))
fitLR <- LRtest(fitrasch1, timecat)
fitLR

#which item violates invariance across data partitions?
Waldtest(fitrasch1, timecat)
plotGOF(fitLR, ctrline = list(col = "gray"), conf = list())

#exclude item 5
fitrasch2 <- RM(zarsub[, -5])
LRtest(fitrasch2, timecat)

#test the Rasch assumptions more directly
set.seed(123)
T1 <- NPtest(as.matrix(zarsub[, -5]), n = 1000, method = "T1") #local independence in item pairs
T1
T11 <- NPtest(as.matrix(zarsub[, -5]),n = 1000, method = "T11") #global local independence test
T11

#plot ICC under the Rasch model
plotjointICC(fitrasch2, xlab = "Subtraction Trait",
             main = "ICCs Subtraction Items")

#in case you want to estimate person values on the theta parameter
zarppar <- person.parameter(fitrasch2)

#use the theta parameter in an external inferential analysis
zareki$theta <- zarppar$theta.table[,1]
summary(aov(theta ~ class, data = zareki))

#2PL using Work Design data
library("ltm")
data("RWDQ")
fit2pl1 <- ltm(RWDQ ~ z1)# item 23 has an extremely low difficulty parameter
RWDQ1 <- RWDQ[,-1] # remove it
fit2pl2 <- ltm(RWDQ1 ~ z1)

#compare Rasch and 2PL
fit_rasch<-rasch(RWDQ[,-1])
anova(fit_rasch, fit2pl2, verbose = FALSE)

#item fit under in each model
item.fit(fit_rasch)
item.fit(fit2pl2)

#plot ICC under the 2PL model
plot(fit2pl2, xlab = "Trait",
     main = "Work Design Items")

#compute person parameters for the 2PL model
ppars <- ltm::factor.scores(fit2pl2,
                            resp.patterns = RWDQ1)$score.dat[, "z1"]

#HOMEWORK ASSIGNEMENT
#Run a 2PL model using the mirt function and the zarsub dataset
#Compare the model fit against the fit for the Rasch model; which one fits the data better?
fit_1PL<-mirt(zarsub, 1, itemtype = 'Rasch',verbose = FALSE, TOL = 0.001)
fit_2PL<-mirt(zarsub, 1, itemtype = '2PL',verbose = FALSE, TOL = 0.001)
anova(fit_1PL, fit_2PL, verbose = FALSE)
