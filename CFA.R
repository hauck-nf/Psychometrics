#Lecture on CFA
##Prof. Dr. Nelson Hauck
##syntax and examples: Patrick Mair, Modern psychometrics with R
#https://www.springer.com/gp/book/9783319931753

#load and clean data
library("MPsychoR")
library("lavaan")

data("Rmotivation")
vind <- grep("ext|int", colnames(Rmotivation)) ## ext/int items
Rmot <- na.omit(Rmotivation[, vind])

#specify and fit the model
mot_model <- '
extrinsic =~ ext1 + ext2 + ext3 + ext4 + ext5 + ext6 +
ext7 + ext8 + ext9 + ext10 + ext11 + ext12
intrinsic =~ int1 + int2 + int3 + int4 + int5'
fitMot <- lavaan::cfa(mot_model, data = Rmot,
                      ordered = names(Rmot))

#check the results
library("semPlot")
semPaths(fitMot, what = "est", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)
summary(fitMot, standardized=TRUE,fit.measures=TRUE)
modindices(fitMot)

#request the matrix that comprise the factor model
inspect(fitMot, what = "est")$theta
inspect(fitMot, what = "std")$lambda
inspect(fitMot, what = "std")$psi

#hierarchical model
vind <- c(1:4, 13:16, 32:35)
Rmot2 <- na.omit(Rmotivation[, vind])

mot_model3 <- '
extrinsic =~ ext1 + ext2 + ext3 + ext4
hybrid =~ hyb1 + hyb2 + hyb3 + hyb4
intrinsic =~ int1 + int2 + int3 + int4
motivation =~ extrinsic + hybrid + intrinsic'
fitMot3 <- lavaan::cfa(mot_model3, data = Rmot2,
                       ordered = names(Rmot2))

semPaths(fitMot3, what = "est", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)

summary(fitMot3, standardized = TRUE, fit.measures = TRUE)

#MIMIC model
vind <- c(1:4, 13:16, 32:35, 39:41)
Rmot3 <- na.omit(Rmotivation[, vind])

mot_model4 <- '
extrinsic =~ ext1 + ext2 + ext3 + ext4
hybrid =~ hyb1 + hyb2 + hyb3 + hyb4
intrinsic =~ int1 + int2 + int3 + int4
motivation =~ extrinsic + hybrid + intrinsic
motivation ~ npkgs + phd'

fitMot4 <- lavaan::cfa(mot_model4, data = Rmot3,
                       ordered = names(Rmot3[1:12]))

#standard multigroup CFA
library("semTools")
data("Bergh")

#configural
GP_model <-'GP =~ c(v1,v1)*EP + c(v2,v2)*HP + c(v3,v3)*DP + SP'
fitBase_conf <-lavaan::cfa(GP_model, data = Bergh, group = "gender",
                      estimator = "MLR")
summary(fitBase_conf, standardized = TRUE, fit.measures = TRUE)

#metric
GP_model <- 'GP =~ EP + HP + DP + SP'
fitBase_met <- lavaan::cfa(GP_model,data = Bergh, group = "gender",
                       group.equal = c("loadings"), estimator = "MLR")
summary(fitBase_met, standardized = TRUE, fit.measures = TRUE)

#scalar
fitBase_esc <- lavaan::cfa(GP_model, data = Bergh,group = "gender",
                       group.equal = c("loadings", "intercepts"),
                       estimator = "MLR")
summary(fitBase_esc, standardized = TRUE, fit.measures = TRUE)

#compare models using Satorra-Bentler scaled chi-square test
anova(fitBase_conf,fitBase_met,fitBase_esc)

#HOME ASSIGNEMENT
#Now test a confirmatory five-factor model using the BFI data from psych package
data("bfi")
