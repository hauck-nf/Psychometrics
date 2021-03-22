#Classical test theory 
##Introductory class on reliability, Graduate School in Psychology, Universidade São Francisco
##Prof. Dr. Nelson Hauck
##syntax and examples: Patrick Mair, Modern psychometrics with R
#https://www.springer.com/gp/book/9783319931753

#load packages  
install.packages("MPsychoR")
library("psych")

###LET'S PRACTICE!###
#EXAMPLE 1: data on hybrid motivation of R package developers, from Mairs (2015)
##https://www.pnas.org/content/112/48/14788
##777 package developers
##assessed on three motivational dimensions: intrinsic, extrinsic, hybrid

library("MPsychoR")
data("Rmotivation")
ind <- grep("hyb", colnames(Rmotivation))
HybMotivation <- na.omit(Rmotivation[,ind]) ## item selection
k <- ncol(HybMotivation) ## number of items

#variance-covariance (VC) matrix of the items
vcmat <- cov(HybMotivation)

#sum of the main diagonal elements (trace)
sigma2_Xi <- tr(vcmat)

#total variance: sum of the variances and covariances on both sides of the diagonal
sigma2_X <- sum(vcmat)

#compute alpha
cronalpha <- k/(k-1)*(1-sigma2_Xi/sigma2_X)
round(cronalpha, 2)

#standard error of measurement
sqrt(sigma2_X)*sqrt(1-cronalpha)

#calculate alpha in a simpler way, using psych package
psych::alpha(HybMotivation)

#greatest lower bound
psych::glb(HybMotivation)

#omega coefficient
psych::omega(HybMotivation)

#Guttman's coefcicients
psych::guttman(HybMotivation)

#compute generalizability theory estimates: dataset in long format
library("reshape2")
Hyb1 <- data.frame(HybMotivation,
                   person = 1:nrow(HybMotivation))
Hyblong <- melt(Hyb1, id.vars = c("person"),
                variable.name = "item")
Hyblong$person <- as.factor(Hyblong$person)

#decomposing variance 
summary(aov(value ~ person + item, data = Hyblong))

#alpha approximation: ((person mean squares-residual mean squares)/person mean squares)
round((0.85-0.15)/0.85, 2)

#intraclass coefficient: items as different raters measuring the same attribute
ICC(HybMotivation)

#decomposing variance via random-effects ANOVA
library("lme4")
VarCorr(lmer(value ~ (1|person) + (1|item), data = Hyblong))

#compute generalizability coefficient
install.packages("gtheory")
library("gtheory")
gfit <- gstudy(data = Hyblong,
               formula = value ~ (1|person) + (1|item))
dfit <- dstudy(gfit, colname.objects = "person",
               colname.scores = "value", data = Hyblong)
round(dfit$generalizability, 3)

#EXAMPLE 2: multiple sources of error
#dataset from Lake and Hoyt (2013)
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3650138/
##181 children
##assessed on self-regulation
##different items, and raters (the published study reports also on different occasions)

data("Lakes")
phydat <- subset(Lakes, subtest == "physical")
phydat$item <- droplevels(phydat$item)
head(phydat)

#G-Study to extract variance components: ANOVA model
#Xpir = ?? + ??p + ??i + ??r + ??pi + ??pr + ??ir + ??pir,e
#grand population mean is ??
#p for the persons, i for the items, and r for the raters
#last effect is actually a residual effect involving the threeway
#interaction and all other sources of error not captured by the specification

#total variance decomposed as
#??2(Xpir ) = ??2p+ ??2i+ ??2r+ ??2pi+ ??2pr+ ??2ir+ ??2pir,e
#??2p between-children variability in self-regulation
#??2i between-item variability
#??2r between-rater variability
#??2pi Idiosyncratic perception of person p on item i (averaged over raters)
#??2pr Idiosyncratic perception of person p by rater r (averaged over items)
#??2ir Idiosyncratic leniency of rater r on item i (averaged over persons)
#??2 pir,e three-way interaction plus error

formula <- score ~ (1|personID) + (1|raterID) + (1|item) +
  (1|personID:raterID) + (1|personID:item) + (1|raterID:item)
gfit <- gstudy(formula = formula, data = phydat)
gfit

#D-Study: use of the information provided by the G-study to design the best possible application of
#the measurement
#for example: if fewer raters are sufficient or more raters are needed or if fewer items are sufficient or
#more items are needed
#nr number of raters 
#ni number of items

#weight the variance components by the number of elements
#ni = 3, nr = 5, nrni = 15

dfit <- dstudy(gfit, colname.objects = "personID",
               colname.scores = "score", data = phydat)
dfit$components

#compute asbsolute error variance
dfit$var.error.abs

#compute relative measurement error variance (only core variance elements of the model)
dfit$var.error.rel

#compute dependability coefficient (absolute error variance)
dfit$dependability

#compute generalizability coefficient (relative error variance)
#closer to alpha
dfit$generalizability


#HOMEWORK ASSIGNEMENT: now calculate reliability estimates (you choose which ones) for the intrinsic motivation items
#from the Rmotivation dataset!!
head(Rmotivation)
names(Rmotivation)
int<-Rmotivation[,c(32:36)]
int <- na.omit(int) 


