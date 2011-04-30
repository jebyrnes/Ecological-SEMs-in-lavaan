###########
#
# Analysis of direct and indirect effects of birds in the Tatoosh intertidal
# from Wootton 1994 Ecology. http://dx.doi.org/10.2307/1939391
# This one is a classic, and great for understanding the basics of path analysis.
# Although, Wootton used partial correlations to calculate coefficients rather
# than SEM, this analysis produces similar results.  DF are off...looking into that.
#
# Note, only including model A for the moment - it's the model of best fit.  Would
# be fun to fit others and use LR Chisq tests to compare them
#
# last tested: 4/30/11
# last lavaan version tested: 0.4-8
#
# byrnes@nceas.ucsb.edu
###########


library(lavaan)
library(MBESS)

##
# Standard deviation vector and correlation matrix
# taken from paper.  Using MBESS to translate them to covariance matrix
##
sdVec<-c(26.76,16.97,15.48,0.90,0.47,0.16)
names(sdVec)<-c("Pollicipes", "Mytilus", "Semibalanus", "lnNucella", "Birds", "TideHeight")

cormat<-matrix(c(1,-0.468,-0.809,-0.349,-0.955,0.039,-0.468,1,-0.103,-0.303,0.399,-0.444,-0.809,-0.103,1,0.532,0.802,0.335,-0.349,-0.303,0.532,1,0.403,-0.210,-0.955,0.399,0.802,0.403,1,0,0.039,-0.444,0.335,-0.210,0,1), ncol=6)

rownames(cormat)<-colnames(cormat)<-names(sdVec)

#the covariance matrix
Wcov<-cor2cov(cormat, sdVec)


####
# Model A testing hypothesis 1
####
modelA<-'
 Pollicipes ~ Birds + TideHeight
 Mytilus ~ Birds + TideHeight + Pollicipes
 Semibalanus ~ Pollicipes + Mytilus + TideHeight
 lnNucella ~ Birds + Pollicipes + Semibalanus + TideHeight
 '
 
 fitA<-sem(modelA, sample.cov=Wcov, sample.nobs=20)
 
 summary(fitA)
 standardizedSolution(fitA)