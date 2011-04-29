##############
#
# Model of best fit from Santos and Cannatella 2011 PNAS
# http://dx.doi.org/10.1073/pnas.1010952108
# Model taken from Figure 3C showing that scale and aposematism jointly load
# onto coloration.
#
# last tested: 4/29/11
# last lavaan version tested: 0.4-8
#
# byrnes@nceas.ucsb.edu#
##############

library(lavaan)
#turn the currelation matrix into a covariance matrix

#correlation matrix and SD vector from the paper's appendix
santo_cor<-matrix(c(1,NA,NA,NA,NA,NA,NA,NA,
0.612,1,NA,NA,NA,NA,NA,NA,
0.913,0.698,1,NA,NA,NA,NA,NA,
0.921,0.676,0.873,1,NA,NA,NA,NA,
0.455,0.7,0.522,0.519,1,NA,NA,NA,
0.372,0.546,0.449,0.43,0.806,1,NA,NA,
0.04,0.202,0.052,0.137,0.53,0.466,1,NA,
0.466,0.659,0.509,0.398,0.564,0.423,0.238,1),nrow=8)

rownames(santo_cor)<-colnames(santo_cor)<-c("Log.Mass","Conspicuous.coloration","Log.RMR","Log.Scope","Alkaloid.quantity","Alkaloid.diversity","Ant.Mite.Specialization","log.Prey")

#standard deviations
santo_sd<-c(0.0999,0.0389,0.0859,0.09,0.035,0.1058,0.0668,0.0806)

santo_cor[which(is.na(santo_cor), arr.ind=T)]<-0
diag(santo_cor)<-0.5
santo_cor<-santo_cor+t(santo_cor)

#OK the actual correlation -> covariance tranlsation
library(MBESS)

santosCov<-cor2cov(santo_cor, santo_sd)

#The best model from the paper, Figure 3C
santosModel<-'
	Scale =~Log.Mass+Log.RMR+Log.Scope+Conspicuous.coloration
	Aposematism =~Alkaloid.quantity+Alkaloid.diversity+Ant.Mite.Specialization+log.Prey+Conspicuous.coloration
	
	Scale ~~ Aposematism'
	
santosFit<-sem(santosModel, sample.cov=santosCov, sample.nobs=21)
summary(santosFit)
standardizedSolution(santosFit)
