####
#
# Example of the direct and indirect effects of elevation on plant cover via fires.
# Model taken from Figure 2.  In the original paper, this example was used
# to illustrate the meaning of standardized coefficients in SEM versus multiple
# regression.  Try using the covariance matrix to createa  correlation matrix
# and calculate partial and semipartial correlation coefficients yourself.  
# How do they compare?
#
# Grace, J.B. and Bollen, K.A. 2005. Interpreting the results from multiple regression and structural 
# equation models. Bulletin of the Ecological Society of America. 86:283-295
#
# last tested: 5/5/11
# last lavaan version tested: 0.4-8
#
# byrnes@nceas.ucsb.edu
###########


library(lavaan)

GB05Mat<-matrix(c(1006.2, -26.2, -139.4, 3636.3, 
                  -26.2,   2.722, 13.47,   -170.4, 
                  -139.4,   13.47, 157.8, -1459.6, 
                  3636.3, -170.4, -1459.6, 66693), ncol=4)

rownames(GB05Mat)<-colnames(GB05Mat)<-c("Plant.Cover", 
"Fire.Severity", "Stand.Age", "Elevation")


model<-'
  Stand.Age ~ Elevation
  Fire.Severity ~ Stand.Age
  Plant.Cover ~ Elevation + Fire.Severity
'

fit<-sem(model, sample.cov=GB05Mat, sample.nobs=90)

summary(fit)

standardizedSolution(fit)