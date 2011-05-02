#####
# Models from Grace and Bollen 2006 USGS report on SEMS
# in Ecology.  Provides multiple examples of latent
# and composite variable uses.  This file is intended
# for folk to get a look at how one can implement
# composite variables in the lavaan 0.4 tree
#
# note: only the competition-colonization models included here
#
# last tested: 4/29/11
# last lavaan version tested: 0.4-8
# source of models: http://pubs.usgs.gov/of/2006/1363/
#
# byrnes@nceas.ucsb.edu
####

library(lavaan)
library(MBESS) #MBESS used to convert the reported correlation matrix to cov


#####
# use the published correlation matrix and sd
# to create a covariance matrix

#sample size
n<-180

#correlation matrix taken from text
bg.cor<-matrix(c(1,NA,NA,NA,NA,NA,NA,
-0.5785,1,NA,NA,NA,NA,NA,
0.6424,-0.5934,1,NA,NA,NA,NA,
-0.2553,0.1844,-0.3146,1,NA,NA,NA,
-0.3369,0.4604,-0.3462,0.5767,1,NA,NA,
-0.0073,-0.0465,-0.0976,0.1324,0.0265,1,NA,
-0.3423,0.207,-0.4062,0.3189,0.2952,0.2394,1),ncol=7)

rownames(bg.cor)<-colnames(bg.cor)<-c("Col.Freq", "Distance", "Age", "Texture", "Moisture", "pH", "Cover")

#fill in the correlation matrix
bg.cor[which(is.na(bg.cor), arr.ind=T)]<-0
diag(bg.cor)<-0.5
bg.cor<-bg.cor+t(bg.cor)

#the standard deviation vector
bg.sd<-c(0.1867,0.8207,0.3390,0.4588,1.1359,0.7507,0.3017)

#convert the correlation matrix and SDs to a covariance matrix
bg.cov<-cor2cov(bg.cor, bg.sd)



###
# Response stays constant across all models, so
# create a latent response submodel that incorporates
# the specified measurement error from the report
###
#loadings for col freq and cover error
#0.1*bg.sd[1]^2 #0.003485689
#0.1*bg.sd[7]^2 #0.009102289

latent.response<-
' #measurement error latents
  Competitors =~ Cover
  Colonization =~ Col.Freq
  Col.Freq ~~ 0.003485689*Col.Freq
  Cover ~~ 0.009102289*Cover'

####
# Model A - latent variables
####
model.a<-paste(sep="\n", latent.response, '
  #latent exogenous vars
  Soil =~ pH + Moisture + Texture
  Landscape =~ Distance + Age
  Soil ~~ Landscape
  
  #regressions
  Competitors ~ Soil + Landscape
  Colonization ~ Soil + Landscape + Competitors')
  
fit.a<-sem(model.a, sample.cov=bg.cov, sample.nobs=n, std.lv=T)
summary(fit.a)

####
# Model B - Composite variables
####
model.b<-paste(sep="\n", latent.response, '
  #composite exogenous vars
  Soil ~ 1*pH + Moisture + Texture
  Soil ~~ 0*Soil

  Landscape ~ 1*Distance + Age
  Landscape ~~ 0*Landscape
  
  #LV definitions for regressions
  Soil =~ I(NA)*Competitors+I(NA)*Colonization
  Landscape =~  I(NA)*Competitors+I(NA)*Colonization
  
  #last bit of regression
  Colonization ~ Competitors
  
  ')

#note the orthogonal argument to make sure the composites are not correlated
fit.b<-sem(model.b, sample.cov=bg.cov, sample.nobs=n, orthogonal=T)
fit.b

summary(fit.b)
standardizedSolution(fit.b)


####
# Model C - Observed variables
####
model.c<-paste(sep="\n", latent.response, '

  #regression relationships!
  Competitors ~ pH + Moisture + Texture + Distance + Age
  Colonization ~ pH + Moisture + Texture + Distance + Age+Competitors')


fit.c<-sem(model.c, sample.cov=bg.cov, sample.nobs=n)
summary(fit.c)
standardizedSolution(fit.c)


####
# Model D - Multiple Composites
# still not working...although, says I have 2DF, but paper says model D has 0....
####
model.d<-paste(sep="\n", latent.response, '
  #composite exogenous vars
  Soil ~ 1*pH + Moisture + Texture
  Soil ~~ 0*Soil

  Soil2 ~ pH + 1*Moisture + Texture
  Soil2 ~~ 0*Soil2

  Landscape ~ 1*Distance + Age
  Landscape ~~ 0*Landscape

  Landscape2 ~ Distance + 1*Age
  Landscape2 ~~ 0*Landscape2

  #LV definitions for regressions
  Soil =~ I(NA)*Competitors
  Landscape =~  I(NA)*Competitors

  Soil2 =~ I(NA)*Colonization
  Landscape2 =~  I(NA)*Colonization
 

  #last bit of regression
  Colonization ~ Competitors

  ')

fit.d<-sem(model.d, sample.cov=bg.cov, sample.nobs=n, orthogonal=TRUE)

summary(fit.d)
standardizedSolution(fit.d)
