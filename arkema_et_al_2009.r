###########
#
# Analysis of direct and indirect effects of giant kelp on the dynamics of competition between 
# sessile algae and invertebrates.  from Arkema et al 2009 Ecology http://dx.doi.org/10.1890/08-1213.1
#
# This was a fun one, and part of one of the first things I helped out with as a postdoc at the 
# Santa Barbara Coastal LTER http://sbc.lternet.edu (all data publically available there).
# This data, however, is a subset for non-urhcin barren transects.
#
# Note, slight differences in parametere estiamtes and standard errors are due
# to differences between lavaan and the sem package.
#
# This work was funded by a grant from the National Science Foundation
# Long Term Ecological Research Site program.
#
# last tested: 5/1/11
# last lavaan version tested: 0.4-8
#
# byrnes@nceas.ucsb.edu
###########

library(lavaan)

#load data
arkData <- read.csv("./arkema_et_al_2009_data.csv")


#get residuals from data to accound for effects of site
arkData<-within(arkData, {
  percent.algae <- lm(percent.algae ~ site, data=arkData)$residuals
  percent.inverts <- lm(percent.inverts ~ site, data=arkData)$residuals
  transect.algae <- lm(transect.algae ~ site, data=arkData)$residuals
})

##
#Simple model where % cover of algae was affected by giant kelp and midcanopy kelps
#while inverts were affected by giant kelp, midcanopy kelps, and other algae
#i.e., giant kelp reduces algal cover, thus indirectly increasing invert cover
##
arkModel<-'
  percent.algae ~ frond.density + transect.algae + percent.holdfasts
  percent.inverts ~ frond.density + transect.algae + percent.holdfasts + percent.algae
  transect.algae ~ frond.density
  '

#Note that this model was fit with Fml, but, due to multivariate non-normality
#we scaled the test of fit and the standard errors 
arkFit <- sem(arkModel, data=arkData, estimator="MLM")
summary(arkFit)

#standardized coefficients are in Fig. 5 of the model
standardizedSolution(arkFit)

#variation explained
inspect(arkFit, "r2")
