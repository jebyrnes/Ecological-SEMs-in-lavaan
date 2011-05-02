######
#
# An example of the multivariate productivity-diversity effect from
# Cardinale et al. 2009 Ecology.  http://dx.doi.org/10.1890/08-1038.1
# This example shows how nutrients and diversity can jointly influence algal biomass in 
# streams and promary production, even when local diversity is a product of nutrient supply.
#
# I like this example as it naturally incorporates a nonlinearity.
#
# Note, for educational purposes, I've provided a reformulated version of the local diversity
# portion of the model combining the two nitrogen variables into a composite
#
# Data from http://www.esapubs.org/archive/ecol/E090/079/suppl-1.htm
#
# last tested: 5/2/11
# last lavaan version tested: 0.4-8
#
# byrnes@nceas.ucsb.edu
###########


library(lavaan)

cards<-read.table("./cardinale_et_al_2008_data.txt", sep="\t", header=T)
cards<-na.omit(cards)

cardModel<-'
  SA ~ logN + logNcen2 + SR
  logChl ~ SA + logN
  GPP ~ logN + logChl
 SR ~~ 0*logN + 0*logNcen2 #from path model - otherwise these are estimated
  
  '

#fixed.x=F in order to allow the dropping of paths between
#SR and logN and LogNcen2
cardFit <- sem(cardModel, data=cards, fixed.x=F)

summary(cardFit)

standardizedSolution(cardFit)


#simple SA model with N and regional SR using a composite
#for demonstration at http://imachordata.com
compositeModel<-'
	#1) define the composite, scale to logN
	Nitrogen ~ logN + 1*logNcen2 #loading on the significant path!

	#2) Specify 0 error variance
	Nitrogen ~~ 0*Nitrogen
  
  	#3) now, because we need to represent this as a latent variable
  	#show how species richness is an _indicator_ of nitrogen
  	Nitrogen =~ SA

	#4) BUT, make sure the variance of SA is estimated
  	SA ~~ SA

	#Regional Richness also has an effect
	SA ~ SR
	
	#And account for the derivation of the square term from the linear term
	logNcen2 ~ logN
  	  	
  	'
 
 # we specify std.lv=T so that the Nitrogen-SA relationship isn't fixed to 1
 compositeFit <-sem(compositeModel, data=cards, std.lv=T)

 summary(compositeFit)
 standardizedSolution(compositeFit)