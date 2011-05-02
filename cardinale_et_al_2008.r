######
#
# Data from http://www.esapubs.org/archive/ecol/E090/079/suppl-1.htm
#
#
#######

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
  