source("~/Dropbox/func.R")
library(AER)
library(stargazer)
library(ivpack)
naesall$education <- car::recode(as.numeric(naesall$education),"1=3;2=1;3=4;4=3;5=2")
table(naesall$education)
nodems <-  ivreg(zero1(affpol)~providers+as.factor(year)|lbb+slope+as.factor(year),data=subset(naesall))
nodemsrobust <- cluster.robust.se(ivmodel = nodems,clusterid = naesall$Staten)

countycontrol <- ivreg(zero1(affpol)~providers+as.factor(year)+percentmale+percentblack+percentwhite+percenthispanic+percentasian+log(Median_income)+Median_age+college+somecollege+log(popdensity2000)+percentpoverty+as.factor(year)|lbb+slope+as.factor(year)+percentmale+percentblack+percentwhite+percenthispanic+percentasian+log(Median_income)+Median_age+college+somecollege+log(popdensity2000)+percentpoverty,data=subset(naesall))

countycontrolsrobust <- cluster.robust.se(ivmodel = countycontrol,clusterid = naesall$Staten)

######
indcontrol <- ivreg(zero1(affpol)~providers+as.factor(year)+percentmale+percentblack+percentwhite+percenthispanic+percentasian+log(Median_income)+Median_age+college+somecollege+log(popdensity2000)+percentpoverty+agecut+education+incomecut+female|lbb+slope+as.factor(year)+percentmale+percentblack+percentwhite+percenthispanic+percentasian+log(Median_income)+Median_age+college+somecollege+log(popdensity2000)+percentpoverty+agecut+education+incomecut+female,data=subset(naesall))
indcontrolrobust <- cluster.robust.se(ivmodel = indcontrol,clusterid = naesall$Staten)

stargazer(nodems,countycontrol,indcontrol,se=list(nodemsrobust[,2],countycontrolsrobust[,2],indcontrolrobust[,2]),covariate.labels=c("\\# of Providers","Year","\\% Male","\\% Black", "\\% White","\\% Hispanic", "\\% Asian", "Median Income (logged)","Median Age","\\% College Degree","\\% Some College","Population Density (logged)","\\% Poverty","Age, 2nd Quartile","Age, 3rd Quartile","Age, 4th Quartile","Age, Missing","Education, Some College","Education, College Degree","Education, Missing","Income, 2nd Quartile","Income, 3rd Quartile","Income, 4th Quartile","Income, Missing","Female","Intercept"), digits=2,multicolumn=T,keep.stat=c("N"),column.labels  = c("No Controls","County Controls","Individal Controls"),dep.var.labels.include = F,no.space=T,float=F,star.cutoffs = c(.05,.01,.001),out="~/Dropbox/sharelatex/Broadband and Polarization/tabs/rnr/ivests.tex")
