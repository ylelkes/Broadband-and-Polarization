library(stargazer)

library(AER)
library(ivpack)
source("~/Dropbox/func.R")
county <-  ivreg(zero1(infeels-outfeels)~log(providers)+as.factor(year)+populationdensity+sexratio+medianage+percentwhite+percentblack+percenthispanic+unemployment+lowed+persistentpoverty+log(percapitaincome)|log(Total)+as.factor(year)+populationdensity+sexratio+medianage+percentwhite+percentblack+percenthispanic+unemployment+lowed+persistentpoverty+log(percapitaincome),data=na.omit(merged))

countycl <- cluster.robust.se(county,clusterid = na.omit(merged)$state)
countyyears <-  ivreg(zero1(infeels-outfeels)~log(providers)*as.factor(year)+populationdensity+sexratio+medianage+percentwhite+percentblack+percenthispanic+unemployment+lowed+persistentpoverty+log(percapitaincome)|log(Total)*as.factor(year)+populationdensity+sexratio+medianage+percentwhite+percentblack+percenthispanic+unemployment+lowed+persistentpoverty+log(percapitaincome),data=na.omit(merged))
countyyearcl <- cluster.robust.se(countyyears,clusterid = na.omit(merged)$state)


countyinterest <-  ivreg(zero1(infeels-outfeels)~log(providers)*(interest)+populationdensity+sexratio+medianage+percentwhite+percentblack+percenthispanic+unemployment+lowed+persistentpoverty+log(percapitaincome)|log(Total)*(interest)+populationdensity+sexratio+medianage+percentwhite+percentblack+percenthispanic+unemployment+lowed+persistentpoverty+log(percapitaincome),data=na.omit(merged))
countyinterestcl <- cluster.robust.se(countyinterest,clusterid = na.omit(merged)$state)


stargazer(county,countyyears,countyinterest,se=list(countycl[,2],countyyearcl[,2],countyinterestcl[,2]))
          
          ,covariate.labels=c("\\# of Providers","Year","\\% Male","\\% Black", "\\% White","\\% Hispanic", "\\% Asian", "Median Income (logged)","Median Age","\\% College Degree","\\% Some College","Population Density (logged)","\\% Poverty","Age, 2nd Quartile","Age, 3rd Quartile","Age, 4th Quartile","Age, Missing","Education, College Degree","Education, Some College","Education, Missing","Income, 2nd Quartile","Income, 3rd Quartile","Income, 4th Quartile","Income, Missing","Female","Intercept"), digits=2,multicolumn=T,keep.stat=c("N"),column.labels  = c("No Controls","County Controls","Individal Controls"),dep.var.labels.include = F,no.space=T,float=F,star.cutoffs = c(.06,.01,.001),out="~/Dropbox/sharelatex/Broadband and Polarization/tabs/rnr/ivests.tex")

