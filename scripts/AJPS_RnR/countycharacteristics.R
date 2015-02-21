require(stringr)
library(dplyr)
library(reshape2)
STR_PAD <- function(x) str_pad(x, width=11, side="left", pad="0")

###
slope <- foreign::read.dta("data/land_elevation//zctasloperoads shared publicly.dta")
xwalk <- read.csv("data/census/countyzipcosswalk.txt",header = T)
xwalk$fipscode <-  paste(
  str_pad(xwalk$STATE, width=2, side="left", pad="0"),
  str_pad(xwalk$COUNTY, width=3, side="left", pad="0"),
  sep="")
xwalk$zcta5 <- (xwalk$ZCTA5)
slope <- merge(xwalk,slope,by="zcta5")
sloped <- slope %>% group_by(fipscode) %>%   summarise(slope=mean(slope))
sloped$fipscode <- as.numeric(sloped$fipscode)


#### County Characteristics
counties <- foreign::read.dta("~/Dropbox/sharelatex/Broadband and Polarization/data/census/county_characteristics.dta")
counties$fipscode <- counties$FIPS
counties$populationdensity <- log(counties$Pop05/(counties$LandArea+counties$WaterArea))
counties$sexratio <- counties$SexRatio05
counties$medianage <- counties$MedianAge05
counties$percentwhite <- counties$PctWhite05
counties$percentblack <- counties$PctBlack05
counties$percenthispanic <- counties$PctH05
counties$unemployment <- counties$UnempRate05
counties$lowed <- counties$LowEduc04
counties$persistentpoverty <- counties$PerstPov04
counties$percapitaincome <- counties$CA05N0030_05
counties$typography <- counties$Typography
counties$typographyz <- counties$TypographyZ
counties$state <- counties$StateName
counties <- merge(counties,sloped,by="fipscode")
######
xwalk <- read.csv("data/census/ZIP_TRACT_032010.csv")
p2004 <- read.csv("data/miner/providers_ziplevel2004.csv")
p2008 <- read.csv("data/miner/providers_ziplevel2008.csv")
providers <- rbind(data.frame(p2004[,1:3],year=2004),data.frame(p2008[,1:3],year=2008))
providers <- merge(xwalk,providers,by.x="ZIP",by.y="zipcode")

providers$TRACT <- STR_PAD(providers$TRACT)
providers$fipscode <- as.numeric(substr(x = providers$TRACT, start = 1, stop = 5))
providers <- providers %>% group_by(fipscode,year) %>%   summarise(providers=mean(zipproviders,na.rm=T))
names(counties)
cchar <- counties[,c(1,471:485)]
countyproviders <- merge(providers,cchar,by="fipscode")
load("naesall_0408.RData")
bbindex2 <- read.csv("data/bbindex2.csv")
bbindex2$state <- gsub("_"," ",bbindex2$State)
#bbindex2$state <- state.abb[match(bbindex2$state,state.name)]
naesall_0408$year

merged <- (merge(countyproviders,bbindex2,by="state"))
merged <- merge(merged,naesall_0408,by=c("fipscode","year"))
save(merged,file="data/dataforrnr.RData")
