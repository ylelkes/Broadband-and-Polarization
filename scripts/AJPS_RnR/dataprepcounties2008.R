##providers by county!

##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##         								
##	NAES and Census Data Coding and Merging  	
##	Yphtach Lelkes						
## 	Last Edited: 11/05/14 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

## setwd
setwd("~/sharelatex/Broadband and Polarization/")
## Load ROW Info
bbindex <- read.csv("data/bbindex2.csv")
## Clean up state variable
bbindex$State <- gsub("_"," ",x = bbindex$State)
## fyi: Variable Total=ROW
bbindex$BB_Index = bbindex$Total
###################################
#   NAES Data 2008                #
###################################
# load data, restricted data and merge, lower names, add year variable
naes2008 <- foreign::read.spss('data/naes/naes2008/naes08-phone-nat-rcs-data-full.sav', to.data.frame=T)
naesres08 <- foreign::read.spss('data/naes/naes2008/NAES08-Phone National RCS Restricted Data.sav', to.data.frame=T,use.value.labels=F)
naes2008all <- merge(naes2008,naesres08,by="RKEY")
names(naes2008all) <- tolower(names(naes2008all))
naes2008all$year <- 2008



## load data on providers by zipcode
#zip08 <- read.csv("data/miner/providers_ziplevel2008.csv")
naes2008all$wfc06[naes2008all$wfc06>=99998]=NA
naes2008all$fipscode <- naes2008all$wfc05

#naeszip2008 <- merge(naes2008all,zip08,by.y="zipcode",by.x="wfc06",all.x=T)
# create variable for state abbreviations
#bbindex$stabb <- state.abb[match(bbindex$State,state.name)]
# merge ROW and NAES + Zipcode
#naesmerged2008 <- merge(naeszip2008,bbindex,by.x="state",by.y="stabb")
# rename fipscode variable


## data on providers at county level from miner
miner <- foreign::read.dta("data/miner/BBindexData.dta")
miner <- subset(miner,year==2008)
naesmerged2008 <- merge(naes2008all,miner,by="fipscode")

## Leader feeling stuff

##########################################################################
# Demographics                                                          #
##########################################################################
# PID includes leaners
naesmerged2008$pid <- car::recode(as.numeric(naesmerged2008$ma01_c),"1='Republican';2='Democrat';else=NA",as.factor=T)
naesmerged2008$pid[which(naesmerged2008$ma03_c=='Republican')]='Republican'
naesmerged2008$pid[which(naesmerged2008$ma03_c=='Democrat')]='Democrat'
naesmerged2008$strongpartisan <- car::recode(as.numeric(naesmerged2008$ma02_c),"1='Strong';else='Weak'",as.factor=T)

naesmerged2008$female <- naesmerged2008$wa01_c
naesmerged2008$wa02_c[naesmerged2008$wa02_c>110]=NA
naesmerged2008$agecut <- as.numeric(gtools::quantcut(naesmerged2008$wa02_c, q=seq(0,1,by=.25), na.rm=TRUE))
naesmerged2008$education <- relevel(car::recode(as.numeric(naesmerged2008$wa03_c),"1:3='HS or Less';4:6='Some College';7='College';8:9='More than college';else=NA",as.factor=T),ref='HS or Less')

naesmerged2008$income <- as.numeric(naesmerged2008$wa04_c)
naesmerged2008$income2 <- as.numeric(naesmerged2008$wa05_c)
naesmerged2008$income[which(is.na(naesmerged2008$income))] = naesmerged2008$income2[which(is.na(naesmerged2008$income))] 
naesmerged2008$income <- car::recode(as.numeric(naesmerged2008$income),"10:12=NA")
naesmerged2008$incomecut <- as.numeric(gtools::quantcut(naesmerged2008$income, q=seq(0,1,by=0.25), na.rm=TRUE))


naesmerged2008$employed <- car::recode(as.numeric(naesmerged2008$wb01_c),"1=1;else=0")
naesmerged2008$haschildren <- car::recode(as.numeric(naesmerged2008$wfa02_c),"0=0;else=1;999=NA")
naesmerged2008$married <- car::recode(as.numeric(naesmerged2008$wfa03_c),"1:2=1;else=0")
naesmerged2008$zipcode <- naesmerged2008$wfc06


naesmerged2008$mc1 <- car::recode(naesmerged2008$aam01_c,"998:999=NA")
naesmerged2008$mc2 <- car::recode(naesmerged2008$aam05_c,"998:999=NA")
naesmerged2008$mc3 <- car::recode(naesmerged2008$aam06_c,"998:999=NA")
naesmerged2008$mc4 <- car::recode(naesmerged2008$aam07_c,"998:999=NA")
naesmerged2008$mc5 <- car::recode(naesmerged2008$aam08_c,"998:999=NA")
naesmerged2008$mccainfeels <- rowMeans(with(naesmerged2008,cbind(mc1,mc2,mc3,mc4,mc5)),na.rm=T)/10

naesmerged2008$ob1 <- car::recode(naesmerged2008$abo01_c,"998:999=NA")
naesmerged2008$ob2 <- car::recode(naesmerged2008$abo05_c,"998:999=NA")
naesmerged2008$ob3 <- car::recode(naesmerged2008$abo06_c,"998:999=NA")
naesmerged2008$ob4 <- car::recode(naesmerged2008$abo07_c,"998:999=NA")
naesmerged2008$ob5 <- car::recode(naesmerged2008$abo08_c,"998:999=NA")
naesmerged2008$obamafeels <- rowMeans(with(naesmerged2008,cbind(ob1,ob2,ob3,ob4,ob5)),na.rm=T)/10

## Feelings towards in-party and feelings towards out-party
naesmerged2008$infeels <- NA
naesmerged2008$outfeels <- NA
naesmerged2008$infeels[which(naesmerged2008$pid=='Republican')] <- naesmerged2008$mccainfeels[which(naesmerged2008$pid=='Republican')]
naesmerged2008$infeels[which(naesmerged2008$pid=='Democrat')] <- naesmerged2008$obamafeels[which(naesmerged2008$pid=='Democrat')]

###
naesmerged2008$outfeels[which(naesmerged2008$pid=='Democrat')] <- naesmerged2008$mccainfeels[which(naesmerged2008$pid=='Democrat')]
naesmerged2008$outfeels[which(naesmerged2008$pid=='Republican')] <- naesmerged2008$obamafeels[which(naesmerged2008$pid=='Republican')]

#############################
poverty <- read.csv("data/census/acs20052009.csv")
poverty$fipscode <- as.numeric(substr(x = poverty$GEOID, start = 8, stop = 15))
############################
# Load County Level Density Data, Merge with NAES Data
############################


############################
#### Merge Census Data with NAES + Zipcode Data
############################
naes2008trimmed <- naesmerged2008[c("year.x","state","zipcode","BB_Index","pid","infeels","outfeels","female","agecut","education","income","incomecut","fipscode","providers")]

density <- read.csv("data/census//maps-county2011.csv")
naes2008trimmed$fipscode <- naes2008trimmed$fips
naes2008trimmed <- merge(naes2008trimmed,density,by="fipscode")
naes2008trimmed <- merge(poverty,naes2008trimmed,by="fipscode")  
### Create variabes that divide number of people with a trait in a county by the number of people in that county
naes2008trimmed$percentmale <- naes2008trimmed$B01001_002E/naes2008trimmed$B01001_001E
naes2008trimmed$percentblack <- naes2008trimmed$B02001_003E/naes2008trimmed$B01001_001E
naes2008trimmed$percenthispanic <- naes2008trimmed$B03001_003E/naes2008trimmed$B01001_001E
naes2008trimmed$percentasian <- naes2008trimmed$B02001_005E/naes2008trimmed$B01001_001E
naes2008trimmed$medianage <- naes2008trimmed$B01002_001E
naes2008trimmed$medincome <- naes2008trimmed$B19301_001E
naes2008trimmed$percentpoverty <-   naes2008trimmed$B17001_002E/naes2008trimmed$B01001_001E
naes2008trimmed$college <- (naes2008trimmed$B15003_022E+naes2008trimmed$B15003_023E+naes2008trimmed$B15003_024E+naes2008trimmed$B15003_025E)/naes2008trimmed$B15003_001E
naes2008trimmed$somecollege <- (naes2008trimmed$B15003_019E+naes2008trimmed$B15003_020E+naes2008trimmed$B15003_021E)/naes2008trimmed$B15003_001E
naes2008trimmed$highschool <- (naes2008trimmed$B15003_017E+naes2008trimmed$B15003_018E)/naes2008trimmed$B15003_001E
naes2008trimmed$education <- factor(naes2008trimmed$education,levels=c("HS or Less","Some College","College","More than college"))
save(naes2008trimmed,file="data/naes/naes2008trimmed.RData")

# libraries and source function
library(ivpack)
library(dplyr)
source("~/func.R")

# load naes data

# load elevation data
elevationdata <- read.delim("data/land_elevation/elevationdata.txt")

# load crosswalk to merge tract to zip
xwalk <- read.csv("data/census/ZIP_TRACT_032010.csv")
# merge cross walk and aggregate to zipcode level
eldata <- tbl_df(merge(elevationdata,xwalk,by.x="X2000.Tract",by.y="TRACT"))
eldata1 <- group_by(eldata,ZIP)
eldataavg <- eldata %>% group_by(ZIP) %>%   summarise(meanel=mean(ELEV.AVG))
# merge naes with elevation data
naesiwthel <- merge(naes2008trimmed,eldataavg,by.y="ZIP",by.x="zipcode")
# mean elevation (in kilometers)
naesiwthel$meanel <- naesiwthel$meanel/1000
naesiwthel$meanel2 <- naesiwthel$meanel^2

slope <- foreign::read.dta("data/land_elevation/zctasloperoads shared publicly.dta")
naesiwthel <- merge(naesiwthel,slope,by.x="zipcode",by.y="zcta5")
