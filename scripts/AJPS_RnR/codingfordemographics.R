require(stringr)
library(dplyr)
library(reshape2)
STR_PAD <- function(x) str_pad(x, width=11, side="left", pad="0")

density2000 <- read.csv("~/Dropbox/sharelatex/Broadband and Polarization/data/census/density2000.csv")
density2000$fipscode <- str_pad(density2000$fipscode,width = 5,side = "left",pad = "0" )
demos <- merge(with(density2000,data.frame(fipscode,popdensity2000)),demos,by="fipscode")
### Elevation
elevationdata <- read.delim("~/Dropbox/sharelatex/Broadband and Polarization/data/land_elevation/elevationdata.txt")

elevationdata$fipscode <- STR_PAD(elevationdata$X2000.Tract)
elevationdata$fipscode <- (substr(x = elevationdata$fipscode, start = 1, stop = 5))
eldataavg <- elevationdata %>% group_by(fipscode) %>%   summarise(meanel=mean(ELEV.AVG/1000))

demos <- merge(eldataavg,demos,by="fipscode")
miner <- foreign::read.dta("~/Dropbox/sharelatex/Broadband and Polarization/data/miner/BBindexData.dta")
miner$id <- miner$fipscode
out <- dcast(miner,fipscode+BB_Index~year,value.var = "providers")
names(out)[3:9] <- paste("providers_",names(out)[3:9],sep="")  
demos <- merge(out,demos,by="fipscode")


slope <- foreign::read.dta("land_elevation//zctasloperoads shared publicly.dta")
xwalk <- read.csv("census/countyzipcosswalk.txt",header = T)
xwalk$fipscode <-  paste(
  str_pad(xwalk$STATE, width=2, side="left", pad="0"),
  str_pad(xwalk$COUNTY, width=3, side="left", pad="0"),
  sep="")
xwalk$zcta5 <- (xwalk$ZCTA5)
slope <- merge(xwalk,slope,by="zcta5")
sloped <- slope %>% group_by(fipscode) %>%   summarise(slope=mean(slope))
demos <- merge(demos,sloped,by="fipscode")
unique(demos$State)

source("http://iangow.me/~igow/code/cluster2.R")
coeftest.cluster(data = naesall,fm = )
demos$percentwhite <- demos$White/demos$Total
demos$percentblack <- demos$Black/demos$Total
demos$percentindian <- demos$Indian/demos$Total
demos$percentasian <- demos$Asian/demos$Total
demos$percentmale <- demos$Male/demos$Total
demos$percentpoverty <- demos$Poverty_Status/demos$Total
demos$college <- demos$Bachelors/demos$Total2
demos$percenthispanic <- demos$Hispanic/demos$Total

demos$somecollege <- demos$Some_College/demos$Total2
demos$highschool <- demos$HS/demos$Total2
demos$lessthanhs <- demos$Less_than_hs/demos$Total2
demos$lessthanhs <- demos$Less_than_hs/demos$Total2

demos$percentwithkids <- demos$Children/demos$Total



df = read.fwf("census/unemployment04.txt", 
              widths=diff(c(0,16,21,29,80,86,100,115,125,132)+1), 
              skip=6,
              colClasses="character")
df = df[1:(nrow(df)-2),] # last two lines are whitespace plus date

#choroplethr needs column named "region", which is numeric county FIPS
df$V2 = str_trim(df$V2) # state fips
df$V3 = str_trim(df$V3) # county fips
df$region=paste0(df$V2,df$V3)
df$region=as.numeric(df$region)

#choroplethr needs one column named "value"
df$unemployment_2004=str_trim(df$V9)
df$unemployment_2004=as.numeric(df$unemployment_2004)

df=df[,c("region", "unemployment_2004")]
unemployment2004  <- df
df = read.fwf("census/unemployment08.txt", 
              widths=diff(c(0,16,21,29,80,86,100,115,125,132)+1), 
              skip=6,
              colClasses="character")
df = df[1:(nrow(df)-2),] # last two lines are whitespace plus date

#choroplethr needs column named "region", which is numeric county FIPS
df$V2 = str_trim(df$V2) # state fips
df$V3 = str_trim(df$V3) # county fips
df$region=paste0(df$V2,df$V3)
df$region=as.numeric(df$region)

#choroplethr needs one column named "value"
df$unemployment_2008=str_trim(df$V9)
df$unemployment_2008=as.numeric(df$unemployment_2008)

unemployment2008=df[,c("region", "unemployment_2008")]

demos$fipscode <- as.numeric(demos$fipscode)


demos <- merge(demos,unemployment,by.x="fipscode",by.y="region")
save(demos,file="~/Dropbox/sharelatex/Broadband and Polarization/data/demographics.RData")
load("data/demographics.RData")
load("naesall_0408.RData")

zip08 <- read.csv("data/miner/providers_ziplevel2008.csv")
naes2008all$wfc06[naesall_0408$>=99998]=NA
naeszip2008 <- merge(naes2008all,zip08,by.y="zipcode",by.x="wfc06",all.x=T)
naesall <- merge(naesall_0408,demos,by="fipscode")

naesall$providers <- NA
naesall$providers[which(naesall$year==2004)]=naesall$providers_2004[which(naesall$year==2004)]
naesall$providers[which(naesall$year==2008)]=naesall$providers_2008[which(naesall$year==2008)]
naesall$infeels
summary(lm(infeels-outfeels~log(BB_Index)+slope,naesall))
naesall$unemployment <- NA
naesall$unemployment[which(naesall$year==2004)]=naesall$unemployment_2004[which(naesall$year==2004)]
naesall$unemployment[which(naesall$year==2008)]=naesall$unemployment_2008[which(naesall$year==2008)]

naesall$incomecut <- as.factor(car::recode(naesall$incomecut,"NA=5"))
naesall$education <- as.character(naesall$education)
naesall$education[is.na(naesall$education)]='Missing'
naesall$agecut <- as.factor(car::recode(naesall$agecut,"NA='5'"))
table(is.na(naesall_0408$education))
naesall <- na.omit(with(naesall,data.frame(affpol=infeels-outfeels,providers_2004,providers_2008,percentmale,percentwhite,percentblack,percentasian,percentpoverty,Median_income,Median_age,college,somecollege,highschool,lessthanhs,popdensity2000,lbb=log(BB_Index),slope,year,Staten,Hispanic, Bachelors, Some_College, HS, Less_than_hs,fipscode,percenthispanic,percentwithkids,percentasian,unemployment,providers,meanel,agecut,incomecut,education,female)))


