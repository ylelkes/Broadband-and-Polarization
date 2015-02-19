library(acs)
library(stringr)

us.county=geo.make(state="*", county="*")
#   Topic: B02001.  Race
acs <- list()
acs[['race']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B02001", col.names="pretty",endyear = 2010,span = 5)))[,c(1,2,3,4,5)]
acs[['sex']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B01001", col.names="pretty",endyear = 2010,span = 5)))[2]
acs[['median_age']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B01002", col.names="pretty",endyear = 2010,span = 5)))[1]
acs[['poverty_status']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B10059", col.names="pretty",endyear = 2010,span = 5)))[2]
acs[['median_income']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B19013", col.names="pretty",endyear = 2010,span = 5)))[1]
acs[['schooltotal']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B23006", col.names="pretty",endyear = 2010,span = 5)))[1]


acs[['lessthanhs']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B23006", col.names="pretty",endyear = 2010,span = 5)))[2]
acs[['hs']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B23006", col.names="pretty",endyear = 2010,span = 5)))[9]
acs[['somecollege']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B23006", col.names="pretty",endyear = 2010,span = 5)))[16]
acs[['college']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B23006", col.names="pretty",endyear = 2010,span = 5)))[23]
acs[['hispanic']] =data.frame(estimate(acs.fetch(geography=us.county,table.number="B03001", col.names="pretty",endyear = 2010,span = 5)))[c(3)]
acs[['children']] =  data.frame(estimate(acs.fetch(geography=us.county,table.number="B23007", col.names="pretty",endyear = 2010,span = 5)))[2]

demos <- data.frame(acs)


names(demos) <- c("Total","White","Black","Indian","Asian","Male","Median_age","Poverty_Status","Median_income","Total2","Less_than_hs","HS","Some_College","Bachelors","Hispanic","Children")
demos$countystate <- rownames(demos)
fips.county$Staten <- state.name[match(fips.county$State,state.abb)]
fips.county$countystate <- paste(fips.county$County.Name,fips.county$Staten,sep=", ")
fips.county$fipscode <- paste(str_pad(fips.county$State.ANSI,width = 2,side = "left",pad=0),str_pad(fips.county$County.ANSI,width = 3,side = "left",pad=0),sep="")
demos <- merge(demos,fips.county,by="countystate")
