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

df=df[,c("region", "unemployment_2008")]
unemployment2008  <- df

