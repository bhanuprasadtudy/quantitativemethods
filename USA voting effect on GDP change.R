CO <- countyComplete[countyComplete[1]=='Colorado',]
nrow(CO)
case <- read.csv('~/Desktop/case01.csv')
case$percentvotes <- case$candidatevotes/case$totalvotes
head(case)
colorado <- case[case[2]=='Colorado',]
head(colorado)
florida <- case[case[2]=='Florida',]
iowa <- case[case[2]=='Iowa',]
colorado12 <- colorado[colorado[1]==2012,]
florida12 <- florida[florida[1]==2012,]
iowa12 <- iowa[iowa[1]==2012,]
colorado16 <- colorado[colorado[1]==2016,]
florida16 <- florida[florida[1]==2016,]
iowa16 <- iowa[iowa[1]==2016,]
colorado16$percentchange <- (colorado16$percentvotes-colorado12$percentvotes)/colorado12$percentvotes
iowa16$percentchange <- (iowa16$percentvotes-iowa12$percentvotes)/iowa12$percentvotes
florida16$percentchange <- (florida16$percentvotes-florida12$percentvotes)/florida12$percentvotes
florida16rep <- florida16[seq(2,nrow(florida16),3),]
nrow(florida16rep)
gdp <- read.csv("~/Desktop/lagdp1218.csv", stringsAsFactors = FALSE)
head(gdp)
flgdp <- (gdp[342:408,])
str(flgdp)
as.numeric(gsub(',','',flgdp[,2]))
as.numeric(gsub(',','',flgdp[,5]))

florida16rep$gdpchange <- (as.numeric(gsub(',','',flgdp[,5]))-as.numeric(gsub(',','',flgdp[,2])))/as.numeric(gsub(',','',flgdp[,2]))
(as.numeric(gsub(',','',flgdp[,5]))-as.numeric(gsub(',','',flgdp[,2])))/as.numeric(gsub(',','',flgdp[,2]))

florida16rep

summary(lm(percentchange ~ gdpchange, data=florida16rep))

iowagdp <- (gdp[822:920,])
iowagdp
iowa16rep <- iowa16[seq(2,nrow(iowa16),3),]
iowa16rep$gdpchange <- (as.numeric(gsub(',','', iowagdp[,5]))-as.numeric(gsub(',','', iowagdp[,2])))/as.numeric(gsub(',','', iowagdp[,2]))

summary(lm(percentchange ~ gdpchange, data=iowa16rep))

cologdp <- (gdp[259:322,])
nrow(colo16rep)
colo16rep <- colorado16[seq(2,nrow(colorado16),3),]
colo16rep$gdpchange <- (as.numeric(gsub(',','', cologdp[,5]))-as.numeric(gsub(',','', cologdp[,2])))/as.numeric(gsub(',','', cologdp[,2]))

summary(lm(percentchange ~ gdpchange, data=colo16rep))

florida16dem <- florida16[seq(1,nrow(florida16),3),]
iowa16dem <- iowa16[seq(1,nrow(iowa16),3),]
colo16dem <- colorado16[seq(1,nrow(colorado16),3),]
florida16dem$gdpchange <- (as.numeric(gsub(',','',flgdp[,5]))-as.numeric(gsub(',','',flgdp[,2])))/as.numeric(gsub(',','',flgdp[,2]))
iowa16dem$gdpchange <- (as.numeric(gsub(',','', iowagdp[,5]))-as.numeric(gsub(',','', iowagdp[,2])))/as.numeric(gsub(',','', iowagdp[,2]))
colo16dem$gdpchange <- (as.numeric(gsub(',','', cologdp[,5]))-as.numeric(gsub(',','', cologdp[,2])))/as.numeric(gsub(',','', cologdp[,2]))

summary(lm(percentchange ~ gdpchange, data=florida16dem))
summary(lm(percentchange ~ gdpchange, data=iowa16dem))
summary(lm(percentchange ~ gdpchange, data=colo16dem))

censusone <- read.csv('~/Desktop/census2015.csv')
censustwo <- read.csv('~/Desktop/census2017.csv')

forreg <- cbind(censusone[2],censusone[3],(censustwo[37]-censusone[37])/censusone[37],(censustwo[8]-censusone[8])/censusone[8],(censustwo[34]-censusone[34])/censusone[34])
head(forreg)
coloforreg <- forreg[forreg[1]=='Colorado',]
iowaforreg <- forreg[forreg[1]=='Iowa',]
floridaforreg <- forreg[forreg[1]=='Florida',]

colofinal <- cbind(colo16rep,coloforreg)
iowafinal <- cbind(iowa16rep,iowaforreg)
floridafinal <- cbind(florida16rep,floridaforreg)

head(colofinal)

summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=colofinal))

summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=iowafinal))

summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=floridafinal))
