# Anthropometric variable evaluation
library(dplyr)
library(readstata13)
library(foreign)


#Import reference data from WHO
setwd("M:/R/Uganda")
ref <- read.csv2('survey.csv', header=TRUE, sep=',')
ref <- select(ref, agemons, WEIGHT, HEIGHT)
ref$agemons <- as.numeric(as.character(ref$agemons))
ref$WEIGHT <- as.numeric(as.character(ref$WEIGHT))
ref$HEIGHT <- as.numeric(as.character(ref$HEIGHT))
ref$agemons <-  round(ref$agemons, 0)
ref$WEIGHT <-  round(ref$WEIGHT, 1)
ref$HEIGHT <-  round(ref$HEIGHT, 0)
colnames(ref) <- c('age', 'weight', 'height')
refb <- ref

#Import observed data in the country
setwd("M:/R/Uganda/2009_10/Data")
obs2009 <- read.dta('GSEC6.dta')
obs2009$h6q28a <- ifelse(is.na(obs2009$h6q28a), obs2009$h6q28b , obs2009$h6q28a)
obs2009 <- select(obs2009, HHID, PID, h6q04, h6q27, h6q28a)
colnames(obs2009) <- c('HHID', 'PID', 'age', 'weight', 'height')
obs2009 <- filter(obs2009, obs2009$height<130)
obs2009 <- filter(obs2009, obs2009$height>40)
                  
setwd("M:/R/Uganda/2010_11/Data")
obs2010 <- read.dta('GSEC6.dta')
obs2010$h6q28a <- ifelse(is.na(obs2010$h6q28a), obs2010$h6q28b , obs2010$h6q28a)
obs2010 <- select(obs2010, HHID, PID, h6q4, h6q27, h6q28a)
colnames(obs2010) <- c('HHID', 'PID', 'age', 'weight', 'height')
obs2010 <- filter(obs2010, obs2010$height<130)
obs2010 <- filter(obs2010, obs2010$height>40)
obs2010 <- filter(obs2010, obs2010$weight>2)
obs2010 <- filter(obs2010, obs2010$age<60)

setwd("M:/R/Uganda/2011_12/Data")
obs2012 <- read.dta('GSEC6A.dta')
obs2012$h6q28a <- ifelse(is.na(obs2012$h6q28a), obs2012$h6q28b , obs2012$h6q28a)
obs2012 <- select(obs2012, HHID, PID, h6q4, h6q27, h6q28a)
colnames(obs2012) <- c('HHID', 'PID', 'age', 'weight', 'height')
obs2012 <- filter(obs2012, obs2012$age<60)
obs2012 <- filter(obs2012, obs2012$height<130)
obs2012 <- filter(obs2012, obs2012$height>40)
obs2012 <- filter(obs2012, obs2012$weight>2)
obs2012 <- filter(obs2012, obs2012$weight<40)

#For WHZ
obs2009b <- select(obs2009, HHID, PID, height, weight)
obs2010b <- select(obs2010, HHID, PID, height, weight)
obs2012b <- select(obs2012, HHID, PID, height, weight)
obs2009b$weight <-  round(obs2009b$weight, 1)
obs2009b$height <-  round(obs2009b$height, 0)
obs2010b$weight <-  round(obs2010b$weight, 1)
obs2010b$height <-  round(obs2010b$height, 0)
obs2012b$weight <-  round(obs2012b$weight, 1)
obs2012b$height <-  round(obs2012b$height, 0)


#Variable mean for the reference, by age / height
ref <- ddply(ref, 'age', mutate, medianweight=median(weight, na.rm=TRUE))
ref <- ddply(ref, 'age', mutate, medianheight=median(height, na.rm=TRUE))
refb <- ddply(refb, 'height', mutate, medianweight=median(weight, na.rm=TRUE))


#Standart deviation of reference population
ref <- ddply(ref, 'age', mutate, sdheight=sd(height, na.rm=TRUE))
ref <- ddply(ref, 'age', mutate, sdweight=sd(weight, na.rm=TRUE))
refb <- ddply(refb, 'height', mutate, sdHW=sd(weight, na.rm=TRUE))

#join the ref values - by age
ref <- select(ref, age,	medianweight,	medianheight,	sdheight,	sdweight)

obs2009 <- left_join(obs2009, ref, by='age')
obs2009 <- unique(obs2009)
colnames(obs2009) <- c('HHID',  
                       'PID',	
                       'age',	
                       'weight',	
                       'height',	
                       'medianweight',	
                       'medianheight',
                       'sdheight',	
                       'sdweight'
                       )

obs2010 <- left_join(obs2010, ref, by='age')
obs2010 <- unique(obs2010)
colnames(obs2010) <- c('HHID',  
                       'PID',  
                       'age',	
                       'weight',	
                       'height',	
                       'medianweight',	
                       'medianheight',
                       'sdheight',	
                       'sdweight'
                       )

obs2012 <- left_join(obs2012, ref, by='age')
obs2012 <- unique(obs2012)
colnames(obs2012) <- c('HHID',  
                       'PID',  
                       'age',	
                       'weight',	
                       'height',	
                       'medianweight',	
                       'medianheight',
                       'sdheight',	
                       'sdweight'
                       )

#join the ref values - by height
refb <- select(refb, height, medianweight, sdHW)

obs2009b <- left_join(obs2009b, refb, by='height')
obs2009b <- unique(obs2009b)
colnames(obs2009b) <- c('HHID',  
                       'PID',	
                       'height',	
                       'weight',	
                       'medianweight',
                       'sdweight'
)

obs2010b <- left_join(obs2010b, refb, by='height')
obs2010b <- unique(obs2010b)
colnames(obs2010b) <- c('HHID',  
                       'PID',
                       'height',	
                       'weight',	
                       'medianweight',	
                       'sdweight'
)

obs2012b <- left_join(obs2012b, refb, by='height')
obs2012b <- unique(obs2012b)
colnames(obs2012b) <- c('HHID',  
                       'PID',	
                       'height',	
                       'weight',	
                       'medianweight',	
                       'sdweight'
)

obs2009$age <-  round(obs2009$age, 0)
obs2009$weight <-  round(obs2009$weight, 1)
obs2009$height <-  round(obs2009$height, 0)
obs2010$age <-  round(obs2010$age, 0)
obs2010$weight <-  round(obs2010$weight, 1)
obs2010$height <-  round(obs2010$height, 0)
obs2012$age <-  round(obs2012$age, 0)
obs2012$weight <-  round(obs2012$weight, 1)
obs2012$height <-  round(obs2012$height, 0)


obs2009b$weight <-  round(obs2009b$weight, 1)
obs2009b$height <-  round(obs2009b$height, 0)
obs2010b$weight <-  round(obs2010b$weight, 1)
obs2010b$height <-  round(obs2010b$height, 0)
obs2012b$weight <-  round(obs2012b$weight, 1)
obs2012b$height <-  round(obs2012b$height, 0)


#HAZ score calculation
obs2009 <- ddply(obs2009, 'age', mutate, HAZ=(height-medianheight)/sdheight)
obs2009 <- ddply(obs2009, 'age', mutate, meanHAZ=mean(HAZ, na.rm=TRUE))

obs2010 <- ddply(obs2010, 'age', mutate, HAZ=(height-medianheight)/sdheight)
obs2010 <- ddply(obs2010, 'age', mutate, meanHAZ=mean(HAZ, na.rm=TRUE))

obs2012 <- ddply(obs2012, 'age', mutate, HAZ=(height-medianheight)/sdheight)
obs2012 <- ddply(obs2012, 'age', mutate, meanHAZ=mean(HAZ, na.rm=TRUE))

#WAZ score calculation
obs2009 <- ddply(obs2009, 'age', mutate, WAZ=(weight-medianweight)/sdweight)
obs2009 <- ddply(obs2009, 'age', mutate, meanWAZ=mean(WAZ, na.rm=TRUE))

obs2010 <- ddply(obs2010, 'age', mutate, WAZ=(weight-medianweight)/sdweight)
obs2010 <- ddply(obs2010, 'age', mutate, meanWAZ=mean(WAZ, na.rm=TRUE))

obs2012 <- ddply(obs2012, 'age', mutate, WAZ=(weight-medianweight)/sdweight)
obs2012 <- ddply(obs2012, 'age', mutate, meanWAZ=mean(WAZ, na.rm=TRUE))


#WHZ score calculation
obs2009b <- ddply(obs2009b, 'height', mutate, WHZ=(weight-medianweight)/sdweight)
obs2009b <- ddply(obs2009b, 'height', mutate, meanWHZ=mean(WHZ, na.rm=TRUE))

obs2010b <- ddply(obs2010b, 'height', mutate, WHZ=(weight-medianweight)/sdweight)
obs2010b <- ddply(obs2010b, 'height', mutate, meanWHZ=mean(WHZ, na.rm=TRUE))

obs2012b <- ddply(obs2012b, 'height', mutate, WHZ=(weight-medianweight)/sdweight)
obs2012b <- ddply(obs2012b, 'height', mutate, meanWHZ=mean(WHZ, na.rm=TRUE))


#Mean by year
mean(obs2009$HAZ, na.rm=TRUE)
mean(obs2010$HAZ, na.rm=TRUE)
mean(obs2012$HAZ, na.rm=TRUE)

mean(obs2009$WAZ, na.rm=TRUE)
mean(obs2010$WAZ, na.rm=TRUE)
mean(obs2012$WAZ, na.rm=TRUE)

mean(obs2009b$WHZ, na.rm=TRUE)
mean(obs2010b$WHZ, na.rm=TRUE)
mean(obs2012b$WHZ, na.rm=TRUE)

#Standart deviation by year
sd(obs2009$HAZ, na.rm=TRUE)
sd(obs2010$HAZ, na.rm=TRUE)
sd(obs2012$HAZ, na.rm=TRUE)

sd(obs2009$WAZ, na.rm=TRUE)
sd(obs2010$WAZ, na.rm=TRUE)
sd(obs2012$WAZ, na.rm=TRUE)

sd(obs2009b$WHZ, na.rm=TRUE)
sd(obs2010b$WHZ, na.rm=TRUE)
sd(obs2012b$WHZ, na.rm=TRUE)

#HAZ by HHID
obs2009 <- ddply(obs2009, 'HHID', mutate, HAZHHID=mean(HAZ, na.rm=TRUE))
obs2010 <- ddply(obs2010, 'HHID', mutate, HAZHHID=mean(HAZ, na.rm=TRUE))
obs2012 <- ddply(obs2012, 'HHID', mutate, HAZHHID=mean(HAZ, na.rm=TRUE))

obs2009 <- ddply(obs2009, 'HHID', mutate, WAZHHID=mean(WAZ, na.rm=TRUE))
obs2010 <- ddply(obs2010, 'HHID', mutate, WAZHHID=mean(WAZ, na.rm=TRUE))
obs2012 <- ddply(obs2012, 'HHID', mutate, WAZHHID=mean(WAZ, na.rm=TRUE))

obs2009b <- ddply(obs2009b, 'HHID', mutate, WHZHHID=mean(WHZ, na.rm=TRUE))
obs2010b <- ddply(obs2010b, 'HHID', mutate, WHZHHID=mean(WHZ, na.rm=TRUE))
obs2012b <- ddply(obs2012b, 'HHID', mutate, WHZHHID=mean(WHZ, na.rm=TRUE))

obs2009b <- select(obs2009b, HHID, WHZHHID)
obs2010b <- select(obs2010b, HHID, WHZHHID)
obs2012b <- select(obs2012b, HHID, WHZHHID)

obs2009 <- left_join(obs2009, obs2009b, by='HHID')
obs2010 <- left_join(obs2010, obs2010b, by='HHID')
obs2012 <- left_join(obs2012, obs2012b, by='HHID')


#Year
obs2009[, 'Year'] <- 2009
obs2010[, 'Year'] <- 2010
obs2012[, 'Year'] <- 2012

#selection variable for csv
HAZ2009 <- select(obs2009, HHID, HAZHHID, WAZHHID, WHZHHID)
HAZ2010 <- select(obs2010, HHID, HAZHHID, WAZHHID, WHZHHID)
HAZ2012 <- select(obs2012, HHID, HAZHHID, WAZHHID, WHZHHID)

HAZ2009 <- unique(HAZ2009)
HAZ2010 <- unique(HAZ2010)
HAZ2012 <- unique(HAZ2012)

#csv construction
#haz <- rbind(HAZ2009, HAZ2010)
#haz <- rbind(haz, HAZ2012)
#haz<- filter(haz, HAZHHID != 'NaN')
#haz<- unique(haz)
#write.csv(haz, 'M:\\R\\Uganda\\2011_12\\output\\haz.csv')


#Plot construction
#library(ggplot2)

#Plot construction HAZ
#p <- ggplot(data=obs2009, aes(x=age, y=HAZ))
#p <- p + stat_smooth()
#print(p)

#p <- ggplot(data=obs2010, aes(x=age, y=HAZ))
#p <- p + stat_smooth()
#print(p)

#p <- ggplot(data=obs2012, aes(x=age, y=HAZ))
#p <- p + stat_smooth()
#print(p)

#Plot construction WAZ
#p <- ggplot(data=obs2009, aes(x=age, y=WAZ))
#p <- p + stat_smooth()
#print(p)

#p <- ggplot(data=obs2010, aes(x=age, y=WAZ))
#p <- p + stat_smooth()
#print(p)

#p <- ggplot(data=obs2012, aes(x=age, y=WAZ))
#p <- p + stat_smooth()
#print(p)

#Plot construction WHZ
#p <- ggplot(data=obs2009, aes(x=age, y=WHZ))
#p <- p + stat_smooth()
#print(p)

#p <- ggplot(data=obs2010, aes(x=age, y=WHZ))
#p <- p + stat_smooth()
#print(p)

#p <- ggplot(data=obs2012, aes(x=age, y=WHZ))
#p <- p + stat_smooth()
#print(p)

