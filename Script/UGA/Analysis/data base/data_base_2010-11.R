##########################
##################
##################
##################
#### DATA BASE CONSTRUCTION -- 2010-2011
##################
##################
##################
##########################

library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(stargazer)
library(leaps)



#Household size - OK
hh <- as.data.frame(table(HHID = as.character(GSEC2$HHID)), unique(GSEC2$HHID))
hh$HHID <- as.character(hh$HHID)


#Number of children under or equal to 5 years old - OK
age <- select(GSEC2, HHID, h2q8)
age2 <- filter(age, h2q8 >= 0)
age5 <- filter(age2, h2q8 <= 5)
age5 <- as.data.frame(table(HHID = as.character(age5$HHID)), unique(age5$HHID))
age5$HHID <- as.character(age5$HHID)


#Dependency ratio 
age <- select(GSEC2, HHID, h2q8)
dep <- filter(age, h2q8 < 15 | h2q8 > 65)
age <- filter(age, h2q8 > 15 | h2q8 < 65)
dep <- as.data.frame(table(HHID = factor(dep$HHID)), unique(dep$HHID))
colnames(dep) <- c('HHID', 'agedep')
age <- as.data.frame(table(HHID = factor(age$HHID)), unique(age$HHID))
dep1<- left_join(age, dep, by='HHID')
dep1$agedep <- ifelse(is.na(dep1$agedep), 0, dep1$agedep)
depratio <- transmute(dep1, depratio=agedep/Freq)
depratio1 <- round(depratio,2)
depratio2 <- bind_cols(as.data.frame(dep1$HHID),as.data.frame(depratio1$depratio))
colnames(depratio2) <- c('HHID', 'depratio')
depratio2$HHID <- as.character(depratio2$HHID)


#Money from abroad - OK
trab <- select(GSEC11, HHID, h11q2, h11q4, h11q7)
trab <- filter(trab, h11q2 == 'Remittances and assistance received from abroad')
trab <- filter(trab, h11q4 == 'Yes')
trab <- select(trab, HHID, h11q4)
trab$HHID <- as.character(trab$HHID)


#FVS - DON'T USE
#div <- select(GSEC15B, HHID, itmcd)
#div2 <- filter(div, itmcd != 'Other Tobacco', itmcd != 'Tobacco', itmcd != 'Cigarettes')
#div3<- as.data.frame(table(HHID = factor(div2$HHID)), unique(div2$HHID))
#div3$HHID <- as.character(div3$HHID)


#Number of differerent crops produced by the household - OK
#Diff with 2011-2012 = not immature question
#Select colones
s1 <- select(AGSEC5A, HHID, cropID, a5aq6a)
colnames(s1) <- c('HHID', 'cropID', 'qharv')
s2 <- select(AGSEC5B, HHID, cropID, a5bq6a) 
colnames(s2) <- c('HHID', 'cropID', 'qharv')
#Enlever les immature et mature dont qtt = 0
s11 <- filter(s1, qharv != 0)
s22 <- filter(s2, qharv != 0)
s11 <- select(s11, HHID, cropID)
s22 <- select(s22, HHID, cropID)
#Merge
crop<-rbind(s11, s22)
#Occurence for each HHID
crop <- crop[order(crop$HHID),]
crp1 <- unique(crop)
crop1 <- as.data.frame(table(HHID = factor(crp1$HHID)), unique(crp1$HHID))
crop1 <- select (crop1, HHID, Freq)
colnames(crop1) <- c('HHID', 'divprod')
crop1$HHID <- as.character(crop1$HHID)


#Household Head sex, age & education level - OK
head <- select(GSEC2, HHID, PID, h2q3, h2q4, h2q8)
head$PID <- as.character(head$PID)
headed <- select(GSEC4, PID, h4q7)
headed$PID <- as.character(headed$PID)
head <- left_join(head, headed, by='PID' )
head <- filter(head, h2q4 == 'Head')
head$HHID <- as.character(head$HHID)
head1 <- select(head, HHID, h2q3, h2q8, h4q7)
df1 <- head1[!duplicated(head1$HHID), ]
df1$h4q7 <- as.numeric(as.character(df1$h4q7))
df1$h4q7 <- ifelse(df1$h4q7==99, is.na(df1$h4q7), df1$h4q7)
df1$h4q7 <- ifelse(df1$h4q7<10, is.na(df1$h4q7), df1$h4q7)



#Total cropped area - OK
#GPS measure and farmer estimation 
gpsarea <- select(AGSEC2A, HHID, a2aq4, a2aq5)
#Priority on GPS measure
gpsarea$a2aq4 <- ifelse(is.na(gpsarea$a2aq4), gpsarea$a2aq5, gpsarea$a2aq4)
gpsarea <- select(gpsarea, HHID, a2aq4)
colnames(gpsarea) <- c('HHID', 'area')
#gpsarea$area[is.na(gpsarea$area)] <- 0
gps2 <- unique(within(gpsarea, {
  area <- ave(area, HHID, FUN = sum)
}))
gps2$HHID <- as.character(gps2$HHID) #take in account GPS and estimation when the first is not available


#DDS / Sum of food items by nutritional group - OK
#h15bq3b = number day ITEM was consumed last week - but not used
dds  <- select(GSEC15B, HHID, itmcd, h15bq3b)
dds$itmcd <- gsub(" ","",dds$itmcd)
dds2 <- filter(dds, itmcd != 'Other Tobacco', itmcd != 'Tobacco', itmcd != 'Cigarettes')
colnames(match)<- c('itmcd', 'group', 'weight')
dds2 <- left_join(dds2, match, by='itmcd')
dds2 <- select(dds2, HHID, group)
dds2 <- unique(dds2)
dds2 <- filter(dds2, dds2$group!='Other')
dds2 <- as.data.frame(table(HHID = factor(dds2$HHID)), unique(dds2$HHID))
colnames(dds2) <- c('HHID', 'dds') 
dds2$HHID <- as.character(dds2$HHID)

#FCS / Sum of food items with a specific weight - OK
dds3 <- filter(dds, itmcd != 'Other Tobacco', itmcd != 'Tobacco', itmcd != 'Cigarettes')
colnames(match)<- c('itmcd', 'group', 'weight')
dds3 <- left_join(dds3, match, by='itmcd')
dds3 <- select(dds3, HHID, h15bq3b, group, weight)
dds3 <- ddply(dds3, c('HHID', 'group'), mutate, freq=sum(h15bq3b, na.rm=TRUE))
dds3 <- select(dds3, HHID, group, weight, freq)
dds3 <- unique(dds3)
dds4 <- filter(dds3, dds3$weight != 0)
dds4$weight <- as.numeric(levels(dds4[,3])[dds4[,3]])
dds4$freq <- as.numeric(dds4$freq)
dds4$freq <- ifelse(dds4$freq>7, 7, dds4$freq)
dds4 <- mutate(dds4, weight=weight*freq)
dds4 <- select(dds4, HHID, weight)
dds5 <- ddply(dds4, 'HHID', mutate, FCS=sum(weight, na.rm=TRUE))
FCS1 <- select(dds5, HHID, FCS)
FCS1<- unique(FCS1)
FCS1$HHID<- as.character(FCS1$HHID)


#Number of household production by nutritional group - OK
colnames(matchcrop) <- c('cropID', 'group', 'code')
matchcrop <- select(matchcrop, code, group)
colnames(matchcrop) <- c('cropID', 'group')
matchcrop$cropID <- as.character(matchcrop$cropID)
crop$cropID <- as.character(crop$cropID)
cropgp<- left_join(crop, matchcrop, by='cropID')
cropgp <- select(cropgp, HHID, group)
cropgp <- unique(cropgp)
cropgp <- as.data.frame(table(HHID = factor(cropgp$HHID)), unique(cropgp$HHID))
colnames(cropgp) <- c('HHID', 'prodgroup') 
cropgp$HHID <- as.character(cropgp$HHID)


#Food expenditure - OK
expend <- select(GSEC15B, HHID, itmcd, h15bq5, h15bq7)
expend <- ddply(expend, 'HHID', mutate, value = sum(h15bq5, h15bq7, na.rm=TRUE))
expend$itmcd <- gsub(" ","",expend$itmcd)
expend2 <- filter(expend, itmcd != 'Other Tobacco', itmcd != 'Tobacco', itmcd != 'Cigarettes')
expend2<- select(expend2, HHID, value)
expend3 <- ddply(expend2, 'HHID', mutate, foodexp = sum(value, na.rm=TRUE) )
expend3 <- select(expend3, HHID, foodexp)
colnames(expend3) <- c('HHID', 'foodexp')
expend3$foodexp <-  expend3$foodexp/1000
expend3 <- unique(expend3)
expend3$HHID <- as.character(expend3$HHID)





# Non-food expenditure - OK
nonexpenda <- select(GSEC15C, HHID, h15cq5)
colnames(nonexpenda) <- c('HHID', 'nonfoodexp') 
nonexpendb <- select(GSEC15D, HHID, h15dq3)
colnames(nonexpendb) <- c('HHID', 'nonfoodexp') 
nonexpende <- select(GSEC15E, HHID, h15eq3)
colnames(nonexpende) <- c('HHID', 'nonfoodexp')
nonexpendc <- filter(expend, itmcd == 'Other Tobacco' | itmcd == 'Tobacco' | itmcd == 'Cigarettes')
nonexpendc<- select(nonexpendc, HHID, h15bq5)
colnames(nonexpendc) <- c('HHID', 'nonfoodexp') 
nonexpend2 <- rbind(nonexpenda, nonexpendb, nonexpendc, nonexpende)
nonexpend2 <- filter(nonexpend2, nonexpend2$nonfoodexp > 0)
nonexpend2$nonfoodexp <- as.numeric(as.character(nonexpend2$nonfoodexp))
nonexpend3 <- ddply(nonexpend2, 'HHID', mutate, nonfood = sum(nonfoodexp, na.rm=TRUE))
nonexpend3 <- select(nonexpend3, HHID, nonfood)
colnames(nonexpend3) <- c('HHID', 'nonfoodexp') 
nonexpend3$nonfoodexp <-  nonexpend3$nonfoodexp/1000
nonexpend3 <- unique(nonexpend3)
nonexpend3$HHID <- as.character(nonexpend3$HHID)


#Total Incomes - OK
inc1  <- select(GSEC11, HHID, h11q5)
colnames(inc1) <- c('HHID', 'incomes')
inc2  <- select(GSEC11, HHID, h11q6)
colnames(inc2) <- c('HHID', 'incomes')
inc <- rbind(inc1, inc2)
inc <- filter(inc, inc$incomes > 0)
inc2 <- ddply(inc, 'HHID', mutate, incomes2 = sum(incomes, na.rm=TRUE))
inc2 <- select(inc2, HHID, incomes2)
colnames(inc2) <- c('HHID', 'incomes')
inc2$incomes <- inc2$incomes/1000
inc2 <- unique(inc2)
inc2$HHID <- as.character(inc2$HHID)


# Urban / Rural + Geo - OK
urb2 <- as.data.frame(paste(GSEC1$region , GSEC1$urban))
urb <- cbind(as.data.frame(GSEC1$HHID), urb2)
colnames(urb) <- c('HHID', 'location')
urb$HHID <- as.character(urb$HHID)

# Urban / Rural only - OK
urbrur <- select(GSEC1, HHID, urban)
urbrur$HHID <- as.character(urbrur$HHID)

#Geo only - OK 
geo <- select(GSEC1, HHID, region)
geo$HHID <- as.character(geo$HHID)



# Livestock - Cattle / Small animals / Poultry - OK
anim1 <- select(AGSEC6A, HHID, a6aq4)
anim1 <- filter(anim1, a6aq4=='Yes')
anim2 <- select(AGSEC6B, HHID, a6bq4)
anim2 <- filter(anim2, a6bq4=='Yes')
anim3 <- select(AGSEC6C, HHID, a6cq4)
anim3 <- filter(anim3, a6cq4=='Yes')
anim1 <- anim1[!duplicated(anim1$HHID), ]
anim2 <- anim2[!duplicated(anim2$HHID), ]
anim3 <- anim3[!duplicated(anim3$HHID), ]
colnames(anim1) <- c('HHID', 'cattle')
colnames(anim2) <- c('HHID', 'smallanim')
colnames(anim3) <- c('HHID', 'poultry')
anim1$HHID <- as.character(anim1$HHID)
anim2$HHID <- as.character(anim2$HHID)
anim3$HHID <- as.character(anim3$HHID)


#Income quartile - OK 
inc3 <- as.data.frame(inc2)
inc3['quantil'] <- NA
inc3$quantil <- with(inc3, cut(incomes, 
                               breaks=quantile(incomes, probs=seq(0,1, by=0.25)), 
                               include.lowest=TRUE,
                               labels=c("Q1","Q2","Q3","Q4")))
inc3 <- select(inc3, HHID, quantil)
inc3$HHID <- as.character(inc3$HHID)


#Distance from public transport point - OK
ptp <- select(GSEC18A, HHID, h18q7)
colnames(ptp) <- c('HHID', 'distpblctrnsp')
ptp$HHID <- as.character(ptp$HHID)


#Number of different non-agricultural income sources - OK
work <- select(GSEC8, HHID, h8q19b)
work <- filter(work,  work$h8q19b != 'ELEMENTARY OCCUPATIONS' & work$h8q19b != 'AGRICULTURAL AND FISHERY WORKERS')
work2 <- as.data.frame(table(HHID = factor(work$HHID)), unique(work$HHID))
colnames(work2) <- c('HHID', 'work')
work2$HHID <- as.character(work2$HHID)


#Proportion of food consumed in previous one week from households own production - OK
farmcons <- select(GSEC15B, HHID, itmcd, h15bq4, h15bq6, h15bq8)
farmcons$itmcd <- gsub(" ","",farmcons$itmcd)
farmcons <- filter(farmcons, itmcd != 'Other Tobacco' | itmcd != 'Tobacco' | itmcd != 'Cigarettes')
#Selection of items with positive qtt
farm <- select(farmcons, HHID, h15bq8)
farmcons <- filter(farmcons, h15bq4>0 | h15bq6>0 | h15bq8>0)
farm <- filter(farm, h15bq8>0)
farmcons <- as.data.frame(table(HHID = factor(farmcons$HHID)), unique(farmcons$HHID))
farm <- as.data.frame(table(HHID = factor(farm$HHID)), unique(farm$HHID))
farmcons2 <- left_join(farmcons, farm, by='HHID')
colnames(farmcons2) <- c('HHID', 'farmconsp', 'fromfarm')
farmcons2$fromfarm[is.na(farmcons2$fromfarm)] <- 0
farmcons2 <- transmute(farmcons2, farmconspsitem=fromfarm/farmconsp)
farmcons2 <- bind_cols(as.data.frame(farmcons$HHID),as.data.frame(farmcons2$farmconspsitem))
colnames(farmcons2) <- c('HHID', 'farmconspsitem')
farmcons2$HHID <- as.character(farmcons2$HHID)


#Simpson's index - OK
s1 <- select(AGSEC5A, HHID, pltid, cropID, a5aq6a)
s1$pltid <- paste(s1$HHID, s1$pltid)
s1$pltid <- gsub(" ","",s1$pltid)
colnames(s1) <- c('HHID', 'cropid', 'cropname',  'qharv')
s2 <- select(AGSEC5B, HHID, pltid, cropID, a5bq6a) 
s2$pltid <- paste(s2$HHID, s2$pltid)
s2$pltid <- gsub(" ","",s2$pltid)
colnames(s2) <- c('HHID', 'cropid', 'cropname', 'qharv')
#Delete immature and qtt = 0
s11 <- filter(s1, qharv != 0)
s22 <- filter(s2, qharv != 0)
s11 <- select(s11, HHID, cropname, cropid)
s22 <- select(s22, HHID, cropname, cropid)
#Merge
crop<-rbind(s11, s22)
crop <- crop[order(crop$HHID),]
crop$cropid <- as.character(crop$cropid)
#Area compute as first area section
gpsarea <- select(AGSEC2A, HHID, prcid, a2aq4, a2aq5)
gpsarea$a2aq4 <- ifelse(is.na(gpsarea$a2aq4), gpsarea$a2aq5, gpsarea$a2aq4)
gpsarea <- select(gpsarea, HHID, prcid, a2aq4)
gpsarea$prcid <- gsub("0","",gpsarea$prcid)
gpsarea$prcid <- paste(gpsarea$HHID, gpsarea$prcid)
gpsarea$prcid <- gsub(" ","",gpsarea$prcid)
gpsarea <- select(gpsarea,  prcid, a2aq4)
colnames(gpsarea) <- c('cropid', 'area')
#Simpson' s index computing
simpson <- left_join(crop, gpsarea, by='cropid')
simpson <- filter(simpson, area>0)
simpson <- select(simpson, HHID, cropname, area)
simpson2 <- ddply(simpson, c('HHID','cropname'), mutate, areabycrop=sum(area, na.rm=TRUE))
simpson2=select(simpson2, HHID, cropname, areabycrop)
simpson2<- unique(simpson2)
simpson2<- ddply(simpson2, 'HHID', mutate, tot=sum(areabycrop))
simpson <- ddply(simpson2, ' HHID', mutate, simpsonindexfvs=1-sum((areabycrop/tot)^2), na.rm=TRUE)
simpson <- select(simpson, HHID, simpsonindexfvs)
simpsonfvs <- unique(simpson)
simpsonfvs$HHID <- as.character(simpsonfvs$HHID)


# Calories Food Consumption - OK
GSEC15B <-read.dta('GSEC15B.dta', convert.factors = NA)
GSEC2 <-read.dta('GSEC2.dta')
unite <- read.csv2('units-2.csv', header=TRUE, sep=',')
nou <- read.csv2('numberofunits.csv', header=TRUE, sep=',')
calories <- read.csv2('itemskcal.csv', header=TRUE, sep=',')
fc  <- select(GSEC15B, HHID, itmcd, untcd, h15bq4, h15bq6, h15bq8, h15bq10)
fc$itmcd <- gsub(" ","",fc$itmcd)
fc$untcd <- gsub(" ","",fc$untcd)
fc$itmcd <- tolower(fc$itmcd)
fc$untcd <- tolower(fc$untcd)
unite$unit <- tolower(unite$unit)
calories$name <- tolower(calories$name)
calories$name <- gsub(" ","",calories$name)
fc <- ddply(fc, c('HHID','itmcd'), mutate, quantity=sum(h15bq4, h15bq6, h15bq8, h15bq10, na.rm=TRUE))
fc <- select(fc, HHID, itmcd, untcd, quantity)
colnames(fc) <- c('HHID', 'item', 'unit', 'quantity')
unite <- select(unite, unit,  kgquantity)
unite$unit <- as.character(unite$unit)
#Unit quantity computation
look <- left_join(fc, unite, by='unit')
look$kgquantity <- as.numeric(as.character(look$kgquantity))
look$kgquantity <- ifelse(look$item=='Freshmilk', (look$kgquantity*1.032), look$kgquantity)
look$kgquantity <- ifelse(look$item=='Cookingoil', (look$kgquantity*0.92), look$kgquantity)
look2 <- filter(look, unit=='NumberofUnits(General)')
colnames(nou) <- c('item', 'kg')
nou$item <- as.character(nou$item)
look2 <- left_join(look2, nou, by='item')
look2 <- select(look2, HHID,  item,  quantity,  kg)
look2<- unique(look2)
colnames(look2) <- c('HHID',  'item',  'quantity',  'kgquantity')
look3 <- filter(look, unit!='numberofunits')
look3 <- select(look3, HHID,  item,  quantity,	kgquantity)
look3$item <- as.character(look3$item)
look2$kgquantity <- as.numeric(as.character(look2$kgquantity))
look <- rbind(look2, look3)
look <- ddply(look, c('HHID','item'), mutate, qtt=quantity*kgquantity)
fc <- select(look, HHID, item, qtt)
colnames(fc) <- c('HHID', 'name', 'qtt')
calories$name <- as.character(calories$name)
#calories computation
fc <- left_join(fc, calories, by='name')
fc <- ddply(fc, c('HHID','name'), mutate, kcalitem=qtt*kcal)
fc <- ddply(fc, c('HHID'), mutate, kcalday=sum(kcalitem, na.rm=TRUE))
#hh <- as.data.frame(table(HHID = as.character(GSEC2$HHID)), unique(GSEC2$HHID))
#hh$HHID <- as.character(hh$HHID)
#fc <- left_join(fc, hh, by='HHID')
#fc <- ddply(fc, c('HHID'), mutate, kcalmember=kcalday/Freq)
kcalhhid <- unique(select(fc, HHID, kcalday))
kcalhhid$kcalday <- ifelse(kcalhhid$kcalday==0, 'NA', kcalhhid$kcalday)
kcalhhid$kcalday <- as.numeric(as.character(kcalhhid$kcalday))


# Calories Food Consumption from HH farm
GSEC15B <-read.dta('GSEC15B.dta', convert.factors = NA)
GSEC2 <-read.dta('GSEC2.dta')
unite <- read.csv2('units-2.csv', header=TRUE, sep=',')
nou <- read.csv2('numberofunits.csv', header=TRUE, sep=',')
calories <- read.csv2('itemskcal.csv', header=TRUE, sep=',')
fc  <- select(GSEC15B, HHID, itmcd, untcd, h15bq8)
fc <- filter(fc, h15bq8>0)
fc$itmcd <- gsub(" ","",fc$itmcd)
fc$untcd <- gsub(" ","",fc$untcd)
fc$itmcd <- tolower(fc$itmcd)
fc$untcd <- tolower(fc$untcd)
unite$unit <- tolower(unite$unit)
calories$name <- tolower(calories$name)
calories$name <- gsub(" ","",calories$name)
colnames(fc) <- c('HHID', 'item', 'unit', 'quantity')
unite <- select(unite, unit, kgquantity)
unite$unit <- as.character(unite$unit)
#Unit quantity computation
look <- left_join(fc, unite, by='unit')
look$kgquantity <- as.numeric(as.character(look$kgquantity))
look$kgquantity <- ifelse(look$item=='Freshmilk', (look$kgquantity*1.032), look$kgquantity)
look$kgquantity <- ifelse(look$item=='Cookingoil', (look$kgquantity*0.92), look$kgquantity)
look2 <- filter(look, unit=='NumberofUnits(General)')
colnames(nou) <- c('item', 'kg')
nou$item <- as.character(nou$item)
look2 <- left_join(look2, nou, by='item')
look2 <- select(look2, HHID,  item,  quantity,  kg)
look2<- unique(look2)
colnames(look2) <- c('HHID',  'item',  'quantity',  'kgquantity')
look3 <- filter(look, unit!='numberofunits')
look3 <- select(look3, HHID,  item,  quantity,	kgquantity)
look3$item <- as.character(look3$item)
look2$kgquantity <- as.numeric(as.character(look2$kgquantity))
look <- rbind(look2, look3)
look <- ddply(look, c('HHID','item'), mutate, qtt=quantity*kgquantity)
fc <- select(look, HHID, item, qtt)
colnames(calories) <- c('item', 'kcal')
calories$item <- as.character(calories$item)
#calories computation
fc <- left_join(fc, calories, by='item')
fc <- ddply(fc, c('HHID','item'), mutate, kcalitem=qtt*kcal)
fc <- ddply(fc, c('HHID'), mutate, kcalday=sum(kcalitem, na.rm=TRUE))
#hh <- as.data.frame(table(HHID = as.character(GSEC2$HHID)), unique(GSEC2$HHID))
#hh$HHID <- as.character(hh$HHID)
#fc <- left_join(fc, hh, by='HHID')
#fc <- ddply(fc, c('HHID'), mutate, kcalmember=kcalday/Freq)
kcalfarmhhid <- unique(select(fc, HHID, kcalday))
kcalfarmhhid$kcalday <- ifelse(kcalfarmhhid$kcalday==0, 'NA', kcalfarmhhid$kcalday)
kcalfarmhhid$kcalday <- as.numeric(as.character(kcalfarmhhid$kcalday))

# Calories Produced
AGSEC5A <-read.dta('AGSEC5A.dta')
AGSEC5B <-read.dta('AGSEC5B.dta')
cropkcal <- read.csv2('matchcropkcal.csv', header=TRUE, sep=',')
cropkcal <- select(cropkcal, code, kcal)
colnames(cropkcal) <- c('cropID', 'kcal')
#select colones
s1 <- select(AGSEC5A, HHID, cropID, a5aq6a, a5aq6d)
colnames(s1) <- c('HHID', 'cropID', 'qharv', 'conv')
s2 <- select(AGSEC5B, HHID, cropID, a5bq6a, a5bq6d) 
colnames(s2) <- c('HHID', 'cropID', 'qharv', 'conv')
#enlever les immature et mature dont qtt = 0
s11 <- filter(s1, qharv != 0)
s22 <- filter(s2, qharv != 0)
#merge
crop<-rbind(s11, s22)
crop <- left_join(crop, cropkcal, by='cropID')
crop <- filter(crop, qharv<99999)
crop <- ddply(crop, c('HHID', 'cropID'), mutate, kcalprod=qharv*conv*kcal)
crop <- ddply(crop, 'HHID', mutate, kcalprodHHID=sum(kcalprod, na.rm=TRUE))
caloriesprod <- select(crop, HHID, kcalprodHHID)
caloriesprod <- unique(caloriesprod)






#Data Frame construction
df2010 <- left_join(hh, age5, by='HHID')
df2010 <- left_join(df2010, depratio2, by='HHID')
df2010 <- left_join(df2010, trab, by = 'HHID')
df2010 <- left_join(df2010, FCS1, by = 'HHID')
df2010 <- left_join(df2010, crop1, by = 'HHID')
df2010 <- left_join(df2010, df1, by = 'HHID')
df2010 <- left_join(df2010, gps2, by = 'HHID')
df2010 <- left_join(df2010, dds2, by = 'HHID')
df2010 <- left_join(df2010, cropgp, by = 'HHID')
df2010 <- left_join(df2010, expend3, by = 'HHID')
df2010 <- left_join(df2010, nonexpend3, by = 'HHID')
df2010 <- left_join(df2010, inc2, by = 'HHID')
df2010 <- left_join(df2010, urb, by = 'HHID')
df2010 <- left_join(df2010, anim1, by = 'HHID')
df2010 <- left_join(df2010, anim2, by = 'HHID')
df2010 <- left_join(df2010, anim3, by = 'HHID')
df2010 <- left_join(df2010, urbrur, by = 'HHID')
df2010 <- left_join(df2010, geo, by = 'HHID')
df2010 <- left_join(df2010, inc3, by = 'HHID')
df2010 <- left_join(df2010, ptp, by = 'HHID')
df2010 <- left_join(df2010, work2, by = 'HHID')
df2010 <- left_join(df2010, farmcons2, by='HHID')
df2010 <- left_join(df2010, simpsonfvs, by='HHID')
df2010 <- left_join(df2010, HAZ2010, by='HHID')
df2010 <- left_join(df2010, kcalhhid, by='HHID')
df2010 <- left_join(df2010, kcalfarmhhid, by='HHID')
df2010 <- left_join(df2010, caloriesprod, by='HHID')




#Adjustments
#df2010$Freq.y <- as.numeric(df2010$Freq.y )
#df2010$Freq.y  <- ifelse(is.na(df2010$Freq.y ), 0, df2010$Freq.y )
#df2010$divprod <- as.numeric(df2010$divprod)
#df2010$divprod[is.na(df2010$divprod)] <- 0
#df2010$h11q4 <- ifelse(is.na(df2010$h11q4), 'No', 'Yes')
df2010$area <- as.numeric(as.character(df2010$area))
df2010$area[is.na(df2010$area)] <- 0
df2010$prodgroup <- ifelse(is.na(df2010$prodgroup), 0, df2010$prodgroup)
#df2010$foodexp <- ifelse(is.na(df2010$foodexp), 0, df2010$foodexp)
#df2010$nonfoodexp <- ifelse(is.na(df2010$nonfoodexp), 0, df2010$nonfoodexp)
#df2010$incomes <- ifelse(is.na(df2010$incomes), 0, df2010$incomes)
df2010$location <- as.factor(df2010$location)
df2010$cattle <- ifelse(is.na(df2010$cattle), 'No', df2010$cattle)
df2010$smallanim <- ifelse(is.na(df2010$smallanim), 'No', df2010$smallanim)
df2010$poultry <- ifelse(is.na(df2010$poultry), 'No', df2010$poultry)
df2010$cattle <- ifelse(df2010$cattle==1, 'Yes', df2010$cattle)
df2010$smallanim <- ifelse(df2010$smallanim==1, 'Yes', df2010$smallanim)
df2010$poultry <- ifelse(df2010$poultry==1, 'Yes', df2010$poultry)
df2010$cattle <- as.factor(df2010$cattle)
df2010$smallanim <- as.factor(df2010$smallanim)
df2010$poultry <- as.factor(df2010$poultry)
df2010$urban <- as.factor(df2010$urban)
df2010$region <- as.factor(df2010$region)
df2010$quantil <- as.factor(df2010$quantil)
df2010$work <- as.numeric(as.character(df2010$work))
df2010$work <- ifelse(is.na(df2010$work), 0, df2010$work)



colnames(df2010)<-c('HHID', 'Householdsize', 'Numberofchildrenunder5yearsold', 'Dependencyratio',
                'Moneyfromabroad', 'FCS', 'Numberofdiffererentcropsproducedbythehousehold',
                'Sexofhouseholdhead', 'Ageofthehouseholdhead', 'Educationlevelofthehouseholdhead',
                'Totalcroppedarea', 'DDS', 'Numberofhouseholdproductionbynutritionalgroup',
                'Foodexpenditure', 'Nonfoodexpenditure', 'Incomes', 'Location', 'LivestockCattle', 'LivestockSmallanimals',
                'LivestockPoultry', 'Urban', 'Region', 'Incomequartile', 'Distancefrompublictransportpoint', 
                'Numberofdifferentnonagriculturalincomesources', 
                'Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction', 'Simpsonsindex',
                'HAZ', 'WAZ', 'WHZ', 'CaloriesbyHH', 'CaloriesbyHHfromfarm', 'Caloriesproduced'
                )

df2010$DDS <- as.numeric(as.character(df2010$DDS))
df2010$Ageofthehouseholdhead <- as.numeric(as.character(df2010$Ageofthehouseholdhead))
df2010$Numberofdiffererentcropsproducedbythehousehold <- as.numeric(as.character(df2010$Numberofdiffererentcropsproducedbythehousehold))
df2010$Numberofchildrenunder5yearsold <- as.numeric(as.character(df2010$Numberofchildrenunder5yearsold))
df2010$Householdsize <- as.numeric(as.character(df2010$Householdsize))

df2010[, 'Year'] <- 2010

#export the df as .csv
write.csv(df2010, 'M:\\R\\Uganda\\2011_12\\output\\df2010.csv')

df2010 <- filter(df2010, Caloriesproduced<500000000)
stargazer(df2010, type="text", out="summary_df2010.txt")

