rm(list=ls())
library(dplyr)
library(plyr)
library(plm)
library(pglm)
library(polycor)
library(stargazer)
library(lmtest)

#setwd("/home/jpp/Desktop/Researchers/Romain/resultsJune4th")
setwd("M:/R/Uganda/2011_12/output")

dt1 <- read.csv("df_balanced.csv", header=TRUE)
head(dt1)

endexg1 <- c("Numberofdiffererentcropsproducedbythehousehold",
          "Totalcroppedarea",
          "Householdsize",
          "Sexofhouseholdhead",
          "Ageofthehouseholdhead",
          "Educationlevelofthehouseholdhead",
          "Foodexpenditure",
          "Nonfoodexpenditure",
          "Incomes",
          "Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction",
          "HeadAgricultureDecision",
          "Region",
          "Numberofdifferentnonagriculturalincomesources",   
          "CaloriesbyHH",
          "DDS",
          "FCS",
          "Agriculturalincomes",
          "NonAgriculturalincomes",
          "Propertyincomes",
          "Investments",
          "Transfers",
          "Numberofhouseholdproductionbynutritionalgroup",
          "Simpsonsindex",
          "Year",
          "HHID")

### Keep only subset of data

endexg2 <- dt1[,names(dt1) %in% endexg1]

### Only data for HHs in all three years
sp1 <- split(endexg2, endexg2$Year)
hh <- list()
hh[[1]] <- sp1[[1]]$HHID
hh[[2]] <- sp1[[2]]$HHID
hh[[3]] <- sp1[[3]]$HHID

keep <- Reduce(intersect,  hh)
df2 <- endexg2[which(endexg2$HHID %in% keep),]


### Recode Kampala to Central
df2$Region <- ifelse(df2$Region == 'Kampala', 'Central', df2$Region)
df2$Region <- ifelse(df2$Region == '1', 'Central', df2$Region)
df2$Region <- ifelse(df2$Region == '2', 'Eastern', df2$Region)
df2$Region <- ifelse(df2$Region == '4', 'Northern', df2$Region)
df2$Region <- ifelse(df2$Region == '5', 'Western', df2$Region)

table(df2$Year,df2$Region)

df2$Region <- as.factor(df2$Region)


########
### Deflated prices
########

df2$Incomes <- ifelse(df2$Year=='2010', df2$Incomes*100/118.6929, df2$Incomes)
df2$Incomes <- ifelse(df2$Year=='2012', df2$Incomes*100/135.329, df2$Incomes)

df2$Foodexpenditure <- ifelse(df2$Year=='2010', df2$Foodexpenditure*100/118.6929, df2$Foodexpenditure)
df2$Foodexpenditure <- ifelse(df2$Year=='2012', df2$Foodexpenditure*100/135.329, df2$Foodexpenditure)

df2$Nonfoodexpenditure <- ifelse(df2$Year=='2010', df2$Nonfoodexpenditure*100/118.6929, df2$Nonfoodexpenditure)
df2$Nonfoodexpenditure <- ifelse(df2$Year=='2012', df2$Nonfoodexpenditure*100/135.329, df2$Nonfoodexpenditure)

df2$Agriculturalincomes <- ifelse(df2$Year=='2009', (df2$Agriculturalincomes*100/118.6929)/1000, df2$Agriculturalincomes)
df2$Agriculturalincomes <- ifelse(df2$Year=='2010', (df2$Agriculturalincomes*100/118.6929)/1000, df2$Agriculturalincomes)
df2$Agriculturalincomes <- ifelse(df2$Year=='2012', (df2$Agriculturalincomes*100/135.329)/1000, df2$Agriculturalincomes)

df2$NonAgriculturalincomes <- ifelse(df2$Year=='2009', (df2$NonAgriculturalincomes*100/118.6929)/1000, df2$NonAgriculturalincomes)
df2$NonAgriculturalincomes <- ifelse(df2$Year=='2010', (df2$NonAgriculturalincomes*100/118.6929)/1000, df2$NonAgriculturalincomes)
df2$NonAgriculturalincomes <- ifelse(df2$Year=='2012', (df2$NonAgriculturalincomes*100/135.329)/1000, df2$NonAgriculturalincomes)

df2$Propertyincomes <- ifelse(df2$Year=='2009', (df2$Propertyincomes*100/118.6929)/1000, df2$Propertyincomes)
df2$Propertyincomes <- ifelse(df2$Year=='2010', (df2$Propertyincomes*100/118.6929)/1000, df2$Propertyincomes)
df2$Propertyincomes <- ifelse(df2$Year=='2012', (df2$Propertyincomes*100/135.329)/1000, df2$Propertyincomes)

df2$Investments <- ifelse(df2$Year=='2009', (df2$Investments*100/118.6929)/1000, df2$Investments)
df2$Investments <- ifelse(df2$Year=='2010', (df2$Investments*100/118.6929)/1000, df2$Investments)
df2$Investments <- ifelse(df2$Year=='2012', (df2$Investments*100/135.329)/1000, df2$Investments)

df2$Transfers <- ifelse(df2$Year=='2009', (df2$Transfers*100/118.6929)/1000, df2$Transfers)
df2$Transfers <- ifelse(df2$Year=='2010', (df2$Transfers*100/118.6929)/1000, df2$Transfers)
df2$Transfers <- ifelse(df2$Year=='2012', (df2$Transfers*100/135.329)/1000, df2$Transfers)


#######################
# Create Panel, run regressions
#######################

Pans1 <- plm.data(df2, c("HHID", "Year"))

Pans1$HeadAgricultureDecision <- as.factor(Pans1$HeadAgricultureDecision)


fcsols1 <- plm(FCS ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              + Region
              , data=Pans1,
              #index=c('HHID', 'Year'),
              #effect = "twoways",
              model="pooling")

summary(fcsols1)

fcswithin1 <- plm(FCS ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              #+ Region
              , data=Pans1,
              effect = "twoways",
              model="within")

summary(fcswithin1)

fcsplmPoisson1 <- pglm(FCS ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              + Region
              , data=Pans1,
              , family = poisson,         
              ,  method = "bfgs",          
              model="within")

summary(fcsplmPoisson1)
print(fcsplmPoisson1)

stargazer(fcsols1, fcswithin1, type="text", out="fcsPLM_OLS_WITHIN.txt")


####################
# DDS
####################


ddsols1 <- plm(DDS ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              + Region
              , data=Pans1,
              #effect = "twoways",
              model="pooling")

plmtest(ddsols1, type=c('bp'))

summary(ddsols1)

ddswithin1 <- plm(DDS ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              #+ Region
              , data=Pans1,
              effect = "twoways",
              model="within")

summary(ddswithin1)

ddsplmPoisson1 <- pglm(DDS ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              + Region
              , data=Pans1,
              #, effect = "twoways"         
              , family = poisson,         
              ,  method = "bfgs",          
              model="within")

summary(ddsplmPoisson1)

stargazer(ddsols1, ddswithin1, type="text", out="ddsPLM_OLS_WITHIN.txt")


###############################
### Calories
###############################

CaloriesbyHHols1 <- plm(CaloriesbyHH ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              + Region
              , data=Pans1,
              #effect = "twoways",
              model="pooling")

plmtest(CaloriesbyHHols1, type=c('bp'))

summary(CaloriesbyHHols1)

CaloriesbyHHwithin1 <- plm(CaloriesbyHH ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              #+ Region
              , data=Pans1,
              effect = "twoways",
              model="within")

summary(CaloriesbyHHwithin1)


CaloriesbyHHplmPoisson1 <- pglm(CaloriesbyHH ~
                Numberofdiffererentcropsproducedbythehousehold
              + Householdsize
              + Sexofhouseholdhead
              + Ageofthehouseholdhead
              + Educationlevelofthehouseholdhead
              + Foodexpenditure
              + Nonfoodexpenditure
              + Incomes
              + Totalcroppedarea
              + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
              + Numberofdifferentnonagriculturalincomesources
              + HeadAgricultureDecision
              + Region
              , data=Pans1,
              , family = poisson,         
              ,  method = "bfgs",          
              model="within")

summary(CaloriesbyHHplmPoisson1)

stargazer(CaloriesbyHHols1, CaloriesbyHHwithin1, type="text", out="caloriesPLM_OLS_WITHIN.txt")
#summary(CaloriesbyHHplmPoisson1)


#fixef(Reg1, "time")
#fixef(Reg1, "individual")


#sapply(Pans1, function(x) class(x))
#sapply(Pans1, function(x) summary(x))

tb1 <- Pans1[,c("Region", "Sexofhouseholdhead", "HeadAgricultureDecision")]
tb1 <- Pans1[,c("Region", "HeadAgricultureDecision")]
tb1 <- Pans1[,c("Region", "Sexofhouseholdhead")]
tb1 <- Pans1[,c("Region", "Sexofhouseholdhead")]
tb1 <- Pans1[,c("Region", "Numberofdifferentnonagriculturalincomesources")] 
table(tb1)


endexg1 <- c("Numberofdiffererentcropsproducedbythehousehold",
          "Totalcroppedarea",
          "Householdsize",
          "Sexofhouseholdhead",
          "Ageofthehouseholdhead",
          "Educationlevelofthehouseholdhead",
          "Foodexpenditure",
          "Nonfoodexpenditure",
          "Incomes",
          "Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction",
          "HeadAgricultureDecision",
          "Region",
          "Numberofdifferentnonagriculturalincomesources",   
          "CaloriesbyHH",
          "DDS",
          "FCS",    
          "Year",
          "HHID")

Pans2 <- Pans1[,c("Region", "Sexofhouseholdhead","HeadAgricultureDecision", "Numberofdifferentnonagriculturalincomesources")]
tb1 <- function(reg, x){table(reg, x)}
reg1 <- Pans2$Region
sapply(Pans2, tb1, reg=reg1)

table(Pans1$Year, Pans1$Region)


write.csv(Pans1$Region, "tmp.csv")



#################
#### Romain adds
#################

#############
# TEST
#############

####
# HAUSMAN
####

fcsfixed <- plm(FCS ~
                    Numberofdiffererentcropsproducedbythehousehold
                  + Householdsize
                  + Sexofhouseholdhead
                  + Ageofthehouseholdhead
                  + Educationlevelofthehouseholdhead
                  + Foodexpenditure
                  + Nonfoodexpenditure
                  + Incomes
                  + Totalcroppedarea
                  + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                  + Numberofdifferentnonagriculturalincomesources
                  + HeadAgricultureDecision
                  + Region
                  , data=Pans1,
                  index=c('HHID', 'Year'),
                  model="within")

fcsfixedtime <- plm(FCS ~
                  Numberofdiffererentcropsproducedbythehousehold
                + Householdsize
                + Sexofhouseholdhead
                + Ageofthehouseholdhead
                + Educationlevelofthehouseholdhead
                + Foodexpenditure
                + Nonfoodexpenditure
                + Incomes
                + Totalcroppedarea
                + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                + Numberofdifferentnonagriculturalincomesources
                + HeadAgricultureDecision
                + Region
                + factor(Year)
                , data=Pans1,
                index=c('HHID', 'Year'),
                model="within")


fcsrandom <- plm(FCS ~
                    Numberofdiffererentcropsproducedbythehousehold
                  + Householdsize
                  + Sexofhouseholdhead
                  + Ageofthehouseholdhead
                  + Educationlevelofthehouseholdhead
                  + Foodexpenditure
                  + Nonfoodexpenditure
                  + Incomes
                  + Totalcroppedarea
                  + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                  + Numberofdifferentnonagriculturalincomesources
                  + HeadAgricultureDecision
                  + Region
                  , data=Pans1,
                  index=c('HHID', 'Year'),
                  model="random")


ddsfixed <- plm(DDS ~
                    Numberofdiffererentcropsproducedbythehousehold
                  + Householdsize
                  + Sexofhouseholdhead
                  + Ageofthehouseholdhead
                  + Educationlevelofthehouseholdhead
                  + Foodexpenditure
                  + Nonfoodexpenditure
                  + Incomes
                  + Totalcroppedarea
                  + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                  + Numberofdifferentnonagriculturalincomesources
                  + HeadAgricultureDecision
                  + Region
                  , data=Pans1,
                  index=c('HHID', 'Year'),
                  model="within")

ddsfixedtime <- plm(DDS ~
                  Numberofdiffererentcropsproducedbythehousehold
                + Householdsize
                + Sexofhouseholdhead
                + Ageofthehouseholdhead
                + Educationlevelofthehouseholdhead
                + Foodexpenditure
                + Nonfoodexpenditure
                + Incomes
                + Totalcroppedarea
                + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                + Numberofdifferentnonagriculturalincomesources
                + HeadAgricultureDecision
                + Region
                + factor(Year)
                , data=Pans1,
                index=c('HHID', 'Year'),
                model="within")

ddsrandom <- plm(DDS ~
                    Numberofdiffererentcropsproducedbythehousehold
                  + Householdsize
                  + Sexofhouseholdhead
                  + Ageofthehouseholdhead
                  + Educationlevelofthehouseholdhead
                  + Foodexpenditure
                  + Nonfoodexpenditure
                  + Incomes
                  + Totalcroppedarea
                  + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                  + Numberofdifferentnonagriculturalincomesources
                  + HeadAgricultureDecision
                  + Region
                  , data=Pans1,
                  index=c('HHID', 'Year'),
                  model="random")


caloriesfixed <- plm(CaloriesbyHH ~
                             Numberofdiffererentcropsproducedbythehousehold
                           + Householdsize
                           + Sexofhouseholdhead
                           + Ageofthehouseholdhead
                           + Educationlevelofthehouseholdhead
                           + Foodexpenditure
                           + Nonfoodexpenditure
                           + Incomes
                           + Totalcroppedarea
                           + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                           + Numberofdifferentnonagriculturalincomesources
                           + HeadAgricultureDecision
                           + Region
                           , data=Pans1,
                           index=c('HHID', 'Year'),
                           model="within")


caloriesfixedtime <- plm(CaloriesbyHH ~
                       Numberofdiffererentcropsproducedbythehousehold
                     + Householdsize
                     + Sexofhouseholdhead
                     + Ageofthehouseholdhead
                     + Educationlevelofthehouseholdhead
                     + Foodexpenditure
                     + Nonfoodexpenditure
                     + Incomes
                     + Totalcroppedarea
                     + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                     + Numberofdifferentnonagriculturalincomesources
                     + HeadAgricultureDecision
                     + Region
                     + factor(Year)
                     , data=Pans1,
                     index=c('HHID', 'Year'),
                     model="within")

caloriesrandom <- plm(CaloriesbyHH ~
                             Numberofdiffererentcropsproducedbythehousehold
                           + Householdsize
                           + Sexofhouseholdhead
                           + Ageofthehouseholdhead
                           + Educationlevelofthehouseholdhead
                           + Foodexpenditure
                           + Nonfoodexpenditure
                           + Incomes
                           + Totalcroppedarea
                           + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                           + Numberofdifferentnonagriculturalincomesources
                           + HeadAgricultureDecision
                           + Region
                           , data=Pans1,
                           index=c('HHID', 'Year'),
                           model="random")

phtest(fcsfixed, fcsrandom)
phtest(ddsfixed, ddsrandom)
phtest(caloriesfixed, caloriesrandom)


####
# ARELLANO
####

coeftest(fcsfixed, vcovHC(fcsfixed, method = 'arellano'))
coeftest(ddsfixed, vcovHC(ddsfixed, method = 'arellano'))
coeftest(caloriesfixed, vcovHC(caloriesfixed, method = 'arellano'))

####
# F-TEST
####

pFtest(fcsfixedtime, fcsfixed)
pFtest(ddsfixedtime, ddsfixed)
pFtest(caloriesfixedtime, caloriesfixed)


####
# Lagrange Multiplier test - (Breusch-Pagan)
####

plmtest(fcsfixed, c('time'), type=('bp'))
plmtest(ddsfixed, c('time'), type=('bp'))
plmtest(caloriesfixed, c('time'), type=('bp'))





########
# DATA Summary
########



library(dplyr)

s2009 <- filter(Pans1, Year==2009)
s2010 <- filter(Pans1, Year==2010)
s2012 <- filter(Pans1, Year==2012)

stargazer(s2009, s2010, s2012, type="text", out="summary_split_year.txt")

east <- filter(Pans1, Region=='Eastern')
west <- filter(Pans1, Region=='Western')
noth <- filter(Pans1, Region=='Northern')
cent <- filter(Pans1, Region=='Central')

stargazer(east, west, noth, cent, type="text", out="summary_split_region.txt")


##########
# Subdivided incomes
##########

fcsolssub <- plm(FCS ~
                 Numberofdiffererentcropsproducedbythehousehold
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure
               + Nonfoodexpenditure
               + Agriculturalincomes
               + NonAgriculturalincomes
               + Propertyincomes
               + Investments
               + Transfers
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               + Region
               , data=Pans1,
               model="pooling")

ddsolssub <- plm(DDS ~
                   Numberofdiffererentcropsproducedbythehousehold
                 + Householdsize
                 + Sexofhouseholdhead
                 + Ageofthehouseholdhead
                 + Educationlevelofthehouseholdhead
                 + Foodexpenditure
                 + Nonfoodexpenditure
                 + Agriculturalincomes
                 + NonAgriculturalincomes
                 + Propertyincomes
                 + Investments
                 + Transfers
                 + Totalcroppedarea
                 + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                 + Numberofdifferentnonagriculturalincomesources
                 + HeadAgricultureDecision
                 + Region
                 , data=Pans1,
                 model="pooling")


caloriesolssub <- plm(CaloriesbyHH ~
                 Numberofdiffererentcropsproducedbythehousehold
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure
               + Nonfoodexpenditure
               + Agriculturalincomes
               + NonAgriculturalincomes
               + Propertyincomes
               + Investments
               + Transfers
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               + Region
               , data=Pans1,
               model="pooling")


stargazer(fcsolssub, ddsolssub, caloriesolssub, type="text", out="summary_sub_incomes_ols.txt")


##########
# Different farm production estimation
##########

fcsols1 <- plm(FCS ~
                   Numberofdiffererentcropsproducedbythehousehold
                 + Householdsize
                 + Sexofhouseholdhead
                 + Ageofthehouseholdhead
                 + Educationlevelofthehouseholdhead
                 + Foodexpenditure
                 + Nonfoodexpenditure
                 + Incomes
                 + Totalcroppedarea
                 + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                 + Numberofdifferentnonagriculturalincomesources
                 + HeadAgricultureDecision
                 + Region
                 , data=Pans1,
                 model="pooling")

ddsols1 <- plm(DDS ~
                   Numberofdiffererentcropsproducedbythehousehold
                 + Householdsize
                 + Sexofhouseholdhead
                 + Ageofthehouseholdhead
                 + Educationlevelofthehouseholdhead
                 + Foodexpenditure
                 + Nonfoodexpenditure
                 + Incomes
                 + Totalcroppedarea
                 + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                 + Numberofdifferentnonagriculturalincomesources
                 + HeadAgricultureDecision
                 + Region
                 , data=Pans1,
                 model="pooling")


caloriesols1 <- plm(CaloriesbyHH ~
                        Numberofdiffererentcropsproducedbythehousehold
                      + Householdsize
                      + Sexofhouseholdhead
                      + Ageofthehouseholdhead
                      + Educationlevelofthehouseholdhead
                      + Foodexpenditure
                      + Nonfoodexpenditure
                      + Incomes
                      + Totalcroppedarea
                      + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                      + Numberofdifferentnonagriculturalincomesources
                      + HeadAgricultureDecision
                      + Region
                      , data=Pans1,
                      model="pooling")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_ols_crops.txt")

fcsols1 <- plm(FCS ~
                 Numberofhouseholdproductionbynutritionalgroup
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure
               + Nonfoodexpenditure
               + Incomes
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               + Region
               , data=Pans1,
               model="pooling")

ddsols1 <- plm(DDS ~
                 Numberofhouseholdproductionbynutritionalgroup
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure
               + Nonfoodexpenditure
               + Incomes
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               + Region
               , data=Pans1,
               model="pooling")


caloriesols1 <- plm(CaloriesbyHH ~
                      Numberofhouseholdproductionbynutritionalgroup
                    + Householdsize
                    + Sexofhouseholdhead
                    + Ageofthehouseholdhead
                    + Educationlevelofthehouseholdhead
                    + Foodexpenditure
                    + Nonfoodexpenditure
                    + Incomes
                    + Totalcroppedarea
                    + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                    + Numberofdifferentnonagriculturalincomesources
                    + HeadAgricultureDecision
                    + Region
                    , data=Pans1,
                    model="pooling")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_ols_crops_group.txt")

fcsols1 <- plm(FCS ~
                 Simpsonsindex
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure
               + Nonfoodexpenditure
               + Incomes
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               + Region
               , data=Pans1,
               model="pooling")

ddsols1 <- plm(DDS ~
                 Simpsonsindex
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure
               + Nonfoodexpenditure
               + Incomes
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               + Region
               , data=Pans1,
               model="pooling")


caloriesols1 <- plm(CaloriesbyHH ~
                      Simpsonsindex
                    + Householdsize
                    + Sexofhouseholdhead
                    + Ageofthehouseholdhead
                    + Educationlevelofthehouseholdhead
                    + Foodexpenditure
                    + Nonfoodexpenditure
                    + Incomes
                    + Totalcroppedarea
                    + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                    + Numberofdifferentnonagriculturalincomesources
                    + HeadAgricultureDecision
                    + Region
                    , data=Pans1,
                    model="pooling")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_ols_simpson.txt")



