rm(list=ls())
library(dplyr)
library(plyr)
library(plm)
library(pglm)
library(polycor)
library(stargazer)
library(lmtest)

##########
# Data importation
##########

setwd("/home/jpp/Desktop/Researchers/Romain/resultsJune4th")
#setwd("M:/R/Uganda/2011_12/output")

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


##########
# Deflated prices
##########

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
# Create Panel
#######################

Pans1 <- plm.data(df2, c("HHID", "Year"))

Pans1$HeadAgricultureDecision <- as.factor(Pans1$HeadAgricultureDecision)

write.csv(Pans1, "GretlPans1.csv")



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



#######################
# Run regressions - Technical regression comparison
#######################


# FVS
######


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

stargazer(fcsols1, fcswithin1, type="text", out="fcsPLM_OLS_WITHIN.txt")



# DDS
######


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

stargazer(ddsols1, ddswithin1, type="text", out="ddsPLM_OLS_WITHIN.txt")



### Calories
#############

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

stargazer(CaloriesbyHHols1, CaloriesbyHHwithin1, type="text", out="caloriesPLM_OLS_WITHIN.txt")


# Time effect estimation
#########################


summary(fixef(fcswithin1, effect='time'))
summary(fixef(ddswithin1, effect='time'))
summary(fixef(CaloriesbyHHwithin1, effect='time'))




#fixef(Reg1, "individual")


#############
# TEST - To dertermine what is the better technique
#############


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


# HAUSMAN - Choose between Fixed or Random effect
##################################################

phtest(fcsfixed, fcsrandom)
phtest(ddsfixed, ddsrandom)
phtest(caloriesfixed, caloriesrandom)



# F-TEST - testing time-fixed effect
#####################################

pFtest(fcsfixedtime, fcsfixed)
pFtest(ddsfixedtime, ddsfixed)
pFtest(caloriesfixedtime, caloriesfixed)



# Lagrange Multiplier test - (Breusch-Pagan) - testing time-fixed effect
###########################################################################

plmtest(fcsfixed, c('time'), type=('bp'))
plmtest(ddsfixed, c('time'), type=('bp'))
plmtest(caloriesfixed, c('time'), type=('bp'))



# ARELLANO - Test heteroskedasticity and serial correlation
############################################################

coeftest(fcsfixed, vcovHC(fcsfixed, method = 'arellano'))
coeftest(ddsfixed, vcovHC(ddsfixed, method = 'arellano'))
coeftest(caloriesfixed, vcovHC(caloriesfixed, method = 'arellano'))


##########################
# Test of other variables
##########################


# Subdivided incomes
#####################

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
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")

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
                 #+ Region
                 , data=Pans1,
                 effect = "twoways",
                 model="within")


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
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")


stargazer(fcsolssub, ddsolssub, caloriesolssub, type="text", out="summary_sub_incomes_plm.txt")

summary(fixef(fcsolssub, effect='time'))
summary(fixef(ddsolssub, effect='time'))
summary(fixef(caloriesolssub, effect='time'))





# Different farm production estimation
#######################################

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
                 #+ Region
                 , data=Pans1,
               effect = "twoways",
               model="within")

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
                 #+ Region
                 , data=Pans1,
               effect = "twoways",
               model="within")


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
                      #+ Region
                      , data=Pans1,
                    effect = "twoways",
                    model="within")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_time_crops.txt")

summary(fixef(fcsols1, effect='time'))
summary(fixef(ddsols1, effect='time'))
summary(fixef(caloriesols1, effect='time'))





## Crop count variable - nutritional group
###########################################


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
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")

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
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")


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
                   # + Region
                    , data=Pans1,
                    effect = "twoways",
                    model="within")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_time_crops_group.txt")

summary(fixef(fcsols1, effect='time'))
summary(fixef(ddsols1, effect='time'))
summary(fixef(caloriesols1, effect='time'))





## Crop count variable - Simpson
##################################



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
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")

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
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")


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
                   # + Region
                    , data=Pans1,
                    effect = "twoways",
                    model="within")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_time_simpson.txt")

summary(fixef(fcsols1, effect='time'))
summary(fixef(ddsols1, effect='time'))
summary(fixef(caloriesols1, effect='time'))



# Non linear variables
##################################



fcsols1 <- plm(FCS ~
                 Numberofdiffererentcropsproducedbythehousehold
               + Householdsize
               + Sexofhouseholdhead
               + Ageofthehouseholdhead^2
               + Educationlevelofthehouseholdhead
               + Foodexpenditure^2
               + Nonfoodexpenditure^2
               + Incomes^2
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")

ddsols1 <- plm(DDS ~
                 Numberofdiffererentcropsproducedbythehousehold
               + Householdsize
               + Sexofhouseholdhead^2
               + Ageofthehouseholdhead
               + Educationlevelofthehouseholdhead
               + Foodexpenditure^2
               + Nonfoodexpenditure^2
               + Incomes^2
               + Totalcroppedarea
               + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
               + Numberofdifferentnonagriculturalincomesources
               + HeadAgricultureDecision
               #+ Region
               , data=Pans1,
               effect = "twoways",
               model="within")


caloriesols1 <- plm(CaloriesbyHH ~
                      Numberofdiffererentcropsproducedbythehousehold
                    + Householdsize
                    + Sexofhouseholdhead^2
                    + Ageofthehouseholdhead
                    + Educationlevelofthehouseholdhead
                    + Foodexpenditure^2
                    + Nonfoodexpenditure^2
                    + Incomes^2
                    + Totalcroppedarea
                    + Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction
                    + Numberofdifferentnonagriculturalincomesources
                    + HeadAgricultureDecision
                    # + Region
                    , data=Pans1,
                    effect = "twoways",
                    model="within")


stargazer(fcsols1, ddsols1, caloriesols1, type="text", out="summary_time_non-linear.txt")

summary(fixef(fcsols1, effect='time'))
summary(fixef(ddsols1, effect='time'))
summary(fixef(caloriesols1, effect='time'))



# WHAT HAPPEN ?
################


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

