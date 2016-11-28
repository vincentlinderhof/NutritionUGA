# -------------------------------------
# Uganda data 2019-2010-2011 survey
# -------------------------------------
rm(list=ls())

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2011/Data" } 
if(Sys.info()["user"] == "linde069"){
    dataPath <- "D:/Analyses/CIMMYT/NutritionUGA"
    setwd("D:/Analyses/CIMMYT/NutritionUGA")
}

sink("Results/myfile.txt", append=TRUE, type="output", split=TRUE)



library(plyr)
library(dplyr)
#install.packages("plm")
library(plm)

#install.packages("pglm")
#install.packages("polycor")
library(pglm)
library(polycor)
library(stargazer)

#install.packages("miscTools")
library(miscTools)
library(Hmisc)
#install.packages("lmtest")
library(lmtest)

##########
# Data importation
##########

dt1 <- read.csv(file.path(dataPath, "Data/df_balanced.csv"))
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
list(sp1)
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

table(df2$Year == '2009')


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

write.csv(Pans1, "Data/GretlPans1.csv")


########
# DATA Creating filter for balanced panel
########

#Householdsize                                                        482   7.031      3.289        1         22     
#FCS                                                                  478   59.759     23.824     6.500     151.500  
#Numberofdiffererentcropsproducedbythehousehold                       482   4.859      1.928        1         12     
#Ageofthehouseholdhead                                                482   47.461     14.045      20         83     
#Educationlevelofthehouseholdhead                                     378   20.780     10.778      10         51     
#Totalcroppedarea                                                     482   5.740      37.885     0.000     816.100  
#DDS                                                                  478   7.715      1.927        2         11     
#Numberofhouseholdproductionbynutritionalgroup                        482   3.247      1.055        1          6     
#Foodexpenditure                                                      481  254.872    322.855     0.000    3,079.350 
#Nonfoodexpenditure                                                   478  161.441    281.960     0.100    2,742.550 
#Incomes                                                              462 1,606.123  5,584.785    0.900   110,340.000
#Numberofdifferentnonagriculturalincomesources                        482   0.091      0.365        0          5     
#Proportionoffoodconsumedinpreviousoneweekfromhouseholdsownproduction 478   0.363      0.174      0.000      1.000   
#Simpsonsindex                                                        437   0.615      0.196      0.000      0.870   
#CaloriesbyHH                                                         478 84,259.070 67,778.090 1,502.000 627,960.500
#Agriculturalincomes                                                  482  773.538   4,347.993    0.000   92,819.370 
#NonAgriculturalincomes                                               482   0.684      8.892      0.000     168.502  
#Propertyincomes                                                      482  348.024   1,179.469    0.000   14,743.930 
#Investments                                                          482   21.977    119.440     0.000    1,718.721 
#Transfers                                                            482  152.804    570.991     0.000    7,801.646 

#newdata <- mtcars[order(mpg, cyl),]

#Sort and subset
attach(dt1)
dtHH <- dt1[order(HHID,Year),]
#dtHH <- dtHH[1:1722, ]
summary(dtHH)

#$dtHH1<-$dtHH[,HHID]

#Education
#ANNEX 2. CODES FOR HIGHEST EDUCATION LEVEL ATTAINED
#Some schooling but not Completed P.1................
10
#Completed P.1......11
#Completed P.2......12
#Completed P.3......13
#Completed P.4......14
#Completed P.5......15
#Completed P.6......16
#Completed P.7......17
#Completed J.1......21
#Completed J.2......22
#Completed J.3......23
#Completed S.1......31
#Completed S.2......32
#Completed S.3......33
#Completed S.4......34
#Completed S.5......35
#Completed S.6......36
#Completed Post primary Specialized training or Certificate 41
#Completed Post secondary Specialized training or diploma 51
#Completed Degree and above 61
#Don't Know 99
summary(Educationlevelofthehouseholdhead)
table(Educationlevelofthehouseholdhead)
dtHH$Educationlevelofthehouseholdhead2 <- dtHH$Educationlevelofthehouseholdhead
dtHH$Educationlevelofthehouseholdhead2change <- 0

codes <- 61
for (i in 1:length(HHID))
{  k <-Educationlevelofthehouseholdhead[i]
  data_(k)<-1}

for (Var in names(Educationlevelofthehouseholdhead)) {
  missing <- sum(is.na(Educationlevelofthehouseholdhead[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}
c(Var,missing)
summary(missing)
table(Educationlevelofthehouseholdhead)

#x <- airquality[, -1] # x is a regression design matrix
#y <- airquality[,  1] # y is the corresponding response

stopifnot(complete.cases(Educationlevelofthehouseholdhead) != is.na(Educationlevelofthehouseholdhead))
ok <- complete.cases(dtHH, Educationlevelofthehouseholdhead)
sum(!ok) # how many are not "ok" ?
#dtHH <- dtHH[ok,]
#dtHH <- dtHH[ok,]

dtHH$sel <- if (Educationlevelofthehouseholdhead != is.na(Educationlevelofthehouseholdhead)) 0 else 1
summary(dtHH$sel)

for (Var in 1:length(Educationlevelofthehouseholdhead)) {  
  if (is.na(dtHH[Var,Educationlevelofthehouseholdhead])  1tHH$sel[,Var]<- 1 Var]) )
  if (missing > 0) {
    print(c(Var,missing))
  }
}


#dtHH$sel <- if (Educationlevelofthehouseholdhead != NA) 1 else 0
#http://stackoverflow.com/questions/17573226/generating-a-moving-sum-variable-in-r

#rm(Moneyfromabroad)
stopifnot(complete.cases(dtHH$Moneyfromabroad) != is.na(dtHH$Moneyfromabroad))
ok <- complete.cases(dtHH, Moneyfromabroad)
sum(!ok) # how many are not "ok" ?
#y <- y[ok]

table(dtHH$Moneyfromabroad)

dtHH$Moneyfromabroad<- if(is.na(dtHH$Moneyfromabroad)==1) 0
table(dtHH$Moneyfromabroad)

summary(dtHH$Moneyfromabroad)
########
# DATA Summary
########

Pans1$CaloriesbyHHmem <- Pans1$CaloriesbyHH / Pans1$Householdsize
Pans1$CaloriesperCapDay <- Pans1$CaloriesbyHH / Pans1$Householdsize / 7

library(dplyr)

s2009 <- filter(Pans1, Year==2009)
s2010 <- filter(Pans1, Year==2010)
s2012 <- filter(Pans1, Year==2012)

stargazer(s2009, s2010, s2012, type="text", out="Results/summary_split_year.txt")

east <- filter(Pans1, Region=='Eastern')
west <- filter(Pans1, Region=='Western')
noth <- filter(Pans1, Region=='Northern')
cent <- filter(Pans1, Region=='Central')

stargazer(east, west, noth, cent, type="text", out="Results/summary_split_region.txt")

east2009 <- filter(Pans1, Region=='Eastern' & Year==2009)
east2010 <- filter(Pans1, Region=='Eastern' & Year==2010)
east2012 <- filter(Pans1, Region=='Eastern' & Year==2012)

stargazer(east2009, east2010, east2012, type="text", out="Results/summary_split_year_east.txt")

west2009 <- filter(Pans1, Region=='Western' & Year==2009)
west2010 <- filter(Pans1, Region=='Western' & Year==2010)
west2012 <- filter(Pans1, Region=='Western' & Year==2012)
stargazer(west2009, west2010, west2012, type="text", out="Results/summary_split_year_west.txt")

noth2009 <- filter(Pans1, Region=='Northern' & Year==2009)
noth2010 <- filter(Pans1, Region=='Northern' & Year==2010)
noth2012 <- filter(Pans1, Region=='Northern' & Year==2012)
stargazer(noth2009, noth2010, noth2012, type="text", out="Results/summary_split_year_north.txt")

cent2009 <- filter(Pans1, Region=='Central' & Year==2009)
cent2010 <- filter(Pans1, Region=='Central' & Year==2010)
cent2012 <- filter(Pans1, Region=='Central' & Year==2012)
stargazer(cent2009, cent2010, cent2012, type="text", out="Results/summary_split_year_central.txt")
stargazer(cent2009, cent2010, cent2012, type="text", out="Results/summary_split_year_central.csv")

stargazer(east2009, east2010, east2012, west2009, west2010, west2012, noth2009, noth2010, noth2012, cent2009, cent2010, cent2012, type="text", out="Results/summary_split_region_year.txt")

# subsamples are removed to save memory space
rm(east2009, east2010, east2012, west2009, west2010, west2012, noth2009, noth2010, noth2012, cent2009, cent2010, cent2012) 
rm(east, west, noth, cent)

myvar <- c("DDS", "FCS", "CaloriesperCapDay")
s2009cor <- s2009[myvar]
cor.mat2009 <- cor(s2009cor, use="complete.obs", method="pearson")
cor.mat2009
rm(s2009sub)

s2010cor <- s2010[ myvar ]
cor.mat2010 <- cor(s2010cor, use="complete.obs", method="pearson")
cor.mat2010
rm(s2010cor)

s2012cor <- s2012[ myvar ]
cor.mat2012 <- cor(s2012cor, use="complete.obs", method="pearson")
cor.mat2012
rm(s2012cor)

stargazer(cor.mat2009, cor.mat2010, cor.mat2012, type="text", out="Results/summary_FNS_correlations_year.txt")



# Simple Scatterplot of DDS and FVS
plot(s2009$FCS, s2009$DDS, main="Coherence between DDS and FCS in 2009 in Uganda", 
     xlab="DDS ", ylab="FCS ", pch=19) 
plot(s2010$FCS, s2010$DDS, main="Coherence between DDS and FCS in 2010 in Uganda", 
     xlab="DDS ", ylab="FCS ", pch=19) 
plot(s2012$FCS, s2012$DDS, main="Coherence between DDS and FCS in 2012 in Uganda", 
     xlab="DDS ", ylab="FCS ", pch=19) 



table(Pans1$FCS,Pans1$DDS)

table(Pans1$DDS,Pans1$Year)
table(Pans1$DDS,Pans1$Region)

table(Pans1$FCS,Pans1$Year)  # Averege per year would be better

rcorr(as.matrix(dt1))

sink()
#library(Deducer)
Deducer::descriptive.table(vars = d(Educationlevelofthehouseholdhead), 
                                    strata=Year,
                                    data=Pans1,
                                    func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))


frequencies(Pans1$Educationlevelofthehouseholdhead)
frequencies(Pans1$Year)
table(Pans1$Educationlevelofthehouseholdhead, Pans1$Year)

# Correlation of variables (numeric and integers) 
Pans1_sub <- Pans1[ -c(1,2) ]
myvars <- c("Foodexpenditure", "Nonfoodexpenditure", "Totalcroppedarea", "Householdsize", "DDS", "FCS",
            "Numberofdiffererentcropsproducedbythehousehold", "Ageofthehouseholdhead", "Numberofhouseholdproductionbynutritionalgroup",
            "")
Pans1_sub <- Pans1_sub[myvars] 
Cor_matrix <- cor(Pans1_sub, use="complete.obs", method="pearson")
Cor_matrix

#######################
# Run regressions - Technical regression comparison
#######################

#To do-check for outliers 
plot(Pans1$Educationlevelofthehouseholdhead,Pans1$DDS)
plot(Pans1$Educationlevelofthehouseholdhead,Pans1$Totalcroppedarea)
plot(Pans1$Totalcroppedarea,Pans1$Ageofthehouseholdhead)
plot(Pans1$Totalcroppedarea,Pans1$Incomes)

plot(Pans1$Educationlevelofthehouseholdhead,Pans1$Ageofthehouseholdhead)
plot(Pans1$Foodexpenditure,Pans1$Nonfoodexpenditure)
plot(Pans1$Foodexpenditure,Pans1$Incomes)
plot(Pans1$Nonfoodexpenditure,Pans1$Incomes)
plot(Pans1$FCS,Pans1$DDS)

plot(Pans1$Educationlevelofthehouseholdhead, type="h")
table(Pans1$Year)

# Annual OLS regressions to check on consistency of regresssion coefficients over time
# Moreover, to check variation in certain variables over time.
# one concern: what to do with missing values on variables. It is a problem when doing panel regressions!

######

fcsols2009 <- lm(FCS ~
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
               , data=s2009)
fcsols2010 <- lm(FCS ~
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
                 , data=s2010)
fcsols2012 <- lm(FCS ~
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
                 , data=s2012)
stargazer(fcsols2009, fcsols2010, fcsols2012, type="text", out="fcsLM_OLS_per_year.txt")

ddsols2009 <- lm(DDS ~
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
                 , data=s2009)
ddsols2010 <- lm(DDS ~
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
                 , data=s2010)
ddsols2012 <- lm(DDS ~
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
                 , data=s2012)
stargazer(ddsols2009, ddsols2010, ddsols2012, type="text", out="ddsLM_OLS_per_year.txt")

calols2009 <- lm(CaloriesbyHH ~
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
                 , data=s2009)
calols2010 <- lm(CaloriesbyHH ~
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
                 , data=s2010)
calols2012 <- lm(CaloriesbyHH ~
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
                 , data=s2012)
stargazer(calols2009, calols2010, calols2012, type="text", out="calLM_OLS_per_year.txt")

# Panel regressions
# FVS
######

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

fcswithin0 <- plm(FCS ~ as.factor(Year),
                   data=Pans1,
                   effect = "twoways",
                   model="within",
                   na.action=na.omit)
fcspanel1 <- fcswithin0

fcspanel2a <- plm(FCS ~ 
                    as.factor(Year) + Numberofdiffererentcropsproducedbythehousehold,
                  data=Pans1,
                  effect = "twoways",
                  model="within",
                  na.action=na.omit)

fcspanel2b <- plm(FCS ~ 
                    as.factor(Year) + Simpsonsindex,
                  data=Pans1,
                  effect = "twoways",
                  model="within",
                  na.action=na.omit)

fcspanel2c <- plm(FCS ~ 
                    as.factor(Year) + Numberofhouseholdproductionbynutritionalgroup,
                  data=Pans1,
                  effect = "twoways",
                  model="within",
                  na.action=na.omit)

fcswithin1a <- plm(FCS ~
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
                  model="within",
                  na.action=na.omit)

fcswithin1c <- plm(FCS ~
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
                  model="within",
                  na.action=na.exclude)
stargazer(fcswithin1, fcswithin1a, fcswithin1c, type="text", out="fcsPLM_OLS_WITHIN_omit.txt")


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

## TO DO: check for multicollinearity in the regressors!
stargazer(fcswithin1, fcsplmPoisson1, type="text", out="fcsPLM_OLS_WITHIN.txt")




# DDS
######

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

stargazer(ddswithin1, ddsplmPoisson1, type="text", out="ddsPLM_OLS_WITHIN.txt")

stargazer(ddswithin1, fcswithin1, type="text", out="ddsfcsPLM_OLS_WITHIN.txt")
#what is going on?



help
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



