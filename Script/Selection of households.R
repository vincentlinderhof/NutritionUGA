#######################################
########### UGANDA 2009-10 ############
#######################################

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionUGA/SurveyData/2009_10/Data"
}
setwd("D:/Analyses/CIMMYT/NutritionUGA")

library(haven)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

location <- read_dta(file.path(dataPath, "GSEC1.dta")) %>%
  select(HHID, REGCODE=region, rural=urban, DISTCODE=h1aq1)
location$rural <- ifelse(location$rural %in% 0, 1, 0)
location$REGNAME <- toupper(as_factor(location$REGCODE))
location$REGNAME <- gsub(" WITHOUT KAMPALA", "", location$REGNAME)
location$REGCODE <- as.numeric(location$REGCODE)


#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH09 <- read_dta(file.path(dataPath, "GSEC2.dta")) %>%
  select(HHID, indidy1=PID, status=h2q4, sex=h2q3,
         yob=h2q9c, age=h2q8)

HH09$status <- toupper(as_factor(HH09$status))
HH09$sex <- as.integer(HH09$sex) # female = 1
HH09$yob <- as.integer(HH09$yob)

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH09$cage <- cut(HH09$age, breaks = c(0, 15, 55, max(HH09$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "GSEC4.dta")) %>%
  select(HHID, indidy1=PID, ed_any=h4q5, grade=h4q7)

ed$ed_any <- ifelse(ed$ed_any %in% c(2, 3), 1, 0) # ever went to school
ed$grade <- as_factor(ed$grade)

# join with HH10 dataframe
HH09 <- left_join(HH09, ed)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH09_x <- group_by(HH09, HHID) %>%
  summarise(N1555=sum(cage %in% "16-55"),
            hhsize=n() )

HH09_y <- left_join(HH09_x, location)

library(Deducer)
frequencies(HH09_y$rural ,r.digits=1)
contingency.tables(HH09_y$hhsize, HH09_y$N1555, HH09_y$rural, data=HH09_y, missing.include=FALSE )

hh09_y_sub <- filter(HH09_y, (rural==1))
frequencies(hh09_y_sub$rural ,r.digits=1)
frequencies(hh09_y_sub$hhsize ,r.digits=1)
frequencies(hh09_y_sub$REGNAME ,r.digits=1)
descriptive.table(vars = d(hhsize, N1555),data= hh09_y_sub, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

hh09_y_sub <- filter(HH09_y, (rural==0))
frequencies(hh09_y_sub$rural ,r.digits=1)
frequencies(hh09_y_sub$hhsize ,r.digits=1)
frequencies(hh09_y_sub$REGNAME ,r.digits=1)
frequencies(hh09_y_sub$REGCODE ,r.digits=1)
frequencies(hh09_y_sub$DISTCODE ,r.digits=1)
descriptive.table(vars = d(hhsize, N1555),data= hh09_y_sub, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

