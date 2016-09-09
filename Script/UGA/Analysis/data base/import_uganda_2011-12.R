#import des donnees 2011-2012


library(readstata13)
library(foreign)


setwd("M:/R/Uganda/2011_12/Data")


####
#GPS
areas <-read.dta('areas_uga_y1_imputed.dta')

#AGRICULTURAL SURVEY DATA
#Household identification particular - Staff details and survey time
AGSEC1 <-read.dta('AGSEC1.dta')
#Current Land Holdings - First/Second Visit
AGSEC2A <-read.dta('AGSEC2A.dta')
AGSEC2B <-read.dta('AGSEC2B.dta')
#Agricultural and labour inputs
AGSEC3A <-read.dta('AGSEC3A.dta')
AGSEC3B <-read.dta('AGSEC3B.dta')
#Crops grown and type of seeds used
AGSEC4A <-read.dta('AGSEC4A.dta')
AGSEC4B <-read.dta('AGSEC4B.dta')
#Quantification of agricultural production
AGSEC5A <-read.dta('AGSEC5A.dta')
AGSEC5B <-read.dta('AGSEC5B.dta')
#Livestock ownership
AGSEC6A <-read.dta('AGSEC6A.dta')
AGSEC6B <-read.dta('AGSEC6B.dta')
AGSEC6C <-read.dta('AGSEC6C.dta')
#Livestock Products
AGSEC7A <-read.dta('AGSEC7A.dta')
AGSEC7B <-read.dta('AGSEC7B.dta')
#Livestock Services
AGSEC8A <-read.dta('AGSEC8A.dta')
AGSEC8B <-read.dta('AGSEC8B.dta')
AGSEC8C <-read.dta('AGSEC8C.dta')
AGSEC8D <-read.dta('AGSEC8D.dta')
AGSEC8E <-read.dta('AGSEC8E.dta')
#Extension services
AGSEC9 <-read.dta('AGSEC9.dta')
#Farm implements and machinery
AGSEC10 <-read.dta('AGSEC10.dta')

# HOUSEHOLD SURVEY DATA
#Household Identification
GSEC1 <-read.dta('GSEC1.dta')
#Household Roster
GSEC2 <-read.dta('GSEC2.dta')
#General Information on Household Members
GSEC3 <-read.dta('GSEC3.dta')
#Education
GSEC4 <-read.dta('GSEC4.dta')
#Health
GSEC5 <-read.dta('GSEC5.dta')
#Child Nutrition and Health
GSEC6A <-read.dta('GSEC6A.dta')
GSEC6B <-read.dta('GSEC6B.dta')
GSEC6C <-read.dta('GSEC6C.dta')
#Labour Force Status
GSEC8 <-read.dta('GSEC8.dta')
#Housing Conditions, Water and Sanitation
GSEC9A <-read.dta('GSEC9A.dta')
GSEC9B <-read.dta('GSEC9B.dta')
#Energy Use
GSEC10A <-read.dta('GSEC10A.dta')
GSEC10B <-read.dta('GSEC10B.dta')
GSEC10C <-read.dta('GSEC10C.dta')
#Other Household Incomes
GSEC11 <-read.dta('GSEC11.dta')
#Non-agricultural Enterprises/Activities 
GSEC12 <-read.dta('GSEC12.dta')
#Household Assets
GSEC14 <-read.dta('GSEC14.dta')
#Household Consumption
GSEC15B <-read.dta('GSEC15B.dta')
GSEC15BB <-read.dta('GSEC15BB.dta')
GSEC15C <-read.dta('GSEC15C.dta')
GSEC15D <-read.dta('GSEC15D.dta')
#Shocks and Coping Strategies
GSEC16 <-read.dta('GSEC16.dta')
#Welfare Indicators and Food Security
GSEC17A <-read.dta('GSEC17A.dta')
GSEC17B <-read.dta('GSEC17B.dta')
GSEC17C <-read.dta('GSEC17C.dta')
#Transport Services
GSEC18 <-read.dta('GSEC18.dta')
GSEC18A <-read.dta('GSEC18A.dta')
GSEC18B <-read.dta('GSEC18B.dta')

#Women questionnaire
#Contraception
WSEC2A <- read.dta('WSEC2A.dta')
#Birth hystory
WSEC2B_1 <- read.dta('WSEC2B_1.dta')
WSEC2B_2 <- read.dta('WSEC2B_2.dta')

####
#total household
totag <- select(AGSEC1, HHID)
tothh <- select(GSEC1, HHID)

####
#Matching Food items / Food group
match <- read.csv2('match.csv', header=TRUE, sep=',')
matchcrop <- read.csv2('matchcrop.csv', header=TRUE, sep=',')