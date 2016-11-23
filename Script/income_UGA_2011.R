# -------------------------------------
#' Income variables for Uganda wave 3
#' (2010-11). Information on income can be
#' found in the following sections of the
#' questionnaire:
#' 
#' off-farm income (household questionnaire):
#'      1. section 8 for off-farm income vars
#'      2. Section 11 contains other household income
#'      3. Section 12 contains non-agro household activities
#' on-farm income (agriculture questionnaire):
#'      1. Sections 2a and 2b question 16 for rent income
#'      2. Section 5a for crop production in first cropping season (2011)
#'      3. Section 5b for crop production in second cropping season (2011)
#'      4. Sections 6A, 6B and 6C for livestock sales
#'      5. Section 8 for livestock products
#'      6. Section 9 for fish sold (not used)
#'
#' The output of this file is a dataframe
#' holding all the income variables for each
#' household 
#' 
#' local currency: Ugandan Shilling
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2011_12/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/UGA/2011_12/Data/"
}

# -------------------------------------
#' off-farm income
# -------------------------------------

#' Section 8 of the household questionnaire
#' contains information about the primary
#' and secondary job of all household members
#' above 5 years old. However, What is not
#' asked is how much income was actually earnt
#' by the respondent in the last week. Instead
#' we only know how much was earnt in the last
#' week and how many months per year the
#' respondent has worked.

#' Section 11 contains other household income
#' Coding between data and questionnaire is wrong
#' h11q2 = h11q3
off_farm_income_other <- read_dta(file.path(dataPath, "GSEC11.dta")) %>%
  select(HHID, code=h11q2, income=h11q5, income_in_kind=h11q6)

off_farm_income_other$income_other <- with(off_farm_income_other,
                                           rowSums(cbind(income, income_in_kind),
                                                   na.rm=TRUE))
miss <- with(off_farm_income_other,
             is.na(income) & is.na(income_in_kind))
off_farm_income_other$income_other[miss] <- NA; rm(miss)

# summarise to the household level
off_farm_income_other <- group_by(off_farm_income_other, HHID) %>%
  summarise(income_other=sum(income_other, na.rm=TRUE))

# -------------------------------------
# income from off farm enterprises and
# activities. (section 12)
# profit is calcualted as average profit
# per month. Care needs to be taken 
# because the profit can be negative.
# revenue on the other hand can not,
# but does not count as income if the
# wages and other costs are very high.
# -------------------------------------

off_farm_ent <- read_dta(file.path(dataPath, "GSEC12.dta")) %>%
  select(HHID, monthspy=h12q12, avg_rev_pm=h12q13,
         avg_wage_pm = h12q15, avg_exp_pm = h12q16,
         avg_oth_pm = h12q17)

off_farm_ent$avg_profit_pm <- with(off_farm_ent,
                                   avg_rev_pm - (avg_wage_pm + avg_exp_pm + avg_oth_pm))

off_farm_ent$revenue_ent <- off_farm_ent$monthspy * off_farm_ent$avg_rev_pm
off_farm_ent$profit_ent <- off_farm_ent$monthspy * off_farm_ent$avg_profit_pm

off_farm_ent <- group_by(off_farm_ent, HHID) %>%
  summarise(profit_ent=sum(profit_ent, na.rm=TRUE),
            revenue_ent=sum(revenue_ent, na.rm=TRUE))


# -------------------------------------
#' on-farm income
# -------------------------------------

# crop production from the first season of 2011
crop1 <- read_dta(file.path(dataPath, "AGSEC5A.dta")) %>%
  select(HHID, parcel_id = parcelID, plot_id = plotID,
         crop_code = cropID, qty_harv = a5aq6a,
         qty2kg = a5aq6d, qty_sold = a5aq7a,
         crop_value = a5aq8)
crop1$qty_sold <- crop1$qty_sold * crop1$qty2kg  

# crop production from the second season of 2011
crop2 <- read_dta(file.path(dataPath, "AGSEC5B.dta")) %>%
  select(HHID, parcel_id = parcelID, plot_id = plotID,
         crop_code = cropID, qty_harv = a5bq6a,
         qty2kg = a5bq6d, qty_sold = a5bq7a,
         crop_value = a5bq8)  
crop2$qty_sold <- crop2$qty_sold * crop2$qty2kg 

# join both together
crop <- rbind(crop1, crop2)

# calculate the full value of crops
# per household

on_farm_income_crop <- group_by(crop, HHID) %>%
  summarise(crop_value_hh=sum(crop_value, na.rm=TRUE))
rm(crop, crop1, crop2)

# rent on land that household member owns
rent1 <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%
  select(HHID, parcel_id=parcelID, rent=a2aq14)

# rent from land that household has rights to
rent2 <- read_dta(file.path(dataPath, "AGSEC2B.dta")) %>%
  select(HHID, parcel_id=parcelID, rent=a2bq14)

# join the two types of rent together
rent <- rbind(rent1, rent2)

# summarise to the household level
on_farm_income_rent <- group_by(rent, HHID) %>%
  summarise(rent_value_hh=sum(rent, na.rm=TRUE))
rm(rent)

# -------------------------------------
# Livestock
# Respondents were asked if they had sold
# livestock. There are large animals (cows)
# medium animals (goats, sheep etc) and
# small animals like poultry. Income
# is recorded from the sale of each 
# separately. In wave 3, in contrast to
# waves 1 and 2, farmers are asked how
# many animals were sold, and the average
# value of each animal sold, rather than
# the average value of all sales of an animal
# -------------------------------------

# big animals
big <- read_dta(file.path(dataPath, "AGSEC6A.dta")) %>%
  select(HHID, lvstckcode=lvstid, lvstk_number_sold=a6aq14a,
         avg_lvstk_value=a6aq14b, slaughter=a6aq15) %>%
  mutate(type="big")
big$lvstk_value <- big$lvstk_number_sold * big$avg_lvstk_value


# medium
med <- read_dta(file.path(dataPath, "AGSEC6B.dta")) %>%
  select(HHID, lvstckcode=lvstid, lvstk_number_sold=a6bq14a,
         avg_lvstk_value=a6bq14b, slaughter=a6bq15) %>%
  mutate(type="medium")
med$lvstk_value <- med$lvstk_number_sold * med$avg_lvstk_value

# small
small <- read_dta(file.path(dataPath, "AGSEC6C.dta")) %>%
  select(HHID, lvstckcode=lvstid, lvstk_number_sold=a6cq14a,
         avg_lvstk_value=a6cq14b, slaughter=a6cq15) %>%
  mutate(type="small")
small$lvstk_value <- small$lvstk_number_sold * small$avg_lvstk_value

# combine big, medium and small
on_farm_income_lvstck <- rbind(big, med, small) %>%
  select(HHID, type, lvstk_value)

# group by household and animal
# category (big, medium and small)
# and summarise to teh household level
on_farm_income_lvstck <- group_by(on_farm_income_lvstck, HHID, type) %>%
  summarise(lvstk_value=sum(lvstk_value, na.rm=TRUE)) %>%
  spread(key = type, value = lvstk_value)

names(on_farm_income_lvstck) <- c("HHID", "large_lvstck_inc",
                                  "medium_lvstck_inc", "small_lvstck_inc")

# -------------------------------------
#' Livestock products sold.
#' unlike the first two waves, respondents
#' There is also information on milk and
#' egg products - but not with sufficient
#' information to calcualte their value to
#' the household.
# -------------------------------------

lvstck_products <- read_dta(file.path(dataPath, "AGSEC8A.dta")) %>%
  select(HHID, product_code=AGroup_ID, qty=a8aq2,
         qty_sold=a8aq3, lvstck_prod_value=a8aq5)

# summarise at the household level
on_farm_income_lvstock_products <- group_by(lvstck_products, HHID) %>%
  summarise(lvstock_prod_value_hh=sum(lvstck_prod_value, na.rm=TRUE))

# -------------------------------------
# fishing - there are questions on
# fish caught and sold, but only how
# much is sold per day and it is no
# recorded how much the sales are per
# year, and therefore not clear how 
# much income was generated from 
# fishing (section 9 of questionnaire)
# -------------------------------------

# -------------------------------------
# Total household income Uganda 2011
# -------------------------------------

# there are joining problems with the HHID
# variable in wave 3 of the data. Make all
# HHID's derived from Agric questionnaire 
# into strings rather than numeric
on_farm_income_crop$HHID <- as.character(on_farm_income_crop$HHID)
on_farm_income_lvstck$HHID <- as.character(on_farm_income_lvstck$HHID) 
on_farm_income_lvstock_products$HHID <- as.character(on_farm_income_lvstock_products$HHID) 
on_farm_income_rent$HHID <- as.character(on_farm_income_rent$HHID) 

income_2011 <- full_join(off_farm_ent, off_farm_income_other)
income_2011 <- full_join(income_2011, on_farm_income_crop)
income_2011 <- full_join(income_2011, on_farm_income_lvstck)
income_2011 <- full_join(income_2011, on_farm_income_lvstock_products)
income_2011 <- full_join(income_2011, on_farm_income_rent)

# take out trash
rm(med, small, big, lvstck_products,
   dataPath, rent1, rent2, off_farm_ent, off_farm_income_other,
   on_farm_income_crop, on_farm_income_lvstck, on_farm_income_lvstock_products,
   on_farm_income_rent)
