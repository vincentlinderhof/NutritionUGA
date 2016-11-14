# -------------------------------------
#' Income variables for Uganda wave 1
#' (2009-10). Inforation on income can be
#' found in the following sections of the
#' questionnaire:
#' 
#' off-farm income (household questionnaire):
#'      1. section 8 for off-farm income vars
#'      2. Section 11 contains other household income
#'      3. Section 12 contains non-agro household activities
#' on-farm income (agriculture questionnaire):
#'      1. Sections 2a and 2b question 16 for rent income
#'      2. Section 5a for crop production in first cropping season (2009)
#'      3. Section 5b for crop production in second cropping season (2009)
#'      4. Sections 6A, 6B and 6C for livestock sales
#'      5. Section 8 for livestock products
#'      6. Section 9 for fish sold (not used)
#'
#' The output of this file is a dataframe
#' holding all the income variables for each
#' household 
#' local currency: Ugandan Shilling
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/UGA/2009_10/Data/"
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
off_farm_income_other <- read_dta(file.path(dataPath, "GSEC11.dta")) %>%
  select(HHID, code=h11aq03, income=h11aq05, income_in_kind=h11aq06)

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
# -------------------------------------

off_farm_ent <- read_dta(file.path(dataPath, "GSEC12.dta")) %>%
  select(HHID, monthspy=h12q12, avg_inc_pm=h12q13)

off_farm_ent$income_ent <- off_farm_ent$monthspy * off_farm_ent$avg_inc_pm

off_farm_ent <- group_by(off_farm_ent, HHID) %>%
  summarise(income_ent=sum(income_ent, na.rm=TRUE))

# -------------------------------------
#' on-farm income
# -------------------------------------

# crop production from the first season of 2009
crop1 <- read_dta(file.path(dataPath, "AGSEC5A.dta")) %>%
  select(HHID, parcel_id = a5aq1, plot_id = a5aq3,
         crop_code = a5aq5, qty_harv = a5aq6a,
         qty2kg = a5aq6d, qty_sold = a5aq7a,
         crop_value = a5aq8)
crop1$qty_sold <- crop1$qty_sold * crop1$qty2kg  

# crop production from the second season of 2009
crop2 <- read_dta(file.path(dataPath, "AGSEC5B.dta")) %>%
  select(HHID, parcel_id = a5bq1, plot_id = a5bq3,
         crop_code = a5bq5, qty_harv = a5bq6a,
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
  select(HHID, parcel_id=a2aq2, rent=a2aq16)

# rent from land that household has rights to
rent2 <- read_dta(file.path(dataPath, "AGSEC2B.dta")) %>%
  select(HHID, parcel_id=a2bq2, rent=a2bq16)

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
# separately
# respondents are asked if they slaughtered
# animals for sale, but the income derived
# is not recorded.
# -------------------------------------

# big animals
big <- read_dta(file.path(dataPath, "AGSEC6A.dta")) %>%
  select(HHID, lvstckcode=a6aq3, lvstk_number_sold=a6aq14,
         lvstk_value=a6aq15, slaughter=a6aq16) %>%
  mutate(type="big")


# medium
med <- read_dta(file.path(dataPath, "AGSEC6B.dta")) %>%
  select(HHID, lvstckcode=a6bq3, lvstk_number_sold=a6bq14,
         lvstk_value=a6bq15, slaughter=a6bq16) %>%
  mutate(type="medium")


# small
small <- read_dta(file.path(dataPath, "AGSEC6C.dta")) %>%
  select(HHID, lvstckcode=a6cq3, lvstk_number_sold=a6cq14,
         lvstk_value=a6cq15, slaughter=a6cq16) %>%
  mutate(type="small")

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
# Livestock products sold.
# -------------------------------------

lvstck_products <- read_dta(file.path(dataPath, "AGSEC8.dta")) %>%
  select(HHID, product_code=a8q2, qty=a8q4, qty_unit=a8q5,
         qty_sold=a8q6, lvstck_prod_value=a8q7)

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
# Total household income Uganda 2009
# -------------------------------------



# take out trash
rm(med, small, big, lvstck, lvstck_products,
   dataPath, rent1, rent2)