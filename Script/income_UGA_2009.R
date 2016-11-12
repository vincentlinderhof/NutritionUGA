# -------------------------------------
#' Income variables for Uganda wave 1
#' (2009-10)
#' section 8 for off-farm income vars
#' Local currency: Ugandan Shilling
#' responded to by all household members
#' of 5 and above
#' Section 11 contains other household income
#' section 12 contains non-agro household activities
#' AG section 2a q16 for rent also 2b q16 for land household has rights to
#' AG section 5A for crop production first season of 2009
#' AG section 5B for crop prod second crop season of 2009
#' AG section 6 for livestock sold in last 12 months part B for small animals
#' part C for poultry
#' section 8 for livestock products
#' section 9 for fish sold
# -------------------------------------

# load packages
library(dplyr)
library(haven)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/UGA/2009_10/Data/"
}

# -------------------------------------
#' off-farm income
# -------------------------------------


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
# -------------------------------------

# big animals - But no slaughter sales values
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

lvstck <- rbind(big, med, small) %>%
  select(HHID, type, lvstk_value)

lvstck <- group_by(lvstck, HHID, type) %>%
  summarise())
  spread(key = type, value = lvstk_value)

# summarise at the household level
on_farm_income_lvstock <- group_by(lvstck, HHID) %>%
  summarise(lvstock_value_hh=sum(lvstk_value, na.rm=TRUE))

# -------------------------------------
# Livestock products sold.
# -------------------------------------

lvstck_products <- read_dta(file.path(dataPath, "AGSEC8.dta")) %>%
  select(HHID, product_code=a8q2, qty=a8q4, qty_unit=a8q5,
         qty_sold=a8q6, qty_value=a8q7)

# summarise at the household level
on_farm_income_lvstock <- group_by(lvstck, HHID) %>%
  summarise(lvstock_value_hh=sum(lvstk_value, na.rm=TRUE))

# -------------------------------------
# fishing - there are questions on
# fish caught and sold, but only how
# much is sold per day and it is no
# recorded how much the sales are per
# year, and therefore not clear how 
# much income was generated from 
# fishing
# -------------------------------------
