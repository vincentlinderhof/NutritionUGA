# -------------------------------------
#' crop production variables:
#'  Uganda 2009
#'  There is a grand total of 5 possible
#'  area measurements that could be used
#'  
#'     1. harvested area (farmer reported) at crop level
#'     2. parcel area (farmer reported)
#'     3. parcel area (gps reported, 25% of plots)
#'     4. parcel area (farmer + gps measured)
#'     5. relative area (explained below)
#'
#'   In addition there are several crop
#'   groups to consider. In this file I 
#'   consider 7 crop groups
#'   
#'      1. fruit
#'      2. CCP: Cash Crop Permanent
#'      3. CCNP: Cash Crop Non Permanent
#'      4. veg: vegetable
#'      5. leg: legumes
#'      6. CTR: Cereals Tubers and Roots
#'      7. Other: everything I didn't recognize
#'      
#'   In addition both maize and wheat have
#'   their own group.
#'   
#'   Output: tbd
# -------------------------------------

# -------------------------------------
# load packages and set working directory
# -------------------------------------

library(tidyr)
library(dplyr)
library(haven)

if(Sys.info()["user"] == "Tomas"){ 
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data" }
if(Sys.info()["user"] == "linde069"){
    dataPath <- "D:/Analyses/CIMMYT/NutritionUGA/SurveyData/2009_10/Data"
    setwd("D:/Analyses/CIMMYT/NutritionUGA")
}

# -------------------------------------
# Get crop production information
# Note that the area measurements are at
# the crop level
# -------------------------------------

# read in data and select key variables
crop_prod <- read_dta(file.path(dataPath, "AGSEC4A.dta")) %>%
  dplyr::select(HHID, parcel_id = a4aq2, plot_id = a4aq4,
         crop_code = a4aq6, harv_area=a4aq8)

# use crop_code as an integer. 
crop_prod$crop_code <- as.integer(crop_prod$crop_code)

# harv area is in acres -> change to hectares
crop_prod$harv_area <- crop_prod$harv_area*0.404686

# -------------------------------------
# Get land information
# Note that the gps measurements are at
# the parcel level
# --------------------------------------

land <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%  
  select(HHID, parcel_id=a2aq2, area_farmer=a2aq5, area_gps=a2aq4)

# 0 area does not ake any sense
land$area_gps <- ifelse(land$area_gps %in% 0, NA, land$area_gps)

# measurements are in acres -> change to hectacres
land$area_gps <- land$area_gps*0.404686
land$area_farmer <- land$area_farmer*0.404686

# make a single area combining gps, non-gps and mixed
land$area_mix <- ifelse(is.na(land$area_gps), land$area_farmer, land$area_gps)
land$area_mix <- ifelse(land$area_mix %in% 0, NA, land$area_mix)

# -------------------------------------
#' join land information with the crop
#' production variables - because the
#' farmer reported areas, and the area
#' planted are at different levels,
#' we cannot say anything about "relative" areas.
# -------------------------------------

crop_prod <- left_join(crop_prod, land); rm(land)

# -------------------------------------
#' make a "relative" area variable.
#' because gps and farmer reported areas
#' are made at the parcel level we do not
#' have an accurate measurement of the
#' area planted for a particular crop
#' or crop group. Instead we have the
#' farmer reported harvest area and the
#' farmer reported plot area. Here I
#' create a variable that assumes farmers
#' can work out "relative" areas, but not
#' absolute/actual areas, and multiply this
#' relative area by the gps measured area
#' for the plot. This hopefully yields a
#' more accurate and continous area
#' measurement than farmer estimated areas
# -------------------------------------

crop_prod$area_rel <- crop_prod$harv_area/crop_prod$area_farmer*crop_prod$area_mix

# -------------------------------------
#' make a variable to record which food 
#' group each crop belongs to:
#' 
#'      1. fruit
#'      2. CCP: Cash Crop Permanent
#'      3. CCNP: Cash Crop Non Permanent
#'      4. veg: vegetable
#'      5. leg: legumes
#'      6. CTR: Cereals Tubers and Roots
# -------------------------------------

fruit <- c(700, 710, 720, 741, 742, 744, 750, 760, 770, 780)
CCP <- c(510, 630, 810, 820, 830, 840, 850, 860, 870, 880)
CTR <- c(610, 112, 120, 141, 150, 620, 440, 640, 650) 
CCNP <- c(520, 530)
veg <- c(410, 420, 430, 450, 460, 470)
leg <- c(210, 221, 222, 223, 224, 310, 320, 330, 340)
maize <-c(130)
wheat <- c(111)
other <- c(fruit, CCP, CTR, CCNP, veg, leg, maize, wheat)

# get a variable with the crop group
crop_prod$type <- character(nrow(crop_prod))
crop_prod <- mutate(crop_prod,
                    type=ifelse(crop_code %in% fruit, "fruit", type),
                    type=ifelse(crop_code %in% CCP, "CCP", type),
                    type=ifelse(crop_code %in% CTR, "CTR", type),
                    type=ifelse(crop_code %in% CCNP, "CCNP", type),
                    type=ifelse(crop_code %in% veg, "veg", type),
                    type=ifelse(crop_code %in% leg, "leg", type),
                    type=ifelse(crop_code %in% 130, "maize", type), # maize has crop code 130
                    type=ifelse(crop_code %in% 111, "wheat", type), # wheat has crop code 111
                    type=ifelse(!crop_code %in% other, "other", type)) 

# -------------------------------------
#' finally make 5 dataframes corresponding
#' to each of the possible area measurements
#' that can be used to create a total area
#' per food group variable. These are:
#' 
#'     1. harvested area (farmer reported)
#'     2. plot area (farmer reported)
#'     3. plot area (gps reported, 25% of plots)
#'     4. plot area (farmer + gps measured)
#'     5. relative area (explained below)
# -------------------------------------

# 1. harvested area (farmer reported)
crop_prod_v <- select(crop_prod, HHID, type, harv_area)
crop_prod_harv_area <- group_by(crop_prod_v, HHID, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = harv_area) 
names(crop_prod_harv_area) <- paste0(names(crop_prod_harv_area), "_harv_area")
names(crop_prod_harv_area)[1] <- "HHID"

# 2. plot area (farmer reported) 
crop_prod_w <- select(crop_prod, HHID, type, area_farmer)
crop_prod_area_farmer <- group_by(crop_prod_w, HHID, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_farmer) 
names(crop_prod_area_farmer) <- paste0(names(crop_prod_area_farmer), "_area_farmer")
names(crop_prod_area_farmer)[1] <- "HHID"

# 3. plot area (gps reported, 25% of plots)
crop_prod_x <- select(crop_prod, HHID, type, area_gps)
crop_prod_area_gps <- group_by(crop_prod_x, HHID, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_gps) 
names(crop_prod_area_gps) <- paste0(names(crop_prod_area_gps), "_area_gps")
names(crop_prod_area_gps)[1] <- "HHID"

# 4. plot area (farmer + gps measured)
crop_prod_y <- select(crop_prod, HHID, type, area_mix)
crop_prod_area_mix <- group_by(crop_prod_y, HHID, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_mix) 
names(crop_prod_area_mix) <- paste0(names(crop_prod_area_mix), "_area_mix")
names(crop_prod_area_mix)[1] <- "HHID"

# 5. relative area 
crop_prod_z <- select(crop_prod, HHID, type, area_rel)
crop_prod_area_rel <- group_by(crop_prod_z, HHID, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_rel) 
names(crop_prod_area_rel) <- paste0(names(crop_prod_area_rel), "_area_rel")
names(crop_prod_area_rel)[1] <- "HHID"

saveRDS(crop_prod_harv_area,   "Data/Crop_prod_harv_area_2009.rds")
saveRDS(crop_prod_area_farmer, "Data/Crop_prod_area_farmer_2009.rds")
saveRDS(crop_prod_area_gps,    "Data/Crop_prod_area_gps_2009.rds")
saveRDS(crop_prod_area_rel,    "Data/Crop_prod_area_rel_2009.rds")
saveRDS(crop_prod_area_mix,    "Data/Crop_prod_area_mix_2009.rds")

# take out trash
rm(crop_prod_v, crop_prod_w, crop_prod_x,
   crop_prod_y, crop_prod_z, CCP, CTR, fruit,
   leg, other, veg, CCNP, crop_prod, dataPath)
