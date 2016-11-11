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

# rent on land that household member owns
rent <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%
  select(HHID, parcel_id=a2aq2, rent=a2aq16)

# rent from land that household has rights to
rent2 <- read_dta(file.path(dataPath, "AGSEC2B.dta")) %>%
  select(HHID, parcel_id=a2bq2, rent=a2bq16)
