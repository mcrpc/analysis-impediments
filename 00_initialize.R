# load packages and set options -------------------------------------------
packages <- c(
  "tidyverse",
  "tidycensus",
  "tmap",
  "sf",
  "magrittr",
  "tigris",
  "here"
)

invisible(lapply(packages, library, character.only = TRUE))
options(tigris_use_cache = TRUE) 
# initialize global variables and set environments -------------------------------
if(!dir.exists(dataDirectory <- here::here("data"))) {
  dir.create(dataDirectory)
}

if(!dir.exists(mapDirectory <- here::here("map"))) {
  dir.create(mapDirectory)
}

censusAPIKey <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"
Sys.setenv(CENSUS_KEY = censusAPIKey)
acs2018VariableTable <- tidycensus::load_variables(2018, "acs5", cache = TRUE)

acsYear <- 2018


# define global functions -------------------------------------------------
addACSYearsToFilename <- function(filename, acsYear) {
  require(stringr)
  firstPart <- str_split(filename, "[.]", simplify = TRUE)[1]
  secondPart <- paste("_ACS-", (acsYear - 4), "-", acsYear, ".", sep = "")
  thirdPart <- str_split(filename, "[.]", simplify = TRUE)[2]
  paste0(firstPart, secondPart, thirdPart, sep = "")
}
coalesceByColumn <- function(df) {
  # coalesce a table by columns -- https://stackoverflow.com/a/45515491
  # this is used to clean the ACS data up after it is retrieved from the
  # API
  return(dplyr::coalesce(!!! as.list(df)))
}

# run the scripts! --------------------------------------------------------
source(here::here("01_retrieve-data.R"))
source(here::here("02_process-data.R"))
source(here::here("03_create-maps.R"))

