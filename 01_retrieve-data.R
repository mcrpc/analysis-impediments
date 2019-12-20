
# initialize local variables ----------------------------------------------
censusYear <- 2010
columnTypeList <- readr::cols(GEOID = col_character())

# retrieve data -----------------------------------------------------------
filename <- addACSYearsToFilename("McLean-County_Tracts.csv", acsYear)
filepath <- paste(dataDirectory, filename, sep = "/")

acsTableList <- c(
  "S1701",
  "S2504",
  "B25003",
  "B25064",
  "B25077",
  "DP05"
)

acsDataTibble <- tryCatch(
  {
    readr::read_csv(
      filepath,
      col_types = columnTypeList
    )
  },
  error = function(err) {
    acsDataTibble <- purrr::map_dfr(
      acsTableList, 
      ~ tidycensus::get_acs(
        geography = "tract",
        table = .,
        state = "17",
        county = "113",
        year = acsYear,
        output = "wide"
      )
    ) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarize_all(coalesceByColumn)
    
    readr::write_csv(acsDataTibble, filepath)
  }
)