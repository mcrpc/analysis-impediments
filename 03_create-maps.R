# define local functions --------------------------------------------------
getACSYearsLabel <- function(acsYear) {
  paste("Data Source: Census ACS 5-year estimates,", acsYear - 4, "-", acsYear)
}

# set up layers -----------------------------------------------------------
countyLayer <- tigris::counties(c("17"), cb = TRUE, class = "sf") %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::select(
    -c(
      COUNTYNS,
      AFFGEOID,
      LSAD,
      ALAND,
      AWATER
    )
  )

tractLayer <- tigris::tracts(
  state = 17,
  county = 113,
  cb = TRUE,
  class = "sf"
) %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
  dplyr::left_join(analysisMapData, by = "GEOID")

cityLayer <- tigris::places(
  state = 17,
  class = "sf"
) %>%
  dplyr::select(
    c(
      GEOID,
      NAME
    )
  ) %>%
  subset(NAME %in% c("Normal", "Bloomington"))

crs <- sf::st_crs("+init=esri:102008 +lon_0=-89")

getMap <- function(
  sf,
  variable,
  title,
  city = "",
  proj = crs,
  n = 5,
  vals = "whole",
  classificationStyle = "jenks",
  palette = "seq",
  backgroundLayer = countyLayer,
  foregroundLayer = cityLayer
) {
  # 2019-12-19 TRRILEY - seems to break tmap plotting
  # require(tidycensus, tidyverse, tmap)
  formatDollarAmount <- function(x, digits = 0) {
    # helper function to format dollar amounts
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
  }
  tmap::tmap_mode("plot")
  if (stringr::str_length(sf$GEOID[1]) == 11) {
    geography = "Census Tracts"
  } else if (stringr::str_length(sf$GEOID[1]) == 12) {
    geography = "Census Block Groups"
  } else {
    geography = "ACS Estimates"
  }
  if (vals == "dollars") {
    legendFormat = list(fun = formatDollarAmount)
    legendTitle = paste("US Dollars,", acsYear)
  } else if (vals == "percent") {
    legendFormat = list(suffix = "%", digits = 2)
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else if (vals == "decimal") {
    legendFormat = list(digits = 2)
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else {
    legendFormat = list(digits = 0)
    legendTitle = paste(geography, acsYear, sep = ", ")
  }
  focusCityLayer <- subset(foregroundLayer, NAME == city)
  otherCityLayer <- subset(foregroundLayer, NAME != city)
  
  map <- tmap::tm_shape(
    backgroundLayer,
    bbox = focusCityLayer,
    projection = proj,
    unit = "mi"
  ) +
    tmap::tm_fill(col = "grey85") +
    tmap::tm_borders(col = "white", lwd = 2) +
    tmap::tm_shape(sf) +
    tmap::tm_fill(
      col = variable,
      n = n,
      style = classificationStyle,
      title = legendTitle,
      # legend.is.portrait = FALSE,
      palette = palette,
      contrast = c(0.3, 0.7)
    ) +
    tmap::tm_borders(col = "grey90", lwd = 1, alpha = .6) +
    tmap::tm_shape(foregroundLayer) +
    tmap::tm_text(
      text = "NAME",
      size = 1.1,
      shadow = T,
      case = "upper",
    ) +
    tmap::tm_shape(otherCityLayer) +
    tmap::tm_borders(col = "grey50", lwd = 1, alpha = .8) +
    tmap::tm_shape(focusCityLayer) +
    tmap::tm_borders(col = "grey20", lwd = 4, alpha = .8) +
    tmap::tm_layout(
      legend.title.size = 1.1,
      legend.title.fontface = "bold",
      legend.position = c("left", "top"),
      legend.bg.color = "white",
      legend.bg.alpha = .7,
      title = title,
      title.bg.color = "white",
      title.bg.alpha = .7,
      title.size = 1.2,
      title.fontface = "bold",
      fontfamily = "sans",
      legend.format = legendFormat,
      frame.lwd = 2,
      outer.bg.color = "#00000000"
    ) +
    tmap::tm_scale_bar(
      width = 0.2,
      text.size = .5
    ) +
    tmap::tm_credits(
      text = getACSYearsLabel(acsYear),
      size = .5,
      bg.color = "white",
      bg.alpha = .7
    )
  map
}

#test map
getMap(
  tractLayer,
  city = "Bloomington",
  variable = "medianGrossRent",
  title = "Median Gross Rent",
  vals = "dollars",
  palette = "YlOrRd"
)

# median housing value
medianHousingValueMapVariable <- "medianHousingValue"
medianHousingValueMapTitle <- "Median Housing Value"
medianHousingValueMapPalette <- "Reds"
# median housing value - bloomington
bloomington_medianHousingValueMap <- getMap(
  tractLayer,
  variable = medianHousingValueMapVariable,
  title = medianHousingValueMapTitle,
  city = "Bloomington",
  vals = "dollars",
  palette = medianHousingValueMapPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Median-Housing-Value.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# median housing value - normal
normal_medianHousingValueMap <- getMap(
  tractLayer,
  variable = medianHousingValueMapVariable,
  title = medianHousingValueMapTitle,
  city = "Normal",
  vals = "dollars",
  palette = medianHousingValueMapPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Median-Housing-Value.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )

# median gross rent
medianGrossRentVariable <- "medianGrossRent"
medianGrossRentTitle <- "Median Gross Rent"
medianGrossRentPalette <- "YlOrRd"
# median gross rent - bloomington
bloomington_medianGrossRentMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = medianGrossRentVariable,
  title = medianGrossRentTitle,
  vals = "dollars",
  palette = medianGrossRentPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Median-Gross-Rent.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# median gross rent - normal
normal_medianGrossRentMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = medianGrossRentVariable,
  title = medianGrossRentTitle,
  vals = "dollars",
  palette = medianGrossRentPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Median-Gross-Rent.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )

# percent white
percentWhiteVariable <- "percent_white"
percentWhiteTitle <- "Percent White"
percentWhitePalette <- "Blues"
# percent white - bloomington
bloomington_percentWhiteMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentWhiteVariable,
  title = percentWhiteTitle,
  vals = "percent",
  palette = percentWhitePalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-White.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent white - normal
normal_percentWhiteMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentWhiteVariable,
  title = percentWhiteTitle,
  vals = "percent",
  palette = percentWhitePalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-White.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )

# percent black
percentBlackVariable <- "percent_black"
percentBlackTitle <- "Percent Black"
percentBlackPalette <- "Blues"
# percent black - bloomington
bloomington_percentBlackMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentBlackVariable,
  title = percentBlackTitle,
  vals = "percent",
  palette = percentBlackPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-Black.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent black - normal
normal_percentBlackMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentBlackVariable,
  title = percentBlackTitle,
  vals = "percent",
  palette = percentBlackPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-Black.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent asian
percentAsianVariable <- "percent_asian"
percentAsianTitle <- "Percent Asian"
percentAsianPalette <- "Blues"
# percent asian - bloomington
bloomington_percentAsianMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentAsianVariable,
  title = percentAsianTitle,
  vals = "percent",
  palette = percentAsianPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-Asian.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent asian - normal
normal_percentAsianMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentAsianVariable,
  title = percentAsianTitle,
  vals = "percent",
  palette = percentAsianPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-Asian.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent hispanic
percentHispanicVariable <- "percent_hispanic"
percentHispanicTitle <- "Percent Hispanic"
percentHispanicPalette <- "Blues"
# percent hispanic - bloomington
bloomington_percentHispanicMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentHispanicVariable,
  title = percentHispanicTitle,
  vals = "percent",
  palette = percentHispanicPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-Hispanic.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent hispanic - normal
normal_percentHispanicMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentHispanicVariable,
  title = percentHispanicTitle,
  vals = "percent",
  palette = percentHispanicPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-Hispanic.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent renter occupied
percentRenterOccupiedVariable <- "percent_renterOccupied"
percentRenterOccupiedTitle <- "Percent Renter Occupied"
percentRenterOccupiedPalette <- "Greens"
# percent renter occupied - bloomington
bloomington_percentRenterOccupiedMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentRenterOccupiedVariable,
  title = percentRenterOccupiedTitle,
  vals = "percent",
  palette = percentRenterOccupiedPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-Renter-Occupied.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent renter occupied - normal
normal_percentRenterOccupiedMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentRenterOccupiedVariable,
  title = percentRenterOccupiedTitle,
  vals = "percent",
  palette = percentRenterOccupiedPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-Renter-Occupied.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent below poverty
percentBelowPovertyVariable <- "percent_belowPoverty"
percentBelowPovertyTitle <- "Percent Annual Income under Poverty"
percentBelowPovertyPalette <- "Greens"
# percent below poverty - bloomington
bloomington_percentBelowPovertyMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentBelowPovertyVariable,
  title = percentBelowPovertyTitle,
  vals = "percent",
  palette = percentBelowPovertyPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-Below-Poverty.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent below poverty - normal
normal_percentBelowPovertyMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentBelowPovertyVariable,
  title = percentBelowPovertyTitle,
  vals = "percent",
  palette = percentBelowPovertyPalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-Below-Poverty.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent housing 4 bedrooms or more
percentOccupiedUnits4orMoreVariable <- "percent_occupiedUnits4orMore"
percentOccupiedUnits4orMoreTitle <- "Percent 4 or more bedrooms"
percentOccupiedUnits4orMorePalette <- "Greens"
# percent housing 4 bedrooms or more - bloomington
bloomington_percentOccupiedUnits4orMoreMap <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentOccupiedUnits4orMoreVariable,
  title = percentOccupiedUnits4orMoreTitle,
  vals = "percent",
  palette = percentOccupiedUnits4orMorePalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-4-Plus-Bedrooms.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent housing 4 bedrooms or more - normal
normal_percentOccupiedUnits4orMoreMap <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentOccupiedUnits4orMoreVariable,
  title = percentOccupiedUnits4orMoreTitle,
  vals = "percent",
  palette = percentOccupiedUnits4orMorePalette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-4-Plus-Bedrooms.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent housing pre 1960
percentOccupiedUnitsPre1960Variable <- "percent_occupiedUnitsPre1960"
percentOccupiedUnitsPre1960Title <- "Percent Housing Units Built Before 1960"
percentOccupiedUnitsPre1960Palette <- "Greens"
# percent housing pre 1960 - bloomington
bloomington_percentOccupiedUnitsPre1960Map <- getMap(
  tractLayer,
  city = "Bloomington",
  variable = percentOccupiedUnitsPre1960Variable,
  title = percentOccupiedUnitsPre1960Title,
  vals = "percent",
  palette = percentOccupiedUnitsPre1960Palette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("COB-Tract_Percent-Housing-Pre-1960.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )
# percent housing pre 1960 - normal
normal_percentOccupiedUnitsPre1960Map <- getMap(
  tractLayer,
  city = "Normal",
  variable = percentOccupiedUnitsPre1960Variable,
  title = percentOccupiedUnitsPre1960Title,
  vals = "percent",
  palette = percentOccupiedUnitsPre1960Palette
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("TON-Tract_Percent-Housing-Pre-1960.pdf", acsYear),
      sep = "/"
    ),
    width = 8.5,
    height = 5.5
  )