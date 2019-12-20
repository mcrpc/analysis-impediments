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
  boundingBox <- if (city %in% c("Normal", "Bloomington")) {
    subset(foregroundLayer, NAME == city)
  } else {
    foregroundLayer
  }
  
  map <- tmap::tm_shape(
    backgroundLayer,
    bbox = boundingBox,
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
    tmap::tm_borders(col = "grey50", lwd = .5) +
    tmap::tm_shape(foregroundLayer) +
    tmap::tm_borders(col = "black", lwd = 2) +
    tmap::tm_text(
      text = "NAME",
      size = 1,
      shadow = T
    ) +
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
    )
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
    )
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
    )
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
    )
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
    )
  )
# percent white - normal


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
    )
  )
# percent black - normal

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
    )
  )
# percent asian - normal

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
    )
  )
# percent hispanic - normal

# percent renter occupied
# percent renter occupied - bloomington
# percent renter occupied - normal
# percent below poverty
# percent below poverty - bloomington
# percent below poverty - normal
# percent housing 4 units or more
# percent housing 4 units or more - bloomington
# percent housing 4 units or more - normal
# percent housing pre 1960
# percent housing pre 1960 - bloomington
# percent housing pre 1960 - normal