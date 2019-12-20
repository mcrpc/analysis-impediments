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
  proj = crs,
  n = 7,
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
  map <- tmap::tm_shape(
    backgroundLayer,
    bbox = foregroundLayer,
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
      palette = palette
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
      legend.position = c("left", "top"),
      legend.title.size = 1.1,
      legend.title.fontface = "bold",
      title = title,
      title.size = 1.2,
      title.fontface = "bold",
      fontfamily = "sans",
      legend.format = legendFormat,
      frame.lwd = 2,
      outer.bg.color = "#00000000",
    ) +
    tmap::tm_scale_bar(
      width = 0.25,
      text.size = .5
    ) +
    tmap::tm_credits(
      text = getACSYearsLabel(acsYear),
      size = .5,
      bg.color = "white",
      bg.alpha = .5
    )
  map
}


medianHousingValueMap <- getMap(
  tractLayer,
  variable = "medianHousingValue",
  title = "Median Housing Value",
  vals = "dollars"
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("Tract_Median-Housing-Value.pdf", acsYear),
      sep = "/"
    )
  )

medianGrossRentMap <- getMap(
  tractLayer,
  variable = "medianGrossRent",
  title = "Median Gross Rent",
  vals = "dollars"
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("Tract_Median-Gross-Rent.pdf", acsYear),
      sep = "/"
    )
  )

percentWhiteMap <- getMap(
  tractLayer,
  variable = "percent_white",
  title = "Percent White",
  vals = "percent"
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("Tract_Percent-White.pdf", acsYear),
      sep = "/"
    )
  )

percentBlackMap <- getMap(
  tractLayer,
  variable = "percent_black",
  title = "Percent Black",
  vals = "percent"
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("Tract_Percent-Black.pdf", acsYear),
      sep = "/"
    )
  )

percentAsianMap <- getMap(
  tractLayer,
  variable = "percent_asian",
  title = "Percent Asian",
  vals = "percent"
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("Tract_Percent-Asian.pdf", acsYear),
      sep = "/"
    )
  )

percentHispanicMap <- getMap(
  tractLayer,
  variable = "percent_hispanic",
  title = "Percent Hispanic",
  vals = "percent"
) %T>%
  tmap::tmap_save(
    filename = paste(
      mapDirectory,
      addACSYearsToFilename("Tract_Percent-Hispanic.pdf", acsYear),
      sep = "/"
    )
  )


estimate_renterOccupied
estimate_belowPoverty
estimate_occupiedUnits4orMore
estimate_occupiedUnitsPre1960
denominator_renterOccupied
denominator_belowPoverty
denominator_occupiedUnits