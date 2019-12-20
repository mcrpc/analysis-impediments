acsVariableList <- c(
  medianHousingValue <- "B25077_001E",
  medianGrossRent <- "B25064_001E",
  estimate_renterOccupied <- "B25003_003E",
  estimate_belowPoverty <- "S1701_C02_001E",
  estimate_occupiedUnits4orMore <- "S2504_C01_024E",
  estimate_occupiedUnitsPre1960 <- c("S2504_C01_014E", "S2504_C01_015E"),
  denominator_renterOccupied <- "B25003_001E",
  denominator_belowPoverty <- "S1701_C01_001E",
  denominator_occupiedUnits <- "S2504_C01_001E",
  percent_white <- "DP05_0064PE",
  percent_black <- "DP05_0065PE",
  percent_asian <- "DP05_0067PE",
  percent_hispanic <- "DP05_0071PE"
)

analysisMapData <- acsDataTibble %>%
  dplyr::transmute(
    GEOID = GEOID,
    NAME = NAME,
    medianHousingValue = rowSums(.[names(.) %in% medianHousingValue]),
    medianGrossRent = rowSums(.[names(.) %in% medianGrossRent]),
    estimate_renterOccupied = rowSums(.[names(.) %in% estimate_renterOccupied]),
    estimate_belowPoverty = rowSums(.[names(.) %in% estimate_belowPoverty]),
    estimate_occupiedUnits4orMore = rowSums(.[names(.) %in% estimate_occupiedUnits4orMore]),
    estimate_occupiedUnitsPre1960 = rowSums(.[names(.) %in% estimate_occupiedUnitsPre1960]),
    denominator_renterOccupied = rowSums(.[names(.) %in% denominator_renterOccupied]),
    denominator_belowPoverty = rowSums(.[names(.) %in% denominator_belowPoverty]),
    denominator_occupiedUnits = rowSums(.[names(.) %in% denominator_occupiedUnits]),
    percent_white = rowSums(.[names(.) %in% percent_white]),
    percent_black = rowSums(.[names(.) %in% percent_black]),
    percent_asian = rowSums(.[names(.) %in% percent_asian]),
    percent_hispanic = rowSums(.[names(.) %in% percent_hispanic]),
    percent_occupiedUnits4orMore = estimate_occupiedUnits4orMore / denominator_occupiedUnits * 100,
    percent_occupiedUnitsPre1960 = estimate_occupiedUnitsPre1960 / denominator_occupiedUnits * 100,
    percent_renterOccupied = estimate_renterOccupied / denominator_renterOccupied * 100,
    percent_belowPoverty = estimate_belowPoverty / denominator_belowPoverty * 100
  )

