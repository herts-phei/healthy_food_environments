graph_data <- function(data, indicator1, indicator2) {
  
  df <- data$table

  df <- df %>%
    dplyr::mutate(Value = round(Value, 2)) %>%
    dplyr::filter(IndicatorName %in% c(indicator1, indicator2)) %>%
    dplyr::select(IndicatorName, AreaName, Value) %>%
    dplyr::filter(!is.na(AreaName), !AreaName %in% c(
      "Hertfordshire", "Broxbourne", "Dacorum",
      "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
      "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"
    )) %>%
    tidyr::pivot_wider(names_from = IndicatorName, values_from = Value) %>%
    na.omit()

  return(df)
  
}

get_ind_name <- function(data, indicator) {
  
  df <- data$table

  ind <- df %>%
    dplyr::filter(IndicatorName %in% c(indicator)) %>%
    dplyr::distinct(IndicatorName) %>%
    dplyr::pull(IndicatorName)

  return(ind)
  
}
