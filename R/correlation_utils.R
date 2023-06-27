
graph_data <- function(data, indicator1, indicator2){
  
  df <- data$table

  df <- df %>%
    dplyr::mutate(Value = round(Value, 2),
                  IndicatorName = ifelse(IndicatorName == "Year 6: Prevalence of obesity (including severe obesity), 3-years data combined", "Year 6: Prevalence of overweight & obesity",
                                         ifelse(IndicatorName == "Reception: Prevalence of overweight (including obesity), 3-years data combined","Reception: Prevalence of overweight & obesity",
                                                IndicatorName))) %>%
    dplyr::filter(IndicatorName %in% c(indicator1, indicator2)) %>%
    dplyr::select(IndicatorName, AreaName, Value) %>%
    dplyr::filter(!is.na(AreaName), !AreaName %in% c("Hertfordshire", "Broxbourne", "Dacorum",
                                                     "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                                                     "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers")) %>%
    tidyr::pivot_wider(names_from = IndicatorName, values_from = Value) %>%
    na.omit()
  
  return(df)
}
# indicator = "Year 6: Prevalence of overweight and obesity (3 years data)"
get_ind_name <- function(data, indicator){

  df <- data$table

  ind <- df %>%
    dplyr::mutate(IndicatorName = ifelse(IndicatorName == "Year 6: Prevalence of obesity (including severe obesity), 3-years data combined", "Year 6: Prevalence of overweight & obesity",
                                         ifelse(IndicatorName == "Reception: Prevalence of overweight (including obesity), 3-years data combined","Reception: Prevalence of overweight & obesity",
                                                IndicatorName))) %>%
    dplyr::filter(IndicatorName %in% c(indicator)) %>%
    dplyr::distinct(IndicatorName) %>%
    dplyr::pull(IndicatorName)

  return(ind)
}
