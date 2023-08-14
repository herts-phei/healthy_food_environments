#
# comparator ="Hertfordshire"
filtered_table_data <- function(data, area, comparator) {
  df <- data$table

  if (comparator == "District") {
    comparator_area <- c(
      "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
      "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"
    )

    comp_df <- df %>%
      dplyr::filter(AreaName %in% comparator_area)

    df <- df %>%
      dplyr::left_join(comp_df, by = c("IndicatorName" = "IndicatorName", "district_name" = "AreaName")) %>%
      dplyr::mutate(comp_area = district_name)
  } else {
    comp_df <- df %>%
      dplyr::filter(AreaName %in% comparator)

    df <- df %>%
      dplyr::left_join(comp_df, by = c("IndicatorName" = "IndicatorName")) %>%
      dplyr::rename(comp_area = AreaName.y, AreaName = AreaName.x, district_name = district_name.x)
  }

  df <- df %>%
    dplyr::mutate(
      diff = dplyr::case_when(
        LowerCI95.0limit.x > UpperCI95.0limit.y ~ "significantly higher than",
        UpperCI95.0limit.x < LowerCI95.0limit.y ~ "significantly lower than",
        is.na(Value.x) ~ "NA value",
        TRUE ~ "statistically similar"
      ),
      value = round(Value.x, 2),
      agg = case_when(
        diff == "significantly higher than" ~ 1,
        IndicatorName == "Herts_quintile" & value == "1" ~ 1,
        IndicatorName == "Fast_food_dist_quintile" & value == "1" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(IndicatorName, AreaName, district_name, value, diff, agg) %>%
    dplyr::filter(!is.na(AreaName)) %>% # fast food
    dplyr::filter(IndicatorName != "Index of Multiple Deprivation (IMD) Score") %>%
    tidyr::pivot_wider(names_from = IndicatorName, values_from = c(value, diff, agg)) %>%
    dplyr::rename(
      "Prevalence_overweight_and_obesity_Year_6" = `value_Year 6: Prevalence of overweight (including obesity), 3-years data combined`,
      "Prevalence_overweight_and_obesity_Reception" = `value_Reception: Prevalence of overweight (including obesity), 3-years data combined`,
      "Diff_year_6" = `diff_Year 6: Prevalence of overweight (including obesity), 3-years data combined`,
      "Diff_Reception" = `diff_Reception: Prevalence of overweight (including obesity), 3-years data combined`,
      "Prevalence_obesity_Year_6" = `value_Year 6: Prevalence of obesity (including severe obesity), 3-years data combined`,
      "Prevalence_obesity_Reception" = `value_Reception: Prevalence of obesity (including severe obesity), 3-years data combined`,
      "Diff_year_6_obesity" = `diff_Year 6: Prevalence of obesity (including severe obesity), 3-years data combined`,
      "Diff_Reception_obesity" = `diff_Reception: Prevalence of obesity (including severe obesity), 3-years data combined`,
      "Herts_quintile" = value_Herts_quintile,
      "fast_food_rate" = `value_Fast food rate per 1000`,
      "fast_food_diff" = `diff_Fast food rate per 1000`,
      "dist_fast_food" = `value_Average distance (min) to fast food`,
      "dist_fast_food_diff" = `diff_Average distance (min) to fast food`,
      "dist_fast_food_quintile" = value_Fast_food_dist_quintile
    ) %>%
    dplyr::mutate(
      fast_food_diff = ifelse(is.na(fast_food_diff), "NA value", fast_food_diff), # TODO diff in why this is NA no value vs supressed show differently as in should show as 0 pre 1000
      `agg_Fast food rate per 1000` = ifelse(is.na(`agg_Fast food rate per 1000`), 0, `agg_Fast food rate per 1000`),
      agg_Herts_quintile = ifelse(is.na(agg_Herts_quintile), 0, agg_Herts_quintile),
      agg_Fast_food_dist_quintile = ifelse(is.na(agg_Fast_food_dist_quintile), 0, agg_Fast_food_dist_quintile),
      agg =
        `agg_Fast food rate per 1000` + agg_Herts_quintile + agg_Fast_food_dist_quintile +
          `agg_Reception: Prevalence of overweight (including obesity), 3-years data combined` +
          `agg_Year 6: Prevalence of overweight (including obesity), 3-years data combined`,
      agg2 = `agg_Fast food rate per 1000` + agg_Fast_food_dist_quintile +
        `agg_Reception: Prevalence of overweight (including obesity), 3-years data combined` +
        `agg_Year 6: Prevalence of overweight (including obesity), 3-years data combined`,
      colour = case_when(
        agg < 1 ~ "#133959",
        agg < 2 ~ "#206095",
        agg < 3 ~ "#8FAFCA",
        agg < 4 ~ "#FFC9A5",
        agg < 5 ~ "#FE781F",
        agg >= 5 ~ "#D0021B",
        TRUE ~ "grey"
      ),
      agg = ifelse(AreaName == "Hertfordshire", "-", agg),
      label_text_temp = case_when(
        Herts_quintile == 1 ~ "Area is in the most deprived quintile. ",
        TRUE ~ ""
      ),
      label_text = case_when(
        agg2 == 1 ~ "1 indicator significantly worse than comparator/in quintile 1.",
        agg2 == 2 ~ "2 indicators significantly worse than comparator/in quintile 1.",
        agg2 == 3 ~ "3 indicators significantly worse than comparator/in quintile 1.",
        agg2 == 4 ~ "4 indicators significantly worse than comparator/in quintile 1.",
        TRUE ~ ""
      ),
      label_text2 = case_when(
        agg2 == 1 ~ "1 other indicator significantly worse than comparator/in quintile 1.",
        agg2 == 2 ~ "2 other indicators significantly worse than comparator/in quintile 1.",
        agg2 == 3 ~ "3 other indicators significantly worse than comparator/in quintile 1.",
        agg2 == 4 ~ "4 other indicators significantly worse than comparator/in quintile 1.",
        TRUE ~ ""
      ),
      label_text = ifelse(agg == 0, "No indicators significantly worse than comparator",
        ifelse(Herts_quintile == 1 & agg2 >= 1, paste0("Area is in the most deprived quintile and ", label_text2),
          paste0(label_text_temp, label_text)
        )
      )
    ) %>%
    dplyr::select(
      -agg2, -label_text_temp, -diff_Herts_quintile, -`agg_Year 6: Prevalence of obesity (including severe obesity), 3-years data combined`,
      -`agg_Fast food rate per 1000`, -agg_Herts_quintile,
      -`agg_Reception: Prevalence of obesity (including severe obesity), 3-years data combined`,
      -`agg_Reception: Prevalence of overweight (including obesity), 3-years data combined`,
      -`agg_Year 6: Prevalence of overweight (including obesity), 3-years data combined`, -label_text2,
      -`agg_Average distance (min) to fast food`, -agg_Fast_food_dist_quintile, -dist_fast_food_diff, -diff_Fast_food_dist_quintile
    ) %>%
    dplyr::select(AreaName, Herts_quintile, everything())

  if (comparator == "District") {
    df <- df %>%
      dplyr::mutate(agg = ifelse(AreaName %in% comparator_area, "-", agg))
  }

  if ("All" %in% area) {
    df <- df
  } else {
    lookup <- data$pcd_ward %>%
      dplyr::filter(`postcode` %in% area)

    lookup_ward <- data$pcd_ward %>%
      dplyr::select(ward_name, district_name) %>%
      dplyr::distinct() %>%
      dplyr::filter(ward_name %in% area)

    if (nrow(lookup) != 0) {
      df_pcd <- df %>%
        dplyr::filter(AreaName %in% lookup$ward_name | AreaName %in% lookup$district_name | AreaName %in% lookup$county)
    }

    df_ward_dist <- df %>%
      dplyr::filter(AreaName %in% area | district_name %in% area | AreaName == "Hertfordshire" | AreaName %in% lookup_ward$district_name)

    if (exists("df_pcd")) {
      df <- df_pcd %>%
        rbind(df_ward_dist) %>%
        dplyr::distinct()
    } else {
      df <- df_ward_dist
    }
  }

  return(df)
}

# data_map = df
# indicator = "Distance to nearest fast food outlet (min) quintiles"
filtered_map_data <- function(data_map, indicator) {
  if (indicator == "Herts IMD deprivation quintile") {
    df <- data_map %>%
      dplyr::select(AreaName, Indicator = Herts_quintile)
  }

  if (indicator == "Distance to nearest fast food outlet quintiles") {
    df <- data_map %>%
      dplyr::select(AreaName, Indicator = dist_fast_food_quintile)
  }


  if (indicator == "Year 6: Prevalence of overweight and obesity") {
    df <- data_map %>%
      dplyr::select(AreaName, Indicator = Prevalence_overweight_and_obesity_Year_6, Sig = Diff_year_6)
  }

  if (indicator == "Reception: Prevalence of overweight and obesity") {
    df <- data_map %>%
      dplyr::select(AreaName, Indicator = Prevalence_overweight_and_obesity_Reception, Sig = Diff_Reception)
  }

  if (indicator == "Fast food rate") {
    df <- data_map %>%
      dplyr::select(AreaName, Indicator = fast_food_rate, Sig = fast_food_diff)
  }

  if (indicator == "Herts IMD deprivation quintile") {
    df <- df %>%
      dplyr::mutate(
        colour = case_when(
          Indicator == 1 ~ "#bccfdf",
          Indicator == 2 ~ "#8fafca",
          Indicator == 3 ~ "#759CBB",
          Indicator == 4 ~ "#206095",
          Indicator == 5 ~ "#133959",
          TRUE ~ NA
        ),
        Sig = case_when(
          Indicator == 1 ~ "1 (most deprived)",
          Indicator == 2 ~ "2",
          Indicator == 3 ~ "3",
          Indicator == 4 ~ "4",
          Indicator == 5 ~ "5 (least deprived)",
          TRUE ~ NA
        ),
        label_text = paste0(" Quintile ", Sig)
      )
  } else if (indicator == "Distance to nearest fast food outlet quintiles") {
    df <- df %>%
      dplyr::mutate(
        colour = case_when(
          Indicator == 1 ~ "#bccfdf",
          Indicator == 2 ~ "#8fafca",
          Indicator == 3 ~ "#759CBB",
          Indicator == 4 ~ "#206095",
          Indicator == 5 ~ "#133959",
          TRUE ~ NA
        ),
        Sig = case_when(
          Indicator == 1 ~ "1 (lowest distance)",
          Indicator == 2 ~ "2",
          Indicator == 3 ~ "3",
          Indicator == 4 ~ "4",
          Indicator == 5 ~ "5 (highest distance)",
          TRUE ~ NA
        ),
        label_text = paste0(" Quintile ", Sig)
      )
  } else {
    df <- df %>%
      dplyr::mutate(
        colour = case_when(
          Sig == "significantly higher than" ~ "#e00000",
          Sig == "statistically similar" ~ "#F6BE00",
          Sig == "significantly lower than" ~ "green",
          TRUE ~ "grey"
        ),
        Sig = case_when(
          Sig == "significantly higher than" ~ "significantly higher than comparator",
          Sig == "statistically similar" ~ "statistically similar to comparator",
          Sig == "significantly lower than" ~ "significantly lower than comparator",
          TRUE ~ "value suppressed"
        ),
        label_text = paste0(" Value: ", Indicator, " | ", Sig)
      )
  }
}
