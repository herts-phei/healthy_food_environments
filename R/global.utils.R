get_params <- function() {
  param <- list()

  pcd_ward <- readRDS("data/public/ward_lookup.rds")
  param$pcd <- pcd_ward %>%
    dplyr::pull(postcode)

  param$ward <- pcd_ward %>%
    dplyr::distinct(ward_name) %>%
    dplyr::pull(ward_name)

  return(param)
}


get_data <- function() {
  data <- list()

  ward_imd_lh <- readxl::read_excel("data/public/ward_imd.xlsx", skip = 3) # from local health needs updating from there

  food_settings <- readRDS("data/public/food_settings.rds")

  food_dist <- read.csv("data/public/AHAH_V3_0.csv") # from consumer data research needs updating from there

  fingertips_data <- readRDS("data/public/fingertips_data.rds")

  pcd_lookup <- readRDS("data/public/pcd_lookup.rds")

  pop <- readRDS("data/public/pop.rds")

  pop_ward <- readRDS("data/public/pop_ward.rds")

  lsoa_pop <- readRDS("data/public/pop_lsoa.rds")

  schools <- readRDS("data/public/schools.rds")

  pcd_ward_param <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::select(postcode, ward_code, ward_name)
  
  data$pcd_param <- pcd_ward_param %>%
    dplyr::pull(postcode)
  
  data$ward_param <- pcd_ward_param %>%
    dplyr::distinct(ward_name) %>%
    dplyr::pull(ward_name)
  

  data$pcd_ward <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::select(postcode, ward_code, ward_name, district_name, county)

  pop_hert <- pop %>%
    dplyr::filter(area_name == "Hertfordshire")

  pop_dist <- pop %>%
    dplyr::filter(area_name %in% c(
      "Broxbourne", "Dacorum",
      "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
      "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"
    ))
  ward_name <- pop_ward %>%
    dplyr::pull(ward_name)

  ward_name <- append(c(
    "Hertfordshire", "Broxbourne", "Dacorum",
    "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
    "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"
  ), ward_name)

  ward_imd <- pcd_lookup %>%
    dplyr::select(ward_code, ward_name, district_name, county) %>%
    dplyr::distinct(ward_name, .keep_all = TRUE) %>%
    dplyr::left_join(ward_imd_lh, by = c("ward_code" = "Code")) %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::arrange(desc(`Index of Multiple Deprivation (IMD) Score`)) %>%
    dplyr::mutate(
      rank = row_number(),
      Herts_quintile = case_when(
        rank >= 1 & rank < 37 ~ 1,
        rank >= 37 & rank < 73 ~ 2,
        rank >= 73 & rank < 109 ~ 3,
        rank >= 109 & rank < 145 ~ 4,
        rank >= 145 ~ 5
      )
    ) %>%
    dplyr::select(-Label, -rank) %>% # , -`Index of Multiple Deprivation (IMD) Score`) %>%
    tidyr::pivot_longer(cols = c(Herts_quintile, `Index of Multiple Deprivation (IMD) Score`), values_to = "Value", names_to = "IndicatorName") %>%
    select(AreaName = ward_name, AreaCode = ward_code, Value, IndicatorName, district_name)

  # fingertips --------------------------------------------------------------

  fingertips_data <- fingertips_data %>%
    dplyr::left_join(select(pop_ward, ward_code, district_name = la_name), by = c("AreaCode" = "ward_code"))

  # food settings -----------------------------------------------------------

  food_types <- food_settings %>%
    dplyr::select(type) %>%
    dplyr::distinct() %>%
    dplyr::mutate(order = seq(from = 1, to = (nrow(.))))

  # for school tab

  food_settings_school <- food_settings %>%
    dplyr::left_join(food_types, by = c("type")) %>%
    filter(type %in% c("Restaurant/Cafe/Canteen", "Retailers - supermarkets/hypermarkets", "Takeaway/sandwich shop", "Mobile caterer", "Pub/bar/nightclub", "Other catering premises"))

  schools <- schools %>%
    dplyr::left_join(pcd_lookup %>% dplyr::select(postcode, latitude, longitude), by = c("Postcode" = "postcode")) %>%
    dplyr::arrange(type) %>%
    dplyr::rename(postcode = Postcode) %>%
    dplyr::mutate(type = ifelse(is.na(type), "Unknown", type))

  school_types <- schools %>%
    dplyr::select(type) %>%
    dplyr::distinct() %>%
    dplyr::mutate(order = seq(from = nrow(food_types) + 1, to = (nrow(.) + nrow(food_types)))) # Start at 5 so that fast food always appears at the top

  schools <- schools %>%
    dplyr::left_join(school_types, by = c("type"))

  map_data <- bind_rows(food_settings_school, schools) %>%
    dplyr::mutate(id = seq(1:nrow(.))) %>%
    dplyr::arrange(order) %>%
    dplyr::mutate(type = factor(type, levels = dplyr::pull(., type) %>% unique())) %>%
    dplyr::mutate(radius = 400)

  map_data_2 <- map_data %>%
    dplyr::mutate(radius = 800)

  data$map_data <- map_data %>%
    dplyr::bind_rows(map_data_2) %>%
    dplyr::mutate(
      group = ifelse(type %in% school_types$type, "school", "food"),
      label = paste0(name, "<br/>", type)
    )

  # for overview tab
  chains <- c("mcdonald|kfc|burger king|five guys|subway|greggs|pizza hut|domino|taco bell|papa john")
  key_ff_terms <- c("burger|chicken|chip|fish bar|pizza|india|china|chinese")

  food_settings <- food_settings %>%
    filter(type %in% c("Takeaway/sandwich shop") | type %in% c(
      "Other catering premises", "Restaurant/Cafe/Canteen", "Retailers - other", "Retailers - supermarkets/hypermarkets",
      "School/college/university"
    ) & stringr::str_detect(tolower(name), chains) |
      type %in% c("Mobile caterer", "Other catering premises", "Restaurant/Cafe/Canteen") & stringr::str_detect(tolower(name), key_ff_terms))

  food_settings_herts <- food_settings %>%
    dplyr::mutate(AreaName = "Hertfordshire") %>%
    dplyr::group_by(AreaName) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(pop_hert, by = c("AreaName" = "area_name")) %>%
    PHEindicatormethods::phe_rate(count, pop, multiplier = 1000)

  food_settings_dist <- food_settings %>%
    dplyr::group_by(district_name) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(pop_dist, by = c("district_name" = "area_name")) %>%
    PHEindicatormethods::phe_rate(count, pop, multiplier = 1000) %>%
    dplyr::rename(AreaName = district_name)

  food_settings_table <- food_settings %>%
    dplyr::group_by(ward_name, district_name) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(pop_ward, by = c("ward_name" = "ward_name")) %>%
    dplyr::mutate(
      count = ifelse(is.na(count), 0, count), # for those with no fastfood put to 0
      district_name = ifelse(is.na(district_name), la_name, district_name)
    ) %>%
    PHEindicatormethods::phe_rate(count, pop, multiplier = 1000) %>%
    dplyr::rename(AreaName = ward_name) %>%
    dplyr::bind_rows(food_settings_dist, food_settings_herts) %>%
    dplyr::select(AreaName, Value = value, LowerCI95.0limit = lowercl, UpperCI95.0limit = uppercl, district_name) %>%
    dplyr::mutate(IndicatorName = "Fast food rate per 1000")



  # food_dist ---------------------------------------------------------------

  lsoa_lookup <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire")

  food_dist <- food_dist %>%
    dplyr::select(lsoa11, ah3ffood) %>%
    dplyr::filter(lsoa11 %in% lsoa_lookup$lsoa_code) %>%
    dplyr::mutate(indicator = "Average distance (min) to fast food") %>%
    dplyr::rename(lsoa_code = lsoa11)

  # function edited from Simon geography project
  fetch_indicator_weighted_geography <- function(data, indicators, geography_column = "lsoa_code", indicator_column = "indicator", value_column = "ah3ffood", nhs_level = "ward", geography_level = "lsoa", remove_na = FALSE) {
    # Control the inputs to ensure they are lower case
    nhs_level <- stringr::str_to_lower(nhs_level)
    geography_level <- stringr::str_to_lower(geography_level)

    # Put together a named vector of the join columns
    join_cols <- c(paste0(geography_level, "_code"))
    names(join_cols) <- geography_column

    # If our dataset is at anything other than LSOA level, we will need to do some weighting

    ward_lsoa_membership <- lsoa_pop %>%
      dplyr::left_join(select(pcd_lookup, lsoa_code, ward_code, district_code, district_name, county_name = county)) %>%
      dplyr::mutate(county_code = "E10000015") %>%
      dplyr::left_join(pop_ward, by = c("ward_code" = "ward_code")) %>%
      dplyr::filter(!is.na(la_name)) %>%
      dplyr::distinct()

    df <- data %>%
      dplyr::inner_join(ward_lsoa_membership, by = join_cols) %>%
      dplyr::filter(!is.na(!!dplyr::sym(paste0(nhs_level, "_code")))) %>%
      dplyr::filter(!!dplyr::sym(indicator_column) %in% indicators)

    # Calculate the population for the nhs_level we want
    level_pop <- ward_lsoa_membership %>%
      dplyr::group_by(!!dplyr::sym(paste0(nhs_level, "_code"))) %>%
      dplyr::summarise(level_total = sum(geography_total)) %>%
      dplyr::ungroup()

    df <- df %>%
      dplyr::mutate(geography_score = geography_total * !!dplyr::sym(value_column)) %>%
      dplyr::left_join(level_pop) %>%
      dplyr::group_by(!!dplyr::sym(paste0(nhs_level, "_name")), !!dplyr::sym(paste0(nhs_level, "_code")), !!dplyr::sym(indicator_column)) %>%
      dplyr::summarise(level_score = sum(geography_score) / level_total) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    return(df)
  }

  ward_district <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::distinct(ward_code, district_name)

  food_dist_ward <- fetch_indicator_weighted_geography(data = food_dist, indicators = "Average distance (min) to fast food") %>%
    dplyr::left_join(ward_district, by = c("ward_code" = "ward_code")) %>%
    dplyr::rename(AreaName = ward_name, AreaCode = ward_code)

  food_dist_district <- fetch_indicator_weighted_geography(data = food_dist, indicators = "Average distance (min) to fast food", nhs_level = "district") %>%
    dplyr::rename(AreaName = district_name, AreaCode = district_code)

  food_dist_county <- fetch_indicator_weighted_geography(data = food_dist, indicators = "Average distance (min) to fast food", nhs_level = "county") %>%
    dplyr::rename(AreaName = county_name, AreaCode = county_code)

  food_dist_ward_quintile <- food_dist_ward %>%
    dplyr::arrange(`level_score`) %>%
    dplyr::mutate(
      indicator = "Fast_food_dist_quintile", rank = row_number(),
      level_score = case_when(
        rank >= 1 & rank < 37 ~ 1,
        rank >= 37 & rank < 73 ~ 2,
        rank >= 73 & rank < 109 ~ 3,
        rank >= 109 & rank < 145 ~ 4,
        rank >= 145 ~ 5
      )
    )

  food_dist_table <- food_dist_county %>%
    dplyr::bind_rows(food_dist_ward, food_dist_district, food_dist_ward_quintile) %>%
    dplyr::select(AreaName, AreaCode, IndicatorName = indicator, Value = level_score, district_name)

  # joined_df ----------------------------------------------------------------

  data$table <- fingertips_data %>%
    dplyr::select(-TimeperiodSortable, -Timeperiod) %>%
    dplyr::bind_rows(ward_imd, food_settings_table, food_dist_table) %>%
    dplyr::mutate(AreaName = factor(AreaName, levels = ward_name))



  return(data)
}
