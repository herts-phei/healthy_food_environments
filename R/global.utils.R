get_params <- function() {
  param <- list()

  pcd_msoa <- readRDS("data/public/msoa_lookup.rds")

  param$pcd <- pcd_msoa %>%
    dplyr::pull(postcode)

  msoa_names <- readRDS("data/public/msoa_data.rds")

  param$msoa <- msoa_names %>%
    dplyr::pull(MSOA_name)

  param$msoa_names <- msoa_names

  return(param)
}


get_data <- function() {
  data <- list()

  msoa_names <- readRDS("data/public/msoa_data.rds")

  msoa_imd <- readRDS("data/public/msoa_imd.rds")

  food_settings <- readRDS("data/public/food_settings.rds")

  food_dist <- read.csv("data/public/AHAH_V4.csv") # from https://data.cdrc.ac.uk/dataset/access-healthy-assets-hazards-ahah

  ncmp_data <- readRDS("data/public/ncmp_data.rds")

  pcd_lookup <- readRDS("data/public/pcd_lookup.rds")

  pop <- readRDS("data/public/pop.rds")

  pop_msoa <- readRDS("data/public/pop_msoa.rds")

  lsoa_pop <- readRDS("data/public/pop_lsoa.rds")

  schools <- readRDS("data/public/schools.rds")

  data$pcd_param <- pcd_lookup %>%
    dplyr::pull(postcode)

  data$msoa_param <- pcd_lookup %>%
    dplyr::distinct(MSOA_name) %>%
    dplyr::pull(MSOA_name)

  data$pcd_msoa <- pcd_lookup %>%
    dplyr::select(postcode, msoa_code, MSOA_name, district_name, county)

  pop_hert <- pop %>%
    dplyr::filter(area_name == "Hertfordshire")

  pop_dist <- pop %>%
    dplyr::filter(area_name %in% c(
      "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
      "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield"
    ))

  msoa_name <- pcd_lookup %>%
    dplyr::distinct(MSOA_name) %>%
    dplyr::pull(MSOA_name)

  msoa_imd_quintile <- msoa_imd |>
    dplyr::rename(`Index of Multiple Deprivation (IMD) Score` = imd_score) |>
    tidyr::pivot_longer(
      cols = c(Herts_quintile, `Index of Multiple Deprivation (IMD) Score`),
      values_to = "Value",
      names_to = "IndicatorName"
    ) %>%
    select(AreaName = MSOA_name, AreaCode = msoa_code, Value, IndicatorName, district_name)


  # ncmp -----------------------------------------------------------------------
  ncmp_data <- ncmp_data |>
    dplyr::left_join(select(pop_msoa, msoa_code, district_name),
      by = c("AreaCode" = "msoa_code"),
      relationship = "many-to-many"
    )


  # food settings --------------------------------------------------------------
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
    dplyr::group_by(MSOA_name, district_name) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(pop_msoa, by = c("MSOA_name", "district_name"), relationship = "many-to-many") %>%
    dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
    PHEindicatormethods::phe_rate(count, pop, multiplier = 1000) %>%
    dplyr::rename(AreaName = MSOA_name) %>%
    dplyr::bind_rows(food_settings_dist, food_settings_herts) %>%
    dplyr::select(AreaName, Value = value, LowerCI95.0limit = lowercl, UpperCI95.0limit = uppercl, district_name) %>%
    dplyr::mutate(IndicatorName = "Fast food rate per 1000")


  # food_dist ------------------------------------------------------------------
  lsoa_lookup <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire")

  food_dist <- food_dist %>%
    dplyr::select(LSOA21CD, ah4ffood) %>%
    dplyr::filter(LSOA21CD %in% lsoa_lookup$lsoa_code) %>%
    dplyr::mutate(indicator = "Average distance (min) to fast food") %>%
    dplyr::rename(lsoa_code = LSOA21CD)

  # function edited from Simon geography project
  fetch_indicator_weighted_geography <- function(data,
                                                 indicators,
                                                 geography_column = "lsoa_code",
                                                 indicator_column = "indicator",
                                                 value_column = "ah4ffood",
                                                 nhs_level = "msoa",
                                                 geography_level = "lsoa",
                                                 remove_na = FALSE) {
    # Control the inputs to ensure they are lower case
    nhs_level <- stringr::str_to_lower(nhs_level)
    geography_level <- stringr::str_to_lower(geography_level)

    # Put together a named vector of the join columns
    join_cols <- c(paste0(geography_level, "_code"))
    names(join_cols) <- geography_column

    # If our dataset is at anything other than LSOA level, we will need to do some weighting

    msoa_lsoa_membership <- lsoa_pop %>%
      dplyr::left_join(
        select(pcd_lookup, lsoa_code, msoa_code, msoa_name,
          district_code, district_name,
          county_name = county
        ),
        by = "lsoa_code"
      ) %>%
      dplyr::mutate(county_code = "E10000015") %>%
      dplyr::left_join(pop_msoa, by = c("msoa_code", "district_name")) %>%
      dplyr::filter(!is.na(district_name)) %>%
      dplyr::distinct()

    df <- data %>%
      dplyr::inner_join(msoa_lsoa_membership, by = join_cols) %>%
      dplyr::filter(!is.na(!!dplyr::sym(paste0(nhs_level, "_code")))) %>%
      dplyr::filter(!!dplyr::sym(indicator_column) %in% indicators)

    # Calculate the population for the nhs_level we want
    level_pop <- msoa_lsoa_membership %>%
      dplyr::group_by(!!dplyr::sym(paste0(nhs_level, "_code"))) %>%
      dplyr::summarise(level_total = sum(geography_total)) %>%
      dplyr::ungroup()

    df <- df %>%
      dplyr::mutate(geography_score = geography_total * !!dplyr::sym(value_column)) %>%
      dplyr::left_join(level_pop) %>%
      dplyr::group_by(
        !!dplyr::sym(paste0(nhs_level, "_name")),
        !!dplyr::sym(paste0(nhs_level, "_code")),
        !!dplyr::sym(indicator_column)
      ) %>%
      dplyr::summarise(level_score = sum(geography_score) / level_total) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    return(df)
  }

  msoa_district <- pcd_lookup %>%
    dplyr::distinct(msoa_code, msoa_name, MSOA_name, district_name)

  food_dist_msoa <- fetch_indicator_weighted_geography(
    data = food_dist,
    indicators = "Average distance (min) to fast food"
  ) %>%
    dplyr::left_join(msoa_district, by = c("msoa_code" = "msoa_code", "msoa_name" = "msoa_name")) %>%
    dplyr::rename(AreaName = MSOA_name, AreaCode = msoa_code)

  food_dist_district <- fetch_indicator_weighted_geography(
    data = food_dist,
    indicators = "Average distance (min) to fast food",
    nhs_level = "district"
  ) %>%
    dplyr::rename(AreaName = district_name, AreaCode = district_code)

  food_dist_county <- fetch_indicator_weighted_geography(
    data = food_dist,
    indicators = "Average distance (min) to fast food",
    nhs_level = "county"
  ) %>%
    dplyr::rename(AreaName = county_name, AreaCode = county_code)

  food_dist_msoa_quintile <- food_dist_msoa %>%
    dplyr::select(-AreaName) |>
    dplyr::left_join(msoa_names, by = c("AreaCode" = "msoa_code", "msoa_name" = "msoa_name")) |>
    dplyr::rename(AreaName = MSOA_name) |>
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
    dplyr::bind_rows(food_dist_msoa, food_dist_district, food_dist_msoa_quintile) %>%
    dplyr::select(AreaName, AreaCode, IndicatorName = indicator, Value = level_score, district_name)


  # joined_df ------------------------------------------------------------------
  data$table <- ncmp_data %>%
    dplyr::bind_rows(msoa_imd_quintile, food_settings_table, food_dist_table) %>%
    dplyr::distinct() |>
    dplyr::mutate(AreaName = factor(AreaName, levels = c(
      "Hertfordshire", "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
      "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield", msoa_name
    )))

  return(data)
}
