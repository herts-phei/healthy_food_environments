get_params <- function() {

  param <- list()

  pcd_lookup <- readxl::read_excel("data/public/pcd_lookup.xlsx")

  pcd_ward <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::select(postcode, ward_code, ward_name)

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

  food_settings <- readxl::read_excel("data/public/food_settings.xlsx")

  fingertips_data <- readxl::read_excel("data/public/fingertips_data.xlsx")

  pcd_lookup <- readxl::read_excel("data/public/pcd_lookup.xlsx")

  pop <- readxl::read_excel("data/public/pop.xlsx")

  pop_ward <- readxl::read_excel("data/public/pop_ward.xlsx")

  schools <- readxl::read_excel("data/public/schools.xlsx")

  data$pcd_ward <- pcd_lookup %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::select(postcode, ward_code, ward_name, district_name, county)

  pop_hert <- pop %>%
    dplyr::filter(area_name == "Hertfordshire")

  pop_dist <- pop %>%
    dplyr::filter(area_name %in% c("Broxbourne", "Dacorum",
      "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
      "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"))
  ward_name <- pop_ward %>%
    dplyr::pull(ward_name)

  ward_name <- append(c("Hertfordshire", "Broxbourne", "Dacorum",
    "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
    "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"), ward_name)

  ward_imd <- pcd_lookup %>%
    dplyr::select(ward_code, ward_name, district_name, county) %>%
    dplyr::distinct(ward_name, .keep_all = TRUE) %>%
    dplyr::left_join(ward_imd_lh, by = c("ward_code" = "Code")) %>%
    dplyr::filter(county == "Hertfordshire") %>%
    dplyr::arrange(desc(`Index of Multiple Deprivation (IMD) Score`)) %>%
    dplyr::mutate(rank = row_number(),
      Herts_quintile = case_when(rank >= 1 & rank < 37 ~ 1,
        rank >= 37 & rank < 73 ~ 2,
        rank >= 73 & rank < 109 ~ 3,
        rank >= 109 & rank < 145 ~ 4,
        rank >= 145 ~ 5)) %>%
    dplyr::select(-Label, -rank, -`Index of Multiple Deprivation (IMD) Score`) %>%
    tidyr::pivot_longer(cols = Herts_quintile, values_to = "Value", names_to = "IndicatorName") %>%
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
    dplyr::mutate(group = ifelse(type %in% school_types$type, "school", "food"),
      label = paste0(name, "<br/>", type))

  # for overview tab
  chains <- c("mcdonald|kfc|burger king|five guys|subway|greggs|pizza hut|domino|taco bell|papa john")
  key_ff_terms <- c("burger|chicken|chip|fish bar|pizza|india|china|chinese")

  food_settings <- food_settings %>%
    filter(type %in% c("Takeaway/sandwich shop") | type %in% c("Other catering premises", "Restaurant/Cafe/Canteen", "Retailers - other", "Retailers - supermarkets/hypermarkets",
      "School/college/university") & stringr::str_detect(tolower(name), chains) |
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
    dplyr::mutate(count = ifelse(is.na(count), 0, count), # for those with no fastfood put to 0
      district_name = ifelse(is.na(district_name), la_name, district_name)) %>%
    PHEindicatormethods::phe_rate(count, pop, multiplier = 1000) %>%
    dplyr::rename(AreaName = ward_name) %>%
    dplyr::bind_rows(food_settings_dist, food_settings_herts) %>%
    dplyr::select(AreaName, Value = value, LowerCI95.0limit = lowercl, UpperCI95.0limit = uppercl, district_name) %>%
    dplyr::mutate(IndicatorName = "Fast food rate per 1000")

  data$table <- fingertips_data %>%
    dplyr::select(-TimeperiodSortable, -Timeperiod) %>%
    dplyr::bind_rows(ward_imd, food_settings_table) %>%
    dplyr::mutate(AreaName = factor(AreaName, levels = ward_name))


  return(data)

}
