school_map_filtered <- function(data, school, food, distance) {

  map_data <- list()

  map_data$df_all <- data$map_data %>%
    dplyr::filter(type %in% c(school, food), radius == distance)

  map_data$df_sch <- data$map_data %>%
    dplyr::filter(type %in% c(school, food), radius == distance) %>%
    dplyr::filter(group == "school")

  map_data$df_food <- data$map_data %>%
    dplyr::filter(type %in% c(school, food), radius == distance) %>%
    dplyr::filter(group == "food")

  return(map_data)

}

school_map_params <- function(data) {

  school_params <- list()

  school_params$school_type <- data$map_data %>%
    dplyr::filter(group == "school") %>%
    dplyr::mutate(type = as.character(type)) %>%
    dplyr::distinct(type) %>%
    dplyr::pull(type)

  school_params$food_type <- data$map_data %>%
    dplyr::filter(group == "food") %>%
    dplyr::mutate(type = as.character(type)) %>%
    dplyr::distinct(type) %>%
    dplyr::pull(type)

  school_params$pal_data <- data$map_data

  return(school_params)

}

leaflet_map <- function(data_full, data_sch, data_food, domain) {

  shp_ltla <- geojsonsf::geojson_sf("data/public/ltla_cut.geojson")

  # Generate colour palettes - one for schools, one for fast food 
  fast_food_pal <- c("#133959", "#206095", "#4682b4", "#66b2ff", "#6082b6", "#8fafca", "#bccfdf") 

  school_pal <- c("#66C2A5", "#B2DF8A", "#33A02C", "#E78AC3", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#bdbdbd") %>% 
    replace(c(9, 10), .[c(10, 9)])
  pal_settings <- leaflet::colorFactor(palette = append(fast_food_pal, school_pal), domain = domain$type)
  map <- data_full %>%
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, minZoom = 7, maxZoom = 20)) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    leaflet::addPolygons(data = shp_ltla, label = ~lad20nm, weight = 2, opacity = 0.8, fillOpacity = 0, color = "grey", smoothFactor = 2, group = "LTLA") %>%
    leaflet::setView(lng = -0.237700, lat = 51.809800, zoom = 9.5) %>%
    leaflet::addLegend(position = "topright", values = ~type, pal = pal_settings, title = "Setting Types") %>%
    leaflet.extras::addFullscreenControl(position = "topleft") %>%
    leaflet::addMeasure(position = "topleft")

  if (nrow(data_sch) != 0 & nrow(data_food) != 0) {

    map <- map %>%
      leaflet::addCircles(data = data_food, lng = ~longitude, lat = ~latitude, color = ~ pal_settings(type), label = ~name, group = ~type, layerId = ~id, popup = ~label, radius = 400) %>%
      leaflet::addCircles(data = data_sch, lng = ~longitude, lat = ~latitude, color = ~ pal_settings(type), group = ~type, radius = ~radius, layerId = ~id, label = ~name, popup = ~label)

  } else if (nrow(data_sch) == 0) {

    map <- map %>%
      leaflet::addCircles(data = data_food, lng = ~longitude, lat = ~latitude, color = ~ pal_settings(type), label = ~name, group = ~type, layerId = ~id, popup = ~label, radius = 400)

  } else {

    map <- map %>%
      leaflet::addCircles(data = data_sch, lng = ~longitude, lat = ~latitude, color = ~ pal_settings(type), group = ~type, radius = ~radius, layerId = ~id, label = ~name, popup = ~label)

  }

  return(map)

}
