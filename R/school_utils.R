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

  fast_food_pal <- c("#BBF870", "#70AE26",  "#718224", "#89DBFF", "#38A9DB", "#0067A3") 

  school_pal <- c("#66C2A5", "#008080", "#E78AC3", "#FB9A99", "#E31A1C", "#800000", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#bdbdbd") %>% 
    replace(c(9, 10), .[c(10, 9)])
  
  pal_settings <- leaflet::colorFactor(palette = append(fast_food_pal, school_pal), domain = domain$type)
  
  map <- data_full %>%
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, minZoom = 7, maxZoom = 18)) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    leaflet::addPolygons(data = shp_ltla, label = ~lad20nm, weight = 2, opacity = 0.8, fillOpacity = 0, color = "grey", smoothFactor = 2, group = "LTLA") %>%
    leaflet::setView(lng = -0.237700, lat = 51.809800, zoom = 9.5) %>%
    leaflet::addLegend(position = "topright", values = ~type, pal = pal_settings, title = "Setting Types") %>%
    leaflet.extras::addFullscreenControl(position = "topleft") %>%
    leaflet::addMeasure(position = "topleft")
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = case_when(data_food$type == "Mobile caterer" ~ 'lightgreen',#orange, purple, pink, red
                            data_food$type == "Other catering premises" ~ 'green',
                            data_food$type == "Pub/bar/nightclub" ~ 'darkgreen',
                            data_food$type == "Restaurant/Cafe/Canteen" ~'lightblue' ,
                            data_food$type == "Retailers - supermarkets/hypermarkets" ~ 'blue',
                            data_food$type == "Takeaway/sandwich shop" ~ "darkblue")
  )
  
  
  if (nrow(data_sch) != 0 & nrow(data_food) != 0) {

    map <- map %>%
      leaflet::addAwesomeMarkers(data = data_food, lng = ~longitude, lat = ~latitude, icon=icons, label= ~name, clusterOptions = markerClusterOptions()) %>%
      leaflet::addCircles(data = data_sch, lng = ~longitude, lat = ~latitude, color = ~ pal_settings(type), group = ~type, radius = ~radius, layerId = ~id, label = ~name, popup = ~label)

  } else if (nrow(data_sch) == 0) {

    map <- map %>%
      leaflet::addAwesomeMarkers(data = data_food, lng = ~longitude, lat = ~latitude, icon=icons, label= ~name, clusterOptions = markerClusterOptions()) 
    
  } else {

    map <- map %>%
      leaflet::addCircles(data = data_sch, lng = ~longitude, lat = ~latitude, color = ~ pal_settings(type), group = ~type, radius = ~radius, layerId = ~id, label = ~name, popup = ~label)

  }

  return(map)

}
