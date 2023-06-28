# UI ----------------------------------------------------------------------

tab_overview_mod <- function(id, label = "overview") {

  ns <- NS(id)

  tabItem(tabName = "overview",
    fluidRow(tablerDash::tablerCard(width = 12,
      fluidRow(width = 12, htmlOutput(ns("background"))),
      fluidRow(column(width = 6, align = "left", selectizeInput(ns("Postcode"), label = "Select area/s", choices = NULL, multiple = TRUE, selected = NULL,
        options = list(maxOptions = 11))),
      column(width = 6, align = "left", selectInput(ns("Comparison"), label = "Select comparator", choices = c("Hertfordshire", "District"), selected = "Hertfordshire")))),
    box(title = "Map", width = 6,
      align = "left",
      leaflet::leafletOutput(ns("map_healthy"), width = "100%", height = 640),
      fluidRow(column(width = 6, align = "left", imageOutput(ns("legend"), height = 85)))),
    box(title = "Table", width = 6,
      downloadButton(ns("exp_table"), "Export table", class = "butt"),
      tags$head(tags$style(".butt{background:#FFFFFF !important; border-color:#FFFFFF; -webkit-box-shadow: 0px; box-shadow: 0px;color: #0b27b4; 
      font-size: 13px;}")),
      reactableOutput(ns("table_overview"))),
    box(title = "Guidance", width = 12, collapsed = TRUE, htmlOutput(ns("data_guide"))))
  )

}

# Server ------------------------------------------------------------------

tab_overview_server <- function(id, overview_data) {

  moduleServer(

    id,

    function(input, output, session) {

      ns <- NS(id)

      rv_over <- reactiveValues()
      param <- get_params()
      pcd <- param$pcd
      ward <- param$ward

      updateSelectizeInput(session, 'Postcode', choices = list(All = "All",
        District = list("Broxbourne", "Dacorum",
          "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
          "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"),
        Ward = ward, Postcode = pcd),
      selected = "All", server = TRUE)

      observe({

        rv_over$filtered_table <- filtered_table_data(data = overview_data(),
          area = input$Postcode,
          comparator = input$Comparison)

      })

      output$background <- shiny::renderText({
        paste0("This application provides local insight into the food environment in Hertfordshire. It includes indicators on
        deprivation, fast food coverage and childhood obesity/overweight. It can be used to aid planning and
        to prioritise geographical areas to focus interventions in.<br><br>",

          "Select the wards/districts you are interested in by either typing a postcode, ward name or district
        into the area filter. Indicators for each wards can be compared to Hertfordshire or the district
        they are located using the comparator dropdown. The exception to this is Herts IMD quintile which is not compared
        to Hertfordshire or the districts.<br><br>")
      })

      output$data_guide <- shiny::renderText({
        paste0("<u>Guidance:</u><br><br>",
               
               "<B>Data updated:</B> 26/05/2023<br><br>",
               
               "<B>Wards:</B> The map uses 2021 ward names and bourndaries.<br><br>",
               
               "<B>Confidence Intervals and significance:</B> Confidence intervals (CIs) show the range of uncertainty (caused by sample size and random variation) 
               around a value and can be used to determine statistical significance. If the CIs around a value don't overlap with the CIs around another, 
               then we can be certain that there is a statistically significant difference between the values. If the CIs around a value overlap with the 
               interval around another, we cannot say with certainty that there is more than a chance difference between the two values. In the above table 
               95% CIs are used to determine if there is a significant difference between an areas value and the comparator areas value for prevalence of overweight and obesity 
               in year 6, prevalence of overweight and obesity in reception and fast food rate per 1000 population. Values which are significantly worse than the comparators value are highlighted in red, values 
               which are significantly better than the comparators value are highlighted in green and values with no significant difference are black.<br><br>",
               
               "<u>Indicator details:</u><br><br>",
               
               "<B>No. indicators sig worse than comparator/ IMD herts quintile 1:</B> This column shows the number of indicators for each ward
               which were significantly higher than the comparator and/or if the ward is in IMD quintile 1.
               This column is the basis for the shading in the adjacent map.<br><br>",
               
               "<B>Herts IMD deprivation quintile:</B> The Index of Multiple Deprivation (IMD) 2019 is an overall measure of
               relative deprivation. Hertfordshire is relatively less deprived than England so local deprivation quintiles
               have been used to highlight differences locally. Hertfordshire Wards have been ranked form most deprived to
               least deprived and then divided into quintiles. Quintile 1 is the most deprived quintile and quintile 5
               the least deprived.<br><br>",
               
               "<B>Prevalence of overweight and obesity in year 6 and reception:</B> These indicators shows the percentage of pupils
                who were classified as overweight or obese, it combines 3 years of data (2019/20-2021/22). 
                Values with numerators of less than 7 have been suppressed (*) and all numerators and 
               denominators have been rounded to the nearest 5.<br><br>",

          '<B>Fast food rate per 1000 population:</B> This indicator is the rate of fast food establishments per 1000 population.
           Fast food establishments include all establishments with the business type "Takeaway/sandwich". Additionally, 
           establishments with specific business types were their name included any of "burger", "chicken", "chip", 
           "fish bar", "pizza", "kebab", "india", "china" or "chinese" or if they were a major fast food outlet have also been included. This is consistent with the methodology
           used in the 2017 Public Health England analysis "Fast food outlets: density by local authority in England".<br><br>')
      })


      output$legend <- renderImage({

        list(src = "www/map_legend.png", height = "100%", width = "120%", contentType = 'image/png')

      }, deleteFile = FALSE)


      output$table_overview <- reactable::renderReactable({
        rv_over$filtered_table %>%
          reactable::reactable(defaultExpanded = FALSE, outlined = FALSE, striped = TRUE, highlight = TRUE,
            filterable = TRUE, resizable = TRUE, defaultPageSize = 11,
            defaultColDef = colDef(cell = function(value) {
              htmltools::div(style = list(fontSize = 12), value)
            }, header = function(value) {
              htmltools::div(style = list(fontSize = 12), value)
            }),
            columns = list(
              Herts_quintile = colDef(cell = function(value) {
                if (is.na(value)) {
                  na = "-"
                } else {
                  value
                }
              }, name = "Herts IMD deprivation quintile (1 most deprived)"),
              `Prevalence_overweight_and_obesity_Year_6` = colDef(cell = function(value, index) {
                colour <- rv_over$filtered_table$Diff_year_6[index]
                if (colour == "significantly higher than") {
                  htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
                }
                else if (colour == "significantly lower than") {
                  htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
                }
                else if (colour == "NA value") {
                  na = "*"
                }
                else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, html = TRUE, name = "Year 6: Prevalence of overweight and obesity"),
              `Prevalence_overweight_and_obesity_Reception` = colDef(cell = function(value, index) {
                colour <- rv_over$filtered_table$Diff_Reception[index]
                if (colour == "significantly higher than") {
                  htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
                }
                else if (colour == "significantly lower than") {
                  htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
                }
                else if (colour == "NA value") {
                  na = "*"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, html = TRUE, name = "Reception: Prevalence of overweight and obesity"),
              # `Prevalence_obesity_Year_6` = colDef(cell = function(value, index) {
              #   colour <- rv_over$filtered_table$Diff_year_6_obesity[index]
              #   if (colour == "significantly higher than") {
              #     htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
              #   }
              #   else if (colour == "significantly lower than") {
              #     htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
              #   }
              #   else if (colour == "NA value") {
              #     na = "*"
              #   } else {
              #     htmltools::div(style = list(fontSize = 12), value)
              #   }
              # }, html = TRUE, name = "Year 6: Prevalence of obesity"),
              # `Prevalence_obesity_Reception` = colDef(cell = function(value, index) {
              #   colour <- rv_over$filtered_table$Diff_Reception_obesity[index]
              #   if (colour == "significantly higher than") {
              #     htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
              #   }
              #   else if (colour == "significantly lower than") {
              #     htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
              #   }
              #   else if (colour == "NA value") {
              #     na = "*"
              #   } else {
              #     htmltools::div(style = list(fontSize = 12), value)
              #   }
              # }, html = TRUE, name = "Reception: Prevalence of obesity"),
              fast_food_rate = colDef(cell = function(value, index) {
                colour <- rv_over$filtered_table$fast_food_diff[index]
                if (colour == "significantly higher than") {
                  htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
                }
                else if (colour == "significantly lower than") {
                  htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
                }
                else if (colour == "NA value") {
                  na = "*"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, html = TRUE, name = "Fast food rate per 1000 population"),
              Diff_Reception = colDef(show = FALSE),
              Diff_year_6 = colDef(show = FALSE),
              Diff_Reception_obesity = colDef(show = FALSE),
              Diff_year_6_obesity = colDef(show = FALSE),
              AreaName = colDef(name = "Area", sticky = "left"),
              district_name = colDef(show = FALSE),
              fast_food_diff = colDef(show = FALSE),
              colour = colDef(show = FALSE),
              agg = colDef(name = "No. indicators sig worse than comparator/ IMD herts quintile 1"),
              label_text = colDef(show = FALSE),
              Prevalence_obesity_Year_6 = colDef(show = FALSE),
              Prevalence_obesity_Reception = colDef(show = FALSE)
              
            )
          )
      })
      
      output$exp_table <- downloadHandler(
        
        filename = "data.csv",
        content = function(con){
          
          data <- rv_over$filtered_table %>%
            dplyr::select("Area" = AreaName, "Herts IMD deprivation quintile (1 most deprived)" = Herts_quintile,
                          "Year 6: Prevalence of overweight and obesity" =`Prevalence_overweight_and_obesity_Year_6`,
                          "Reception: Prevalence of overweight and obesity" = Prevalence_overweight_and_obesity_Reception,
                          "Fast food rate per 1000 population" =fast_food_rate, "No. indicators sig worse than comparator/ IMD herts quintile 1"= agg, "District" = district_name)
            write.csv(data, con)
        }
        
      )

      output$map_healthy <- leaflet::renderLeaflet({

        shp_ward <- geojsonsf::geojson_sf("data/public/shp_ward.geojson") %>%
          dplyr::arrange(ward_name)

        shp_hert <- geojsonsf::geojson_sf("data/public/shp_hert.geojson")

        map_health_df <- rv_over$filtered_table %>%
          dplyr::filter(!AreaName %in% c("Hertfordshire", "Broxbourne", "Dacorum",
            "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
            "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers")) %>%
          dplyr::arrange(AreaName)

        map_health_df <- shp_ward %>%
          dplyr::right_join(map_health_df, by = c("ward_name" = "AreaName"))

        map_health_df %>%
          leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, minZoom = 7, maxZoom = 20, zoomControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          leaflet::addPolygons(data = shp_hert, weight = 2, opacity = 0.8, fillOpacity = 0, smoothFactor = 2, color = "grey") %>%
          leaflet::addPolygons(data = map_health_df, layerId = map_health_df$ward_name, label = paste(map_health_df$ward_name, "|", map_health_df$label_text), weight = 2, opacity = 0.8, fillOpacity = 0.8, color = map_health_df$colour, smoothFactor = 2) %>%
          leaflet.extras::addFullscreenControl(position = "topright") %>%
          htmlwidgets::onRender(
            "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }")

      })
    })
}
