# UI ----------------------------------------------------------------------

tab_overview_mod <- function(id, label = "overview") {
  ns <- NS(id)

  tabItem(
    tabName = "overview",

    # ---- CSS: Nudge the legend up and left (Option A only) ----
    tags$head(
      tags$style(HTML("
        .overview-legend-wrap {
          margin-top: -16px;   /* move up: increase magnitude for more shift (e.g., -30px) */
          margin-left: -12px;  /* move left: use negative to go left, positive to go right */
          /* If you prefer smoother movement, you can use:
             transform: translate(-12px, -20px);
             but stick with margins if you want the element to keep its layout space. */
        }
      "))
    ),
    fluidRow(
      tablerDash::tablerCard(
        width = 12,
        fluidRow(width = 12, htmlOutput(ns("background"))),
        fluidRow(
          column(
            width = 6, align = "left",
            selectizeInput(
              ns("Postcode"),
              label = "Select area/s", choices = NULL, multiple = TRUE, selected = "All",
              options = list(maxOptions = 11)
            )
          ),
          column(
            width = 6, align = "left",
            selectInput(
              ns("Comparison"),
              label = "Select comparator",
              choices = c("Hertfordshire", "District"),
              selected = "Hertfordshire"
            )
          )
        )
      ),
      box(
        title = "Map", width = 6, align = "left",
        fluidRow(
          column(
            width = 5, align = "left",
            selectInput(
              ns("Indicators"),
              label = "Select Indicator",
              choices = c(
                "No. indicators sig worse than comparator/quintile 1",
                "Herts IMD deprivation quintile",
                "Year 6: Prevalence of overweight and obesity",
                "Reception: Prevalence of overweight and obesity",
                "Fast food rate",
                "Distance to nearest fast food outlet quintiles"
              ),
              selected = "No. indicators sig worse than comparator/quintile 1"
            )
          ),
          # ---- Legend column wrapped for easy positioning (Option A) ----
          column(
            width = 6,
            div(class = "overview-legend-wrap", uiOutput(ns("legend")))
          )
        ),
        leaflet::leafletOutput(ns("map_healthy"), width = "100%", height = 640),
        fluidRow(column(width = 12, align = "left", uiOutput(ns("imd_text"))))
      ),
      box(
        title = "Table", width = 6,
        downloadButton(ns("exp_table"), "Export table", class = "butt"),
        tags$head(tags$style(".butt{background:#FFFFFF !important; border-color:#FFFFFF; -webkit-box-shadow: 0px; box-shadow: 0px;color: #0b27b4;
      font-size: 13px;}")),
        reactableOutput(ns("table_overview"))
      ),
      box(title = "Indicator guidance", width = 12, collapsed = TRUE, htmlOutput(ns("data_guide")))
    )
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
      msoa <- param$msoa

      updateSelectizeInput(session, "Postcode",
        choices = list(
          All = "All",
          District = list(
            "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
            "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"
          ),
          MSOA = msoa, Postcode = pcd
        ),
        selected = "All", server = TRUE
      )

      observe({
        rv_over$filtered_table <- filtered_table_data(
          data = overview_data(),
          area = input$Postcode,
          comparator = input$Comparison
        )
      })

      output$background <- shiny::renderText({
        paste0(
          "This application provides local insight into the food environment in Hertfordshire. It includes indicators on
        deprivation, fast food coverage and childhood obesity/overweight. It can be used to aid planning and
        prioritise geographical areas to focus interventions in.<br><br>",
          'Select the area of interest by entering a postcode, MSOA name or district into the area filter. Indicators for each MSOA can be compared to Hertfordshire or the district
        they are located in using the comparator dropdown. The exceptions to this are "Herts IMD quintile" and "Distance to nearest fast food outlet quintiles" which are not compared
        to Hertfordshire or the districts. Information on the indicators can be accessed by expanding the "Indicator Guidance" section below.<br><br>'
        )
      })

      output$data_guide <- shiny::renderText({
        paste0(
          "<B>Data updated:</B> 02/12/2025<br><br>",
          "<B>MSOAs:</B> MSOA's are medium sized geographic areas, with 5,000-15,000 residents. MSOA's can be grouped in Local Authorities. The MSOA's
          used in this report are from 2021 ONS census. <br><br>",
          "<B>Herts IMD deprivation quintile:</B> The Index of Multiple Deprivation (IMD) 2025 is an overall measure of
          relative deprivation. Hertfordshire is relatively less deprived than England so local deprivation quintiles
          have been used to highlight differences locally. Hertfordshire MSOAs have been ranked from most deprived to
          least deprived and then divided into quintiles. Quintile 1 is the most deprived quintile and quintile 5
          the least deprived.<br><br>",
          "<B>Prevalence of overweight and obesity in Reception and Year 6:</B> These indicators shows the percentage of pupils
          who were classified as overweight or obese, it combines 3 years of data (2022/23 - 2024/25). Values highlighted in red
          have a significantly higher prevalence than the comparator and those highlighted in green have a significantly lower prevalence than the comparator.
          Values based on a numerator that is 7 or less have been suppressed (*) and all numerators and
          denominators have been rounded to the nearest 5. Trends in Reception and Year 6 overweight and obesity over time can be found on fingertips
          <a href='https://fingertips.phe.org.uk/profile/national-child-measurement-programme/data#page/4/gid/8000011/pat/159/par/K02000001/ati/15/are/E92000001/iid/90316/age/200/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1'>here.</a><br><br>",
          '<B>Fast food rate per 1,000 population:</B> This indicator is the rate of fast food establishments per 1,000 population.
           Fast food establishments include all establishments with the business type "Takeaway/sandwich". Additionally,
           establishments with specific business types with any of "burger", "chicken", "chip",
           "fish bar", "pizza", "kebab", "india", "china" or "chinese" included in their name or if they were a major fast food outlet have also been included. This is consistent with the methodology
           used in the 2017 Public Health England analysis "Fast food outlets: density by local authority in England". Values highlighted
           in red have a significantly higher rate than the comparator and those highlighted in green have a significantly lower
          rate than the comparator. This data was last downloaded on 02/12/2025. Population data used is from 2024 ONS LSOA population predictions. <br><br>',
          "<B> Distance to nearest fast food outlet quintiles: </B> This indicator is a relative measure of distance measured in minutes individuals in different MSOAs need to travel to reach their closest
          fast food outlet. The indicator uses data from the Access to Healthy Assets and Hazards (AHAH) index. The AHAH index calculates the distance to the closest fast food outlet at a LSOA level.
          To create a MSOA value the LSOA values were weighted according to the population contribution of the LSOA to the MSOA The MSOA values were then ranked
          and split into quintiles with quintile 1 containing the MSOAs with the lowest average distance to their nearest fast food outlet and quintile 5 containing the MSOAs with the
          highest average distance to their closest fast food outlet. This data is for 2024. <br><br>",
          "<B>No. indicators sig worse than comparator/quintile 1:</B> This indicator counts the number of other indicators which
          were significantly higher than the comparator, and/or if the MSOA is in Herts IMD deprivation quintile 1, or quintile 1 for distance to
          nearest fast food outlet."
        )
      })

      output$legend <- renderUI({
        if (input$Indicators == "No. indicators sig worse than comparator/quintile 1") {
          img(src = "map_legend.png", height = "130%", width = "123%")
        } else {
          return(paste(""))
        }
      })

      output$imd_text <- renderUI({
        if (input$Indicators == "Herts IMD deprivation quintile") {
          return(paste("*Herts IMD quintiles are not compared to Hertfordshire or districts. MSOAs in Hertfordshire have been ranked and divided into quintiles, see indicator guidance for details."))
        } else if (input$Indicators == "Distance to nearest fast food outlet quintiles") {
          return(paste("*Distance to nearest fast food outlet quintiles are not compared to Hertfordshire or districts. MSOAs in Hertfordshire have been ranked and divided into quintiles, see indicator guidance for details."))
        } else if (input$Indicators == "No. indicators sig worse than comparator/quintile 1") {
          return(paste("*This map displays the number of indicators that are significantly worst than the comparator or in quintile 1 for each MSOA in Hertfordshire."))
        } else {
          return(paste(""))
        }
      })

      output$table_overview <- reactable::renderReactable({
        rv_over$filtered_table %>%
          reactable::reactable(
            defaultExpanded = FALSE, outlined = FALSE, striped = TRUE, highlight = TRUE,
            filterable = FALSE, resizable = TRUE, defaultPageSize = 11,
            defaultColDef = colDef(cell = function(value) {
              htmltools::div(style = list(fontSize = 12), value)
            }, header = function(value) {
              htmltools::div(style = list(fontSize = 12), value)
            }),
            columns = list(
              Herts_quintile = colDef(cell = function(value) {
                if (is.na(value)) {
                  na <- "-"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, name = "Herts 2025 IMD deprivation quintile (1 most deprived)"),
              `Prevalence_overweight_and_obesity_Reception` = colDef(cell = function(value, index) {
                colour <- rv_over$filtered_table$Diff_Reception[index]
                if (colour == "significantly higher than") {
                  htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
                } else if (colour == "significantly lower than") {
                  htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
                } else if (colour == "NA value") {
                  na <- "*"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, html = TRUE, name = "Reception: Prevalence of overweight and obesity (2022/23 - 2024/25)"),
              `Prevalence_overweight_and_obesity_Year_6` = colDef(cell = function(value, index) {
                colour <- rv_over$filtered_table$Diff_year_6[index]
                if (colour == "significantly higher than") {
                  htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
                } else if (colour == "significantly lower than") {
                  htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
                } else if (colour == "NA value") {
                  na <- "*"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, html = TRUE, name = "Year 6: Prevalence of overweight and obesity (2022/23 - 2024/25)"),
              dist_fast_food_quintile = colDef(cell = function(value) {
                if (is.na(value)) {
                  na <- "-"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, name = "Distance to nearest fast food outlet quintiles (1 lowest distance) (2024)"),
              fast_food_rate = colDef(cell = function(value, index) {
                colour <- rv_over$filtered_table$fast_food_diff[index]
                if (colour == "significantly higher than") {
                  htmltools::div(style = list(fontSize = 12, color = "#e00000", fontWeight = "bold"), value)
                } else if (colour == "significantly lower than") {
                  htmltools::div(style = list(fontSize = 12, color = "green", fontWeight = "bold"), value)
                } else if (colour == "NA value") {
                  na <- "*"
                } else {
                  htmltools::div(style = list(fontSize = 12), value)
                }
              }, html = TRUE, name = "Fast food rate per 1,000 population (2025)"),
              Diff_Reception = colDef(show = FALSE),
              Diff_year_6 = colDef(show = FALSE),
              AreaName = colDef(name = "Area", sticky = "left", filterable = TRUE),
              district_name = colDef(show = FALSE),
              fast_food_diff = colDef(show = FALSE),
              dist_fast_food_diff = colDef(show = FALSE),
              dist_fast_food = colDef(show = FALSE),
              colour = colDef(show = FALSE),
              agg = colDef(name = "No. indicators sig worse than comparator/quintile 1"),
              label_text = colDef(show = FALSE),
              Prevalence_obesity_Year_6 = colDef(show = FALSE),
              Prevalence_obesity_Reception = colDef(show = FALSE)
            )
          )
      })

      output$exp_table <- downloadHandler(
        filename = "data.csv",
        content = function(con) {
          data <- rv_over$filtered_table %>%
            dplyr::select(
              "Area" = AreaName,
              "Herts 2025 IMD deprivation quintile (1 most deprived)" = Herts_quintile,
              "Reception: Prevalence of overweight and obesity (2022/23 - 2024/25)" = Prevalence_overweight_and_obesity_Reception,
              "Year 6: Prevalence of overweight and obesity (2022/23 - 2024/25)" = `Prevalence_overweight_and_obesity_Year_6`,
              "Fast food rate per 1,000 population (2025)" = fast_food_rate,
              "Distance to nearest fast food outlet (2024)" = dist_fast_food,
              "Distance to nearest fast food outlet quintiles (2024)" = dist_fast_food_quintile,
              "No. indicators sig worse than comparator/ IMD herts quintile 1" = agg, "District" = district_name
            )
          write.csv(data, con)
        }
      )

      output$map_healthy <- leaflet::renderLeaflet({
        shp_msoa <- geojsonsf::geojson_sf("data/public/shp_msoa.geojson") %>%
          dplyr::arrange(MSOA_name)

        shp_hert <- geojsonsf::geojson_sf("data/public/shp_hert.geojson")

        shp_dist <- geojsonsf::geojson_sf("data/public/shp_dist.geojson")

        if (input$Indicators == "No. indicators sig worse than comparator/quintile 1") {
          map_health_df <- rv_over$filtered_table
        } else {
          map_health_df <- filtered_map_data(data_map = rv_over$filtered_table, indicator = input$Indicators)
        }

        map_health_df <- map_health_df %>%
          dplyr::filter(!AreaName %in% c(
            "Hertfordshire", "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere",
            "North Hertfordshire", "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers"
          )) %>%
          dplyr::arrange(AreaName)

        map_health_df <- shp_msoa %>%
          dplyr::right_join(map_health_df, by = c("MSOA_name" = "AreaName"))

        map_health_df <- map_health_df %>%
          leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, minZoom = 7, maxZoom = 20, zoomControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          leaflet::addPolygons(data = shp_hert, weight = 2, opacity = 0.8, fillOpacity = 0, smoothFactor = 2, color = "black") %>%
          leaflet::addPolygons(
            data = map_health_df, layerId = map_health_df$MSOA_name, label = paste(map_health_df$MSOA_name, "|", map_health_df$label_text),
            weight = 2, opacity = 0.8, fillOpacity = 0.8, fillColor = map_health_df$colour, color = "#565656", smoothFactor = 2
          ) %>%
          leaflet::addPolylines(data = shp_dist, weight = 2, opacity = 0.8, fillOpacity = 0, smoothFactor = 2, color = "black") %>%
          leaflet.extras::addFullscreenControl(position = "topright") %>%
          htmlwidgets::onRender(
            "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }"
          )

        if (input$Indicators %in% c(
          "Year 6: Prevalence of overweight and obesity",
          "Reception: Prevalence of overweight and obesity", "Fast food rate"
        )) {
          map_health_df %>%
            leaflet::addLegend("bottomright", colors = c("green", "#F6BE00", "#e00000", "grey"), labels = c("Significantly lower", "Significantly similar", "Significanlty higher", "Value suppressed"))
        } else if (input$Indicators == "Herts IMD deprivation quintile") {
          map_health_df %>%
            leaflet::addLegend("bottomright", colors = c("#bccfdf", "#8fafca", "#759CBB", "#206095", "#133959"), labels = c("Quintile 1 (most deprived)", "Q2", "Q3", "Q4", "Quintile 5 (least deprived)"))
        } else if (input$Indicators == "Distance to nearest fast food outlet quintiles") {
          map_health_df %>%
            leaflet::addLegend("bottomright", colors = c("#bccfdf", "#8fafca", "#759CBB", "#206095", "#133959"), labels = c("Quintile 1 (lowest distance)", "Q2", "Q3", "Q4", "Quintile 5 (highest distance)"))
        } else {
          map_health_df
        }
      })
    }
  )
}
