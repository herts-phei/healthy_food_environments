# UI ----------------------------------------------------------------------

tab_schools_mod <- function(id, label = "schools") {

  ns <- NS(id)

  tabItem(tabName = "schools",

    fluidRow(box(title = NULL, width = 3,
      htmlOutput(ns("filter_text")),
      uiOutput(ns("school_selector")),
      uiOutput(ns("food_selector")),
      selectInput(ns("radius_selector"), "Select school radius", choices = c("400", "800"),
        selected = "400", multiple = FALSE),
      actionButton(ns("button"), "Go")),
    box(title = "Map", width = 9, height = 700,
      align = "left", leaflet::leafletOutput(ns("school_map"), width = "100%", height = 660))

  ))
}

# Server ------------------------------------------------------------------

tab_schools_server <- function(id, school_data) {

  moduleServer(

    id,

    function(input, output, session) {

      ns <- NS(id)

      output$filter_text <- renderUI({
        p(paste0("This tab is an interactive mapping tool that allows you to visualise the geographical position of school and food settings. Please select from the filters the setting types and click the 'Go' button to refresh the map."))
      })

      school_map_params <- school_map_params(data = get_data())

      output$school_selector <- renderUI({

        selectInput(ns("school_selector"), "Select school type",
          choices = school_map_params$school_type, selected = school_map_params$school_type, multiple = TRUE)

      })

      output$food_selector <- renderUI({

        selectInput(ns("food_selector"), "Select food setting type",
          choices = school_map_params$food_type, selected = school_map_params$food_type, multiple = TRUE)

      })

      values <- reactiveValues(school = school_map_params$school_type, food = school_map_params$food_type, radius = "400")

      observeEvent(input$button, {

        values$school <- input$school_selector
        values$food <- input$food_selector
        values$radius <- input$radius_selector

      })

      output$school_map <- leaflet::renderLeaflet({


        data_school_map <- school_map_filtered(data = school_data(), school = values$school, food = values$food,
          distance = values$radius)

        leaflet_map(data_full = data_school_map$df_all, data_sch = data_school_map$df_sch, data_food = data_school_map$df_food,
          domain = school_map_params$pal_data)

      })

    }

  )
}
