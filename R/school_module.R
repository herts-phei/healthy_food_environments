# UI ----------------------------------------------------------------------

tab_schools_mod <- function(id, label = "schools") {
  ns <- NS(id)

  tabItem(
    tabName = "schools",
    fluidRow(
      box(
        title = NULL, width = 3,
        htmlOutput(ns("filter_text")),
        uiOutput(ns("school_selector")),
        uiOutput(ns("food_selector")),
        selectInput(ns("radius_selector"), "Select school radius",
          choices = c("400", "800"),
          selected = "400", multiple = FALSE
        ),
        actionButton(ns("button"), "Go")
      ),
      box(
        title = "Map", width = 9, height = 700,
        align = "left", leaflet::leafletOutput(ns("school_map"), width = "100%", height = 660)
      ),
      box(title = "School setting type guidance", width = 12, collapsed = TRUE, htmlOutput(ns("school_guide")))
    )
  )
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
          choices = school_map_params$school_type, selected = c("Primary", "Secondary"), multiple = TRUE
        )
      })

      output$food_selector <- renderUI({
        selectInput(ns("food_selector"), "Select food setting type",
          choices = school_map_params$food_type, selected = c("Takeaway/sandwich shop"), multiple = TRUE
        )
      })

      values <- reactiveValues(school = c("Primary", "Secondary"), food = c("Takeaway/sandwich shop"), radius = "400")

      observeEvent(input$button, {
        values$school <- input$school_selector
        values$food <- input$food_selector
        values$radius <- input$radius_selector
      })

      output$school_map <- leaflet::renderLeaflet({
        data_school_map <- school_map_filtered(
          data = school_data(), school = values$school, food = values$food,
          distance = values$radius
        )

        leaflet_map(
          data_full = data_school_map$df_all, data_sch = data_school_map$df_sch, data_food = data_school_map$df_food,
          domain = school_map_params$pal_data
        )
      })

      output$school_guide <- shiny::renderText({
        paste0(
          "<B>First:</B>  - A school with Years reception to Year 4 (ages 4 to 9).<br>",
          "<B>Infant</B> - A school with Years reception to Year 2 (ages 4 to 7).<br>",
          "<B>Junior</B> - A school with Years 3 to 6 (ages 8 to 11).<br>",
          "<B>Middle</B> - A school with Years 5 to 8 (ages 9 to 13).<br>",
          "<B>Nursery</B> - A school for young children, usually from three to five years old.<br>",
          "<B>Primary</B> - A school starting at Reception to Year 6 (ages 5 to 11).<br>",
          "<B>Pupil Referral Unit</B> - For children who aren't able to attend school, reasons for this include,
                being excluded from mainstream school, or needs which cannot be met in mainstream provision.<br>",
          "<B>Secondary</B> - A school with Years 7 to 13 (ages 11 to 18).<br>",
          "<B>Special</B> - For children with significant special educational needs.<br>",
          "<B>University Technical College</B> - Specialise in subjects like engineering and construction and teach these subjects along with business skills
               and using IT. Pupils study academic subjects as well as practical subjects leading to technical qualifications. The curriculum is designed
               by the university and employers, who also provide work experience for students.<br>"
        )
      })
    }
  )
}
