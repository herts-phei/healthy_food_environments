library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bs4Dash)
library(reactable)
library(sf)
library(leaflet)
library(plotly)
library(lubridate)
library(bsicons)
library(ggpmisc)

theme <- app_theme() |> bs_add_rules(app_css())

# ui ----------------------------------------------------------------------


ui <- tagList(
  # Include custom JS
  tags$head(tags$script(src = "sidebar.js")),

  # Banner with logos
  app_banner(
    title = "Healthy Food Environments",
    left_logo = "Corporate Logo White PNG.png",
    right_logo = "epi_white.png"
  ),

  # Main layout using bslib
  bslib::page_sidebar(
    theme = theme,
    fillable = TRUE,

    # Sidebar navigation
    sidebar = bslib::sidebar(
      title = "Navigation", open = "desktop", width = 200,
      tags$nav(
        class = "sidebar-menu",
        tags$a(
          href = "#", class = "sidebar-link active",
          `data-value` = "overview",
          bs_icon("house", class = "sidebar-icon", title = "Overview"),
          tags$span("Overview")
        ),
        tags$a(
          href = "#", class = "sidebar-link",
          `data-value` = "correlation",
          bs_icon("graph-up", class = "sidebar-icon", title = "Correlation"),
          tags$span("Correlation")
        ),
        tags$a(
          href = "#", class = "sidebar-link",
          `data-value` = "schools",
          bs_icon("building", class = "sidebar-icon", title = "Schools"),
          tags$span("Schools")
        )
      )
    ),

    # Hidden nav panels for content
    bslib::navset_hidden(
      id = "page",
      bslib::nav_panel("Overview",
        value = "overview",
        div(class = "p-3", tab_overview_mod("overview"))
      ),
      bslib::nav_panel("Correlation",
        value = "correlation",
        div(class = "p-3", tab_correlation_mod("correlation"))
      ),
      bslib::nav_panel("Schools",
        value = "schools",
        div(class = "p-3", tab_schools_mod("schools"))
      )
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Ensure default tab is selected on load
  observeEvent(TRUE,
    {
      if (is.null(input$page)) bslib::nav_select("page", "overview")
    },
    once = TRUE
  )

  # Sidebar clicks update the main navset
  observeEvent(input$sidebar_select, ignoreInit = TRUE, {
    bslib::nav_select("page", input$sidebar_select)
  })

  # Sync sidebar active state with current page
  observeEvent(input$page, ignoreInit = TRUE, {
    session$sendCustomMessage("setActiveSidebar", input$page)
  })

  # Reactive data
  rv <- reactiveValues()
  rv$data <- get_data()

  # Server modules
  tab_overview_server("overview", overview_data = reactive(rv$data))
  tab_correlation_server("correlation", correlation_data = reactive(rv$data))
  tab_schools_server("schools", school_data = reactive(rv$data))
}

shinyApp(ui, server)
