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

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  dark = NULL,
  header = dashboardHeader(
    title = HTML(paste("Healthy Food", "Environments", sep = "<br/>")), # "Healthy Food Environments",
    tags$img(
      src = "team_grey_png.png",
      title = "Healthy Food Envrionments", height = "30px"
    )
  ),

  # sidebar
  sidebar = bs4DashSidebar(
    status = "grey-dark",
    elevation = 3,
    skin = "light",
    width = "165px",
    minifield = F,
    bs4SidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      #menuItem("Correlation", tabName = "correlation", icon = icon("line-chart")),
      menuItem("Schools", tabName = "schools", icon = icon("school"))
    ),
  ),
  body <- dashboardBody(
    tabItems(
      tab_overview_mod("overview"),
      #tab_correlation_mod("correlation"),
      tab_schools_mod("schools")
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$data <- get_data()

  tab_overview_server("overview", overview_data = reactive(rv$data))

  #tab_correlation_server("correlation", correlation_data = reactive(rv$data))

  tab_schools_server("schools", school_data = reactive(rv$data))
}

shinyApp(ui, server)
