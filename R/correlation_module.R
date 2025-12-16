# UI ----------------------------------------------------------------------

tab_correlation_mod <- function(id, label = "correlation") {
  
  ns <- NS(id)

  tabItem(
    tabName = "correlation",
    fluidRow(
      box(
        title = NULL, width = 3,
        htmlOutput(ns("text_correlation")),
        selectInput(ns("indicator1"),
          label = "Select x-axis (horizontal) variable", choices = c(
            "Reception: Prevalence of overweight and obesity",
            "Year 6: Prevalence of overweight and obesity",
            "Index of Multiple Deprivation (IMD) Score",
            "Fast food rate per 1000", "Average distance (min) to fast food"
          ),
          multiple = FALSE, selected = "Fast food rate per 1000"
        ),
        selectInput(ns("indicator2"),
          label = "Select y-axis (vertical) variable", choices = c(
            "Reception: Prevalence of overweight and obesity",
            "Year 6: Prevalence of overweight and obesity",
            "Index of Multiple Deprivation (IMD) Score",
            "Fast food rate per 1000", "Average distance (min) to fast food"
          ),
          multiple = FALSE, selected = "Year 6: Prevalence of overweight and obesity)"
        )
      ),
      box(
        title = "Correlation graph", width = 9,
        plotlyOutput(ns("graph")),
        htmlOutput(ns("text_imd"))
      )
    ),
    box(
      title = "Guidance and caveats", width = 12,
      htmlOutput(ns("correlation_guide"))
    )
  )
  
}


# Server ------------------------------------------------------------------

tab_correlation_server <- function(id, correlation_data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      rv_correlation <- reactiveValues()

      observe({
        rv_correlation$graph <- graph_data(
          data = correlation_data(),
          indicator1 = input$indicator1,
          indicator2 = input$indicator2
        )
      })


      output$graph <- renderPlotly({
        ind_name1 <- get_ind_name(
          data = correlation_data(),
          indicator = input$indicator1
        )

        ind_name2 <- get_ind_name(
          data = correlation_data(),
          indicator = input$indicator2
        )

        g <- ggplot(rv_correlation$graph, aes(x = get(ind_name1), y = get(ind_name2))) +
          geom_point(aes(text = paste(AreaName, "(x = ", get(ind_name1), ", y = ", get(ind_name2), ")")), color = "#8FAFCA") +
          # geom_smooth(method = 'lm') +
          ggpmisc::stat_fit_glance(method = "lm", geom = "text", aes(label = paste0("R<sup>2</sup> = ", round(..r.squared.., 3))), label.y = "top") +
          theme_classic() +
          xlab(ind_name1) +
          ylab(ind_name2)

        ggplotly(g, tooltip = paste("text"))
      })

      output$text_correlation <- shiny::renderText({
        paste0("This tab shows the relationship (correlation) between indicators (variables). Each point refers to an MSOA Please see below guidance and caveats which need to be
               considered when interpreting these graphs.<br><br>")
      })


      output$text_imd <- shiny::renderText({
        if (input$indicator1 == "Index of Multiple Deprivation (IMD) Score" | input$indicator2 == "Index of Multiple Deprivation (IMD) Score") {
          return(paste("*The higher the IMD score the more relatively deprived an area is."))
        } else {
          return(paste(""))
        }
      })

      output$correlation_guide <- shiny::renderText({
        paste0(
          "<u>Guidance:</u><br><br>",
          
          "<B>Indicators:</B> For details on individual indicators please see the guidance section in the overview tab.<br><br>",
          
          "<B>Correlation:</B> Correlation expresses the extent that two variables are linearly related.<br><br>",
          
          "<B>R<sup>2</sup>:</B> The R-squared value measures the strength of correlation, it is always between 0 and 1. An R-squared value of 0.7 or higher demonstrates
               the variables have a strong correlation where as an R-squared of 0.4 or below demonstrates a very little or no correlation.<br><br>",
          "<u>Caveats:</u><br><br>",
          
          "<ul><li>It is impossible to prove causation with correlation. Two variables having a strong correlation doesn't mean that one was caused by the other.</li><br>",
          
          "<li>There could be other variables acting as confounders. A confounder is a variable which has a relationship with both variables of interest and therefore
               distorts the true relationship between the variables.</li><br>",
          
          "<li>Outliers are data points which differ significantly from other observations, they can inflate or deflate the correlation between variables.</li></ul>"
        )
      })
    }
  )
  
}
