#' Basic organization of Little App user interface
#'
#' @export
LA_body <- function() {
  dashboardBody(
    plotOutput("main_plot", height = "400px"),
    HTML("<br>"),
    tabBox(
      title = "Info", width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "100px",
      tabPanel("Codebook", htmlOutput("codebook")),
      tabPanel("Statistics", htmlOutput("statistics")),
      tabPanel("Explain", htmlOutput("explain")),
      tabPanel("R commands", htmlOutput("rcode")),
      tabPanel("Debug",
               textOutput("debug_text"),
               #plotOutput("debug_plot"),
               tableOutput("debug_table"))
    ),
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      section.sidebar .shiny-input-container {
          /* Proper spacing around inputs. */
          padding: 1px 1px 1px 1px;
         /* Wrap content (important for inline inputs). */
         white-space: no-wrap;
      }
      .shiny_input_container .control-label {

      }
    ')))
  )
}



