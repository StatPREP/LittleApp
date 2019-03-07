#' Builds the main panel of a little app
#'
#' You can add additional tabs via the ...
#'
#' @param ... Additional `tabPanels()` to include in the tab box
#' @param plot_widget Controls how the plotting frame is set  up.
#' Default: `plotOutput("main_plot", height = "400px")`. Must come *after* the
#' `...`` arguments.
#'

#'
#' By default, the plot widget will be set up as  a simple `plotOutput()` shiny object.
#' By using the `plot_widget` argument, you  can add brush  controls or choose to
#' substitute something entirely for the plot space in the app.
#'
#' @return The HTML which creates the dashboard body
#'
#' @examples
#' LA_body()  # a default app
#' one <- tabPanel("A new tab", plotOutput("second_plot"), height = "400px")
#' two <- tabPanel("Another new tab", tableOutput("data_display"))
#' LA_body(one, two)
#'
#' @export
LA_body <- function(..., plot_widget = NULL) {
  dashboardBody(
    shinyjs::useShinyjs(),
    if (is.null(plot_widget))
      plotOutput("main_plot", height = "400px")
    else
      plot_widget,
    HTML("<br>"),
    tabBox(
      title = "Info", width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "100px",
      tabPanel("Codebook", htmlOutput("codebook")),
      tabPanel("Statistics", htmlOutput("statistics")),
      tabPanel("Explain", htmlOutput("explain")),
      ...,
      tabPanel("R commands", htmlOutput("rcode")),
      tabPanel("Debug",
               textOutput("debug_text"),
               plotOutput("debug_plot"),
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



