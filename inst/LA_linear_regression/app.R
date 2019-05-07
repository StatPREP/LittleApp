#
# Simple linear regression
#
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(LittleApp, quietly=TRUE)
library(markdown, quietly=TRUE)
library(mosaic, quietly=TRUE)
library(ggformula, quietly=TRUE)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Model controls", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      checkboxInput("show_model_values", "Show model values") %>%
        tighten(bottom = -10),
      checkboxInput("trace_horizontally", "Trace horizontally") %>%
        tighten(bottom = -10),
      checkboxInput("trace_vertically", "Trace vertically") %>%
        tighten(bottom = -10),
      hr(),
      p("Model details:") %>% tighten(bottom = -10),
      checkboxInput("interaction", "Use interaction for covar.", value = TRUE) %>%
        tighten(bottom = -10),
      numericInput("spline_order", "Degree of model:", min = 0, max = 5, value = 1) %>%
        tighten(bottom = -10)
  )

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Regression models",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6)
      , LA_sample_ui(6)
      , my_special_controls
      #, LA_inference(6)
      #, bookmarkButton()
    ),

    LA_body(
      plot_widget = plotOutput("main_plot", height = "400px",
                                brush = brushOpts(id="yruler",  direction  = "y"))
    )
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectNumeric()
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  # Add your own renderers, reactives, and observers here.
  # For instance ...

  model_formula <- reactive({
    if (input$interaction) get_flexible_formula()
    else get_model_formula()
  })

  output$main_plot <- renderPlot({
    smoother_plot(get_frame_formula(),
                  model_formula(),
                  get_sample(), color = get_color_formula(),
                  trace_vert = input$trace_vertically,
                  trace_horiz = input$trace_horizontally,
                  show_mod_vals = input$show_model_values,
                  ruler = input$yruler) %>%
      gf_lims(y = get_y_range()) # to keep scale constant across samples

    })
  # Other built-in output widgets besides output$main_plot
  # output$codebook <- renderText({ Your HTML })
  output$statistics <- renderText({
    report <- simple_regression_report(model_formula(), data = get_sample())
    HTML(report)
  })
  output$explain <- renderText({HTML(includeHTML("explain.html"))})
  output$rcode <- renderText({HTML(includeHTML("r-commands.html"))})
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})
}

shinyApp(UI, SERVER, enableBookmarking = "url")
