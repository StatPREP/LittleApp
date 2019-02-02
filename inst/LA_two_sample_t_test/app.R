#
# A blank tempate for a Little App
#
library(shiny)
library(shinydashboard)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Inference", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      checkboxInput("show_mean", "Show mean?", value = TRUE) %>%
        tighten(bottom = -10),
      checkboxInput("show_ci", "Show conf. interval?", value = TRUE) %>%
        tighten(bottom = -10),
      selectInput("interval_level", "Confidence level",
                  choices = list("99%" = 0.99, "95%" = 0.95, "92%" = 0.92,
                                 "90%" = 0.90, "80%" = 0.80,
                                 "67%" = 0.67, "50%" = 0.50),
                  selected = 0.95) %>%
        tighten(bottom = -10),
      checkboxInput("show_t", "Show t interval?", value = FALSE) %>%
        tighten(bottom = -10),
      checkboxInput("var_equal", "Equal variance?", value =  TRUE) %>%
        tighten(bottom = -10),
      hr(),
      checkboxInput("shuffle", "Shuffle groups?", value = FALSE)
  )

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Template App",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6)
      , LA_sample_ui(6)
      # , LA_inference(6)    # No resampling
      , my_special_controls
      #bookmarkButton()
    ),

    LA_body() # The body is entirely pre-defined
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectCategorical(max_levels = 2, none = FALSE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectNone()

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  # Add your own renderers, reactives, and observers here.
  # For instance ...

  get_app_data <- reactive({
    this_data <-  req(get_sample())
    if (input$shuffle) this_data[[2]] <- shuffle(this_data[[2]])

    this_data
  })

  output$main_plot <- renderPlot({

    two_sample_t_plot(get_frame_formula(), get_app_data(),
                      level = as.numeric(input$interval_level),
                      show_mean = input$show_mean,
                      show_ci = input$show_ci,
                      show_t = input$show_t,
                      var_equal = input$var_equal)
    })
  # Other built-in output widgets besides output$main_plot
  # output$codebook <- renderText({ Your HTML })
  output$statistics <- renderText({
   text <- capture.output(t.test(get_frame_formula(),
                                 get_app_data(),
                                 var.equal = input$var_equal  )) %>%
     paste(., collapse = "\n")
   HTML(paste("<pre>", text, "</pre>"))
  })
  output$explain <- renderText({
    HTML(includeHTML("explain.html"))
  })
  output$rcode <- renderText({HTML(
    includeHTML("r-commands.html"))
  })
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})
}

shinyApp(UI, SERVER, enableBookmarking = "url")
