#
# t-test  Little App
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
      sliderInput("null_mu", "Null hypothesis 'mu'", min = 0, max = 1, value = 0.5) %>%
        tighten(bottom = -10),
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
      title = "The t-test",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      ,shinyjs::useShinyjs()
      , LA_data_source(6)
      , LA_sample_ui(6)
      # , LA_inference(6)    # No resampling
      , my_special_controls
      #bookmarkButton()
    ),

    LA_body(
      tabPanel("Raw data", tableOutput("raw_data"))
    )
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectCategorical(max_levels = 2, none = TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectNone()

  shinyjs::hide("covar") # no covariate in  this app

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  null_hypothesis <- reactive({
    if ("null_mu" %in% names(input)) input$null_mu
    else 0
  })

  observeEvent(input$var_y, {
      yvar <- get_response_var()
      miny <- min(yvar, na.rm = TRUE)
      maxy <- max(yvar, na.rm = TRUE)
      sdy <- sd(yvar, na.rm = TRUE)
      updateSliderInput(session, "null_mu",
                        min = pretty(pmin(miny,0) - sdy)[1],
                        max = pretty(pmax(0, maxy) + sdy)[2],
                        value = 0,
                        step = pretty(sdy)[1]/10)
  })
  observe({
    if (no_explanatory_var())  {
      shinyjs::show("null_mu")
      shinyjs::hide("show_t")
      shinyjs::hide("shuffle")
      shinyjs::hide("var_equal")
    } else {
      shinyjs::hide("null_mu")
      shinyjs::show("show_t")
      shinyjs::show("shuffle")
      shinyjs::show("var_equal")
    }
  })

  get_app_data <- reactive({
    this_data <-  req(get_sample())
    if (input$shuffle && ! no_explanatory_var())
      this_data[[2]] <- shuffle(this_data[[2]])

    this_data
  })

  output$raw_data <- renderTable({
    get_sample() %>% head(500) %>%
      ungroup() %>%
      mutate(row_number = 1:nrow(.))
  })

  output$main_plot <- renderPlot({
    if (no_explanatory_var()) {
      one_sample_t_plot(get_frame_formula(), get_app_data(),
                        level = as.numeric(input$interval_level),
                        show_mean = input$show_mean,
                        show_ci = input$show_ci,
                        null_hypothesis = null_hypothesis()) %>%
        gf_labs(title = "One-sample t-test")
    } else {
      two_sample_t_plot(get_frame_formula(), get_app_data(),
                        level = as.numeric(input$interval_level),
                        show_mean = input$show_mean,
                        show_ci = input$show_ci,
                        show_t = input$show_t,
                        var_equal = input$var_equal) %>%
        gf_labs(title = "Two-sample t-test")
    }
  })
  # Other built-in output widgets besides output$main_plot
  # output$codebook <- renderText({ Your HTML })
  output$statistics <- renderText({
    text <- if (no_explanatory_var()) {
      capture.output(t.test(get_response_var(), mu = null_hypothesis()))
    } else {
      capture.output(t.test(get_frame_formula(),
                            get_app_data(),
                            var.equal = input$var_equal  ))
    }
   HTML(paste("<pre>", text %>% paste(., collapse = "\n"), "</pre>"))
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
