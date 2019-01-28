#
# A template for a Little App
# this one is about jittering
#

library(shiny)
library(shinydashboard)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)

jitter_controls <-
  box(title = "Jitter controls", width = 12, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      sliderInput("jitter_width",  "Jitter width",
                  min = 0, max = 1, value = 0.0) %>% tighten(bottom = -10),
      sliderInput("jitter_height", "Jitter height",
                  min = 0, max = 1, value = 0.0) %>% tighten(bottom = -10),
      sliderInput("jitter_alpha",  "Transparency",
                  min = 0, max = 1, value = 1) %>% tighten(bottom = -10),
      checkboxInput("violin", "Show violin plot") %>%
        tighten(bottom = -10)
  )

UI <- dashboardPage(
  dashboardHeader(
    title = "Jittered point plots",
    titleWidth = "90%"
  ),
  dashboardSidebar(
    width = 350,
    LA_data_source(6),
    LA_variables_ui(6),
    LA_sample_ui(6),
    LA_inference(6),
    jitter_controls
  ),

  LA_body()
)

SERVER <- function(input, output, session) {
    the_data <- reactiveValues()
    app_state <- reactiveValues(n_trials = 0, Trials = data.frame())

    # App-specific selection of variables


    # Choose the variables
    select_x <- function(x) x %>% filter(!numeric, n_levels <= 5) %>% .$vname
    select_y <- function(x) x %>% .$vname
    select_z <- function(x) character(0) # no covariates possible

    # Reactives and observers used throughout the various Little Apps
    LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
    LA_standard_reactives(input, output, session, the_data, app_state)

    output$debug_text <- renderText({
      input$new_trial
      app_state$n_trials
      app_state$Trials
      input$accumulate_trials
      input$resample
      input$shuffle
      glue::glue("{req(app_state$n_trials)} trials taken so {nrow(app_state$Trials)} rows of randomization trial data.\n")
    })



    construct_plot <- reactive({
      req(input$var_y, input$var_x)
      the_formula <- as.formula(glue::glue("{input$var_y} ~ {input$var_x}"))
      # The next line gets all the accumulated randomization trials.
      Trials <- get_all_trials()
      P <-
        get_sample()  %>%
        gf_jitter( the_formula, seed = 101,
                   width = input$jitter_width,
                   height = input$jitter_height,
                   alpha = input$jitter_alpha)
      if (input$violin) P <- P %>% gf_violin(alpha = 0.2, fill = "blue")

      return(P)
    })
    output$main_plot <- renderPlot({ construct_plot() })
    output$little_copy <- renderPlot({
      construct_plot()
    })
    output$rcode <- renderText({
      HTML(includeHTML("r-commands.html"))
    })
    output$explain <- renderText({
      HTML(includeHTML("explain.html"))
    })
    output$statistics <- renderText({
      HTML(includeHTML("statistics.html"))
    })
  }

shinyApp(UI, SERVER)

