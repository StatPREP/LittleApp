#
# A template for a Little App
# this one is about jittering
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)

# App-specific controls
jitter_controls <-
  box(title = "Jitter controls", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      noUiSliderInput("jitter_width",  "Jitter width",
                  min = 0, max = 1, value = 0.0) ,
      noUiSliderInput("jitter_height", "Jitter height", orientation  = "vertical",
                  min = 0, max = 1, value = 0.0, height = "200px", direction = "rtl"),
      noUiSliderInput("jitter_alpha",  "Transparency",
                  min = 0, max = 1, value = 1),
      hr(),
      checkboxInput("violin", "Show violin plot") %>%
        tighten(bottom = -10)
  )

UI <- function(request) { #for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Jittered point plots",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350,
      p(" "),
      jitter_controls,
      LA_data_source(6, covariate = FALSE),
      LA_sample_ui(6)
    ),

    LA_body()
  )
}

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
      req(the_data$initialized)

      # Make the plot
      the_formula <- as.formula(glue::glue("{input$var_y} ~ {input$var_x}"))
      plot_data  <- get_sample()
      if (input$var_y %in% names(plot_data)  && input$var_y %in% names(plot_data))  {
        # The next line gets all the accumulated randomization trials.
        Trials <- get_all_trials()
        P <-
          get_sample()  %>%
          gf_jitter( the_formula, seed = 101,
                     width = input$jitter_width,
                     height = input$jitter_height,
                     alpha = input$jitter_alpha)
        if (input$violin) P <- P %>% gf_violin(alpha = 0.2, fill = "blue")
      } else {
        P <- gf_text(1 ~ 1, label = "New data set arriving ...") %>% gf_theme(theme_void())
      }
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

    observe({ # Make vertical jittering on a reasonable scale for numeric variables
      old_val <- input$jitter_height
      jitter_amt <- if (req(get_response_type()) == "numeric") {
        possibilities <- pretty(abs(diff(range(req(get_response_var())))) / 10)
        possibilities[possibilities > 0][1]
      } else 1

      if (old_val > jitter_amt) old_val <- 0

      updateNoUiSliderInput(session, "jitter_height", range = c(0, jitter_amt),
                        value = old_val)
    })

}

shinyApp(UI, SERVER, enableBookmarking = "url")

