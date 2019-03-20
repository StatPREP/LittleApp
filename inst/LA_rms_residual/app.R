#
# RMS  optimization
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Straight-line controls", width = 12, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      noUiSliderInput("slope", "Slope", min = 0, max  = 1,
                      value = c(.25,  .75)),
      noUiSliderInput("offset", "Vertical position", min = 0, max = 1,
                      value = c(.25, .75)),
      checkboxInput("validation_set", "x-validation", value = FALSE) %>%
        tighten(bottom = -10),
      actionButton("new_validation", "New validation set"))

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Optimizing RMS error",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6, covariate = FALSE)    # These are controls available
      , LA_sample_ui(6)      # Comment out any you don't want
      , my_special_controls  # ... and add in your own here.
      #bookmarkButton()      # You can change the order of these
                             # to get a layout you like.
    ),

    LA_body() # The body is entirely pre-defined
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

  best_fit <- reactive({
    y <- get_response_var()
    x <- get_explanatory_var()
    mod <- lm(y ~ x)
    spread <-  diff(confint(mod)[2,]) * sqrt(nrow(get_sample()))/ sqrt(50)
    range <- coef(mod)[2] + spread*c(-1,1)
    res <- list(slope = coef(mod)[2], intercept = coef(mod)[1],
         slope_range = extendrange(range,  f = .3),
         offset_range = extendrange(mean(y,  na.rm=TRUE) + c(-1,1) * diff(range(y))/3,  f = -0.35),
         xmean = mean(x, na.rm = TRUE))
    cat(paste(capture.output(res), collapse = "\n"))
    res
  })

  # NEED TO ADD IN STATISTICS TAB LISTING RMS error on original and validation set.

  observe({
    if (input$validation_set) shinyjs::show("new_validation")
    else shinyjs::hide("new_validation")
  })

  observe({
    best <- best_fit()
    updateNoUiSliderInput(session, "slope", range = best$slope_range,
                          value = extendrange(best$slope_range, f = -.2))

    updateNoUiSliderInput(session, "offset", range = best$offset_range,
                          value = extendrange(best$offset_range, f = -0.3))
  })

  get_lines  <- reactive({
    best <- best_fit()
    Slopes <- tibble(
      slopes = c(input$slope,  rev(input$slope)),
      offsets = c(input$offset, input$offset)
    ) %>%
      mutate(intercepts = offsets - best$xmean * slopes)
    cat(paste(capture.output(Slopes),  collapse = "\n"))
    Slopes
  })

  output$main_plot <- renderPlot({
    best <- best_fit()
    Lines <- get_lines()
    P <- standard_dot_plot()
    P %>%
      gf_abline(slope = best$slope, intercept = best$intercept, color = "blue") %>%
      gf_abline(slope = ~ slopes, intercept = ~ intercepts, data = Lines,  color = "brown")

    })
  # Other built-in output widgets besides output$main_plot
  # output$codebook <- renderText({ Your HTML })
  # output$statistics <- renderText({Your HTML})
  # output$explain <- renderText({Your HTML})
  # output$rcode <- renderText({Your HTML})
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})
}

shinyApp(UI, SERVER, enableBookmarking = "url")
