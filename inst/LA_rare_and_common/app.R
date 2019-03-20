#
# Rare  and common little app
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)
library(knitr)
library(kableExtra)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Define common/rare", width = 12, background = "black",
      status = "primary", solidHeader = TRUE,
      noUiSliderInput("common", "Range for common", min = 0, max  = 1,
                  value = c(.25,  .75)),

       noUiSliderInput("rare_left", "Rare (left)",  inline = TRUE,
                       min = 0, max = 1, value = 0.5, width = '45%'),
       span("    "),
       noUiSliderInput("rare_right",  "Rare (right)",  inline = TRUE,
                       min = 0, max  = 1, value = 0.5, width = '45%'),
      collapsible = FALSE, collapsed = FALSE,
      checkboxInput("show_normal", "Overlay normal dist", value = FALSE),
      checkboxInput("show_violin", "Show as violin", value = FALSE) %>%
        tighten(bottom = -10) %>% tighten())

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Common and rare values",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , p(" ")
      , LA_data_source(6, covariate = FALSE) # no covariate in this app
      # , LA_sample_ui(6) # deleted so all the data is used
      , my_special_controls
    ),

    LA_body() # The body is entirely pre-defined
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectCategorical(max_levels = 8, none = TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  # Add your own renderers, reactives, and observers here.

  observe({
    Y <- get_response_var()
    response_range <- extendrange(Y, f = 0.1)
    sensible <- as.numeric(quantile(Y, c(.1, .9)))
    updateNoUiSliderInput(session, "common", range = response_range,
                          value = sensible)
  })
  observe({
    # Keep the  definition  of rare OUTSIDE the range of common.
    response_range <- extendrange(get_response_var(), f = 0.13)

    left <- input$common[1]
    right <- input$common[2]
    rare_left <- input$rare_left
    rare_right <- input$rare_right
    if (rare_left > left)
      rare_left <- response_range[1] + (left - response_range[1])/2
    if (rare_right < right)
      rare_right <-  response_range[2] + (right - response_range[2])/2
    updateNoUiSliderInput(session, "rare_left", range = c(response_range[1], left),
                          value = rare_left)
    updateNoUiSliderInput(session, "rare_right", range = c(right, response_range[2]),
                          value = rare_right)
  })
  our_labels <- reactive({
    c("rare_left", "uncommon_left",
      "common",
      "uncommon_right", "rare_right")
  })
  output$main_plot <- renderPlot({
    req(input$common,  input$rare_left, input$rare_right)
    plot_density_y(yvar=get_response_var(),
                   xvar = get_explanatory_var(),
                   violin = input$show_violin,
                   show_normal  = input$show_normal,
                   breaks = c(input$rare_left, input$common, input$rare_right),
                   labels = our_labels()
    ) %>%
      gf_labs(y = ifelse(input$show_violin, input$var_y, "Relative probability"),
              x = ifelse(input$show_violin, input$var_x, input$var_y))
    })
  # Other built-in output widgets besides output$main_plot
  # output$codebook <- renderText({ Your HTML })
    output$statistics <- renderText({
      cat("Writing table\n")
      this_data <- tibble(y = get_response_var(), x = get_explanatory_var())
      this_data$label <-
        cut(this_data$y,
            breaks = c(-Inf, input$rare_left, input$common, input$rare_right, Inf),
            labels = our_labels())
      tab <- mosaic::tally(label ~ x, data = this_data, format = "percent")
      #names(tab)[1] <- input$var_x
      knitr::kable(tab, digits = 2, format = "html") %>%
        kableExtra::kable_styling(full_width = FALSE)
    })
  # output$explain <- renderText({Your HTML})
  # output$rcode <- renderText({Your HTML})
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})
}

shinyApp(UI, SERVER, enableBookmarking = "url")
