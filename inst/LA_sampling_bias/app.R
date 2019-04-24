#
# Simulating sampling bias
#
library(shiny)
library(shinydashboard)
library(LittleApp)
library(markdown)
library(mosaic)
library(mosaicCore)
library(ggformula)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Sampling bias", width = 12, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      # using the covariate slot to be the biasing variable
      p("Sliders set the relative sampling frequencies"),
      tighten(selectizeInput("covar", "Biasing variable", choices = list("1"))),
      sliderInput("level1",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level2",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level3",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level4",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level5",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten())

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Template App",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6, covar  =  FALSE)    # These are controls available
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
  select_x <- LA_selectCategorical(max_levels = 1, none = TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 5, none = TRUE)

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)
  # turn off the sliders at the beginning

  for (k in  1:5)
  shinyjs::hide(paste0("level", k))

  get_weights <- reactive({
    if (input$covar == 1)  return() # not initialized
    levels <- unique(na.omit(as.character(the_data$frame[[input$covar]])))
    # Change the display of the sampling bias sliders
    for (slider in 1:5){
      this_slider <- paste0("level", slider)
      if (length(levels) < slider) {
        shinyjs::hide(this_slider)
      }  else {
        shinyjs::show(this_slider)
        updateSliderInput(session, this_slider, label = levels[slider],
                          value = input[[this_slider]])
      }
    }

    if (length(levels) > 1 )
      return(tibble(.value. = levels,  rel_freq = c(input$level1, input$level2,
                                            input$level3, input$level4,
                                            input$level5)[1:(length(levels))]))

  })

  population_parameter <- reactive({
    shiny::req(the_data$frame)
    mean(the_data$frame[[input$var_y]],  na.rm = TRUE)
  })
  get_biased_sample <- reactive({
    if (input$covar == 1) return(get_sample())
    input$new_sample     # for the dependency
    req(the_data$frame)
    req(input$var_y != 1) # make sure it's initialized to a variable
    req(input$var_y %in% c(names(the_data$frame))) # that's in the data frame
    req(no_explanatory_var() || input$var_x %in% names(the_data$frame))

    weight_levels <- get_weights()
    cat(paste(capture.output(weight_levels), collapse = "\n"))


    # Need to turn this into a vector of the same length as the_data$frame
    weights <- the_data$frame[input$covar]
    names(weights) <- ".value." # to correspond to weight levels
    weights <- weights %>%  left_join(weight_levels)
    cat(paste(paste(lapply(weights, head), collapse = " "), "\n"))

    LittleApp:::get_a_sample(input_sample_size(),
                 input_stratify(),
                 input$var_x,
                 c(input$var_y, input$var_x, input_covar(), input$facet_by),
                 the_data$frame,
                 get_sample_seed(),
                 weights = weights[["rel_freq"]]
                 )
  })

  output$main_plot <- renderPlot({
    parameter <- population_parameter()
    Biased <- get_biased_sample()
    Stats <- df_stats(as.formula(paste(input$var_y, "~ 1")),
                      data = Biased, mean = mean, ci.mean)
    cat(paste(capture.output(table(Biased[[input$covar]])), collapse = "\n"), "\n")
    P <- LA_dot_layer(get_frame_formula(),
                      data = Biased,
                      color = get_color_formula(),
                      width = .1,
                      height = jitter_height(),
                      alpha = dot_alpha()) %>%
      gf_hline(yintercept = parameter, color = "blue") %>%
      gf_errorbar(lower + upper ~ 1.2, data = Stats, width = 0.1, inherit = FALSE)
    P
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
