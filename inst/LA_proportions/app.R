#
# Proportions Little App
#
library(shiny)
library(shinydashboard)
library(LittleApp)
library(markdown)
library(SDSdata)
library(mosaic)
library(ggformula)
library(dplyr)
library(forcats)
library(mosaicCore)
library(mosaicModel)


# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Model", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      selectInput("selected_level", "Selected level",
                  choices = c("A", "B"), selected = "A") %>% tighten(),
      checkboxInput("logistic", "Use logistic regression"),
      selectInput("smoothing", "Smoothing/Curviness",
                  choices = c("NA" = 0, "linear - 1" = 1, 2:6)) %>% tighten(),
      checkboxInput("show_ci", "Show conf. interval", FALSE),
      hr(),
      checkboxInput("shuffle",  "Shuffle response variable.", FALSE) %>%
        tighten(bottom = -10))

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Proportions",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6)
      , LA_sample_ui(6)
      , my_special_controls
      #bookmarkButton()      # You can change the order of these
                             # to get a layout you like.
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
  select_x <- LA_selectAll(none = FALSE)
  select_y <- LA_selectCategorical(max_levels = 8)
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  observe({
    all_y <- the_data$frame[[input$var_y]]
    choices <- sort(unique(all_y))
    selected <- names(sort(- table(all_y)))[1]
    updateSelectInput(session, "selected_level",  choices = choices, selected = selected)
    },
    priority = 10)

  output$main_plot <- renderPlot({
    req(input$selected_level)
    discrete_explanatory <- get_explanatory_type() != "numeric"
    jitter_width <- ifelse(discrete_explanatory, 0.2, 0.0)
    covar_formula <- if (input$covar == 1) "" else glue::glue("* {input$covar}")
    model_formula <-
      if (input$smoothing %in% c(0,1)) glue::glue("binary ~ {input$var_x} {covar_formula}")
        else glue::glue("binary ~ ns({input$var_x}, {input$smoothing}) {covar_formula}")
    model_formula <-  as.formula(model_formula)
    data_formula <-
      if (input$var_x == 1) {
        as.formula(paste("binary~ 1"))
      } else {
        as.formula(paste0("binary ~", input$var_x))
      }
    color_formula <-
      if (input$covar == 1) "black" else as.formula(glue::glue(" ~ {input$covar}"))


    the_data <- get_sample()
    if (! input$selected_level %in% unique(get_response_var())) return()
    second_val <- input$selected_level
    the_data$binary <- as.numeric(get_response_var() == second_val)
    if (input$shuffle) the_data$binary <- sample(the_data$binary)
    P <- gf_jitter(data_formula, data = the_data,
                   color = color_formula,
                   height = 0.1, width = as.numeric(jitter_width),
                   seed = 12345,
                   alpha = LA_point_alpha(get_overall_sample_size()))
    # fit and evaluate model
    mod <- if (input$logistic)
      glm(model_formula, data = the_data, family = binomial)
    else lm(model_formula, data = the_data)

    model_vals <- mosaicModel::mod_eval(mod, nlevels = Inf,
                                        interval = "confidence")
    model_vals$upper <- ifelse(model_vals$upper > 1.1, 1.1, model_vals$upper)
    model_vals$lower <- ifelse(model_vals$lower < -0.1, -0.1, model_vals$lower)
    model_vals$explan <- model_vals[[input$var_x]]


    if (input$show_ci) {
      P <- P %>% gf_errorbar(lower + upper ~ explan,
                             data = model_vals, color = color_formula,
                             alpha = ifelse(discrete_explanatory, 0.75, 0.3))
    }

    if (discrete_explanatory) {
      P <- P %>% gf_errorbar(model_output + model_output ~ explan,
                             color = color_formula,
                             data = model_vals)
    } else {
      P <- P %>% gf_line(model_output ~ explan,
                         data = model_vals, size = 2,
                         color = color_formula, alpha = 0.5)
    }


    second_axis = dup_axis(name = "Labels",
                           breaks = c(0, 1),
                           labels = c("other", input$selected_level))
    P <- P %>% gf_refine(scale_y_continuous(
      name = glue::glue("Probability of {input$var_y} == {second_val}"),
      breaks = seq(0, 1, by = 0.2),
      limits = c(-.2, 1.2),
      labels = seq(0,1,by = 0.2),
      sec.axis = second_axis)) %>%
      gf_theme(legend.position = "top")

    add_y_ruler( P, x_range = get_x_range(), ruler = input$yruler )
  }
  )
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
