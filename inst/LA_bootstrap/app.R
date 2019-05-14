#
# Bootstrapping app
#
library(shiny)
library(shinydashboard)
library(LittleApp)
library(markdown)
library(mosaic)
library(ggformula)
library(ungeviz)

quantile97.5 <<- function(x) unname(quantile(x, probs = 0.975, na.rm = TRUE))
quantile95 <<- function(x) unname(quantile(x,  probs = 0.95, na.rm = TRUE))
quantile90  <<- function(x) unname(quantile(x,  probs = 0.90, na.rm = TRUE))
quantile75  <<- function(x) unname(quantile(x,  probs = 0.75, na.rm = TRUE))
mean_plus_2sd <<- function(x) {mean(x, na.rm = TRUE) + 2 * sd(x, na.rm = TRUE)}
# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
bootstrap_controls <-
  box(title = "Bootstrap Trials", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      radioButtons("bs_display", "Display", choices = c("One trial", "Many trials")) %>%
        tighten(bottom = -10),
      selectInput("ntrials", "# of trials", choices =
                    c("5" = 5, "10" = 10, "20" = 20,
                      "50" = 50, "100" = 100),
                  selected = 50)  %>%
        tighten(bottom = -10),
      actionButton("new_bs_trial", "New Trial",
                   icon = icon("dice"),
                   style = "font-size: 16px; margin-left: 0px;" ) %>%
        tighten(top = 0, bottom = 0))

model_controls <-
  box(title = "Statistic/Model", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      selectInput("stat", "Statistic",
                  choices = c("mean", "median", "max", "min", "quantile75",
                              "quantile90", "quantile95", "quantile97.5", "mean_plus_2sd")) %>%
        tighten(bottom = -10),
      numericInput("spline_order", "Model order", min = 0, max = 3, value = 1) %>%
        tighten(bottom = -10)
  )


# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Resampling and Bootstrapping",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350, p("")
      , LA_data_source(6)
      , LA_sample_ui(6)
      , bootstrap_controls
      , model_controls
      #bookmarkButton()

    ),

    LA_body() # The body is entirely pre-defined
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectAll(none =  TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)

  shinyjs::hide("covar") # no covariate in  this app

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  # set display of controls appropriately
  observeEvent(get_explanatory_type(), {
    if (get_explanatory_type() == "numeric") {
      shinyjs::hide("stat")
      shinyjs::show("spline_order")
    } else {
      shinyjs::show("stat")
      shinyjs::hide("spline_order")
    }
  })

  observe({
    if (input$bs_display == "Many trials") {
      shinyjs::show("ntrials")
      shinyjs::hide("new_bs_trial")
    } else {
      shinyjs::hide("ntrials")
      shinyjs::show("new_bs_trial")
    }
  })

  output$main_plot <- renderPlot({
    input$new_bs_trial # for the dependency
    x_type <- get_explanatory_type()
    stat_fun <- input$stat

    P <-
      if (input$bs_display == "One trial") {
        if (x_type == "numeric")
          show_bootstrap_sample_cont(get_sample(), get_frame_formula(), ns = get_spline_order())
        else
          show_bootstrap_sample_disc(get_sample(), get_frame_formula(), stat = stat_fun)
      } else {
        ntrials <- as.integer(input$ntrials)
        if (x_type == "numeric") {
          show_bootstrap_ensemble_cont(get_sample(), get_frame_formula(),
                                       ntrials = ntrials,
                                       ns = get_spline_order(),
                                       pt_alpha = 0.5, color = "#0000C050",
                                       se = FALSE)
        } else {
          show_bootstrap_ensemble_disc(get_sample(), get_frame_formula(),
                                       ntrials = ntrials, width = 0.2,
                                       stat = stat_fun, alpha_pts = 0.5)
        }
      }

    # The mean +- 2 sd might be outside of the range of the data,
    # unlike all the other stats
    if (input$stat %in% c("mean_plus_2sd", "quantile97.5", "quantile95", "quantile90")) P
    else P %>% gf_lims(y  = get_y_range())
    })
  # Other built-in output widgets besides output$main_plot
  # output$statistics <- renderText({Your HTML})
  output$explain <- renderText({includeHTML("explain.html")})
  output$rcode <- renderText({includeHTML("r-commands.html")})
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})
}

shinyApp(UI, SERVER, enableBookmarking = "url")
