#
# Point plots
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)
library(DT)

# App-specific controls
point_plot_controls <-
  box(title = "Additional graphics", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      checkboxInput("jitter",  "Jitter categorical variables.") %>% tighten(bottom = -10),
      hr(),
      checkboxInput("show_violin", "Show density  violin") %>% tighten(bottom = -10)
  )

UI <- function(request) { #for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Point plots",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350,
      p(" "),
      point_plot_controls,
      LA_data_source(6, covariate = TRUE, facet = TRUE),
      LA_sample_ui(6)
    ),

    LA_body(plot_widget = plotOutput("main_plot", height = "400px",
                                     brush = brushOpts(id="yruler",  direction  = "y")))
  )
}

SERVER <- function(input, output, session) {
    the_data <- reactiveValues()
    app_state <- reactiveValues(n_trials = 0, Trials = data.frame())

    # App-specific selection of variables


    # Choose the variables
    #select_x <- function(x) x %>% filter(!numeric, n_levels <= 5) %>% .$vname
    select_y <- select_x <- LA_selectAll()
    select_z <- LA_selectAll(none = TRUE)

    # Reactives and observers used throughout the various Little Apps
    LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z, select_z)
    LA_standard_reactives(input, output, session, the_data, app_state)

    output$debug_text <- renderText({
      " "
    })


    output$main_plot <- renderPlot({
      xrange <- c(0, 2) # default for no explanatory variable
      xvar <- get_explanatory_var()
      if  (length(xvar)  != 1) {
        if (get_explanatory_type() == "numeric") xrange = range(xvar)
        else xrange = c(1, length(unique(xvar)) + 0.5)
      }
      jitter_h <- jitter_w <- 0
      This_data = get_sample()
      if (input$jitter) {
        if (get_explanatory_type()  != "numeric") jitter_w  = 0.2
        if (get_response_type() != "numeric") jitter_h = 0.2
      }
      This_data[input$covar] <- get_covar_discrete() # Always discrete
      if ("facet_by" %in% names(input)) {
        This_data[input$facet_by] <- get_facet_var() # Always  discrete
      }
      P <- LA_dot_layer(get_frame_formula(), data = This_data,
                        color = get_color_formula(),
                        width = jitter_w,
                        height = jitter_h,
                        alpha = LA_point_alpha(input_sample_size()),
                        seed = 101
      )  %>%
        gf_lims(y = get_y_range())  # to keep scale constant across samples

      # Add faceting as appropriate
      if ("facet_by" %in% names(input) && input$facet_by != "1") {
        P <- P %>% gf_facet_grid(as.formula(glue::glue("~ {input$facet_by}")))
      }
      if (input$show_violin) P <- P %>% add_violin(This_data, input$var_x, input$var_y, 3)

      add_y_ruler( P, x_range = xrange, ruler = input$yruler )
    })

    output$rcode <- renderText({ " "
      #HTML(includeHTML("r-commands.html"))
    })
    output$explain <- renderText({ " "
      #HTML(includeHTML("explain.html"))
    })
    output$statistics <- renderText({
      HTML(paste("<pre>",
                 paste(capture.output(head(get_sample(), 30)), collapse = "\n"),
                 "</pre>")
      )
    })


}

shinyApp(UI, SERVER, enableBookmarking = "url")

