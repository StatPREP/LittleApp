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
  box(title = "Just for my app", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      checkboxInput("do_you_like_it", "Do you like it?", value = FALSE) %>%
        tighten(bottom = -10))

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Template App",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6)    # These are controls available
      , LA_sample_ui(6)      # Comment out any you don't want
      , LA_inference(6)
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
  select_x <- LA_selectCategorical(max_levels = 8, none = TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  # Add your own renderers, reactives, and observers here.
  # For instance ...

  output$main_plot <- renderPlot({
    cat("Alpha is", dot_alpha(), "\n")
    P <- standard_dot_plot()
    P %>%  gf_labs(title = ifelse(input$do_you_like_it,
                                  "I'm glad you like it.",
                                  "Please check the 'Do you like it?' box!"))
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
