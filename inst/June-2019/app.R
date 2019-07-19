#
# Prototype of LittleApp generation 3

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggformula)

#  Set the controls here
Control_menu <-  dropdown(
  inputId = "annotations",
  size = "sm",
  circle = FALSE,
  label = "Controls",
  width = "180px",
  actionButton("do_something", "Do something!")
)
Control_menu2 <- NULL # A slot for another control menu if you want it

# When implementing a new app,
# customize the contents of this function, without changing
# the calling interface.
main_plot_function <- function(Sample, vnames, input_values) {
  gf_jitter(response ~ explanatory, data = Sample,
            color = "black") %>%
  gf_labs(y = vnames["response"], x = vnames["explanatory"])
}


# Boilerplate
Tmp <- select_data_frame_UI("select_frame")
Data_menu <- Tmp[[1]]
Sample_menu <- Tmp[[2]]
New_sample_button <-
  actionButton("new_sample", "New sample", icon=icon("child"),
               style = "color: white; background-color: green; font-size: 16px; margin-left: 0px;")


History_menu <-  dropdown(
    inputId = "history_button",
    size = "sm",
    circle = FALSE,
    label = "History",
    width = "180px",
    checkboxInput("show_both", "Show frozen", value = FALSE),
    actionButton("freeze", label = "Freeze plot")
  )
# Define UI for application that draws a histogram
ui <-
    function(request) {
        fluidPage(
            fillPage(
                # Application title
                fillRow(p("Little App: Statistical Modeling")),
                br(),

                #choose_data_frame_UI("get_data"), sample_size_UI("get_N"),
                br(),
                fillRow(Data_menu, br(), New_sample_button, Sample_menu, br(), Control_menu, Control_menu2, History_menu),
                br(),
                hr(),

                conditionalPanel(condition = "! input.show_both",
                                 plotOutput("full_width", width = "100%")
                ),
                conditionalPanel(condition = "input.show_both",
                                 fillRow(
                                     plotOutput("half_width", width = "100%"),
                                     plotOutput("savedPlot", width = "100%")
                                 )
                )
            )
        )
    }

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  shinyjs::useShinyjs()
  state <- list()
  state$frame <- "mosaicData::Galton"
  state$resp <- "father"
  state$explan <- "mother"
  state$covar <- ""
  state$covar2 <- ""
  state$n <- 20

  # Placeholder for the history
  current_plot <- previous_plot <- gf_text(1 ~ 1, label = "No history yet.")

  # Grab parameters from the URL query
  initialized = FALSE
  observe({
    query <- parseQueryString(session$clientData$url_search)
    for (name in names(query))
      state[name] <<- query[[name]]

    state["n"] <<- as.integer(state[["n"]])

    # I don't know why I have to do this
    foo <- force(state$n)
    # Do this here so that the variable selectors have a chance to be set up.
    if (!initialized) {
      updateSelectizeInput(session, inputId = "select_frame-frame_name",
                           selected = state$frame)
      updateSelectInput(session, inputId = "select_frame-samp_size-samp_n",
                        selected = foo)
      initialize = TRUE
    }
  }, priority=1000)


  # update the saved plot on freeze-button push
  observe({
    if (input$freeze > 0) {
      previous_plot <<- current_plot
      updateCheckboxInput(session, "show_both", value = TRUE)
    }
  })
  observe({
    # make the next plot
    current_plot <<- make_plot()

    # Make freeze an action button and copy the current plot whenever it's pressed.
    # This will need to be in a separate observer()
    output$half_width <- renderPlot({ current_plot })
    output$full_width <- renderPlot({ current_plot })
    output$savedPlot <- renderPlot({previous_plot})
  })


  make_plot <- reactive({
    # Copy over the input values to be able to hand them to the plotting function.
    input_names <- names(input)
    input_values <- lapply(input_names, function(nm) input[[nm]]) %>%
      setNames(input_names)

    Sample <- RAW$get_sample()
    vnames <- RAW$var_names()
    main_plot_function(Sample, vnames, input_values)
    # gf_jitter(response ~ explanatory, data = Sample, color = "black") %>%
    #   gf_labs(y = vnames["response"], x = vnames["explanatory"])
  })

  RAW <- callModule(select_data_frame, "select_frame", state = state,
                    new_sample = reactive(input$new_sample))
}

# Run the application
shinyApp(ui = ui, server = server)
