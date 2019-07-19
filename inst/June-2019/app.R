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
    checkboxInput("show_both", label = "Show history", value = FALSE),
    checkboxInput("freeze", label = "Freeze history", value = FALSE)
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


  # update the plots as needed
  observe({
    newplot <- make_plot()

    # Make freeze an action button and copy the current plot whenever it's pressed.
    # This will need to be in a separate observer()

    if (! isolate(input$freeze)) previous_plot <<- current_plot
    current_plot <<- newplot

    output$half_width <- renderPlot({ current_plot })
    output$full_width <- renderPlot({ current_plot })
    output$savedPlot <- renderPlot({previous_plot})
  })


  make_plot <- reactive({
    tmp <- RAW$get_sample()
    vnames <- RAW$var_names()
    gf_jitter(response ~ explanatory, data = tmp, color = "black") %>%
      gf_labs(y = vnames["response"], x = vnames["explanatory"])
  })

  RAW <- callModule(select_data_frame, "select_frame", state = state,
                    new_sample = reactive(input$new_sample))
}

# Run the application
shinyApp(ui = ui, server = server)
