#
# Prototype of LittleApp generation 3

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggformula)

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
                fillRow(
                  dropdown(
                    select_data_frame_UI("select_frame"),

                    inputId = "data_button",
                    size = "sm",
                    circle = FALSE,
                    label = "Data & variables",
                    width = "150px"
                  ),

                  actionButton("annotations", "Annotations"),
                  prettyToggle("save_plot", label_on = "Dismiss plot",
                               label_off = "Hold plot",
                               shape = "curve", outline = TRUE)
                ),
                br(),
                hr(),

                conditionalPanel(condition = "! input.save_plot",
                                 plotOutput("full_width", width = "100%")
                ),
                conditionalPanel(condition = "input.save_plot",
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
    state <- list()
    state$frame <- "mosaicData::Galton"
    state$resp <- "father"
    state$explan <- "mother"
    state$covar <- ""
    state$covar2 <- ""
    state$n <- 20

    observe({
      query <- parseQueryString(session$clientData$url_search)
      for (name in names(query))
        state[name] <<- query[[name]]

      state["n"] <<- as.integer(state[["n"]])

      # I don't know why I have to do this
      foo <- force(state$n)
      # Do this here so that the variable selectors have a chance to be set up.
      updateSelectizeInput(session, inputId = "select_frame-frame_name",
                           selected = state$frame)
      updateSelectInput(session, inputId = "select_frame-samp_size-samp_n",
                        selected = foo)
      }, priority=1000)




    shinyjs::useShinyjs()

    onBookmark(function(state) {
        cat("Starting the bookmark.\n")
        browser()
        state$values$data <- RAW$var_choices()
    })

    onRestore(function(state) {
        browser()
        cat("State names are:", paste(names(state), collapse = ", "), "\n")
    })

    onRestored(function(state) {

        browser()
    })

    output$half_width <- renderPlot({ make_plot() })
    output$full_width <- renderPlot({ make_plot() })
    output$savedPlot <- renderPlot({
        if (input$save_plot)  make_plot()
        else NULL
        })
    make_plot <- reactive({
        gf_density( ~ waiting, data = faithful, fill = 'darkgray', color = 'white')
    })

    RAW <- callModule(select_data_frame, "select_frame", state = state)



    observe({
    cat("Selected frame is", RAW$name(), "\n")
        cat("Variable names are", paste(RAW$var_names(), collapse = ", "), "\n")
    })


    }

# Run the application
#enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)
