#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggformula)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fillPage(
        # Application title
        fillRow(p("Little App: Statistical Modeling")),
        br(),

        #choose_data_frame_UI("get_data"), sample_size_UI("get_N"),
        br(),
        fillRow(dropdown( select_data_frame_UI("select_frame"),
                          select_var_UI("response_name", "response var", optional = FALSE),
                          select_var_UI("explanatory_name", "explanatory var"),
                          select_var_UI("covar", "second explanatory var", show = FALSE),
                          select_var_UI("covar2", "third explanatory var", show = FALSE),
                          sample_size_UI("samp_size"),
                          inputId = "data_button",
                          size = "sm",
                          circle = FALSE,
                          label = "Data & variables"),

                actionButton("annotations", "Annotations"),
                prettyToggle("save_plot", label_on = "Dismiss plot",
                             label_off = "Hold plot",
                             shape = "curve", outline = TRUE)),
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    shinyjs::useShinyjs()
    output$half_width <- renderPlot({ make_plot() })
    output$full_width <- renderPlot({ make_plot() })
    output$savedPlot <- renderPlot({
        if (input$save_plot)  make_plot()
        else NULL
        })
    make_plot <- reactive({
        gf_density( ~ waiting, data = faithful, fill = 'darkgray', color = 'white')
    })

    RAW <- callModule(select_data_frame, "select_frame")
    RESPONSE <- callModule(select_var, "response_name",
                              suitable = function(x) TRUE,
                              raw = RAW$get_raw)
    EXPLANATORY <- callModule(select_var, "explanatory_name",
                           suitable = function(x) is.numeric(x),
                           raw = RAW$get_raw)
    COVAR <- callModule(select_var, "covar",
                           suitable = function(x) TRUE,
                           raw = RAW$get_raw)
    COVAR2 <- callModule(select_var, "covar2",
                              suitable = function(x) FALSE,
                              raw = RAW$get_raw)
    N <- callModule(sample_size, "samp_size", nmax = RAW$max_n)
    observe({
    cat("Selected frame is", RAW$name(), "\n")
    })
    observe({
        cat("Response var is", EXPLANATORY$var_name(), "\n")
    })


    }

# Run the application
shinyApp(ui = ui, server = server)
