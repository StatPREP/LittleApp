#' Basic control blocks for Little Apps
#'
#' @export
LA_data_source <- function(width = 12, collapsed = TRUE) {
  box(title = "Data source", width = width, status = "primary", solidHeader = FALSE,
      collapsible = TRUE, collapsed = collapsed,
      background = "black",
      selectInput("frame", "Data frame",
                  choices = LA_available_data()) %>% tighten(top=0),
      bookmarkButton())
}
#' @export
LA_variables_ui <- function(width = 12, collapsed = TRUE) {
  box(title = "Variables", width = width, status = "primary", solidHeader = FALSE,
      collapsible = TRUE, collapsed = collapsed, background = "black",
      tighten(selectizeInput("var_y", "Response", c("a", "b"))),
      tighten(selectizeInput("var_x", "Explanatory", c("a", "b"))),
      tighten(selectizeInput("covar", "Covariates", c("a", "b"), multiple = TRUE))
  )
}
#' @export
LA_sample_ui <- function(width = 12, collapsed = FALSE) {
  box(title = "Sample", width = width, status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = collapsed,
      background = "black",
      actionButton("new_sample", "New sample", icon=icon("child"),
                   style = "color: white; background-color: green; font-size: 16px; margin-left: 0px;" ) %>%
        tighten(bottom = 10, top = -10),
      selectInput("samp_size", "Sample size n",
                  choices = c(5,10,20,50,100,200,500,1000,2000),
                  selected = 100) %>% tighten(),
      checkboxInput("stratify", "Stratify", value = FALSE) %>% tighten()
  )
}

#' @export
LA_inference <- function(width = 6, collapsed = FALSE){
  box(title = "Inference", width = width, status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = collapsed,
      background = "black",
      checkboxInput("resample", "Resample") %>% tighten(),
      checkboxInput("shuffle", "Randomize") %>% tighten(),
      actionButton("new_trial", "New Trial",
                   icon = icon("dice"),
                   style = "font-size: 16px; margin-left: 0px;" ) %>%
        tighten(top = 0, bottom = 0),
      checkboxInput("accumulate_trials", "Accumulate") %>% tighten()
  )
}
#' @export
LA_body <- function() {
  dashboardBody(
    plotOutput("main_plot", height = "400px"),
    HTML("<br>"),
    tabBox(
      title = "Info", width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "100px",
      tabPanel("Codebook", htmlOutput("codebook")),
      tabPanel("Statistics", htmlOutput("statistics")),
      tabPanel("Explain", htmlOutput("explain")),
      tabPanel("R commands", htmlOutput("rcode")),
      tabPanel("Debug",
               textOutput("debug_text"),
               #plotOutput("debug_plot"),
               tableOutput("debug_table"))
    ),
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      section.sidebar .shiny-input-container {
          /* Proper spacing around inputs. */
          padding: 1px 1px 1px 1px;
         /* Wrap content (important for inline inputs). */
         white-space: no-wrap;
      }
      .shiny_input_container .control-label {

      }
    ')))
  )
}



