#' Modules for selecting data
#'
#' Does something
#'
#'
#'
#'
#'
#' @export
two_plots_UI <- function(id) {
  ns <- NS(id)
  tagList(
     plotOutput(ns("current_plot"), width = "100%"),
     plotOutput(ns("saved_plot"), width = "100%")
  )
}
#' @export
two_plots <- function(input, output, session, data, annots, display) {
  observe({
    DF <- data$get_raw()
    sample <- data$get_sample()
    cat("In two_plots\n")
    output$current_plot <- renderPlot({plot(1:10)})
    output$saved_plot <- renderPlot({plot(1:20)})
  })
}
#'
#' @export
select_var_UI <- function(id, label, optional = TRUE, show = TRUE) {
  ns <- NS(id)
  if (!show) return(NULL)
  selectizeInput(ns("selector"), label = label, choices = NULL, width="150px",
                 options = list(
                   placeholder = ifelse(optional, "None required", 'Select a variable'),
                   onInitialize = I('function() { this.setValue(""); }')
                 )
              )
}
# write filters in terms of x
#' @export
select_var <- function(input, output, session, suitable, raw, value) {
  var_choices <- reactive({
    unlist(lapply(req(raw()), suitable))
    })
  observe({
    choices <- var_choices()
    isolate(
    updateSelectizeInput(session, "selector",
                      choices = names(raw())[choices],
                      selected = ifelse(input$selector == "", value, input$selector)))
  })
  return(
    list(
      var_name = reactive({input$selector}),
      var_choices = var_choices
    )
  )
}
#' @export
select_data_frame_UI <-
  function(id,
           canned = list(Galton = "mosaicData::Galton",
                         NHANES = "LittleApp::NHANES2"),
           types = c("numeric", "2 levels", NA, NA)) {
    ns <- NS(id)

    list(
      dropdown(
        inputId = "data_button",
        size = "sm",
        circle = FALSE,
        label = "Data & variables",
        width = "180px",

        selectizeInput(ns("frame_name"), "Data source", choices = canned,
                       width = "150px",
                       options = list(
                         placeholder = 'Select a data frame',
                         onInitialize = I('function() { this.setValue(""); }')
                       ) %>% tighten()
        ),
        select_var_UI(ns("response_name"), "response var", optional = FALSE),
        select_var_UI(ns("explanatory_name"), "explanatory var"),
        select_var_UI(ns("covar"), "second explanatory var", show = FALSE),
        select_var_UI(ns("covar2"), "third explanatory var", show = FALSE)
      ),
      dropdown(
        inputId = "sample_button",
        size = "sm",
        circle = FALSE,
        label = "Sampling",
        width = "180px",
        selectInput(ns("samp_n"), "Sample size n", width = "150px",
                    choices = c(5, 10, 20, 50, 100, 200, 500, 1000, 2000),
                    selected = 50),
        stratify <- checkboxInput(ns("stratify"), label = "Stratify", value = FALSE)
      )
    )
  }
#' @export
select_data_frame <- function(input, output, session, state, new_sample,...) {
  max_n <- reactive({nrow(get_raw())})

  get_raw <- reactive({
    input$frame_name
    cat("Accessing raw data\n")
    eval(parse(text = input$frame_name))
  })

  possible_sizes <- reactive({
    population_size = nrow(get_raw())
    choices <- c(outer(c(5, 10, 20), c(1, 10, 100, 1000, 10000, 100000)))
    choices <- as.list(choices[choices < population_size])
    choices["sampling frame"] <- population_size

    return(choices)
  })

  observe({
    current_n <- req(input$samp_n)
    if (!current_n %in% possible_sizes()) current_n <- 50
    updateSelectInput(session, "samp_n",
                      choices = possible_sizes(),
                      selected = current_n)
  })

  codebook <- reactive({"The codebook will be here"})

  name <- reactive({input$frame_name})

  RESPONSE <- callModule(select_var, "response_name",
                         suitable = function(x) TRUE,
                         raw = get_raw, value = state$resp)
  EXPLANATORY <- callModule(select_var, "explanatory_name",
                            suitable = function(x) is.numeric(x),
                            raw = get_raw, value = state$explan)
  COVAR <- callModule(select_var, "covar",
                      suitable = function(x) TRUE,
                      raw = get_raw, value = state$covar)
  COVAR2 <- callModule(select_var, "covar2",
                       suitable = function(x) FALSE,
                       raw = get_raw, value = state$covar2)

  get_sample <- reactive({
    variables <- var_names()
    req(variables["response"] != "")
    req(input$samp_n)
    new_sample() # for the dependency
    cat("Getting sample\n")
    # Need to add in stratification logic

    get_raw()[variables] %>%
      dplyr::sample_n(size = input$samp_n) %>%
      setNames(names(variables)) # standardize to response, explanatory, covar, covar2
  })

  # The selected variables
  var_names <- reactive({
    c(response = RESPONSE$var_name(), explanatory = EXPLANATORY$var_name(),
      COVAR$var_name(), explanatory = COVAR2$var_name())
  })

  # The possibilities for each of the variables
  var_choices <- reactive({
    list(response = RESPONSE$var_choices(),
         explanatory = EXPLANATORY$var_choices(),
         covar = COVAR$var_choices(),
         covar2 = COVAR2$var_choices(),
    )
  })

  return(
    list(get_raw = get_raw,
         get_sample = get_sample,
         max_n = max_n,
         codebook = codebook,
         name = name,
         var_names = var_names,
         var_choices = var_choices,
         new_sample = reactive(input$new_sample))
  )
}



