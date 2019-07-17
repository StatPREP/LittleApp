#' Modules for selecting data
#'
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
    updateSelectizeInput(session, "selector",
                      choices = names(raw())[choices],
                      selected = value)
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
    tagList(
      selectizeInput(ns("frame_name"), "Data source", choices = canned,
                     width = "150px",
                     options = list(
                       placeholder = 'Select a data frame',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      ),
      select_var_UI(ns("response_name"), "response var", optional = FALSE),
      select_var_UI(ns("explanatory_name"), "explanatory var"),
      select_var_UI(ns("covar"), "second explanatory var", show = FALSE),
      select_var_UI(ns("covar2"), "third explanatory var", show = FALSE),
      sample_size_UI(ns("samp_size"))
    )

  }
#' @export
select_data_frame <- function(input, output, session, state, ...) {
  max_n <- reactive({nrow(get_raw())})

  get_raw <- reactive({
    req(input$frame_name)
    eval(parse(text = input$frame_name))
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
  N <- callModule(sample_size, "samp_size", nmax = max_n)



  var_names <- reactive({
    c(response = RESPONSE$var_name(), explanatory = EXPLANATORY$var_name(),
      COVAR$var_name(), explanatory = COVAR2$var_name())
  })

  var_choices <- reactive({
    list(response = RESPONSE$var_choices(),
         explanatory = EXPLANATORY$var_choices(),
         covar = COVAR$var_choices(),
         covar2 = COVAR2$var_choices(),
         n_sample = N$n_sample()
    )
  })

  return(
    list(get_raw = get_raw,
         max_n = max_n,
         codebook = codebook,
         name = name,
         var_names = var_names,
         var_choices = var_choices)
  )
}



#' @export
sample_size_UI <- function(id, ...) {
  ns <- NS(id)
  tagList(
  samp_n <- selectInput(ns("samp_n"), "Sample size n", width = "150px",
                        choices = c(5, 10, 20, 50, 100, 200, 500, 1000, 2000),
                        selected = 50),
  stratify <- checkboxInput(ns("stratify"), label = "Stratify", value = FALSE)
  )
}
#' @export
sample_size <- function(input, output, session, nmax) {

  possible_sizes <- reactive({
    population_size = nmax()
    choices <- c(outer(c(5, 10, 20), c(1, 10, 100, 1000, 10000, 100000)))
    choices <- as.list(choices[choices < population_size])
    choices["sampling frame"] <- population_size

    return(choices)
  })
  observe({
    nmax()
    current_n <- req(input$samp_n)
    if (!current_n %in% possible_sizes()) current_n <- 50
    updateSelectInput(session, "samp_n",
                      choices = possible_sizes(),
                      selected = current_n)
  })

  # All the relevant inputs are in TMP
  return(
    list(
      n_sample = reactive(input$samp_n),
      stratify = reactive(input$stratify)
    )
  )

}
