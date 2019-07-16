#' Modules for selecting data
#'
#' @export
select_var_UI <- function(id, label, optional = TRUE, show = TRUE) {
  ns <- NS(id)

  if (!show) return(NULL)
  selectizeInput(ns("selector"), label = label, choices = NULL,
                 options = list(
                   placeholder = ifelse(optional, "None required", 'Select a variable'),
                   onInitialize = I('function() { this.setValue(""); }')
                 ))
}
# write filters in terms of x
#' @export
select_var <- function(input, output, session, suitable, raw) {
  var_choices <- reactive({
    unlist(lapply(req(raw()), suitable))
    })
  observe({
    updateSelectizeInput(session, "selector",
                      choices = names(raw())[var_choices()])
  })
  return(list(var_name = reactive({input$selector})))
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
                  options = list(
                    placeholder = 'Select a data frame',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
      )
    )
  }
#' @export
select_data_frame <- function(input, output, session, ...) {
  get_raw <- reactive({
    req(input$frame_name)
    eval(parse(text = input$frame_name))
  })
  max_n <- reactive({nrow(get_raw())})
  codebook <- reactive({"The codebook will be here"})
  name <- reactive({input$frame_name})

  list(get_raw = get_raw, max_n = max_n, codebook = codebook, name = name)
}



#' @export
sample_size_UI <- function(id, ...) {
  ns <- NS(id)
  tagList(
  samp_n <- selectizeInput(ns("samp_n"), "Sample size n",
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
    updateSelectizeInput(session, "samp_n",
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
