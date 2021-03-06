#' Bring up a template for a new little app in the editor
#'
#' @export
LA_template_for_app <-
  function() {
    system.file("LA_TEMPLATE/app.R", package = "LittleApp") %>%
      readLines(.) %>%
      paste(., collapse = "\n") %>%
      rstudioapi::documentNew(.)
  }
