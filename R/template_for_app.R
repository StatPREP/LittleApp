#' Bring up a template for a new little app in the editor
#'
#' @export
LA_template_for_app <-
  function() {
    system.file("LA_TEMPLATE/app.R", package = "littleapp2") %>%
      readLines(.) %>%
      paste(., collapse = "\n") %>%
      rstudioapi::documentNew(.)
  }
