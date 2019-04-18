#' Run a Little App
#'
#' @param app character string  naming the Little App. Can also be
#' a regex that uniquely identifies the appl
#'
#' @export
LA_run <- function(name, where = c("browser", "viewer")) {
  Apps <- get_app_info(name)
  runApp(system.file(Apps[1,  "name"], package = "LittleApp"))
}
