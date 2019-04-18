#' Get a listing of the Little Apps including where they are deployed
#'
#' @param pattern an (optional) regex to identify specific apps
#'
#' Returns a data frame giving the names of the Little Apps and the URL where they are deployed.
#' If `pattern` is given, return the info for just the corresponding little apps.
#'
#' @examples
#' get_app_info("jitter")
#' @export
get_app_info <- function(pattern = NULL) {
  file_names <-
    dir(path = system.file(package = "LittleApp"),
        recursive = TRUE,
        pattern = "dcf$")
  file_names <- file_names[grepl("rsconnect", file_names)]
  read_dcf_file <- function(name) {
    yaml::read_yaml(paste0(system.file(package = "LittleApp"), "/", name))
  }
  whole_set <- lapply(file_names, read_dcf_file)

  Res <- tibble::tibble(
    name = unlist(lapply(whole_set, function(x) x[["name"]])),
    url = unlist(lapply(whole_set, function(x) x[["url"]]))
  )
  if (is.null(pattern)) return(Res)
  # look for a matching app
  index <- grep(pattern, Res$name)
  if (length(index) == 0) stop("No such app in LittleApp package.")
  Res[index,]
}





