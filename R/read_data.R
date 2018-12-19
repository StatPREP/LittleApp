#' Load a data set for a Little App
#'
#' @details Ideally, basic data for Little Apps should consist of four components:
#' 1. `frame` - the data frame containing the data
#' 2. `codebook` - a list containing a text description of each variable. The list names
#' should be the corresponding variable names in `frame`.
#' 3. `overall` - a character string describing `frame` as a whole.
#' 4. `types` - a data frame containing one row for each variable, as produce by `LA_var_types()`.
#'
#' You can also use `LA_read_data()` to read an ordinary data frame, either as a `url`
#' or from a package. In this case, the four components will be constructed, but
#' `codebook` and `overall` will be uninformative.
#'
#' @param data_name: character string suitable for `load()`ing the data
#' @param package: character string naming the package where the .rda file exists
#' @param url: character string giving the URL of an .rda file.
#'
#' @return An environment holding the components of "basic data"
#'
#' @examples
#' \dontrun{
#' LA_read_data("Health", package = "littleapp2")
#' LA_read_data(url = "http://StatPREP.org/LittleApps/Cars.rda")
#' }
#'
#' @export
LA_read_data <- function(data_name = "Health", package = "littleapp2", url = NULL) {
  this_env <- new.env()
  if (is.null(url)) data(list = data_name, package = package, envir = this_env)
  else load(url, envir = this_env)

  # if loading an .rda in the wrong format, create the needed structures.
  if ( ! "frame" %in% names(this_env)) {
    contents <- ls(this_env)
    if (length(contents) > 1 ) stop("Ordinary data files should have a single item: a data frame.")
    this_env$frame <- this_env[[contents]]
    remove(list = contents, envir = this_env)
    this_env$overall <- "No description given."
    this_env$codebook <- paste("No description for", names(this_env$frame))
    this_env$codebook <- as.list(this_env$codebook)
    names(this_env$codebook) <- names(this_env$frame)
    this_env$types <- LA_var_types(this_env$frame)
  }

  this_env
}
