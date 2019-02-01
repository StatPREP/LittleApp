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
#'
#' @return An environment holding the components of "basic data"
#'
#' @examples
#' \dontrun{
#' LA_read_data("Health", package = "littleapp2")
#' }
#'
#' @export
LA_read_data <- function(data_name = "Health", package = "littleapp2") {
  this_env <- new.env()
  #cat("Start reading data\n")
  data(list = data_name, package = package, envir = this_env)
  contents <- names(this_env)
  # if loading an .rda in the wrong format (that is, without documentation)
  # create the needed structures.
  if ( ! "frame" %in% contents) {
    if (length(contents) > 1 )
      stop("Ordinary data files should have a single item: a data frame.")
    this_env$frame <- this_env[[contents]]
  }
  #cat("Setting up types\n")
  if (! "types" %in% contents) this_env$types <- LA_var_types(this_env$frame)
  #cat("Setting up documentation\n")
  if (! "codebook" %in% contents) {
    # get documentation from package
    db <- tools::Rd_db(package)
    doc_contents <- db[[paste0(data_name, ".Rd")]]
    this_env$codebook <- paste(capture.output(tools::Rd2HTML(doc_contents)),
                               collapse = "\n")
  }
  #cat("Finish reading data\n")
  this_env
}

#` @export
get_a_sample <- function(size, stratify, strat_var, vars, frame){
  frame <- frame[names(frame) %in% vars] %>% na.omit()
  F <- if (stratify && strat_var != "1" && !is.numeric(frame[strat_var])) {
    # need to resample in case there are not enough
    # cases in any given stratum
    frame %>% group_by(!!as.name(strat_var)) %>%
      sample_n(size = size, replace = TRUE)
  }  else {
    frame %>% sample_n(size = pmin(nrow(frame), size))
  }

  F

}

