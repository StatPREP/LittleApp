#' Compute a short description of the type of a variable
#'
#' @return a data frame with four fields
#' - vname the name of the variable
#' - numeric logical saying if the var is numeric
#' - class the variable's class
#' - n_levels how many unique levels (whether numeric or categorical)
#'
#' @param .data a data frame
#' @export
LA_var_types <- function(.data) {
  f <- function(x) {
    data.frame(numeric = is.numeric(x),
               class = class(x)[1],
               n_levels = length(unique(x)),
               stringsAsFactors = FALSE)
  }
  Tmp <- lapply(.data, f)

  res <- bind_cols(vname = names(Tmp), bind_rows(Tmp))

  res
}


