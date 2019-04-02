#' Specifies the available data sets
#'
#' @export
LA_available_data <- function() {
  list(NHANES = "NHANES:NHANES",
       Galton = "Galton:mosaicData",
       Births_2014 = "Births_2014:SDSdata",
       CPS85 = "CPS85:mosaicData",
       diamonds = "diamonds:ggplot2",
       SAT_test = "SATx:SDSdata"
       )
}
