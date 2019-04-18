#' Selectors for variables
#'
#' You may wish to restrict the selection of variables to a specific type,
#' e.g. categorical or perhaps categorical with fewer than n levels. These
#' functions create such selectors, which are then passed to the standard reactives.
#' See `LA_var_types` for the structure on which these selectors operate
#'
#'
#' @export
LA_selectNumeric <- function(none = FALSE) {
  function(x) {
    x %>% filter(numeric) %>% .$vname %>%
      add_choose_none(.,  none)
  }
}
#' @export
LA_selectCategorical <- function(max_levels = 5, none = FALSE) {
  function(x) {
    x %>%
    filter(!numeric, n_levels <= max_levels) %>%
    .$vname %>%
    add_choose_none(.,  none)
  }
}
#' @export
LA_selectNone <- function() {
  function(x) character(0)
}
#' @export
LA_selectAll <- function(none = FALSE) {
  function(x) x$vname %>% add_choose_none(., none)
}

add_choose_none <- function(vars, none = FALSE) {

  if (none) c(".none." = 1, ".none." = 1, vars)
  else vars
}
