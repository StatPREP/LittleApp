#' Add violins to a plot
#'
#' If there is a numerical explanatory variable, break it into three divisions.
#' Otherwise, use the categorical explanatory variable directly.
#'
#' @param P The plot so far
#' @param data The data to be used (same as in P)
#' @param xvar the x variable values
#' @param yvar_name the name of the y variable
#'
#' @export
add_violin <- function(P = NULL, data, xvar_name,  yvar_name, nlevels = 3) {
  if (is.numeric(data[[xvar_name]])) {
    data[[xvar_name]] <- mosaic::ntiles(data[[xvar_name]], nlevels, format = "center")
    formula <-  as.formula(glue::glue("{yvar_name} ~ {xvar_name}"))
    group_formula <- as.formula(glue::glue("~ {xvar_name}"))
    width = mean(abs(diff(unique(data[[xvar_name]]))))/4
    gf_violin(P, formula, group = group_formula, width = width,
              data = data, inherit = FALSE, alpha = 0.75, fill = "gray", color = NA)
  } else
    gf_violin(P, data = data, alpha =  0.75, color = NA, fill = "gray")
}
