#' Plots and plot helpers
#'
#' @export
LA_dot_layer <- function(formula, data, color, width, height, alpha = 1, seed = 101) {
  P <- gf_jitter(formula,
                 data = data,
                 color = color,
                 width = width,
                 height = height,
                 alpha = alpha,
                 seed = seed
  ) %>% gf_theme(legend.position = "top")

  if (rlang::f_rhs(formula) == 1)
    P <- P %>% gf_lims(x = c(0, 2)) %>% gf_theme(no_x_axis)

  P
}
