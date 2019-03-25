#' Make an augmented plot
#'
#' A standard plot, but with bars on the side indicating spread of
#' actual values and model values
#'
#' @param plot_formula A formula of the form y ~ x
#' @param model_formula A formula for the model (may include nonlinearities, interactions)
#' @param data the data frame used to fit the model and plot the points
#' @param color the gf_point argument for color
#' @param height the gf_jitter argument for jitter height
#' @param width the gf_jitter argument for jitter width
#' @param alpha the gf_jitter argument for alpha
#' @param trace_vert logical. Whether to draw thin lines from the actual value to the model values
#' @param trace_horiz logical. Whether to draw thin lines from the model values to the bars on  the side
#' @param show_mod_vals logical. Whether to place a small dot at the model values
#'
#' @examples
#' smoother_plot(height ~ mother, height ~ splines::ns(mother, 2) * sex, Galton, color = ~ sex)
#'
#'
#' @export
smoother_plot <- function(plot_formula, model_formula, data,
                          color = "black", width = 0, height = 0, alpha  = 0.5,
                          trace_vert = TRUE, trace_horiz = TRUE,
                          show_mod_vals = TRUE) {
  explan_name <- as.character(rlang::f_rhs(plot_formula))
  response_name <- as.character(rlang::f_lhs(plot_formula))
  # Get the model functions for plotting
  mod <- lm(model_formula, data = data)
  # Construct points over the full range of the model for the line
  mod_fun_data <- mosaicModel::mod_eval(mod, nlevels = Inf)
  # Construct model output at data points
  mod_vals <- mosaicModel::mod_eval(mod, data = data)
  # figure out where to plot the horizontal bars on the right
  x_range <- range(mod_fun_data[[explan_name]])
  ebar_width <- diff(x_range) / 15
  x_max <- x_range[2] + 0.25 * diff(x_range)
  x_mod_vals <- x_range[2] + 0.1 * diff(x_range)
  x_raw_vals <- x_range[2] + 0.2 * diff(x_range)
  mod_vals$x_mod_vals <- x_mod_vals
  mod_vals$x_raw_vals <- x_raw_vals
  mod_vals$raw <- data[[response_name]]
  ebar_width <- diff(x_range)/15
  std_bars <- data.frame(base = rep(mean(mod_vals$raw, na.rm = TRUE), 2),
                         x = c(x_mod_vals, x_raw_vals))
  std_bars$top <- std_bars$base + c(sd(mod_vals$model_output, na.rm = TRUE),
                                    sd(mod_vals$raw, na.rm = TRUE))
  P <-
    gf_point(plot_formula, data = data, color = color) %>%
    gf_line(as.formula(paste("model_output ~ ", explan_name)),
            data = mod_fun_data, color = color) %>%
    gf_theme(legend.position = "top")

  P <- P %>%
    gf_errorbar(model_output + model_output ~ x_mod_vals, data = mod_vals,
                width = 1*ebar_width, color = color, alpha = 0.25) %>%
    gf_errorbar(raw + raw ~ x_raw_vals, data = mod_vals,
                width = 1*ebar_width, color = "red", alpha = 0.25)
  P <- P %>%
    gf_errorbar(base + top ~ x, data = std_bars,
                width = ebar_width/2, size = 1.5, color = I(c("blue", "black")), alpha = 1.0)

  # # confidence bands
  # # gf_ribbon() not working for some unknown reason.
  #
  # P <- P %>% gf_ribbon(data = mod_fun_data,
  #               as.formula(paste("lower + upper ~ ", V$explan)),
  #               alpha = 0.2, fill = color_formula(), color = NA)
  # Make sure to define INTERVALS above and to place it in user interface


  if (show_mod_vals)
    P <- P %>% gf_point(as.formula(paste("model_output ~", explan_name)),
                        data = mod_vals, color = color,
                        shape = 2)

  if (trace_horiz)
    P <- P %>% gf_segment(
      as.formula(paste("model_output + model_output ~ ", explan_name, "+ x_mod_vals")),
      data = mod_vals,
      color = "black", size = 0.1)

  if (trace_vert)
    P <- P %>% gf_segment(as.formula(paste("model_output + raw ~ ",
                                           explan_name, "+", explan_name)),
                          data = mod_vals,
                          color = "black", size = 0.1)



  P
}
