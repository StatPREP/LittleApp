#' @export
smoother_plot <- function(state) {
  color_formula <- function() {
    if (length(state$covar) == 0 | any(state$covar == "None selected")) "black"
    else as.formula(paste("~", state$covar))
  }
  # Get the model functions for plotting
  mod <- lm(state$model_formula, data = state$data)
  mfundata <- mosaicModel::mod_eval(mod, nlevels = Inf)

  mod_vals <- mod_eval(mod, data = state$data)
  x_range <- range(mfundata[[state$explan]])
  ebar_width <- diff(x_range) / 15
  x_max <- x_range[2] + 0.25 * diff(x_range)
  x_mod_vals <- x_range[2] + 0.1 * diff(x_range)
  x_raw_vals <- x_range[2] + 0.2 * diff(x_range)
  mod_vals$x_mod_vals <- x_mod_vals
  mod_vals$x_raw_vals <- x_raw_vals
  mod_vals$raw <- state$data[[state$response]]
  ebar_width <- diff(x_range)/15
  std_bars <- data.frame(base = rep(mean(mod_vals$raw, na.rm = TRUE), 2),
                         x = c(x_mod_vals, x_raw_vals))
  std_bars$top <- std_bars$base + c(sd(mod_vals$model_output, na.rm = TRUE),
                                    sd(mod_vals$raw, na.rm = TRUE))
  frame_formula <- as.formula(paste(state$response, "~", state$explan))
  P <-
    gf_point(frame_formula, data = state$data, color = color_formula()) %>%
    gf_line(as.formula(paste("model_output ~ ", state$explan)),
            data = mfundata, color = color_formula()) %>%
    gf_theme(legend.position = "top")

  P <- P %>%
    gf_errorbar(model_output + model_output ~ x_mod_vals, data = mod_vals,
                width = 1*ebar_width, color = "red", alpha = 0.25) %>%
    gf_errorbar(raw + raw ~ x_raw_vals, data = mod_vals,
                width = 1*ebar_width, color = "red", alpha = 0.25)
  P <- P %>%
    gf_errorbar(base + top ~ x, data = std_bars,
                width = ebar_width/2, size = 2, color = "black", alpha = 1.0)

  # # confidence bands
  # # gf_ribbon() not working for some unknown reason.
  #
  # P <- P %>% gf_ribbon(data = mfundata,
  #               as.formula(paste("lower + upper ~ ", V$explan)),
  #               alpha = 0.2, fill = color_formula(), color = NA)
  # Make sure to define INTERVALS above and to place it in user interface


  if (state$show_mod_vals)
    P <- P %>% gf_point(as.formula(paste("model_output ~", state$explan)),
                        data = mod_vals, color = color_formula(),
                        shape = 2)

  if (state$trace_horiz)
    P <- P %>% gf_segment(
      as.formula(paste("model_output + model_output ~ ", state$explan, "+ x_mod_vals")),
      data = mod_vals,
      color = "black", size = 0.1)

  if (state$trace_vert)
    P <- P %>% gf_segment(as.formula(paste("model_output + raw ~ ",
                                           state$explan, "+", state$explan)),
                          data = mod_vals,
                          color = "black", size = 0.1)



  P
}
