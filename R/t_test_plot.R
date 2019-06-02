#' Two-sample t-test plot
#'
#' @export
two_sample_t_plot <-  function(formula, data, level = 0.95,
                               show_mean = TRUE, show_ci = TRUE,
                               show_t = TRUE, var_equal = TRUE,
                               y_range = NULL,  ruler = NULL) {
  var_y <- as.character(formula[[2]])
  if (is.null(y_range)) y_range = range(data[[var_y]], na.rm = TRUE)
  color_formula <- formula[c(1,3)] # one-sided formula

  P <-
    LA_dot_layer(formula = formula, data = data,
                 color = color_formula, width =  0.15, height = 0,
                 alpha = LA_point_alpha(nrow(data)), seed = 101) %>%
    gf_theme(legend.position = "none")
  Stats <-
    df_stats(formula, data = data,  mn = mean,
             ci = ci.mean(level = !!level)) %>%
    mutate(xpos = c(1.25, 1.75))


  if (show_mean) {
    P <- do.call(gf_errorbar, list(P, mn + mn ~ xpos,
                                   data = Stats,
                                   color = color_formula,
                                   width = 0.25,
                                   size = 2,
                                   show.legend = FALSE))
  }


  if (show_ci) {
    P <- do.call(gf_errorbar, list(P,
                                   ci_lower + ci_upper ~ xpos,
                                   data = Stats,
                                   color = color_formula,
                                   width = 0.13,
                                   size = 1.5,
                                   show.legend = FALSE))
  }
  if (show_t) {
    tmp <- stats::t.test(formula, data = data,
                         var.equal = var_equal, conf.level = level)

    left_mean <- tmp$estimate[2]
    res <- left_mean + tmp$conf.int

    res <- as.list(res)
    names(res) <- c("low", "high")
    res$left_mean <- left_mean
    res$midpoint <- 1.5
    res$right_of_midpoint <- 2.05
    res$p_label <- nice_p(tmp$p.value, 3)

    T_stats <- as.data.frame(res) %>%
      mutate(low = pmax(low, y_range[1]),
             high = pmin(high, y_range[2]))
    P <- P %>%
      gf_errorbar(low + high ~ midpoint, data = T_stats,
                  width = 1, show.legend = FALSE) %>%
      gf_text(high ~ right_of_midpoint, color = "black", label = ~ p_label,
              data = T_stats, vjust = 0)
  }


  # As much as possible, keep all samples on same scale,
  #  but display whole of confidence interval
  total_range <- range(y_range, c(min(Stats$ci_lower), max(Stats$ci_upper)))
  P <- P %>% gf_lims(y = total_range)
  if ( ! is.null(ruler)) {
    height <- signif(ruler$ymax - ruler$ymin, 3)
    Ruler_info <- data.frame(ymin = ruler$ymin,
                             ymax = ruler$ymax,
                             ymid = (ruler$ymin + ruler$ymax) / 2,
                             x = 0.8, stringsAsFactors = FALSE,
                             label = glue::glue("Top: {signif(ruler$ymax,3)} \nHeight: {height} \nBottom: {signif(ruler$ymin,3)} "))

  # Gosh! I don't know why I need to  do it this way, in raw ggplot
     P <- P  +
      geom_segment(aes(y = Ruler_info$ymin,
                       yend = Ruler_info$ymax,
                       x = Ruler_info$x,
                       xend = Ruler_info$x),
                   color  = "black",
                   arrow = arrow(ends = "both", length = unit(0.1, "inches"))) +
      geom_text(aes(y = Ruler_info$ymid,
                    x = Ruler_info$x,
                    label = Ruler_info$label),
                color = "black",
                hjust = 1)
  }

  P
}

#' One-sample t test plot
#' @export
one_sample_t_plot <- function (formula, data, level = 0.95,
                               show_mean = TRUE, show_ci = TRUE,
                               null_hypothesis = 0, y_range = NULL,
                               ruler = NULL) {
  var_y <- as.character(formula[[2]])
  if (is.null(y_range)) y_range = range(data[[var_y]], na.rm = TRUE)
  P <-
    LA_dot_layer(formula = formula, data = data, color = "black", width =  0.15, height = 0,
                 alpha = LA_point_alpha(nrow(data)), seed = 101) %>%
    gf_theme(legend.position = "none")
  Stats <-
    df_stats(formula, data = data,  mn = mean,
             ci = ci.mean(level = !!level)) %>%
    mutate(xpos = c(1.5))

  if ( ! is.null(ruler)) {
    height <- signif(ruler$ymax - ruler$ymin, 3)
    Ruler_info <- data.frame(ymin = ruler$ymin,
                             ymax = ruler$ymax,
                             ymid = (ruler$ymin + ruler$ymax) / 2,
                        x = 0.6, stringsAsFactors = FALSE,
                        label = glue::glue("Top: {signif(ruler$ymax,3)} \nHeight: {height} \nBottom: {signif(ruler$ymin,3)} "))
     P <- P  %>% gf_segment(ymin + ymax ~ x + x,
                            arrow = arrow(ends = "both",
                                          length = unit(0.1, "inches")),
                            data = Ruler_info) %>%
     gf_text(ymid ~ x, label = ~ label, data = Ruler_info, hjust = 1)
  }
  if (show_mean) {
    P <- do.call(gf_errorbar, list(P, mn + mn ~ xpos,
                                   data = Stats,
                                   color = "blue",
                                   width = 0.25,
                                   size = 2,
                                   show.legend = FALSE))
  }


  if (show_ci) {
    tmp <- stats::t.test(data[[var_y]], mu = null_hypothesis)
    res <- data.frame(conf.int = tmp$conf.int)

    res$top <- max(tmp$conf.int)
    res$midpoint <- 1.75
    res$p_label <- nice_p(tmp$p.value, 3)

    T_stats <- as.data.frame(res)

    P <-
      P <- do.call(gf_errorbar, list(P,
                                     ci_lower + ci_upper ~ xpos,
                                     data = Stats,
                                     color = "blue",
                                     width = 0.13,
                                     size = 1.5,
                                     show.legend = FALSE)) %>%
      gf_hline(yintercept = null_hypothesis) %>%
      gf_text(top ~ midpoint, color = "black", label = ~ p_label,
              data = T_stats, vjust = 0)
  }
  # As much as possible, keep all samples on same scale,
  #  but display whole of confidence interval
  total_range <- range(y_range, c(min(Stats$ci_lower), max(Stats$ci_upper)))
  P %>% gf_lims(y = total_range)

}
