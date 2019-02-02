#' Two-sample t-test plot
#'
#' @export
two_sample_t_plot <-  function(formula, data, level = 0.95,
                               show_mean = TRUE, show_ci = TRUE,
                               show_t = TRUE, var_equal = TRUE) {
  color_formula <- formula[c(1,3)] # one-sided formula
  ci_formula  <- formula


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
                         var.equal = var_equal)

    left_mean <- tmp$estimate[2]
    res <- left_mean + tmp$conf.int

    res <- as.list(res)
    names(res) <- c("low", "high")
    res$left_mean <- left_mean
    res$midpoint <- 1.5
    res$p_label <- nice_p(tmp$p.value, 3)

    T_stats <- as.data.frame(res)
    P <- P %>%
      gf_errorbar(low + high ~ midpoint, data = T_stats,
                  width = 1, show.legend = FALSE) %>%
      gf_text(high ~ midpoint, color = "black", label = ~ p_label,
              data = T_stats, vjust = 0)
  }

  P
}
