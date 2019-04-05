#' Plots for striped densities
#' A Little App graphic dividing a density up into labeled groups.

#' @param yval numerical vector of values for which to show the density
#' @param xval optional vector defining the groups each `yval` value belongs to. Typically
#' a character vector or factor of the same length as `yval`.
#' @param violin Logical flag specifying whether the graph should have the
#' traditional density vs y frame or  be in the form of a violin plot.
#' @param show_normal Logical flag specifying whether to overlay a normal density
#' @param breaks numerical  vector of y values at which to divide the density plot
#' @param qbreaks numerical vector, like `breaks`, but giving the cumulatave probabilities at which
#' to divide the density plot. If `qbreaks` is specified, it will override `breaks`. That is,
#' `breaks` will be automatically set  to the quantiles defined by `qbreaks`.
#' @param labels character vector of labels for the regions defined by `breaks`. There
#' must have length 1 greater than `breaks`, since k numbers in `breaks` defines k+1 regions.
#' The labels must be unique: no repeats.
#' @param kill_in_labels a character string giving a regular expression identifying components
#' of each label to delete when displaying labels.  Default: `"_.+$"`, which deletes
#' any text following `_` in the labels. (Implication: If you want two  labels to
#' display identically, use `_` as the separator for a  unique-making suffix, e.g. `_left` and `_right`.)
#' @param show_percent Logical indicating whether to show the percent covered by
#' each of the divisions defined by `breaks` or `qbreaks`.
#' @param vwidth width for the violins
#' @param max_x_levels maximum number of levels of `xvar` (default 10) to throw an error indicating
#' that there are "too  many" breaks. This is really just a guard against accidental
#' specification of `xvar` as a numeric vector with lots and lots of levels that
#' would lmake the graphic confusing. As needed, override this.
#'
#' @examples
#' plot_density_y(rnorm(10000), c(1:2))
#' plot_density_y(rexp(10000) * c(1, 2), xvar = c("minor", "major"),
#' qbreaks = c(.1, .25, .75, .9)) %>%
#'   gf_lims(x  = c(0, 6))
#' @export
plot_density_y <- function(yvar=NULL, xvar = "all", violin = FALSE,
                           show_normal = FALSE,
                           breaks = c(-3, -2, 2, 3),
                           qbreaks = NULL,
                           labels = c("rare_left", "uncommon_left",
                                      "common", "uncommon_right", "rare_right"),
                           kill_in_labels = "_.+$", # ".+" to kill entirely
                           show_percent = TRUE,
                           vwidth = 0.4,
                           max_x_levels = 10) {
  if (length(unique(xvar)) > max_x_levels)  stop("Too many levels. See argument max_x_levels.")
  # Split up yvar by levels of xvar
  if (length(xvar) != length(yvar)) # make sure xvar is 1 to 1 with yvar
    xvar <- rep(xvar, length.out = length(yvar))

  short_labels <- gsub(kill_in_labels,  "", labels)


  grand_mean <- mean(yvar, na.rm = TRUE)
  grand_sd   <-   sd(yvar, na.rm = TRUE)
  grand_range<- extendrange(yvar, f = 0.1)
  Densities <- NULL
  Labels <- NULL
  Normal <- NULL
  Stripes <- NULL
  Density_scale <- NULL
  y_grid_values <- seq(grand_range[1], grand_range[2], length = 500)
  for (xvalue in unique(xvar)) {
    This_data <- yvar[xvar == xvalue]
    this_mean <- mean(This_data, na.rm = TRUE)
    this_sd   <-   sd(This_data, na.rm = TRUE)
    Tmp <- with(density(This_data), tibble(x, y)) %>%
      mutate(level = xvalue, common  = "common")
    if ( ! is.null(qbreaks)) breaks <- as.numeric(quantile(This_data, qbreaks))
    Tmp$common <- cut(Tmp$x,
                      breaks = c(-Inf, breaks, Inf),
                      labels = labels)
     # count how many in each percent
     In_groups <- tibble(y = This_data) %>%
       mutate(label = as.character(
         cut(y, breaks = c(-Inf, breaks, Inf), labels = labels))) %>%
       group_by(label)  %>%
       summarise(percent = 100 * n() / nrow(.)) %>%
       mutate(level = xvalue) %>%
       na.omit()
     # figure out where to place the annotations
     dpos <- diff(breaks)
     dpos <- c(breaks[1] - dpos[1],
               breaks,
               breaks[length(breaks)] + dpos[length(dpos)])
     label_positions <- (dpos[-1] + dpos[-length(dpos)]) / 2.0
     Foo <- tibble(x = label_positions, y = mean(Tmp$y) / 2,
                               short = short_labels, label = labels)
     Foo <- Foo %>% left_join(In_groups) %>%
       mutate(percent = round(percent, 1)) %>% na.omit()
     if (show_percent) Foo$short <- paste0(Foo$short, ": ", sprintf("%3.1f", Foo$percent), "%")

     Theory <- tibble(x = y_grid_values, y = dnorm(x, mean = this_mean, sd = this_sd), level = xvalue) %>%
       na.omit()
     These_stripes <- tibble(x = this_mean + this_sd * (-3):3,
                            y = dnorm(x,  mean = this_mean, sd = this_sd),
                            level = xvalue) %>% na.omit()
     For_scale <- tibble(scale = dnorm(0, sd=this_sd), level = xvalue)
     # Accumulate across values of xvar
     Densities <- bind_rows(Densities, Tmp) %>% na.omit()
     Labels <- bind_rows(Labels, Foo) %>% na.omit()
     Normal <- bind_rows(Normal, Theory) %>% na.omit()
     Stripes <- bind_rows(Stripes, These_stripes) %>% na.omit()
     Density_scale <- bind_rows(Density_scale, For_scale) %>% na.omit()
  }

  if (violin) {
    # Scale by the density of the normal distribution at the center
    Densities <- Densities %>%
      # create a column of integers to center the violins around
      mutate(xpos = as.numeric(as.factor(level))) %>%
      # scale the densities so that the maximimum is <vwidth>
      left_join(Density_scale) %>%
      mutate(y = vwidth * y / max(scale)) %>%
      mutate(left = xpos - y, right = xpos + y) %>%
      na.omit()
      # group_by(level) %>%
      # mutate(y =  y / (mean(y)*diff(range(x)))) %>%
      # ungroup() %>%
      # mutate(y = vwidth  * y / max(y)) %>%
      # # symmetrize the density around the integers
      # mutate(left = xpos - y, right = xpos  + y)
    # ribbon plot and labels
    P <- gf_blank( x ~ level, data =  Densities) %>%
      gf_segment(x + x ~ left + right, data = Densities, color = ~ common, alpha = 0.75)

    if(show_normal) { # Turn the theory into violins
      Normal <- Normal %>%
        mutate(xpos = as.numeric(as.factor(level))) %>%
        left_join(Density_scale) %>%
        mutate(y  =  vwidth * y / max(scale)) %>%
        mutate(left = xpos - y, right = xpos + y) %>%
        na.omit()
      P <- P %>%
        gf_segment(x + x ~ left + right, data = Normal, alpha = 0.45, color = "white")
      Stripes <- Stripes %>%
        mutate(xpos = as.numeric(as.factor(level))) %>%
        left_join(Density_scale) %>%
        mutate(y  =  vwidth * y / max(scale)) %>%
        mutate(left = xpos - y, right = xpos + y) %>%
        na.omit()
      P <- P %>%
        gf_segment(x + x ~ left + right, data = Stripes, color = "white", alpha = 0.75)
    }
    P <- P %>%
      gf_label(x ~ level, label = ~short, data = Labels, color = ~ label, fill = "white", alpha = 0.5) %>%
      gf_theme(
        scale_y_continuous(
          sec.axis = sec_axis( ~ (. - grand_mean) / grand_sd, name = "z values",
                               breaks = (-3):3, labels = c("-3sd","-2sd", "-1sd", "mean",
                                                           "+1sd", "+2sd", "+3sd"))
        ))

  } else { # traditional density vs y plot
    P <- gf_vline(xintercept = grand_mean + grand_sd * (-3):3, color = "white", alpha = 0.1) %>%
      gf_ribbon(y + 0 ~ x, data = Densities, fill = ~ common,
                color = NA, alpha = 0.75)
    if (show_normal) {
      P <- P %>%
        gf_ribbon(y + 0 ~ x,  data = Normal, fill  = "white", alpha = 0.45) %>%
        gf_segment(y + 0 ~ x + x, data = Stripes,  color = "white", alpha = 0.5)
    }
    P <- P %>%
      gf_text(data = Labels, gformula = y ~ x, group = ~ level,
              label = ~ short,  fill = NA, inherit = FALSE,
              angle = 90, hjust = 0, vjust = 0.5) %>%
      gf_theme(
        scale_x_continuous(
          sec.axis = sec_axis( ~ (. - grand_mean) / grand_sd, name = "z values",
                               breaks = (-3):3, labels = c("-3sd","-2sd", "-1sd", "mean",
                                                           "+1sd", "+2sd", "+3sd"))
        ))



    if (length(unique(xvar)) > 1)
      P <- P %>% gf_facet_grid(level ~ ., drop = TRUE)
  }

  P  %>%
    gf_theme(legend.position = "none") %>%
    gf_theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}





