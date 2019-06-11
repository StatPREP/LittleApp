#' Add rulers and violins to plots
#'
#' @param P The plot so far
#' @param x_range (xmin, xmax) range of the x variable (for positioning the ruler)
#' @param ruler the shiny input corresponding to the ruler, e.g. `input$yruler``
#'

#'
#'
#' @export
add_y_ruler <- function(P, x_range, ruler = NULL) {


  if (is.null(ruler)) return(P)

  # otherwise, add the Y ruler
  height <- signif(ruler$ymax - ruler$ymin, 3)
  Ruler_info <- data.frame(ymin = ruler$ymin,
                           ymax = ruler$ymax,
                           ymid = (ruler$ymin + ruler$ymax) / 2,
                           x = 0.8, stringsAsFactors = FALSE,
                           label = glue::glue("Top: {signif(ruler$ymax,3)} \nDiff: {height} \nBottom: {signif(ruler$ymin,3)} "))
  # Gosh! I don't know why I need to  do it this way, in raw ggplot
  P <- P  +
    geom_segment(aes(y = Ruler_info$ymin,
                     yend = Ruler_info$ymax,
                     x = max(x_range),
                     xend = max(x_range)),
                 color  = "black",
                 arrow = arrow(ends = "both", length = unit(0.1, "inches"))) +
    geom_label(aes(y = Ruler_info$ymid,
                  x = max(x_range),
                  label = Ruler_info$label),
              color = "black",
              hjust = 1)


  P

}
