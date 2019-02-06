#' Graphics for bootstrapping and sampling
#'
#'
#'
#'
#' @export
show_bootstrap_sample_cont <- function(data, formula, ns = 3) {
  name_x <- as.character(formula[[3]])
  name_y <- as.character(formula[[2]])
  curve_formula <- y ~ splines::ns(x, ns)
  # bootstrapper object
  bsr <- bootstrapper(1)
  BSpts <- bsr(data)
  ggplot(data, aes_string(x = name_x, y = name_y)) +
    geom_point(shape = 21, size = 6, fill = "white") +
    geom_text(label = "0", hjust = 0.5, vjust = 0.5, size = 10/.pt) +
    geom_point(data = BSpts, aes(group = .row), shape = 21, size = 6, fill = "blue", alpha = 1) +
    geom_text(data = BSpts, aes(label = .copies, group = .row), hjust = 0.5, vjust = 0.5, size = 10/.pt, color = "white") +
    geom_smooth(data = BSpts, method = "lm", se = FALSE,
                formula = curve_formula, color = "blue") +
    geom_smooth(data = data, method = "lm", se = FALSE,
                formula = curve_formula, color = "black")

}
#' @export
show_bootstrap_sample_disc <- function(data, formula, nreps =  1, width = 0.2,
                                       stat = mean) {
  name_x <- as.character(formula[[3]])
  name_y <- as.character(formula[[2]])

  data[[name_x]] <- as.factor(data[[name_x]])

  offsets <- sample(seq(-width, width, length = nrow(data)))
  data[["xj"]] = as.numeric(data[[name_x]]) + offsets
  data <- group_by_(data, name_x) # attempt to get bootstrapping by groups
  bsr <- bootstrapper(1)
  pts <- bsr(data)
  pts <- pts[!duplicated(pts$.original_id), ]

  Stats <- mosaicCore::df_stats(formula, data = data, stat = stat )
  Stats2 <- mosaicCore::df_stats(formula, data = pts, stat = stat )

  P <- ggplot(data, aes_string(x = name_x, y = name_y)) + geom_blank() +
    geom_point(shape = 21, size = 6, fill = "white", aes(x = xj), data = data) +
    geom_text(label = "0", hjust = 0.5, vjust = 0.5, size = 10/.pt, aes(x = xj)) +
   geom_point(data = pts, aes(x = xj, group = .row),
                shape = 21, size = 6, fill = "blue", alpha = .75) +
    geom_text(data = pts, aes(x = xj, label = .copies, group = .row),
              hjust = 0.5, vjust = 0.5, size = 10/.pt, color = "white")
  if (any(grepl("lower", names(Stats)))) {
    # NOT WORKING
    P <- P +
      geom_errorbar(data = Stats,
                    aes_string(ymin  = "stat_lower", ymax = "stat_upper", x = name_x),
                    width = 0.3) +
      geom_errorbar(data = Stats2,
                    aes_string(ymin = "stat_lower", ymax = "stat_upper", x = name_x,
                        width = 0.3), color = "blue")
  } else {
    P <- P +
      geom_hpline(data = Stats,
                  aes_string(y = "stat", x= name_x),  width = 0.3) +
      geom_hpline(data = Stats2,
                  aes_string(y = "stat", x = name_x, width = 0.3), color = "blue")
  }

  P
}
