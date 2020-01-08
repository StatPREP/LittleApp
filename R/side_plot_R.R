#' Graphics for the 2020 revisions of Little Apps
#'
#' plot_values() creates the auxilliary graphic showing
#' the distribution of response and model values. Note
#' that covariates are not displayed even if they  have been
#' included in the model values. This simplifies the display.
#'
#' @param raw the response variable as a vector
#' @param fitted the fitted model values as a vector
#' @param explan the explanatory variable as a vector
#' @param sd if `TRUE`, show the interval of plus-or-minus one
#' standard deviation
#' @param R if `TRUE` show R  and R-squared in the graph.
#' @param F if `TRUE`  show n and F statistic
#' @param violin if `TRUE` show a violin plot.
#' @examples
#' with(mosaicData::Galton,
#' side_plot_R(height, fitted(lm(height ~ father)),
#'   explan = father))
#' @import ggplot2
#' @import ggformula
#' @import dplyr
#' @export
side_plot_R <- function(raw, fitted, explan = "bogus", dflex = 1,
                        violin = FALSE,
                        R2 = TRUE,
                        F = TRUE,
                        sd = TRUE) {

  Pts <- data.frame(raw = raw, model = fitted) %>%
    tidyr::gather(key = source,  value = v) %>%
    mutate(xpos = source)
  if (is.numeric(explan)) {
    explan <- explan  -  median(explan)
    explan <- ((explan ) /
                 diff(range(explan))) / 2
    Pts  <- Pts %>%
      dplyr::mutate(xpos = c(explan + 2,  explan + 1))
    P <-
      ggformula::gf_jitter(v ~ source,  data = Pts,  alpha = 0) %>%
      ggformula::gf_jitter(v ~ xpos, data = Pts, width = 0.1,
                color =  ~  source,  alpha  = 0.2)
  } else {
    P <- ggformula::gf_jitter(v ~ source, data  = Pts,
                   color = ~ source, alpha  = .2,
                   width = 0.4, height = 0)
  }

  Stats <-  mosaicCore::df_stats(v ~ source, data = Pts,
                     m = mean, var = var)  %>%
    dplyr::mutate(high  =  m  + sqrt(var), low = m - sqrt(var))
  R2val <- Stats$var[1] /  Stats$var[2]

  if (violin)
    P <- P %>%
    ggformula::gf_violin(fill = ~ source, alpha  = 0.2,
              color = ~ source)
  if (sd)
    P <- P %>%
    ggformula::gf_errorbar(high + low ~ source, data = Stats,
                inherit  = FALSE) %>%
    ggformula::gf_point(m ~ source, data = Stats, color = "black",
             inherit = FALSE)
  label <- ""
  Rlabel <- paste0("R=", signif(sqrt(R2val), 2),
                   "\nR^2=", signif(R2val, 2), "\n")
  if (R2) {
    label <- Rlabel
  }
  Fval <- ((length(raw) - dflex)/ dflex) * (R2val / (1 - R2val))
  Flabel <- paste("n=", length(raw),
                  "\ndflex=",  dflex, "\nF=",
                  signif(Fval, 2) )
  Rlabel <- paste(Rlabel,  Flabel)
  if (F) {
    label <- paste(label,  Flabel)
  }
  if (F || R2) {
  P <- P %>%
    ggformula::gf_label(low ~ 2.5, data = Stats[2,],
             label = label,
             label.size  = 0, fill = "blue",
             color  =  "black",  vjust  =  0,
             hjust  = 1,
             alpha  = 0.2)
  }
  Label <- gf_label(1 ~ 1, label =  Rlabel) %>%
    gf_theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(color = NA))

  P  <-
    P %>% ggformula::gf_labs(x = "", y = "") %>%
    ggformula::gf_theme(axis.ticks.x = element_blank(),
             axis.text.y = element_blank(),
             legend.pos = "none") %>%
    ggformula::gf_refine(scale_colour_manual(
      values = c("blue", "black")))
  return(list(P = P, label=Label, stats =
                list(R2=R2val, n=length(raw), dflex=dflex)))

}
