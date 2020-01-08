#' Make the basic plots
#'
#' A basic plot shows two or three variables:
#' - a response
#' - an explanatory variable
#' - a covariate (optional)
#' Constructs two graphics, one showing the data
#' and (optionally) the fitted model, the other showing
#' showing the side plot comparing the distribution
#' of raw to  model values
#' @param formula of form A ~ B or A ~ B + C. You can also
#' use,  say, ns(B,2) or * instead of +. But at most two
#' explanatory variables.
#' @param data a data frame with the sample of data to display
#' @param show_model flag to draw the model
#' @param sd  logical flag to show the plus-or-minus 1 sd in  the side plot
#' @param R2 logical flag to show the R-squared in the side plot
#' @param F  logical flag to show the F value in the side plot
#'
#' @examples
#' make_model_plots(wage ~ splines::ns(educ, 2)*sex,  data = mosaicData::CPS85)
#' @return  A list with the two plots (P1 and P2) and
#' the fitted model
#' @export

make_model_plots <- function(formula, data,
                       show_model = TRUE,
                       sd  =  TRUE, R2 = FALSE,
                       F = FALSE) {
  response <- data[[formula[[2]]]]
  response_name <- as.character(formula[[2]])
  top  <- NULL # a flag
  if (length(unique(response))==2) {
        levels = as.character(unique(response))
        top = levels[2]
        bottom = levels[1]
        response <- as.numeric(response == top)
        data[[response_name]] <- response
      }

  explanatory_vars <- all.vars(formula[[3]])

  covariate <- NULL
  explanatory <- data[[explanatory_vars[1]]]
  explanatory_name <- explanatory_vars[[1]]
  # is there  a  covariate
  if (length(explanatory_vars) == 2) {
    covariate <- data[[explanatory_vars[2]]]

    covariate_name <- explanatory_vars[2]
  }
  model <- lm(formula, data)
  data$model_output  <- fitted(model)

  #  pull  out the  spatial  formula
  plot_formula <- as.formula(paste(response_name, "~",
                                   explanatory_name))
  color_formula <-
    if  (is.null(covariate)) "black"
  else as.formula(paste("~", covariate_name))
  alpha <- LA_point_alpha(nrow(data))
  P1 <-
    if  (is.numeric(explanatory) &&
         length(unique(explanatory)) == 2 ) {
      gf_point(plot_formula, data  = data,
               color  = color_formula, alpha = alpha)
    } else {
      gf_jitter(plot_formula,  data = data,
                color  = color_formula,
                alpha = alpha,
                width  =  0.2, height = 0.1)
    }
  if (!is.null(top)) {
    # format the  y  scale nicely  for a dicotomous
    # variable
    P1 <- P1 %>%
      gf_refine(
      scale_y_continuous(
        response_name, breaks  =  c(0, .25, .5, .75, 1),
        labels = c(bottom, .25,  .5, .75, top),
        limits = c(-.1, 1.1)))
  }

    if (show_model) {
      color_formula <-
        if  (is.null(covariate)) "blue"
      else as.formula(paste("~", covariate_name))
      if (is.numeric(explanatory)) {
        mod_plot_formula <-
          as.formula(paste("model_output ~",
                           explanatory_name))
        P1  <- P1 %>%
          gf_line(mod_plot_formula, data  = data,
                  color = color_formula)
      } else {
        mod_plot_formula <-
          as.formula(paste("model_output  + model_output ~",  explanatory_name))
        P1 <- P1 %>%
          gf_errorbar(mod_plot_formula,
                      data = data,
                      color  =  color_formula)
      }
    }



  P2 <- side_plot_R(response,
                    data$model_output,
                    explan = explanatory,
                    df = model$rank -  1,
                    violin = FALSE,
                    R2 = R2,
                    F = F,
                    sd = sd)
  if (!is.null(top))
    P2$P <- P2$P  %>% gf_lims(y  = c(-.1, 1.1))

  list(P1 = P1 %>% gf_theme(legend.position  =  "left"),
       P2  = P2,
       model = model)
}
