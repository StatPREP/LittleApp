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
                       sd  =  TRUE, R2 = TRUE,
                       F = TRUE) {
  response <- data[[formula[[2]]]]
  response_name <- as.character(formula[[2]])

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
  P1 <-
    if  (is.numeric(explanatory)) {
      gf_point(plot_formula, data  = data,
               color  = color_formula, alpha = 0.2)
    } else {
      gf_jitter(plot_formula,  data = data,
                color  = color_formula,
                alpha = 0.2,  width  =  0.2)
    }
  P1 <-
    if (show_model) {
      color_formula <-
        if  (is.null(covariate)) "blue"
      else as.formula(paste("~", covariate_name))
      if (is.numeric(explanatory)) {
        mod_plot_formula <-
          as.formula(paste("model_output ~",
                           explanatory_name))
        P1 %>%
          gf_line(mod_plot_formula, data  = data,
                  color = color_formula)
      } else {
        mod_plot_formula <-
          as.formula(paste("model_output  + model_output ~",  explanatory_name))
        P1 %>%
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

  list(P1 = P1 %>% gf_theme(legend.position  =  "left"),
       P2  = P2,
       model = model)
}
