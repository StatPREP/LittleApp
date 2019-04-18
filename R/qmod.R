#' Simple model value finding function
#'
#' Fits a model and extracts the fitted values. Some part of the
#' model values is assigned to the explanatory variable and the rest to the
#' covariates.
#'
#' @param data a data frame
#' @param mod_formula a formula for the model. The first of the explanatory
#' variables (if any) will be treated as the x variable, the rest will be the
#' covariates
#' @param testing a hold out data set for testing the model (default: use training data)
#' @param keep_all Flag (default: FALSE) to return all the columns in `data` or
#' just the ones named as response, explanatory variables, or covariates.
#'
#' @return A data frame with columns from the data (see `keep_all`), and with
#' - `mod_total` the model-value column,
#' - `from_x` the part of `mod_total` contributed to by the explanatory variable
#' - `covar` the part of `mod_total` contributed to by the covariates (including
#' the intercept)
#' - `xperp` the part of the explanatory variable perpendicular to the covariates (includ
#' the intercept)
#'
#' @export
qmod <- function(data, mod_formula, testing = data, keep_all = FALSE) {
  explan_var <- all.vars(mod_formula[[3]])[1]
  response_var <- all.vars(mod_formula[[2]])
  nexplan <- length(all.vars(mod_formula[[3]]))

  # handle the y ~ 1 case specially
  if (nexplan == 0) {
    return(
      data[response_var] %>%
        mutate(mod_total = mean(data[[response_var]], na.rm = TRUE),
               from_x = 0,
               covar = total,
               xperp = 0)
    )
  }

  # We're going to modify this a bit, but keep the original for the output
  for_training <- data

   # get the model values
  the_levels <- unique(for_training[[explan_var]])
  if (! is.numeric(the_levels)) {
    if (length(unique(the_levels)) > 2) warning("Contrasting first level with remaining levels")
    for_training[[explan_var]] <- as.numeric(for_training[[explan_var]] == the_levels[1])
    testing[[explan_var]] <- as.numeric(testing[[explan_var]] == the_levels[1])

  }
  response_levels <- unique(for_training[[response_var]])
  if (! is.numeric(response_levels)) {
    if (length(unique(response_levels)) > 2) warning("Contrasting first respose level with remaining levels")
    for_training[[response_var]] <- as.numeric(for_training[[response_var]] == response_levels[1])
  }

  mod <- lm(mod_formula, data = for_training)
  # get the part of the model values due to the covariates
  just_covars <-  testing
  just_covars[[explan_var]] <- mean(testing[[explan_var]])
  covar <- predict(mod, newdata = just_covars)


  # get the part of the explanatory variable unexplained by the covariates
  if (nexplan > 1) {
    formula2  <- mod_formula
    formula2[[2]] <- as.name(explan_var)
    formula2[[3]][2] <- 1
    mod2 <- lm(formula2, data = testing)
    xperp <- resid(mod2)
  } else {
    xperp <- testing[[explan_var]] - mean(testing[[explan_var]], na.rm = TRUE)
  }
    # assemble the results
  if (! keep_all) testing <- testing[unique(all.vars(mod_formula))]
  mod_vals <- predict(mod, newdata = testing)
  bind_cols(testing,
            data.frame(mod_total = mod_vals,
                       from_x = mod_vals - covar,
                       covar = covar,
                       xperp = xperp))
}

