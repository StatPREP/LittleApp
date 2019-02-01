#' Statistical reports on models
#'
#' @export
simple_regression_report <- function(model_formula, data) {
  mod <- lm(model_formula,  data = data)
  # If more than two coefficients, then little-r is  meaningless.
  # If two, get sign of little-r from second coefficient
  coefficients <- coef(mod)

  Stats <- broom::glance(mod)

  little_r <-
    if(length(coefficients) == 2) {
      round(sign(coefficients[2]) * sqrt(Stats$r.squared), 3)
    } else {
      "invalid for nonlinear models and models with covariates."
    }

  anova_table <- capture.output(anova(mod))
  anova_table <- anova_table[-(1:2)]
  anova_table <- paste(anova_table, collapse = "\n")
  regression_table <- paste( capture.output(broom::tidy(mod))[-c(1,3)], collapse = "\n")

  string <-  "
<h3>Statistics on the model</h3>
<ul>
<li>little-r is {little_r}</li>
<li>R<sup>2</sup> = {round(Stats$r.squared, 3)}</li>
<li>Adjusted R<sup>2</sup> =  {round(Stats$adj.r.squared, 3)}
<li>residual standard error = {signif(Stats$sigma, 3)}</li>
<li>F-statistic:  F = {round(Stats$statistic,1)} with {Stats$df} over {Stats$df.residual} degrees of freedom.</li>
<li>p-value from F-statistic:  {round(Stats$p.value, 4)}</li>
</ul>
<p>\"Adjusted R<sup>2</sup>\" mathematically scales R<sup>2</sup> to what it would be with
  with just 1 degree of freedom. This enables it to be compared across models with different
  degrees of freedom.</p>

<h3>Regression table</h3>
<pre>
{regression_table}
</pre>

<h3>ANOVA table</h3>
<pre>
{anova_table}
</pre>
  "

  glue::glue(string)
}
