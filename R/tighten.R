#' Tighten up the UI
#'
#' @export
tighten <- function(control, top=-10, bottom=-20) {
  style <- glue::glue("display: inline-block;vertical-align:top; width: 125px;
      margin-top: {top}px; margin-bottom: {bottom}px;")
  div(style=style, control)
}
