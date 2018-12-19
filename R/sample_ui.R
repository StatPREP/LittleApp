#' Little App interface for sampling/resampling

#' @export
LA_inference <- function(width = 6, resample = TRUE){
  box(title = "Inference", width = width, status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE,
      background = "black",
      checkboxInput("resample", "Resample") %>% tighten(),
      checkboxInput("shuffle", "Randomize") %>% tighten(),
      actionButton("new_trial", "New Trial",
                   icon = icon("dice"),
                   style = "font-size: 16px; margin-left: 0px;" ) %>%
        tighten(top = 0, bottom = 0),
      checkboxInput("accumulate_trials", "Accumulate") %>% tighten()
  )
}
