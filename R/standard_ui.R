#' Basic control blocks for Little Apps
#'
#' @param width 1-12 twitter bootstrap width of control
#' @param collapsed Logical indicating whether to display box as collapsed initially
#'
#' @export
LA_data_source <- function(width = 12, collapsed = FALSE, covariate = TRUE, facet = FALSE) {
  box(title = "Data source", width = width, status = "primary", solidHeader = FALSE,
      collapsible = TRUE, collapsed = collapsed,
      background = "black",
      selectInput("frame", "Data frame",
                  choices = LA_available_data()) %>% tighten(top=0),
      # Still under development
      # conditionalPanel(condition = "input.frame == 'Your own data'",
      #                 textInput("URL", "URL", value = ""))
      hr(),
      p("Variables selected"),
      # 1 stands for "no variable selected"
      tighten(selectizeInput("var_y", "Response", choices = list("1"))),
      tighten(selectizeInput("var_x", "Explanatory", choices = list("1"))),
      if (covariate) tighten(selectizeInput("covar", "Covariate", choices = list("1"))) else NULL,
      if (facet) tighten(selectizeInput("facet_by", "Facet by:", choices = list("1"))) else NULL
      )
}

#' @export
LA_sample_ui <- function(width = 12, collapsed = FALSE) {
  box(title = "Sample", width = width, status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = collapsed,
      background = "black",
      actionButton("new_sample", "New sample", icon=icon("child"),
                   style = "color: white; background-color: green; font-size: 16px; margin-left: 0px;" ) %>%
        tighten(bottom = 10, top = -10),
      selectInput("samp_size", "Sample size n",
                  choices = c(5,10,20,50,100,200,500,1000,2000),
                  selected = 0) %>% tighten(), # 0 is temporary
      checkboxInput("stratify", "Stratify", value = FALSE) %>% tighten()
  )
}

#' @export
LA_inference <- function(width = 6, collapsed = FALSE){
  box(title = "Inference", width = width, status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = collapsed,
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
