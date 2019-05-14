#
# RMS  optimization
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(knitr)
library(kableExtra)
library(LittleApp)
library(markdown)
library(mosaic)
library(ggformula)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Straight-line controls", width = 12, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      noUiSliderInput("slope", "Slope", min = 0, max  = 1,
                      value = c(.25,  .75)),
      noUiSliderInput("offset", "Vertical position", min = 0, max = 1,
                      orientation = "vertical", height = "150px", width = "150px",
                      value = c(.25, .75),  direction = "rtl"),
      hr(),
      checkboxInput("confband", "Show confidence band",  value = FALSE),
      hr(),
      actionButton("new_validation", "New validation set",
                   icon = icon("dice"),
                   style = "font-size: 16px; margin-left: 0px;") %>%  tighten()
      )


# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Sum of square errors",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , p("")
      , LA_data_source(6, covariate = FALSE)    # These are controls available
      , LA_sample_ui(6)      # Comment out any you don't want
      , my_special_controls  # ... and add in your own here.
      #bookmarkButton()      # You can change the order of these
                             # to get a layout you like.
    ),

    LA_body(tabPanel("SSE", plotOutput("sse_plot"))) # The body is entirely pre-defined
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame

  select_x <- function(x) {
    x %>% filter(numeric &  spread < 10000.0) %>% .$vname %>%
      LittleApp:::add_choose_none(.,  FALSE)
  }
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  LA_standard_reactives(input, output, session, the_data, app_state)

  new_validation_set <- function(){ # Note: not reactive
    LittleApp:::get_a_sample(
      input_sample_size(),
      input_stratify(),
      input$var_x,
      c(input$var_x, input$var_y, input_covar()),
      the_data$frame,
      Sys.time() + as.integer(runif(1, min = -100000, max = 100000)))
  }

  best_fit <- reactive({
    cat("In best_fit()\n")
    y <- get_response_var()
    x <- get_explanatory_var()
    mod <- lm(y ~ x)
    spread <-  diff(confint(mod)[2,]) * sqrt(nrow(get_sample()))/ sqrt(50)
    range <- coef(mod)[2] + spread*c(-1,1)
    res <- list(slope = coef(mod)[2], intercept = coef(mod)[1],
         slope_range = extendrange(range,  f = .3),
         offset_range = extendrange(mean(y,  na.rm=TRUE) + c(-1,1) * diff(range(y))/3,  f = -0.35),
         xmean = mean(x, na.rm = TRUE))
    res
  })


  observe({
    cat("Updating sliders\n")
    best <- best_fit()
    updateNoUiSliderInput(session, "slope", range = best$slope_range,
                          value = extendrange(best$slope_range, f = -.2))

    updateNoUiSliderInput(session, "offset", range = best$offset_range,
                          value = extendrange(best$offset_range, f = -0.3))
  })

  get_lines  <- reactive({
    cat("In  get_lines()\n")
    input$new_validation # for the dependency
    best <- best_fit()
    y <- get_response_var()
    x <- get_explanatory_var()

    Slopes <- tibble(
      slopes = c(best$slope, input$slope,  rev(input$slope)),
      offsets = c(mean(y, na.rm = TRUE), input$offset, input$offset)
    ) %>%
      mutate(intercepts = offsets - best$xmean * slopes) %>%
      mutate(ID = LETTERS[1:nrow(.)])

    sse <- numeric(nrow(Slopes))
    XV_sse <- as.data.frame(matrix(0, nrow= nrow(Slopes), ncol  = 25))
    names(XV_sse) <- paste0("xv_", 1:ncol(XV_sse))
    cat(paste(paste0(capture.output(Slopes), collapse = "\n"), "\n"))
    for (k in  1:nrow(Slopes)) {
      sse[k] <- sum((y - (Slopes$intercepts[k] + x * Slopes$slopes[k]))^2)
      for (j in 1:ncol(XV_sse)) {
        VS <- isolate(new_validation_set())
        XV_sse[k, j] <- sum((VS[[1]] -
                               (Slopes$intercepts[k] + VS[[2]] * Slopes$slopes[k]))^2)
      }
    }
    cat(paste(paste0(capture.output(sse),  collapse = ", "), "\n"))
    Slopes$sse <- sse

    bind_cols(Slopes,  XV_sse)
  })
  output$sse_plot <- renderPlot({
    cat("SSE plot\n")
    Lines <- get_lines()
    Performance <- Lines %>% dplyr::select(-slopes, -offsets, -intercepts)
    XV <- tidyr::gather(dplyr::select(Performance, -sse),
                        key = "trial", value = "value", -ID)

    P <- gf_errorbar(sse + sse ~ ID, data = Performance, color = ~ ID)
    P %>%
      gf_jitter(value ~ ID, data = XV,  color = ~ ID, width = .2, height = 0, inherit = FALSE) %>%
      gf_labs(y = "Sum of Square Errors", x = "Model line") %>%
      gf_theme(legend.position = "none")
  })
  output$main_plot <- renderPlot({
    cat("Main plot\n")
    best <- isolate(best_fit())
    Lines <- get_lines()
    P <- standard_dot_plot()
    if (input$confband) P <- P %>%  gf_lm(interval = "confidence", color = NA)
    P %>%
      gf_abline(slope = ~ slopes, intercept = ~ intercepts, data = Lines,  color = ~ID,
                size = 2, alpha = 0.6)

    })
  # Other built-in output widgets besides output$main_plot

  output$statistics <- renderText({
    Lines <- get_lines()
    Lines %>% dplyr::select(ID = ID, slope = slopes, intercept = intercepts, SSE = sse) %>%
      knitr::kable(., format = "html", digits = 3) %>%
      kableExtra::kable_styling(full_width = FALSE)
  })
  output$explain <- renderText({HTML(includeHTML("explain.html"))})
  # output$rcode <- renderText({Your HTML})
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})
}

shinyApp(UI, SERVER, enableBookmarking = "url")
