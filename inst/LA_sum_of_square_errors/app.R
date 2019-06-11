#
# RMS  optimization
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(SDSdata)
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
  select_facet <- select_z

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  # LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  # LA_standard_reactives(input, output, session, the_data, app_state)

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

  ###############################
  #
  # Standard server components
  #
  ###############################

  input_sample_size <- reactive({
    if ("samp_size" %in% names(input)) as.integer(req(input$samp_size))
    else -1 # flag for all the  data
  })
  # # assign("input_sample_size", input_sample_size, env = calling_env)

  input_stratify <- reactive({
    if ("stratify" %in% names(input)) input$stratify
    else FALSE
  })
  # assign("input_stratify", input_stratify, env = calling_env)

  input_covar <- reactive({
    if ("covar" %in% names(input)) input$covar
    else 1
  })
  # assign("input_covar", input_covar, env = calling_env)

  get_overall_sample_size <- reactive({
    # handles when the sample is stratified
    nrow(get_sample())
  })
  # assign("get_overall_sample_size", get_overall_sample_size, env = calling_env)

  # The whole range, not just the range of the sample
  get_y_range <- reactive({
    if (get_response_type() == "numeric")
      range(the_data$frame[[input$var_y]], na.rm = TRUE)
    else c(1, length(unique(the_data$frame[[input$var_y]])))
  })
  # assign("get_y_range", get_y_range, env = calling_env)

  get_x_range <- reactive({
    xrange <- c(0, 2) # default for no explanatory variable
    xvar <- get_explanatory_var()
    if  (length(xvar)  != 1) {
      if (get_explanatory_type() == "numeric") xrange = range(xvar)
      else xrange = c(1, length(unique(xvar)) + 0.5)
    }
    return(xrange)
  })
  # assign("get_x_range", get_x_range, env = calling_env)

  get_sample <- reactive({
    #input$new_sample     # for the dependency but maybe thiis is handled by get_sample_seed
    req(the_data$frame)
    req(input$var_y %in% c(names(the_data$frame))) # that's in the data frame
    req(no_explanatory_var() || input$var_x %in% names(the_data$frame))
    # remove the resampling trials
    #isolate(app_state$n_trials <- 0)

    LittleApp:::get_a_sample(input_sample_size(),
                             input_stratify(),
                             input$var_x,
                             c(input$var_y, input$var_x, input_covar(), input$facet_by),
                             the_data$frame,
                             get_sample_seed())
  })
  # assign("get_sample", get_sample, env = calling_env)


  # Need this intermediate, because sometimes there is no explanatory variable
  # This will be signaled by NA
  get_explanatory_var <- reactive({
    if (no_explanatory_var()) 1
    else get_sample()[[input$var_x]]
  })
  # assign("get_explanatory_var", get_explanatory_var, env = calling_env)

  get_response_var <- reactive({
    get_sample()[[req(input$var_y)]]
  })
  # assign("get_response_var", get_response_var, env = calling_env)

  get_explanatory_type <- reactive({
    X <- get_explanatory_var()
    if (length(X) == 1) return("constant")
    else if (is.numeric(X)) return("numeric")
    else return("categorical")
  })
  # assign("get_explanatory_type", get_explanatory_type, env = calling_env)

  get_response_type <- reactive({
    # categorical, numeric, logical, probability
    if (input$var_y == "1") return("invalid") # not yet initialized
    Y <- get_response_var()
    if (!is.numeric(Y)) {
      if (is.logical(Y)) return("logical")
      else return("categorical")
    } else {
      if (min(Y) >= 0 && max(Y) <= 1) return("probability")
      else return("numeric")
    }
  })
  # assign("get_response_type", get_response_type, env = calling_env)

  get_covar_discrete <- reactive({
    # This will always be discrete
    values <- if (! "covar" %in% names(input)) 1
    else get_sample()[[input$covar]]
    if (length(values) > 1 && is.numeric(values))
      values  <- mosaic::ntiles(values, n = 3, format = "interval", digits = 2 )

    values
  })
  # assign("get_covar_discrete", get_covar_discrete, env = calling_env)

  get_facet_var <- reactive({
    # this will always be discrete
    values <- if (! "facet_by" %in% names(input)) 1
    else get_sample()[[input$facet_by]]
    if (length(values) > 1 && is.numeric(values))
      values  <- mosaic::ntiles(values, n = 3, format = "interval", digits = 2 )

    values
  })
  # assign("get_facet_var", get_facet_var, env = calling_env)

  # Inf if numeric and more than 10, the actual number of levels otherwise
  get_n_explanatory_groups <- reactive({
    X <- get_explanatory_var()
    n_unique <- length(unique(X))
    if (is.numeric(X) && n_unique >= 10) Inf
    else n_unique
  })
  # assign("get_n_explanatory_groups", get_n_explanatory_groups, env = calling_env)

  # Formula  for  y  ~  x with faceting
  get_frame_formula <- reactive({
    req(input$var_y, input$var_x)
    as.formula(glue::glue("{input$var_y} ~ {input$var_x}"))
  })
  # assign("get_frame_formula", get_frame_formula, env = calling_env)

  get_spline_order <- reactive({
    if ("spline_order" %in% names(input)  && !is.na(input$spline_order)) as.numeric(input$spline_order)
    else 1
  })
  # assign("get_spline_order", get_spline_order, env = calling_env)

  jitter_height <- reactive({
    if ("jitter_height" %in% names(input)) input$jitter_height
    else if (get_explanatory_type() == "numeric") 0
    else 0.2
  })
  # assign("jitter_height", jitter_height, env = calling_env)

  jitter_width <- reactive({
    if ("jitter_width" %in% names(input)) input$jitter_width
    else if (get_response_type() == "numeric") 0
    else 0.2
  })
  # assign("jitter_width", jitter_width, env = calling_env)

  dot_alpha <- reactive({
    if ("dot_alpha" %in% names(input)) input$dot_alpha
    else LA_point_alpha(get_overall_sample_size())
  })
  # assign("dot_alpha", dot_alpha, env = calling_env)

  get_color_formula <- reactive({
    if (no_covariate()) "black"
    else as.formula(glue::glue(" ~ {input_covar()}"))
  })
  # assign("get_color_formula", get_color_formula, env = calling_env)

  get_sample_seed <- reactive({
    # reset the seed each time the "new sample" button is pressed
    input$new_sample
    Sys.time()
  })
  # assign("get_sample_seed", get_sample_seed, env = calling_env)

  standard_dot_plot <- reactive({
    LA_dot_layer(get_frame_formula(),
                 data = get_sample(),
                 color = get_color_formula(),
                 width = jitter_width(),
                 height = jitter_height(),
                 alpha = dot_alpha())
  })
  # assign("standard_dot_plot", standard_dot_plot, env = calling_env)


  no_explanatory_var <- reactive({
    req(input$var_x) == "1"
  })
  # assign("no_explanatory_var", no_explanatory_var, env = calling_env)

  no_covariate <- reactive({
    req(input_covar()) == "1"
  })
  # assign("no_covariate", no_covariate, env = calling_env)

  #  no interaction
  get_model_formula <- reactive({
    order <- get_spline_order()
    # For zeroth order, use -input$var_x so that mod_eval knows var_x is an "input"
    if (order == 0) string <- glue::glue("{input$var_y} ~ (1 - {input$var_x})")
    else if (order == 1) string <- glue::glue("{input$var_y} ~ {input$var_x}")
    else string <- glue::glue("{input$var_y} ~ splines::ns({input$var_x}, {order})")
    if (input_covar() != "1")
      string <- string %>%  paste(., glue::glue(" + {input_covar()}"))

    as.formula(string)
  })
  # assign("get_model_formula", get_model_formula, env = calling_env)

  # a potentially curvy formula (natural cubic splines)
  # with a linear interaction with the covariate
  get_flexible_formula <- reactive({
    order <- get_spline_order()
    if (order == 0) return(get_model_formula())
    else if (order == 1) string <- glue::glue("{input$var_y} ~ {input$var_x}")
    else string <- glue::glue("{input$var_y} ~ splines::ns({input$var_x}, {order})")
    if (input_covar() != "1")
      string <- string %>%  paste(., glue::glue(" * {input_covar()}"))

    as.formula(string)
  })
  # assign("get_flexible_formula", get_flexible_formula, env = calling_env)

  get_resample <- reactive({
    cat("New resampling trial.\n")
    the_samp <- get_sample()
    if (input$stratify) {  #resample within groups
      n_groups <- # how many stratification groups
        length(unique(the_samp[[input$var_x]]))
      the_samp %>%
        group_by(!!as.name(input$var_x)) %>%
        sample_n(size = nrow(the_samp) / n_groups, replace = TRUE)
    } else {
      the_samp %>% sample_n(size = nrow(the_samp), replace = TRUE)
    }
  })
  # assign("get_resample", get_resample, env = calling_env)
  get_trial <- reactive({
    Tmp <-
      if (input$resample) get_resample()
    else get_a_sample(get_sample_size(),
                      input$stratify,
                      input$var_x,
                      c(input$var_y, input$var_x, input_covar()),
                      the_data$frame,
                      seed = Sys.time())


    # Randomization as needed
    if (input$shuffle) Tmp[[input$var_y]] <- shuffle(Tmp[[input$var_y]])

    Tmp
  })

  observe({
    tmp <- unlist(strsplit(input$frame, ":", fixed = TRUE))
    Tmp <- LA_read_data(data_name = tmp[1], package = tmp[2])

    the_data$frame <- Tmp$frame
    the_data$codebook <- Tmp$codebook
    the_data$description <- Tmp$overall
    the_data$types <- Tmp$types

    if (is.list(the_data$codebook)) { # use customized documentation
      description <- the_data$description
      var_y_desc <- the_data$codebook[[isolate(input$var_y)]]
      var_x_desc <- the_data$codebook[[isolate(input$var_x)]]
      var_explain <- glue::glue("<ul><li>{input$var_y}:  {var_y_desc}</li><li>{input$var_x}:  {var_x_desc}</li></ul>")
      output$codebook <- renderText({paste(description, var_explain)})
    } else {
      output$codebook <- renderText({HTML(the_data$codebook)})
    }
  },
  priority = 10)

  # turn off the stratify switch when explanatory variable is numeric
  # but the control retains whatever value it had
  observe({
    if (get_explanatory_type() %in% c("numerical", "probability"))
      shinyjs::disable("stratify")
    else shinyjs::enable("stratify")
  })


  observe({
    input$frame
    output$debug_table <- renderTable(the_data$types)
    vnames_y <- select_y(the_data$types)
    vnames_x <- vnames_facet <- select_x(the_data$types)
    vnames_z <- select_z(the_data$types)
    updateSelectInput(session, "var_y", choices =  vnames_y,
                      selected = vnames_y[1])
    updateSelectInput(session, "var_x", choices =  vnames_x,
                      selected = vnames_x[pmin(2, length(vnames_x))])
    updateSelectInput(session, "covar", choices =  vnames_z,
                      selected = vnames_z[1])
    if (! is.null(select_facet)) {
      vnames_facet <- select_facet(the_data$types)
      updateSelectInput(session, "facet_by", choices = vnames_facet,
                        selected = vnames_facet[1])
    }


    the_data$initialized <- TRUE
  }, priority = 10)


  observe({
    req(input$frame)
    n_possible <- c(outer( c(1, 2, 5), c(10, 100, 1000, 10000), FUN = "*"))
    n_possible <- n_possible[ n_possible != nrow(req(the_data$frame))]
    # -1 is a flag for "all the data" Can't use Inf for some reason
    n_possible <- c(2, 5, n_possible[n_possible <= nrow(the_data$frame)], -1)
    choices <- as.list(n_possible)
    names_for_choices <- choices
    names_for_choices[length(names_for_choices)] <- "Sampling frame (all the data)"
    names(choices) <- names_for_choices
    names(choices)[length(choices)] <- "Sampling frame (all the data)" # IS THIS REDUNDANT?
    if ("samp_size" %in% names(input)) {
      updateSelectInput(session, "samp_size",
                        choices = choices, selected = 50)
    }
  })

  observe({
    if (no_explanatory_var() || "numeric" == get_explanatory_type()) shinyjs::hide("stratify")
    else shinyjs::show("stratify")
  })

  observe({
    if ("facet_by" %in% names(input))
      updateSelectInput

  })

}

shinyApp(UI, SERVER, enableBookmarking = "url")
