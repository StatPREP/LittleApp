#
# Simulating sampling bias
#
library(shiny)
library(shinydashboard)
library(LittleApp)
library(markdown)
library(SDSdata)
library(mosaic)
library(mosaicCore)
library(ggformula)

# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
my_special_controls <-
  box(title = "Sampling bias", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      # using the covariate slot to be the biasing variable
      p("Sliders set the relative sampling frequencies"),
      tighten(selectizeInput("covar", "Biasing variable", choices = list("1"))),
      sliderInput("level1",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level2",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level3",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level4",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      sliderInput("level5",  "level name",
                      min = 0, max = 1, value = 1) %>% tighten(),
      hr(),
      selectInput("conf_level", "Confidence level",
                  choices = c("50%" = .5, "80%" = .8, "95%" = .95,
                              "99%" = .99, "99.9%" = .999, "99.99%" = .9999),
                  selected = .95))

# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Confidence intervals and sampling bias",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350
      , LA_data_source(6, covar  =  FALSE)    # These are controls available
      , LA_sample_ui(6)      # Comment out any you don't want
      , my_special_controls  # ... and add in your own here.
      #bookmarkButton()      # You can change the order of these
                             # to get a layout you like.
    ),

    LA_body(plot_widget =
              plotOutput("main_plot", height = "400px",
                         brush = brushOpts(id="yruler",  direction  = "y"))) # The body is entirely pre-defined
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectCategorical(max_levels = 1, none = TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 5, none = TRUE)
  select_facet <- select_z

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  #LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  #LA_standard_reactives(input, output, session, the_data, app_state)

  # turn off the sliders at the beginning
  for (k in  1:5) shinyjs::hide(paste0("level", k))

  observe({ #hide sliders when no covariate
    if (input$covar == "1") {
      for (slider in 1:5){
        this_slider <- paste0("level", slider)
        shinyjs::hide(this_slider)
      }
    }
  })

  get_weights <- reactive({
    #  cat("get_weights()\n")
    if (input$covar == 1)  {
      return() # not initialized
    }
    bias_var <- the_data$frame[[input$covar]]
    levels <-
      if  (is.factor(bias_var)) levels <- na.omit(levels(bias_var))
      else unique(na.omit(as.character(bias_var)))
    # Change the display of the sampling bias sliders
    for (slider in 1:5){
      this_slider <- paste0("level", slider)
      if (length(levels) < slider) {
        shinyjs::hide(this_slider)
      }  else {
        shinyjs::show(this_slider)
        updateSliderInput(session, this_slider, label = levels[slider],
                          value = input[[this_slider]])
      }
    }

    if (length(levels) > 1 )
      return(tibble(.value. = levels,  rel_freq = c(input$level1, input$level2,
                                            input$level3, input$level4,
                                            input$level5)[1:(length(levels))]))

  })

  population_parameter <- reactive({
    if (input$var_y == 1) return(0) # not  initialized
    shiny::req(the_data$frame)
    mean(the_data$frame[[input$var_y]],  na.rm = TRUE)
  })

  get_biased_sample <- reactive({
    # cat("get_biased_sample\n")
    if (input$covar == 1) return(get_sample())
    input$new_sample     # for the dependency
    req(the_data$frame)
    req(input$var_y != 1) # make sure it's initialized to a variable
    req(input$var_y %in% c(names(the_data$frame))) # that's in the data frame
    req(no_explanatory_var() || input$var_x %in% names(the_data$frame))
    weight_levels <- get_weights()
    # cat(paste(capture.output(weight_levels), collapse = "\n"))


    # Need to turn this into a vector of the same length as the_data$frame
    weights <- the_data$frame[input$covar]
    names(weights) <- ".value." # to correspond to weight levels
    weights <- suppressWarnings(weights %>%  left_join(weight_levels))
    # cat(paste(paste(lapply(weights, head), collapse = " "), "\n"))

    LittleApp:::get_a_sample(input_sample_size(),
                 input_stratify(),
                 input$var_x,
                 c(input$var_y, input$var_x, input_covar(), input$facet_by),
                 the_data$frame,
                 get_sample_seed(),
                 weights = weights[["rel_freq"]]
                 )
  })
  stat_report <- reactive({
    if (input$covar == 1) return("No biasing variable selected")
    paste(letters, collapse = " ")
    population <- the_data$frame[[input$covar]]
    sample <- get_biased_sample()[[input$covar]]
    T1 <- prop.table(table(population)) %>%
                             round(3) %>% as.data.frame()
    T2 <- prop.table(table(sample)) %>%
      round(3) %>% as.data.frame()
    report  <- T1 %>% left_join(T2,  by = c("population" = "sample"))
    names(report) <- c(input$covar, "population", "sample")
    res <- paste(paste(capture.output(report), collapse = "\n"), "\n")

    res
  })

  output$main_plot <- renderPlot({
    parameter <- population_parameter()
    Biased <- get_biased_sample()
    clevel <- as.numeric(input$conf_level)
    Stats <- df_stats(as.formula(paste(input$var_y, "~ 1")),
                      data = Biased, mean = mean, ci.mean(!!clevel))
    #  cat(paste(capture.output(table(Biased[[input$covar]])), collapse = "\n"), "\n")
    P <- LA_dot_layer(get_frame_formula(),
                      data = Biased,
                      color = get_color_formula(),
                      width = .1,
                      height = jitter_height(),
                      alpha = dot_alpha()) %>%
      gf_hline(yintercept = parameter, color = "blue") %>%
      gf_errorbar(lower + upper ~ 1.2, data = Stats, width = 0.1, inherit = FALSE)
    P %>%
      add_y_ruler(x_range = c(0,2), ruler = input$yruler ) %>%
      gf_theme(theme_bw(base_size = 16))
    })
  # Other built-in output widgets besides output$main_plot
  # output$codebook <- renderText({ Your HTML })
  output$statistics <- renderText({
    HTML(paste("<pre>", stat_report(), "</pre>", sep = "\n"))
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
