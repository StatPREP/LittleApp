#
# Bootstrapping app
#
library(shiny)
library(shinydashboard)
library(LittleApp)
library(SDSdata)
library(markdown)
library(mosaic)
library(ggformula)
library(ungeviz)

quantile97.5 <<- function(x) unname(quantile(x, probs = 0.975, na.rm = TRUE))
quantile95 <<- function(x) unname(quantile(x,  probs = 0.95, na.rm = TRUE))
quantile90  <<- function(x) unname(quantile(x,  probs = 0.90, na.rm = TRUE))
quantile75  <<- function(x) unname(quantile(x,  probs = 0.75, na.rm = TRUE))
mean_plus_2sd <<- function(x) {mean(x, na.rm = TRUE) + 2 * sd(x, na.rm = TRUE)}
# App-specific controls
# Define one or more such control boxes with the special controls
# needed for your app
bootstrap_controls <-
  box(title = "Bootstrap Trials", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      radioButtons("bs_display", "Display", choices = c("One trial", "Many trials")) %>%
        tighten(bottom = -10),
      selectInput("ntrials", "# of trials", choices =
                    c("5" = 5, "10" = 10, "20" = 20,
                      "50" = 50, "100" = 100),
                  selected = 50)  %>%
        tighten(bottom = -10),
      actionButton("new_bs_trial", "New Trial",
                   icon = icon("dice"),
                   style = "font-size: 16px; margin-left: 0px;" ) %>%
        tighten(top = 0, bottom = 0))

model_controls <-
  box(title = "Statistic/Model", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      selectInput("stat", "Statistic",
                  choices = c("mean", "median", "max", "min", "quantile75",
                              "quantile90", "quantile95", "quantile97.5", "mean_plus_2sd")) %>%
        tighten(bottom = -10),
      numericInput("spline_order", "Model order", min = 0, max = 3, value = 1) %>%
        tighten(bottom = -10)
  )


# Boilerplate for the UI

UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Resampling and Bootstrapping",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350, p("")
      , LA_data_source(6)
      , LA_sample_ui(6)
      , bootstrap_controls
      , model_controls
      #bookmarkButton()

    ),

    LA_body() # The body is entirely pre-defined
  )
}

# Boilerplate for the SERVER
SERVER <- function(input, output, session) {
  # App-specific selection of variables from the data frame
  select_x <- LA_selectAll(none =  TRUE)
  select_y <- LA_selectNumeric()
  select_z <- LA_selectCategorical(max_levels = 8, none = TRUE)
  select_facet <- select_z

  shinyjs::hide("covar") # no covariate in  this app

  # Reactives and observers used throughout the various Little Apps
  the_data <- reactiveValues()
  app_state <- reactiveValues(n_trials = 0, Trials = data.frame())
  #LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
  #LA_standard_reactives(input, output, session, the_data, app_state)

  # set display of controls appropriately
  observeEvent(get_explanatory_type(), {
    if (get_explanatory_type() == "numeric") {
      shinyjs::hide("stat")
      shinyjs::show("spline_order")
    } else {
      shinyjs::show("stat")
      shinyjs::hide("spline_order")
    }
  })

  observe({
    if (input$bs_display == "Many trials") {
      shinyjs::show("ntrials")
      shinyjs::hide("new_bs_trial")
    } else {
      shinyjs::hide("ntrials")
      shinyjs::show("new_bs_trial")
    }
  })

  output$main_plot <- renderPlot({
    input$new_bs_trial # for the dependency
    x_type <- get_explanatory_type()
    stat_fun <- input$stat

    P <-
      if (input$bs_display == "One trial") {
        if (x_type == "numeric")
          show_bootstrap_sample_cont(get_sample(), get_frame_formula(), ns = get_spline_order())
        else
          show_bootstrap_sample_disc(get_sample(), get_frame_formula(), stat = stat_fun)
      } else {
        ntrials <- as.integer(input$ntrials)
        if (x_type == "numeric") {
          show_bootstrap_ensemble_cont(get_sample(), get_frame_formula(),
                                       ntrials = ntrials,
                                       ns = get_spline_order(),
                                       pt_alpha = 0.5, color = "#0000C050",
                                       se = FALSE)
        } else {
          show_bootstrap_ensemble_disc(get_sample(), get_frame_formula(),
                                       ntrials = ntrials, width = 0.2,
                                       stat = stat_fun, alpha_pts = 0.5)
        }
      }

    # The mean +- 2 sd might be outside of the range of the data,
    # unlike all the other stats
    if (input$stat %in% c("mean_plus_2sd", "quantile97.5", "quantile95", "quantile90")) P
    else P %>% gf_lims(y  = get_y_range())

    P %>%  gf_theme(theme_bw(base_size = 16))

    })
  # Other built-in output widgets besides output$main_plot
  # output$statistics <- renderText({Your HTML})
  output$explain <- renderText({includeHTML("explain.html")})
  output$rcode <- renderText({includeHTML("r-commands.html")})
  # output$debug_text <- renderText({Your text})
  # output$debug_plot <- renderPlot({Your plot})
  # output$debug_table <- renderTable({Your table})

  ###############################
  #
  # Standard server components
  #
  # Copy this into each little app
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
