#
# Center and spread
#
library(shiny, quietly = TRUE, verbose = FALSE)
library(shinydashboard, quietly = TRUE, verbose = FALSE)
library(SDSdata, quietly = TRUE, verbose = FALSE)
library(LittleApp, quietly = TRUE, verbose = FALSE)
library(markdown, quietly = TRUE, verbose = FALSE)
library(mosaic, quietly = TRUE, verbose = FALSE)
library(ggformula, quietly = TRUE, verbose = FALSE)

# App-specific controls
choose_stats <-
  box(title = "Display stat", width = 6, background = "black",
      status = "primary", solidHeader = TRUE,
      collapsible = FALSE, collapsed = FALSE,
      checkboxInput("show_mean", "Mean", value = FALSE) %>%
        tighten(bottom = -10),
      checkboxInput("show_median", "Median", value = FALSE) %>%
        tighten(bottom = -10),
      checkboxInput("show_sd", "Std. deviation", value = FALSE) %>%
        tighten(bottom = -10),
      checkboxInput("violin", "Density violin", value = FALSE) %>%
        tighten(bottom = -10),
      hr(),
      checkboxInput("show_ci", "Conf. interval on mean:", value = FALSE) %>%
        tighten(bottom = -10),
      checkboxInput("show_pred_interval", "Summary interval", value = FALSE) %>%
        tighten(bottom = -10),
      selectInput("interval_level", "Interval  level",
                  choices = list("99%" = 0.99, "95%" = 0.95,
                                 "90%" = 0.90, "80%" = 0.80,
                                 "67%" = 0.67, "50%" = 0.50),
                  selected = 0.95)
  )


UI <- function(request) { #it's a function  for bookmarking
  dashboardPage(
    dashboardHeader(
      title = "Center and spread",
      titleWidth = "90%"
    ),
    dashboardSidebar(width = 350, p("")
      , LA_data_source(6, covariate = FALSE)
      , choose_stats
      , LA_sample_ui(6)
      #, LA_inference(6),
      #, bookmarkButton()
    ),

    LA_body(plot_widget =
              plotOutput("main_plot", height = "400px",
                         brush = brushOpts(id="yruler",  direction  = "y")))
  )
}



SERVER <- function(input, output, session) {
    the_data <- reactiveValues()
    app_state <- reactiveValues(n_trials = 0, Trials = data.frame())

    # App-specific selection of variables


    # Choose the variables
    select_x <- LA_selectCategorical(max_levels = 8, none = TRUE)
    select_y <- LA_selectNumeric()
    select_z <- LA_selectNone()
    select_facet <- NULL # for most apps

    # Reactives and observers used throughout the various Little Apps
    #LA_standard_reactives(input, output, session, the_data, app_state, environment())
    #LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)

    output$debug_text <- renderText({
      input$new_trial
      app_state$n_trials
      app_state$Trials
      input$accumulate_trials
      input$resample
      input$shuffle
      glue::glue("{req(app_state$n_trials)} trials taken so {nrow(app_state$Trials)} rows of randomization trial data.\n")
    })

    construct_plot <- reactive({
      req(input$var_y, input$var_x)
      req(the_data$initialized)

      xrange <- c(0, 2) # default for no explanatory variable
      xvar <- get_explanatory_var()
      if  (length(xvar)  != 1) {
        if (get_explanatory_type() == "numeric") xrange = range(xvar)
        else xrange = c(1, length(unique(xvar)) + 0.5)
      }

      # Make the plot
      the_formula <- as.formula(glue::glue("{input$var_y} ~ {input$var_x}"))
      plot_data  <- get_sample()
      if (input$var_y %in% names(plot_data)  && input$var_y %in% names(plot_data))  {
        # The next line gets all the accumulated randomization trials.
        # Trials <- get_all_trials()

        level <- as.numeric(input$interval_level)
        mfun <- function(x) mosaicCore::ci.median(level)

        Stats <- mosaicCore::df_stats(the_formula, data = plot_data,
                          mean = mean, mean = mosaicCore::ci.mean(!!level),
                          median = median, sd = sd,
                          prediction = coverage(!!level),
                          na.action = "na.pass")

        Stats$interval_level = input$interval_level
        For_sd_ruler <-
          mosaicCore::df_stats(the_formula, data = plot_data,
                                  mean = mean, sd = sd) %>%
          mutate(center = mean, pos_sd = mean + sd, neg_sd =  mean - sd,
                 pos_2sd = mean + 2*sd, neg_2sd =  mean - 2*sd,) %>%
          select( - sd,  - center)
        For_sd_ruler_labels <- For_sd_ruler %>%
          tidyr::gather(key = label, value = vertical, `pos_sd`, neg_sd, pos_2sd, neg_2sd, mean)

        output$debug_table <- renderTable(For_sd_ruler_labels)
        P <- get_sample()  %>%
          gf_jitter( the_formula, seed = 101,
                     width = 0.10,
                     alpha = 0.5)

        if (input$violin) P <- P %>% gf_violin(alpha = 0.2, fill = "blue")
        if (input$show_median) {
          this_formula <- as.formula(glue::glue("median + median ~ {input$var_x}"))
          P <- P %>% gf_errorbar(this_formula, data = Stats, color = "blue", size = 2)
        }
        if (input$show_pred_interval) {
          this_formula <- as.formula(glue::glue("prediction_lower + prediction_upper ~ {input$var_x}"))
          P <- P %>% gf_errorbar(this_formula, data = Stats, color = "gray", alpha = 0.5,  size = 4)
        }
        if (input$show_sd) {
          this_formula <- as.formula(glue::glue("`pos_sd` + neg_sd ~ {input$var_x}"))
          P <- P %>% gf_errorbar(this_formula, data = For_sd_ruler, color = "red", width = 0.1)
          this_formula <- as.formula(glue::glue("pos_2sd + mean ~ {input$var_x}"))
          P <- P %>% gf_errorbar(this_formula, data = For_sd_ruler, color = "red", width = 0.15)
          this_formula <- as.formula(glue::glue("neg_2sd + mean ~ {input$var_x}"))
          P <- P %>% gf_errorbar(this_formula, data = For_sd_ruler, color = "red", width = 0.15)
          this_formula <- as.formula(glue::glue("vertical ~ {input$var_x}"))
          P <- P %>% gf_text(this_formula, label = ~ label, color = "red",
                             data = For_sd_ruler_labels, nudge_x = 0.1, hjust = 0)
        }
        if (input$show_mean) {
           this_formula <- as.formula(glue::glue("mean + mean ~ {input$var_x}"))
           P <- P %>% gf_errorbar(this_formula, data = Stats, color = "black", size = 2)
           if (input$show_ci) {
             this_formula <- as.formula(glue::glue("mean_lower + mean_upper ~ {input$var_x}"))
             P <- P %>% gf_errorbar(this_formula, data = Stats, color = "black", size = 1, width = 0.8)
           }
        }
        if (input$var_x == "1")
          P <- P %>% gf_lims(x = c(0,2)) %>%
          gf_labs(x = "")  %>%
          gf_theme(no_x_axis) %>%
          add_y_ruler(x_range = xrange, ruler = input$yruler )
      } else {
        P <- gf_text(1 ~ 1, label = "New data set arriving ...") %>% gf_theme(theme_void())
      }

      if (get_response_type() == "numeric") { # can't set scale for categorical variable
        P <- P %>% gf_lims(y = get_y_range())  # to keep scale constant across samples
      }
      return(P)
    })
    output$main_plot <- renderPlot({ construct_plot() })
    output$little_copy <- renderPlot({
      construct_plot()
    })
    output$rcode <- renderText({
      HTML(includeHTML("r-commands.html"))
    })
    output$explain <- renderText({
      HTML(includeHTML("explain.html"))
    })

    stat_table <- reactive({
      level <- as.numeric(input$interval_level)
      input$show_mean
      if (input$var_y == "1")
        return("Not available yet. Select a statistic to display.")
      level <- as.numeric(input$interval_level)
      the_formula <- get_frame_formula()
        # as.formula(glue::glue("{input$var_y} ~ {input$var_x}"))
      Stats <- mosaicCore::df_stats(the_formula, data = get_sample(),
                                    n = length,
                                    mean = mean, mean = mosaicCore::ci.mean(!!level),
                                    median = median, sd = sd,
                                    summary = coverage(!!level),
                                    na.action = "na.pass")

      Stats$interval_level <- as.numeric(input$interval_level)
      Res <- Stats  %>% mutate_if(is.numeric, function(x) signif(x, digits = 4)) %>%
        {if (!input$show_mean)    select(., - mean) else .} %>%
        {if (!input$show_ci)  select(., -mean_lower, -mean_upper) else .} %>%
        {if (!input$show_median)  select(.,  -median) else .} %>%
        {if (!input$show_sd) select(.,  -sd) else .} %>%
        {if (!input$show_pred_interval) select(.,  -summary_lower,  -summary_upper) else .}

     Res
    })


    output$statistics <- renderText({
      knitr::kable(stat_table(), format = "html") %>%
        kableExtra::kable_styling() %>%
        HTML()
    })

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

