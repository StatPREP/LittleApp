#' Basic  server elements for Little Apps
#'
#' Randy points out that I need to document this and have a list of the
#' standard reactives.
#'
#' @export
LA_standard_reactives <-
  function(input, output, session, the_data, app_state) {
    input_sample_size <<- reactive({
      if ("samp_size" %in% names(input)) as.integer(req(input$samp_size))
      else -1 # flag for all the  data
    })
    input_stratify <<- reactive({
      if ("stratify" %in% names(input)) input$stratify
      else FALSE
    })
    input_covar <<- reactive({
      if ("covar" %in% names(input)) input$covar
      else 1
    })
    get_overall_sample_size <<- reactive({
      # handles when the sample is stratified
      nrow(get_sample())
    })
    # The whole range, not just the range of the sample
    get_y_range <<- reactive({
      if (get_response_type() == "numeric")
        range(the_data$frame[[input$var_y]], na.rm = TRUE)
      else length(unique(the_data$frame[[input$var_y]]))
    })
    get_sample <<- reactive({
      input$new_sample     # for the dependency
      req(the_data$frame)
      req(input$facet_by)
      req(input$var_y != 1) # make sure it's initialized to a variable
      req(input$var_y %in% c(names(the_data$frame))) # that's in the data frame
      req(no_explanatory_var() || input$var_x %in% names(the_data$frame))
      # remove the resampling trials
      isolate(app_state$n_trials <<- 0)

      get_a_sample(input_sample_size(),
                   input_stratify(),
                   input$var_x,
                   c(input$var_y, input$var_x, input_covar(), input$facet_by),
                   the_data$frame,
                   get_sample_seed())
    })
    # Need this intermediate, because sometimes there is no explanatory variable
    # This will be signaled by NA
    get_explanatory_var <<- reactive({
      if (no_explanatory_var()) 1
      else get_sample()[[input$var_x]]
    })
    get_response_var <<- reactive({
      get_sample()[[req(input$var_y)]]
    })
    get_explanatory_type <<- reactive({
      X <- get_explanatory_var()
      if (length(X) == 1) return("constant")
      else if (is.numeric(X)) return("numeric")
      else return("categorical")
    })

    get_response_type <<- reactive({
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
    get_covar_discrete <<- reactive({
      # This will always be discrete
      values <- if (! "covar" %in% names(input)) 1
      else get_sample()[[input$covar]]
      if (length(values) > 1 && is.numeric(values))
        values  <- mosaic::ntiles(values, n = 3, format = "interval", digits = 2 )

      values

    })
    get_facet_var <<- reactive({
      # this will always be discrete
      values <- if (! "facet_by" %in% names(input)) 1
                else get_sample()[[input$facet_by]]
      if (length(values) > 1 && is.numeric(values))
        values  <- mosaic::ntiles(values, n = 3, format = "interval", digits = 2 )

      values

    })

    # Inf if numeric and more than 10, the actual number of levels otherwise
    get_n_explanatory_groups <<- reactive({
      X <- get_explanatory_var()
      n_unique <- length(unique(X))
      if (is.numeric(X) && n_unique >= 10) Inf
      else n_unique
    })

    # Formula  for  y  ~  x with faceting
    get_frame_formula <<- reactive({
      req(input$var_y, input$var_x)
      as.formula(glue::glue("{input$var_y} ~ {input$var_x}"))
    })

    get_spline_order <<- reactive({
      if ("spline_order" %in% names(input)) as.numeric(input$spline_order)
      else 1
    })

    jitter_height <<- reactive({
      if ("jitter_height" %in% names(input)) input$jitter_height
      else if (get_explanatory_type() == "numeric") 0
      else 0.2
    })
    jitter_width <<- reactive({
      if ("jitter_width" %in% names(input)) input$jitter_width
      else if (get_response_type() == "numeric") 0
      else 0.2
    })

    dot_alpha <<- reactive({
      if ("dot_alpha" %in% names(input)) input$dot_alpha
      else LA_point_alpha(get_overall_sample_size())
    })

    get_color_formula <<- reactive({
        if (no_covariate()) "black"
        else as.formula(glue::glue(" ~ {input_covar()}"))
    })

    get_sample_seed <<- reactive({
      # reset the seed each time the "new sample" button is pressed
      input$new_sample
      Sys.time()
    })

    standard_dot_plot <<- reactive({
      LA_dot_layer(get_frame_formula(),
                   data = get_sample(),
                   color = get_color_formula(),
                   width = jitter_width(),
                   height = jitter_height(),
                   alpha = dot_alpha())
    })

    no_explanatory_var <<- reactive({
      req(input$var_x) == "1"
    })
    no_covariate <<- reactive({
      req(input_covar()) == "1"
    })
    #  no interaction
    get_model_formula <<- reactive({
      order <- get_spline_order()
      # For zeroth order, use -input$var_x so that mod_eval knows var_x is an "input"
      if (order == 0) string <- glue::glue("{input$var_y} ~ (1 - {input$var_x})")
      else if (order == 1) string <- glue::glue("{input$var_y} ~ {input$var_x}")
      else string <- glue::glue("{input$var_y} ~ splines::ns({input$var_x}, {order})")
      if (input_covar() != "1")
        string <- string %>%  paste(., glue::glue(" + {input_covar()}"))

      as.formula(string)
    })
    # a potentially curvy formula (natural cubic splines)
    # with a linear interaction with the covariate
    get_flexible_formula <<- reactive({
      order <- get_spline_order()
      if (order == 0) return(get_model_formula())
      else if (order == 1) string <- glue::glue("{input$var_y} ~ {input$var_x}")
      else string <- glue::glue("{input$var_y} ~ splines::ns({input$var_x}, {order})")
      if (input_covar() != "1")
        string <- string %>%  paste(., glue::glue(" * {input_covar()}"))

      as.formula(string)
    })

    get_resample <<- reactive({
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
    get_trial <<- reactive({
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

    zero_out_trials <<- reactive({
      app_state$n_trials <<- 0
      app_state$Trials <<- data.frame()
      app_state$Trials
    })

  }

#' @export
LA_standard_observers <-
  function(input, output, session, the_data, app_state,
           select_x = function(x) x$vname, select_y = select_x, select_z = select_y, select_facet = NULL){
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
    })

    # turn off the stratify switch when explanatory variable is numeric
    # but the control retains whatever value it had
    observe({
      if (get_explanatory_type() %in% c("numerical", "probability"))
         shinyjs::disable("stratify")
      else shinyjs::enable("stratify")
    })


    observe({
      output$debug_table <- renderTable(the_data$types)
      vnames_y <- select_y(the_data$types)
      vnames_x <- vnames_facet <- select_x(the_data$types)
      vnames_z <- select_z(the_data$types)
      updateSelectInput(session, "var_y", choices =  vnames_y)
      updateSelectInput(session, "var_x", choices =  vnames_x,
                        selected = vnames_x[pmin(2, length(vnames_x))])
      updateSelectInput(session, "covar", choices =  vnames_z)
      if (! is.null(select_facet)) {
        vnames_facet <- select_facet(the_data$types)
        updateSelectInput(session, "facet_by", choices = vnames_facet)
      }


      the_data$initialized <<- TRUE
          })


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

    # delete all trials  when new sample or accumulate_trials is turned  off
    observeEvent({c(input$resample, input$shuffle); get_sample()}, zero_out_trials())
    observeEvent(input$accumulate_trials,
                 if (!input$accumulate_trials) zero_out_trials())

    get_all_trials <<- reactive({ app_state$Trials })

    observe({
      if (no_explanatory_var() || "numeric" == get_explanatory_type()) shinyjs::hide("stratify")
      else shinyjs::show("stratify")
    })

    observe({
      if ("facet_by" %in% names(input))
        updateSelectInput

    })

    observeEvent(input$new_trial, {
      if (isolate(input$accumulate_trials)) {
        isolate(app_state$n_trials <<- app_state$n_trials + 1)
        app_state$Trials <<- get_trial() %>%
          mutate(.trial = isolate(app_state$n_trials)) %>%
          bind_rows(app_state$Trials, .)
      } else {
        isolate(app_state$n_trials <<- 1 )
        app_state$Trials <<- get_trial() %>%
          mutate(.trial = isolate(app_state$n_trials))
      }

      cat(isolate(app_state$n_trials), "trials run\n")

      cat(paste("There are", isolate(app_state$n_trials), "with", isolate(nrow(app_state$Trials)), "rows\n"))
    })

  }
