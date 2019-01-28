#' Basic  server elements for Little Apps
#'
#' @export
LA_standard_reactives <-
  function(input, output, session, the_data, app_state) {
    get_a_sample <<- function(size, stratify, strat_var, frame){

      if (stratify) {
        # need to resample in case there are not enough
        # cases in any given stratum
        frame %>% group_by(!!as.name(strat_var)) %>%
          sample_n(size = size, replace = TRUE)
      }  else {
        frame %>% sample_n(size = size)
      }

    }
    get_sample <<- reactive({
      input$new_sample     # for the dependency
      input$var_y
      input$var_x
      input$covar
      # remove the resampling trials
      isolate(app_state$n_trials <<- 0)
      req(input$samp_size) # for the dependency
      get_a_sample(as.numeric(req(input$samp_size)),
                   input$stratify,
                   input$var_x,
                   the_data$frame)
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
      else get_a_sample(as.numeric(input$samp_size),
                        input$stratify,
                        input$var_x,
                        the_data$frame)


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
           select_x = function(x) x$vname, select_y = select_x, select_z = select_y){
    observe({
      tmp <- unlist(strsplit(input$frame, ":", fixed = TRUE))
      Tmp <- LA_read_data(data_name = tmp[1], package = tmp[2])
      the_data$frame <- Tmp$frame
      the_data$codebook <- Tmp$codebook
      the_data$description <- Tmp$overall
      the_data$types <- Tmp$types

      description <- the_data$description
      var_y_desc <- the_data$codebook[[isolate(input$var_y)]]
      var_x_desc <- the_data$codebook[[isolate(input$var_x)]]
      var_explain <- glue::glue("<ul><li>{input$var_y}:  {var_y_desc}</li><li>{input$var_x}:  {var_x_desc}</li></ul>")
      output$codebook <- renderText({paste(description, var_explain)})
    })

    observe({
      output$debug_table <- renderTable(the_data$types)
      vnames_y <- select_y(the_data$types)
      vnames_x <- select_x(the_data$types)
      vnames_z <- select_z(the_data$types)
      updateSelectInput(session, "var_y", choices =  vnames_y)
      updateSelectInput(session, "var_x", choices =  vnames_x,
                        selected = vnames_x[pmin(2, length(vnames_x))])
      updateSelectInput(session, "covar", choices =  vnames_z[!vnames_z %in% c(vnames_x, vnames_y)])
          })


    observe({
      req(input$frame)
      n_possible <- c(outer( c(1, 2, 5), c(10,100,1000,10000), FUN = "*"))
      n_possible <- n_possible[ n_possible != nrow(req(the_data$frame))]
      n_possible <- c(5, n_possible[n_possible <= nrow(the_data$frame)],
                      nrow(req(the_data$frame)))
      choices <- as.list(n_possible)
      names_for_choices <- choices
      names_for_choices[length(names_for_choices)] <- "Population"
      names(choices) <- names_for_choices
      names(choices)[length(choices)] <- "Population" # IS THIS REDUNDANT?
      updateSelectInput(session, "samp_size",
                        choices = choices, selected = 20)
    })

    # delete all trials  when new sample or accumulate_trials is turned  off
    observeEvent({c(input$resample, input$shuffle); get_sample()}, zero_out_trials())
    observeEvent(input$accumulate_trials,
                 if (!input$accumulate_trials) zero_out_trials())

    get_all_trials <<- reactive({ app_state$Trials })



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
