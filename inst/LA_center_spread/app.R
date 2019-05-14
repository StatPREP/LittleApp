#
# Center and spread
#
library(shiny)
library(shinydashboard)
library(LittleApp)
library(markdown)
library(mosaic)
library(ggformula)

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

    # Reactives and observers used throughout the various Little Apps
    LA_standard_observers(input, output, session, the_data, app_state, select_x, select_y, select_z)
    LA_standard_reactives(input, output, session, the_data, app_state)

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
  }

shinyApp(UI, SERVER, enableBookmarking = "url")

