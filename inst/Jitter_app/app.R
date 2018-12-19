#
# A template for a Little App
# this one is about jittering
#

library(shiny)
library(shinydashboard)
library(littleapp2)
library(markdown)
library(mosaic)
library(ggformula)




shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Jittered point plots",
      titleWidth = "90%"
    ),
    dashboardSidebar(
      width = 350,
      LA_data_ui(6),
      box(title = "Jitter controls", width = 6, background = "black",
          status = "primary", solidHeader = TRUE,
          collapsible = FALSE, collapsed = FALSE,
          sliderInput("jitter_width",  "Jitter width",
                      min = 0, max = 1, value = 0.0) %>% tighten(bottom = -10),
          sliderInput("jitter_height", "Jitter height",
                      min = 0, max = 1, value = 0.0) %>% tighten(bottom = -10),
          sliderInput("jitter_alpha",  "Transparency",
                      min = 0, max = 1, value = 1) %>% tighten(bottom = -10)
      ),
      LA_inference(6)
    ),
    dashboardBody(
      plotOutput("main_plot", height = "400px"),
      HTML("<br>"),
      tabBox(
        title = "Info", width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "100px",
        tabPanel("Codebook", htmlOutput("codebook")),
        tabPanel("Statistics", htmlOutput("statistics")),
        tabPanel("Explain", htmlOutput("explain")),
        tabPanel("R commands", htmlOutput("rcode")),
        tabPanel("Debug",
                 textOutput("debug_text"),
                 #plotOutput("debug_plot"),
                 tableOutput("debug_table"))
      ),
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      section.sidebar .shiny-input-container {
          /* Proper spacing around inputs. */
          padding: 1px 1px 1px 1px;
         /* Wrap content (important for inline inputs). */
         white-space: no-wrap;
}

      .shiny_input_container .control-label {

      }
    ')))
    )
  ),
  server = function(input, output, session) {
    the_data <- reactiveValues()
    app_state <- reactiveValues(n_trials = 0, Trials = data.frame())



    LA_data_server(input, output, session, the_data, app_state)

    # Function to get a sample, resample trial, sets of trials
    get_a_sample <- function(size, stratify, strat_var, frame){
      Tmp <- if (stratify) frame %>% group_by(!!as.name(strat_var))
             else frame

      Tmp %>% sample_n(size = size)
    }
    get_sample <- reactive({
      input$new_sample     # for the dependency
      input$var1
      input$var2
      input$covar
      # remove the resampling trials
      isolate(app_state$n_trials <<- 0)
      req(input$samp_size) # for the dependency
      get_a_sample(as.numeric(req(input$samp_size)),
                   input$stratify,
                   input$var2,
                   the_data$frame)
    })

    get_resample <- reactive({
      cat("New resampling trial.\n")
      the_samp <- get_sample()
      if (input$stratify) {  #resample within groups
        n_groups <- # how many stratification groups
          length(unique(the_samp[[input$var2]]))
        the_samp %>%
          group_by(!!as.name(input$var2)) %>%
          sample_n(size = nrow(the_samp) / n_groups, replace = TRUE)
      } else {
        the_samp %>% sample_n(size = nrow(the_samp), replace = TRUE)
      }
    })
    get_trial <- reactive({
      Tmp <-
        if (input$resample) get_resample()
      else get_a_sample(as.numeric(input$samp_size),
                        input$stratify,
                        input$var2,
                        the_data$frame)


      # Randomization as needed
      if (input$shuffle) Tmp[[input$var1]] <- shuffle(Tmp[[input$var1]])

      Tmp
    })


    output$debug_text <- renderText({
      input$new_trial
      app_state$n_trials
      app_state$Trials
      input$accumulate_trials
      input$resample
      input$shuffle
      glue::glue("{req(app_state$n_trials)} trials taken so {nrow(app_state$Trials)} rows of randomization trial data.\n")
    })

    get_all_trials <- reactive({ app_state$Trials })
    # delete all trials  when new sample or accumulate_trials is turned  off
    #observeEvent(get_sample(), zero_out_trials())
    observeEvent({c(input$resample, input$shuffle); get_sample()}, zero_out_trials())
    #observeEvent(input$shuffle, zero_out_trials())
    observeEvent(input$accumulate_trials,
                 if (!input$accumulate_trials) zero_out_trials())

    zero_out_trials <- reactive({
      app_state$n_trials <<- 0
      app_state$Trials <<- data.frame()
      app_state$Trials
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

    # Choose the variables
    select1 <- function(x) x %>% filter(!numeric, n_levels <= 5) %>% .$vname
    select2 <- function(x) x  %>% filter(numeric, n_levels > 20) %>% .$vname
    observe({
      # output$debug_table <- renderTable(the_data$types)
      vnames1 <- select2(the_data$types)
      vnames2 <- select1(the_data$types)
      cat("Types has {nrow(the_data$types)} rows\n")
      updateSelectInput(session, "var1", choices =  vnames1)
      updateSelectInput(session, "var2", choices =  vnames2,
                        selected = vnames2[pmin(2, length(vnames2))])
    })

    construct_plot <- reactive({
      req(input$var1, input$var2)
      the_formula <- as.formula(glue::glue("{input$var1} ~ {input$var2}"))
      # The next line gets all the accumulated randomization trials.
      Trials <- get_all_trials()
      P <-
        get_sample()  %>%
        gf_jitter( the_formula, seed = 101,
                   width = input$jitter_width,
                   height = input$jitter_height,
                   alpha = input$jitter_alpha)

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
    output$statistics <- renderText({
      HTML(includeHTML("statistics.html"))
    })
  }
)
