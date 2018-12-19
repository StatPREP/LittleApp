#' A data entry block for Little Apps
#'
#' @export
LA_data_ui <- function(width = 12 ) {
  box(title = "Sample", width = width, status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE,
      background = "black",
      selectInput("frame", "Data frame",
                  choices = LA_available_data()) %>% tighten(top=0),

      tighten(selectizeInput("var1", "Response", c("a", "b"))),
      tighten(selectizeInput("var2", "Explanatory", c("a", "b"))),
      tighten(selectizeInput("covar", "Covariates", c("a", "b"), multiple = TRUE)),
      HTML("<hr><br>") %>% tighten(),
      actionButton("new_sample", "New sample", icon=icon("child"),
                   style = "color: white; background-color: green; font-size: 16px; margin-left: 0px;" ) %>%
                     tighten(bottom = 10, top = -10),
      selectInput("samp_size", "Sample size n",
                  choices = c(5,10,20,50,100,200,500,1000,2000),
                  selected = 100) %>% tighten(),
      checkboxInput("stratify", "Stratify", value = FALSE) %>% tighten()
  )
}

#' @export
LA_data_server <-
  function(input, output, session, the_data, app_state,
           select1 = function(x) x$vnames, select2 = select1){
    observe({
      tmp <- unlist(strsplit(input$frame, ":", fixed = TRUE))
      Tmp <- LA_read_data(data_name = tmp[1], package = tmp[2])
      the_data$frame <- Tmp$frame
      the_data$codebook <- Tmp$codebook
      the_data$description <- Tmp$overall
      the_data$types <- Tmp$types

      description <- the_data$description
      var1_desc <- the_data$codebook[[isolate(input$var1)]]
      var2_desc <- the_data$codebook[[isolate(input$var2)]]
      var_explain <- glue::glue("<ul><li>{input$var1}:  {var1_desc}</li><li>{input$var2}:  {var2_desc}</li></ul>")
      output$codebook <- renderText({paste(description, var_explain)})
    })

  }
