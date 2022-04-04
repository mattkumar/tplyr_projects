# libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(Tplyr)
library(reactable)
library(reactablefmtr)
library(haven)
library(highcharter)
library(survival)

# source dependencies
source("www/deps.R")

# ui
ui <- fluidPage(
  useShinyjs(),
  tags$style(font_string),
  tags$style(css_string),
  br(),
  h1("TLFs, Tplyr and Shiny", 
     align = "center" ),
  h3("Click on any result cell to explore those subjects further",
     align = "center"),
  br(),
  div(class = "center2",
      reactableOutput('main_tlf')),
  hidden(
    uiOutput('data_panel')
  ),
  hidden(
    uiOutput('ae_panel')
  ),
  hidden(
    uiOutput('tte_panel')
  )
)

# server
server <- function(input, output) {
  # load data
  load("www/data.RData")

    # setup reactives for clicked cells
  # this will be used for data drill downs/visuals
  row <- reactive(dat[input$row$index,1]$row_id)
  other <- reactive(dat[input$row$index,1])
  col <- reactive(input$col$column)
  
  # table display of the demographics table
  output$main_tlf <- renderReactable({
    print(row()) 
    print(other())
    print(col())
    reactable(dat,
              width = "auto",
              onClick = js_string,
              theme = my_theme,
              pagination = FALSE,
              highlight = TRUE,
              sortable = FALSE,
              compact = TRUE,
              defaultColDef = colDef(
                align = "center",
              ),
              columns = list(
                row_id = colDef(
                  show = FALSE
                ),
                row_label1 = colDef(
                  name = "Demographic",
                  align = "center"
                ),
                row_label2 = colDef(
                  name = "",
                  align = "left"
                ),
                var1_Placebo = colDef(
                  name = "Placebo N=79"
                ),
                var1_Total = colDef(
                  name = "Total N=234"
                ),
                `var1_Xanomeline High Dose` = colDef(
                  name = "Xanomeline High Dose N=74"
                ),
                `var1_Xanomeline Low Dose` = colDef(
                  name = "Xanomeline Low Dose N=81"
                )
              )
    ) 
  }) 
  
  # drilled data
  output$drill <- renderReactable({
    
    validate(
      need(!(col() %in% c('row_label1','row_label2')),
           'Select a result cell of the table')
    )
    
    # subset + display clicked data
    t %>%
      get_meta_subset(row(), col()) %>%
      reactable(.,
                width = "auto",
                style = list(
                  fontSize = "12px",
                  color = "#A6E22E"
                ),
                theme = my_theme2,
                defaultPageSize = 10,
                highlight = FALSE,
                compact = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  style = cell_style(.,
                                     border_width = "thin",
                                     border_color = "#A2A39C",
                                     border_style = "dotted")
                )
      )
  })
  
  # renderUI that shows the drill down data
  output$data_panel <- renderUI({
    absolutePanel(id = "controls",
                  class = "panel panel-default center2",
                  top = 300,
                  right = 55,
                  width = 375,
                  draggable = TRUE,
                  height = "auto",
                  h5('Subject Data',
                     align = "center"),
                  reactableOutput("drill")
    )
  })
  
  
  # AE graph
  output$ae_graph <- renderHighchart({
    
    validate(
      need(!(col() %in% c('row_label1','row_label2')),
           'Select a result cell of the table')
    )
    
    # subset + process clicked data
    ae_data <- t %>%
      get_meta_subset(row(), col()) %>%
      inner_join(adae) %>%
      select(USUBJID, AEDECOD, AESEV) %>%
      count(AEDECOD, sort = TRUE) %>%
      slice(1:5) %>%
      select(AEDECOD) %>%
      inner_join(adae) %>%
      count(AEDECOD, AESEV) 
    
    # visualize top 5 AEDECODs, by severity
      hchart(
        ae_data,
        type = "column", 
        hcaes(
          x = AEDECOD, 
          y = n, 
          group = AESEV
          )
        ) %>%
      hc_add_theme(
        hc_theme_monokai()
      ) %>%
      hc_xAxis(
        labels = list(
          style = list(
            color = "#FFF"
          )
        )
      ) %>%
      hc_yAxis(
        labels = list(
          style = list(
            color = "#FFF"
          )
        )
      )
        
  })
  
  # renderUI that shows the AE graph
  output$ae_panel <- renderUI({
    absolutePanel(id = "controls2",
                  class = "panel panel-default center2",
                  top = 550,
                  left = 55,
                  width = 375,
                  draggable = TRUE,
                  height = "auto",
                  h5('Top 5 Frequent AEs',
                     align = "center"),
                  highchartOutput("ae_graph")
    )
  })
  
  
  # TTE graph
  output$tte_graph <- renderHighchart({
    
    validate(
      need(!(col() %in% c('row_label1','row_label2')),
           'Select a result cell of the table')
    )
    
    # subset + process clicked data
    tte_data <- t %>%
      get_meta_subset(row(), col()) %>%
      inner_join(adtte)
    
   
    # if total column is detected, show KM by TRT
    # otherwise, just show conditional KM
    if(str_detect(col(), "Total")) {
      fit <- survfit(
        Surv(AVAL, 1-CNSR) ~ TRT01P, 
        data = tte_data
        )
      mc <- NULL
    } else {
      fit <- survfit(
        Surv(AVAL, 1-CNSR) ~ 1, 
        data = tte_data
        )
      mc <- "#FFFF00"
    }
    
    hchart(
      fit, 
      ranges = TRUE,
      markerColor = mc
      ) %>%
      hc_add_theme(
        hc_theme_monokai()
      ) %>%
      hc_xAxis(
        labels = list(
          style = list(
            color = "#FFF"
          )
        )
      ) %>%
      hc_yAxis(
        labels = list(
          style = list(
            color = "#FFF"
          )
        )
      )
  })
  
  
  # renderUI that shows the TTE graph
  output$tte_panel <- renderUI({
    absolutePanel(id = "controls3",
                  class = "panel panel-default center2",
                  top = 30,
                  left = 55,
                  width = 375,
                  draggable = TRUE,
                  height = "auto",
                  h5('Efficacy Graph',
                     align = "center"),
                  highchartOutput("tte_graph")
    )
  })
  
  # only show the absolute panels on click
  observeEvent(col(), {
    show("data_panel")
    show("ae_panel")
    show("tte_panel")
  })
}

# run
shinyApp(ui = ui, server = server)