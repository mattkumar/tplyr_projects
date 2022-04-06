# libraries
library(shiny)
library(shinyjs)
library(tidyverse)
#remotes::install_github("atorus-research/Tplyr@20beb73cd4c202308afbbb57c1f2b6e1590295b7")
library(Tplyr)
library(reactable)
library(reactablefmtr)
library(haven)
library(highcharter)
library(survival)
 
### js for reactable
js_string <- JS("
  function(rowInfo, colInfo) {
   if (window.Shiny) {
      Shiny.setInputValue('row', { index: rowInfo.index + 1 })
      Shiny.setInputValue('col', {column: colInfo.id})
    }}")

### css for shiny
font_string <- HTML("@import url('https://fonts.googleapis.com/css2?family=Inconsolata&display=swap');")

css_string <- HTML(".center2 {
                      margin: auto;
                      width: 50%;
                      box-shadow: 10px 10px 8px 10px #c1c1c1;
                      padding: 20px;
                    }
                   
                   h1, h5 {
                    font-weight: bold;
                    text-shadow: 2px 2px #c1c1c1;
                   }
              
                   body {
                    zoom: 0.85;
                    font-family: Inconsolata;
                   }
                   
                   .rt-td-inner:active, .rt-td-inner:hover {
                     background-color: #A6E22E !important; 
                   }
                   
                   .shiny-output-error-validation {
                     color: #A6E22E;
                     font-weight: bold;
                     font-size: 20px;
                   }
                   
                   .my_green {
                     color: #A6E22E;
                     font-weight: bold;
                   }
                   
                   ")


# ui
ui <- fluidPage(
  useShinyjs(),
  tags$style(font_string),
  tags$style(css_string),
  br(),
  h1("TLFs, Tplyr and Shiny", 
     align = "center" ),
  HTML("<h3 style = 'text-align:center'>Click on any <span class = 'my_green'>result cell</span> of the demographics table to view additional, linked TLFs for those subjects</h3>"),
  HTML("<h3 style = 'text-align:center'>Clicking on any cell in the <strong>total column</strong> offers by-treatment visualizations</h3>"),
  HTML("<h3 style = 'text-align:center'>TLFs appearing on the side can be <strong>dragged/repositioned</strong> with your mouse</h3>"),
  br(),
  div(class = "center2",
      h5('Table 1-1: Demographics <<safety analysis set>>',
         align = "left"),
      reactableOutput('main_tlf')),
  br(),
  br(),
  hidden(div(class = "center2", 
             h5('Table 2-1: Adverse Events by System Organ Class, Preferred Term <<safety analysis set>>',
                align = "left"),
             id = "ae_tlf_panel",
             reactableOutput("ae_sub_tlf"))),
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
  
  
  # source dependencies
  source("www/deps.R")
  
  # setup reactive values for clicked cells
  # this will be used for data drill downs/visuals
  row <- reactive(dat[input$row$index,1]$row_id)
  col <- reactive(input$col$column)
  tplyr_subset_data <- reactive(t %>%get_meta_subset(row(), col()))
  
  # table display of the demographics table
  output$main_tlf <- renderReactable({
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
                html = TRUE
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
                `var1_Xanomeline Low Dose` = colDef(
                  name = "Xanomeline Low Dose<br>N=84"
                ),
                `var1_Xanomeline High Dose` = colDef(
                  name = "Xanomeline High Dose<br>N=84"
                ),
                var1_Placebo = colDef(
                  name = "Placebo<br>N=86"
                ),
                var1_Total = colDef(
                  name = "Total<br>N=254"
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
    tplyr_subset_data() %>%
      reactable(.,
                width = "auto",
                style = list(
                  fontSize = "12px",
                  color = "#A6E22E"
                ),
                theme = my_theme2,
                defaultPageSize = 20,
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
                  top = 30,
                  right = 55,
                  width = 460,
                  draggable = TRUE,
                  height = "auto",
                  h5('Listing 1-1: Subject listing of Demographics <<safety analysis set>>',
                     align = "left"),
                  reactableOutput("drill")
    )
  })
  
  
  # AE graph
  output$ae_graph <- renderHighchart({
    
    validate(
      need(!(col() %in% c('row_label1','row_label2')),
           'Select a result cell of the table')
    )
    
    ae_data <- t %>%
      get_meta_subset("c1_1", "var1_Total") %>%
      inner_join(adae, by = c('USUBJID' = 'USUBJID')) %>%
      select(USUBJID, AEDECOD, AESEV) %>%
      count(AEDECOD, sort = TRUE) %>%
      slice(1:5) %>%
      select(AEDECOD) %>%
      inner_join(adae) %>%
      inner_join(adsl %>% select(USUBJID, TRT01P)) %>%
      count(AEDECOD, AESEV, TRT01P) 
    
    if(str_detect(col(), "Total")) {
      
      ae_data %>% 
        group_by(AEDECOD,TRT01P) %>% 
        summarize(n = sum(n)) %>%
        hchart(
          .,
          type = "column", 
          hcaes(
            x = AEDECOD, 
            y = n, 
            group = TRT01P
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
        
      
    } else { 
        
      # visualize top 5 AEDECODs, by severity
      ae_data %>% 
        group_by(AEDECOD, AESEV) %>% 
        summarize(n = sum(n)) %>%
        hchart(
          .,
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
      }
  })
  
  # renderUI that shows the AE graph
  output$ae_panel <- renderUI({
    absolutePanel(id = "controls2",
                  class = "panel panel-default center2",
                  top = 550,
                  left = 55,
                  width = 460,
                  draggable = TRUE,
                  height = "auto",
                  h5('Figure 2-1: Top 5 Adverse Events by Preferred Term <<safety analysis set>>',
                     align = "left"),
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
    tte_data <- tplyr_subset_data() %>%
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
                  width = 460,
                  draggable = TRUE,
                  height = "auto",
                  h5('Figure 3-1: Time to First Dermatologic Event - Kaplan-Meier <<safety analysis set>>',
                     align = "left"),
                  highchartOutput("tte_graph")
    )
  })
  
  output$ae_sub_tlf <- renderReactable({

    validate(
      need(!(col() %in% c('row_label1','row_label2')),
           'Select a result cell of the table')
    )

    # subset clicked data from adae
    inp <- tplyr_subset_data() %>%
      inner_join(adae)


    # build AE table
    t2 <- tplyr_table(inp, TRT01P) %>%
      add_layer(
        group_count(
          vars(
            AEBODSYS, AEDECOD
          )
        )
      ) %>%
      build()


    # rough data mgmt
    t2 <- t2 %>%
      mutate(row_label2 = Hmisc::capitalize(tolower(row_label2))) %>%
      mutate(test2 = str_replace_all(row_label2, " ","&nbsp;")) %>%
      select(row_label1, test2, starts_with("var"))

    # reactable
    t2 %>%
      reactable(.,
                width = "auto",
                defaultColDef = colDef(html=TRUE),
                theme = my_theme,
                groupBy = c('row_label1'),
                highlight = TRUE,
                pagination = FALSE,
                sortable = FALSE,
                compact = TRUE,
                columns = list(
                  row_label1 = colDef(
                    name = "System Organ Class"
                  ),
                  test2 = colDef(
                    name = "System Organ Class<br>&nbsp;Preferred Term"
                  ),

                  `var1_Xanomeline High Dose` = colDef(
                    name = "Xanomeline High Dose",
                    align = "center"
                  ),
                  `var1_Xanomeline Low Dose` = colDef(
                    name = "Xanomeline Low Dose",
                    align = "center"
                  ),
                  var1_Placebo = colDef(
                    name = "Placebo",
                    align = "center"
                  )
                )
      )

  })
  
  
  # only show the panels on click
  observeEvent(col(), {
    show("data_panel")
    show("ae_panel")
    show("tte_panel")
    show("ae_tlf_panel")
  })
}

# run
shinyApp(ui = ui, server = server)