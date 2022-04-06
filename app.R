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
js_string <- JS("function(rowInfo, colInfo) {
    // Only handle click events on the 'details' column
    if (colInfo.id !== 'tlf1') {
      return
    }

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
      Shiny.setInputValue('show_tlf1', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
  }")

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
                   
                   .mybuttonclass1 {
                      background-color: #F92672; 
                      color: white;
                      text-align: center;
                      text-decoration: none;
                      display: inline-block;
                      font-size: 12px;
                   }
                    
                    .mybuttonclass2 {
                      background-color: #66D9EF; 
                      color: white;
                      text-align: center;
                      text-decoration: none;
                      display: inline-block;
                      font-size: 12px;
                    }
                    
                    .mybuttonclass3 {
                      background-color: #FFFF00; 
                      color: black;
                      text-align: center;
                      text-decoration: none;
                      display: inline-block;
                      font-size: 12px;
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
  HTML("<h3 style = 'text-align:center'>Click on any <span class = 'my_green'>button</span> of the demographics table to view additional, linked TLFs for that row of subjects</h3>"),
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
             reactableOutput("ae_sub_tlf")))
)

# server
server <- function(input, output) {
  
  
  # source dependencies
  source("www/deps.R")
  
  # setup reactives for clicked cells
  # this will be used for data drill downs/visuals
  row1 <- reactive(dat[input$show_tlf1$index,1]$row_id)
  
  
  # table display of the demographics table
  output$main_tlf <- renderReactable({
    reactable(dat %>% mutate(tlf1 = NA,
                             tlf2 = NA,
                             tlf3 = NA),
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
                ),
                tlf1 = colDef(
                  name = "AE TLF",
                  sortable = FALSE,
                  cell = function() htmltools::tags$button("Click To View", class = "mybuttonclass1")
                  
                ),
                tlf2 = colDef(
                  name = "AE TLF",
                  sortable = FALSE,
                  cell = function() htmltools::tags$button("Click To View", class = "mybuttonclass2")
                  
                ),
                tlf3 = colDef(
                  name = "AE TLF",
                  sortable = FALSE,
                  cell = function() htmltools::tags$button("Click To View", class = "mybuttonclass3")
                  
                )
              )
    ) 
  }) 
  
  
  
  output$ae_sub_tlf <- renderReactable({
    
    validate(
      need(length(row1()) == 1,
           'Select a result cell of the table')
    )
    
    # subset clicked data from adae
    inp <- t %>%
      get_meta_subset(row1(), "var1_Total") %>%
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
  
  observeEvent(input$show_tlf1, {
    show("ae_tlf_panel")
  })
  
}

# run
shinyApp(ui = ui, server = server)