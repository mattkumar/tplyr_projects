### Various assets used throughout the app

### Tplyr defaults
options(
  tplyr.count_layer_default_formats =
    list(n_counts = f_str("xxx (xx.x%)", n, pct)),
  
  tplyr.desc_layer_default_formats =
    list('N'         = f_str('xx', n),
         'Mean (SD)' = f_str('xx.xx (xx.xxx)', mean, sd),
         'Median'    = f_str('xx.x', median),
         'Min, Max'  = f_str('xx, xx', min, max))
)


### Tplyr Table Construction
t <- tplyr_table(adsl, TRT01P) %>%
  add_total_group() %>%
  add_layer(
    group_count(AGEGR1, b = "Age (%)")
  ) %>%
  add_layer(
    group_count(SEX, b = "Gender")
  ) %>%
  add_layer(
    group_count(ETHNIC, b = "Ethnicity")
  ) %>%
  add_layer(
    group_desc(HEIGHTBL, b = "Height (m)")
  ) %>%
  add_layer(
    group_desc(WEIGHTBL, b = "Weight (kg)")
  ) %>%
  add_layer(group_desc(
    BMIBL, b = "Body Mass Index (kg/m2)")
  ) %>%
  add_layer(
    group_desc(MMSETOT, b = "MMSE Total Score")
  )

### build, with metadata = TRUE
dat <- t %>%
  build(metadata = TRUE)

### unfill utility function - https://github.com/tidyverse/tidyr/issues/250
unfill_vec <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, "", x)
}

### clean up things a bit
dat <- dat %>%
  mutate_at(.vars = vars(row_label1),
            .funs = unfill_vec) %>%
  select(row_id,
         row_label1,
         row_label2,
         `var1_Xanomeline Low Dose`,
         `var1_Xanomeline High Dose`,
         var1_Placebo,
         var1_Total)

### reactable theme
my_theme <- reactableTheme(
  backgroundColor = "#272822",
  borderColor = "#A2A39C",
  #highlightColor = "#A6E22E",
  style = list(
    fontFamily = "Inconsolata",
    color = "#FFF"
  )
)

### appreciably different reactable theme
my_theme2 <- reactableTheme(
  backgroundColor = "#272822",
  borderColor = "#A2A39C",
  style = list(
    fontFamily = "Inconsolata",
    color = "#6d6e66"
  )
)

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
                   
                   ")


# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_Canada.1252  LC_CTYPE=English_Canada.1252   
# [3] LC_MONETARY=English_Canada.1252 LC_NUMERIC=C                   
# [5] LC_TIME=English_Canada.1252    
#
# other attached packages:
# [1] survival_3.2-13      highcharter_0.9.4    haven_2.4.3          reactablefmtr_2.0.0 
# [5] reactable_0.2.3.9000 Tplyr_0.4.4          forcats_0.5.1        stringr_1.4.0       
# [9] dplyr_1.0.7          purrr_0.3.4          readr_2.1.1          tidyr_1.1.4         
# [13] tibble_3.1.6        ggplot2_3.3.5        tidyverse_1.3.1      shinyjs_2.1.0       
# [17] shiny_1.7.1         
