### Various assets used throughout the app

### Data
# adsl <- haven::read_xpt(paste0("https://github.com/phuse-org/TestDataFactory/",
#                                "raw/main/Updated/TDF_ADaM/adsl.xpt")) %>%
#   filter(SAFFL == "Y")
# 
# adtte <- haven::read_xpt(paste0("https://github.com/phuse-org/TestDataFactory/",
#                                 "raw/main/Updated/TDF_ADaM/adtte.xpt"))
# 
# adae <- haven::read_xpt(paste0("https://github.com/phuse-org/TestDataFactory/",
#                                "raw/main/Updated/TDF_ADaM/adae.xpt"))

adsl  <- readRDS(file = "www/adsl.RDS")
adae  <- readRDS(file = "www/adae.RDS")
adtte <- readRDS(file = "www/adtte.RDS")

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

