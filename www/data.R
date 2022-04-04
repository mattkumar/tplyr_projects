### original data sources
adsl <- haven::read_xpt(paste0("https://github.com/phuse-org/TestDataFactory/",
                        "raw/main/Updated/TDF_ADaM/adsl.xpt"))

adtte <- haven::read_xpt(paste0("https://github.com/phuse-org/TestDataFactory/",
                         "raw/main/Updated/TDF_ADaM/adtte.xpt"))

adae <- haven::read_xpt(paste0("https://github.com/phuse-org/TestDataFactory/",
                        "raw/main/Updated/TDF_ADaM/adae.xpt"))


# save for quick loading
save.image(file = "data.RData")
