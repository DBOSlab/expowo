## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load-------------------------------------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  megaGen(family = c("Aristolochiaceae", "Begoniaceae", "Martyniaceae"),
#          thld = 500,
#          verbose = TRUE,
#          save = TRUE,
#          dir = "results_megaGen/",
#          filename = "Arist_Begon_Martyniaceae_search")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioGenera")
family <- c("Aristolochiaceae", "Begoniaceae")
genus <- c("Aristolochia", "Begonia")
species_number <- angioGenera$species_number[angioGenera$genus %in% genus]
authors <- c("L.", "L.")
scientific_name <- c("Aristolochia L.", "Begonia L.")
powo_uri <- angioGenera$powo_uri[angioGenera$genus %in% genus]
kew_id <- angioGenera$kew_id[angioGenera$genus %in% genus]

res <- data.frame(family, genus, authors, scientific_name,
                  species_number, kew_id, powo_uri)

knitr::kable(res,
             row.names = FALSE,
             caption = "TABLE 1. A general `megaGen` search to mining the
             megadiverse genera for some specific angiosperm families,
             based on a specified threshold of 500 species.")

## ---- eval = FALSE------------------------------------------------------------
#  data(POWOcodes)
#  
#  megaGen(POWOcodes$family,
#          thld = 500,
#          verbose = TRUE,
#          save = TRUE,
#          dir = "results_megaGen/",
#          filename = "all_megadiverse_angiosperm_genera")

