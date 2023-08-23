## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  res <- powoSpecies(family = c("Cabombaceae", "Martyniaceae"),
#                     hybrid = FALSE,
#                     verbose = TRUE,
#                     save = FALSE,
#                     dir = "results_powoSpecies",
#                     filename = "Cabom_Martyniaceae_search")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioData")
fam <- c("Cabombaceae", "Martyniaceae")
df <- angioData[angioData$family %in% fam, ]

knitr::kable(df[-c(2, 3, 4, 5, 10, 11, 13)],
             row.names = FALSE,
             caption = "TABLE 1. A general `powoSpecies` search for mining all 
             accepted species for some specific angiosperm families.")

## ---- eval = FALSE------------------------------------------------------------
#  ACM <- powoSpecies(family = c("Aristolochiaceae", "Cabombaceae", "Martyniaceae"),
#                     hybrid = FALSE,
#                     country = c("Brazil", "Colombia"),
#                     verbose = FALSE,
#                     save = FALSE,
#                     dir = "results_powoSpecies",
#                     filename = "country_constrained_search")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioData")
fam <- c("Aristolochiaceae", "Cabombaceae", "Martyniaceae")
df <- angioData[angioData$family %in% fam, ]
country <- c("Brazil", "Colombia")
df <- df[df$native_to_country %in% country, ]

knitr::kable(df[-c(2, 3, 4, 5, 10, 11, 12, 13)],
             row.names = FALSE,
             caption = "TABLE 2. A `powoSpecies` search based on a specified 
             country vector.")

## ---- eval = FALSE------------------------------------------------------------
#  MC <- powoSpecies(family = c("Martyniaceae", "Cabombaceae"),
#                    genus = c("Proboscidea", "Cabomba"),
#                    hybrid = FALSE,
#                    country = c("Argentina", "French Guiana", "Brazil"),
#                    verbose = TRUE,
#                    save = FALSE,
#                    dir = "results_powoSpecies",
#                    filename = "country_and_genus_constrained_search")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioData")
genus <- c("Proboscidea", "Cabomba")
country <- c("Argentina", "French Guiana", "Brazil")
df <- angioData[angioData$genus %in% genus, ]
df <- df[df$native_to_country %in% country, ]

knitr::kable(df[-c(2, 3, 10, 11, 13)],
             row.names = FALSE,
             caption = "TABLE 3. A `powoSpecies` search based on specified genus
             and country vectors.")

## ---- eval = FALSE------------------------------------------------------------
#  utils::data("POWOcodes")
#  
#  ALL_spp <- powoSpecies(POWOcodes$family,
#                         hybrid = TRUE,
#                         verbose = TRUE,
#                         save = FALSE,
#                         dir = "results_powoSpecies",
#                         filename = "all_angiosperm_species")

## ---- eval = FALSE------------------------------------------------------------
#  utils::data("POWOcodes")
#  
#  ALL_spp <- powoSpecies(POWOcodes$family,
#                         hybrid = TRUE,
#                         verbose = TRUE,
#                         rerun = TRUE,
#                         save = FALSE,
#                         dir = "results_powoSpecies",
#                         filename = "all_angiosperm_species")

