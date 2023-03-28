## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load-------------------------------------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  ABM <- powoGenera(family = c("Aristolochiaceae", "Begoniaceae", "Martyniaceae"),
#                    hybrid = FALSE,
#                    verbose = FALSE,
#                    save = FALSE,
#                    dir = "results_powoGenera",
#                    filename = "Arist_Begon_Martyniaceae_search")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("POWOcodes")

res <- powoGenera(family = c("Aristolochiaceae", "Begoniaceae", "Martyniaceae"),
                  hybrid = FALSE,
                  verbose = FALSE,
                  save = FALSE)

knitr::kable(res[-c(7:10)],
             row.names = FALSE,
             caption = "TABLE 1. A general `powoGenera` search for mining all 
             accepted genera for some specific angiosperm families.")

## ---- eval = FALSE------------------------------------------------------------
#  data(POWOcodes)
#  
#  ALL_gen <- powoGenera(POWOcodes$family,
#                        hybrid = TRUE,
#                        verbose = TRUE,
#                        save = FALSE,
#                        dir = "results_powoGenera",
#                        filename = "all_angiosperm_genera")

## ---- eval = FALSE------------------------------------------------------------
#  data(POWOcodes)
#  
#  ALL_gen <- powoGenera(POWOcodes$family,
#                        hybrid = TRUE,
#                        verbose = TRUE,
#                        rerun = TRUE,
#                        save = FALSE,
#                        dir = "results_powoGenera",
#                        filename = "all_angiosperm_genera")

## ---- eval = FALSE------------------------------------------------------------
#  ADFL <- powoGenera(family = c("Aristolochiaceae", "Dipterocarpaceae",
#                                "Fagaceae", "Lecythidaceae"),
#                     hybrid = FALSE,
#                     country = c("Argentina", "Brazil", "French Guiana"),
#                     verbose = FALSE,
#                     save = FALSE,
#                     dir = "results_powoGenera",
#                     filename = "country_constrained_search")
#  

## ---- echo = FALSE, warning = FALSE-------------------------------------------
res <- powoGenera(family = c("Aristolochiaceae", "Dipterocarpaceae", 
                             "Fagaceae", "Lecythidaceae"),
                  hybrid = FALSE,
                  country = c("Argentina", "Brazil", "French Guiana"),
                  verbose = FALSE,
                  save = FALSE)

knitr::kable(res[-c(7:12)],
             row.names = FALSE,
             caption = "TABLE 2. A `powoGenera` search based on a specified 
             country vector.")

## ---- eval = FALSE------------------------------------------------------------
#  AL <- powoGenera(family = c("Aristolochiaceae", "Lecythidaceae"),
#                   genus = c("Asarum", "Bertholletia"),
#                   hybrid = FALSE,
#                   country = c("Brazil", "French Guiana"),
#                   verbose = TRUE,
#                   save = FALSE,
#                   dir = "results_powoGenera",
#                   filename = "genus_country_constrained_search")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
res <- powoGenera(family = c("Aristolochiaceae", "Lecythidaceae"),
                  genus = c("Asarum", "Bertholletia"),
                  hybrid = FALSE,
                  country = c("Brazil", "French Guiana"),
                  verbose = FALSE,
                  save = FALSE)

knitr::kable(res[-c(8:12)],
             row.names = FALSE,
             caption = "TABLE 3. A `powoGenera` search based on specified genus 
             and country vectors.")

