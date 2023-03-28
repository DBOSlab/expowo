## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load-------------------------------------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  BL_dist <- powoSpDist(family = c("Begoniaceae", "Lecythidaceae"),
#                        species = c("Hillebrandia sandwicensis", "Lecythis pisonis"),
#                        verbose = TRUE,
#                        save = FALSE,
#                        dir = "results_powoSpDist",
#                        filename = "Begoniaceae_Lecythidaceae")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioData")
fam <- c("Begoniaceae", "Lecythidaceae")
df <- angioData[angioData$family %in% fam, ]
species <- c("Hillebrandia sandwicensis", "Lecythis pisonis")
df <- df[df$taxon_name %in% species, ]

knitr::kable(df[-c(2, 3, 6, 9, 11, 13)],
             row.names = FALSE,
             align = 'c',
             caption = "TABLE 1. A general `powoSpDist` search for mining 
             distribution of two angiosperm species.")

## ---- eval = FALSE------------------------------------------------------------
#  data(POWOcodes)
#  
#  ALL_dist <- powoSpDist(POWOcodes$family,
#                         species = NULL,
#                         verbose = TRUE,
#                         save = FALSE,
#                         dir = "results_powoSpDist",
#                         filename = "all_angiosperm_distribution")

