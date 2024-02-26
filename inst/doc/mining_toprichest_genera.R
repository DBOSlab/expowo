## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ----eval = FALSE-------------------------------------------------------------
#  ABL_top <- topGen(family = c("Aristolochiaceae", "Begoniaceae", "Lecythidaceae"),
#                    limit = NULL,
#                    verbose = TRUE,
#                    save = FALSE,
#                    dir = "results_topGen",
#                    filename = "Aristo_Bego_Lecythidaceae_search")

## ----echo = FALSE, warning = FALSE--------------------------------------------
library(dplyr, warn.conflicts = FALSE)
utils::data("angioGenera")
family <- c("Begoniaceae", "Aristolochiaceae", "Lecythidaceae")

genus <- angioGenera$genus[angioGenera$family %in% family]
species_number <- angioGenera$species_number[angioGenera$family %in% family]
family <- angioGenera$family[angioGenera$family %in% family]
table <- data.frame(family, genus, species_number)
res <- table %>% arrange(desc(table$species_number)) %>% group_by(family) %>% 
  slice(1:10)

knitr::kable(res,
             row.names = FALSE,
             align = 'c',
             caption = "TABLE 1. A general `topGen` search to mining the top 
             most species rich genera for some specific angiosperm 
             families.")

## ----eval = FALSE-------------------------------------------------------------
#  data(POWOcodes)
#  
#  ALL_top <- topGen(POWOcodes$family,
#                    limit = 10,
#                    verbose = TRUE,
#                    save = FALSE,
#                    dir = "results_topGen",
#                    filename = "all_toprichest_angiosperm_genera")

