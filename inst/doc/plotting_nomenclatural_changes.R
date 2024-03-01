## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ----eval = FALSE-------------------------------------------------------------
#  newdata <- powoSpecies(family = "Asteraceae",
#                         genus = c("Achillea", "Matricaria", "Launaea"),
#                         synonyms = TRUE,
#                         save = FALSE,
#                         dir = "Asteraceae_results",
#                         filename = "Asteraceae_spp")

## ----eval = FALSE-------------------------------------------------------------
#  accGraph(inputdf = newdata,
#           verbose = TRUE,
#           spp_acc = FALSE,
#           spp_changes = TRUE,
#           spp_changes_col = "genus",
#           genus_plots = TRUE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Asteraceae_",
#           format = "jpg")

## ----echo = FALSE, out.width = "600px", fig.cap = "FIGURE 1. The temporal dynamics of nomenclatural changes in three genera."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_all_data.png", 
                        dpi = 300)

