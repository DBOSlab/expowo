## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  accGraph(inputdf = "Crotalaria_spp",
#           verbose = TRUE,
#           spp_acc = TRUE,
#           spp_changes = TRUE,
#           save_data_to_plot = FALSE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Crotalaria",
#           format = "jpg")
#  

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 1. Accumulation graph of new species discoveries in the big genus _Crotalaria_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Crotalaria.png", 
                        dpi = 300)

## ---- eval = FALSE------------------------------------------------------------
#  newdata <- powoSpecies(family = "Asteraceae",
#                         genus = c("Achillea", "Matricaria", "Launaea"),
#                         synonyms = TRUE,
#                         save = FALSE,
#                         dir = "Asteraceae_results",
#                         filename = "Asteraceae_spp")

## ---- eval = FALSE------------------------------------------------------------
#  accGraph(inputdf = newdata,
#           verbose = TRUE,
#           spp_acc = TRUE,
#           spp_changes = TRUE,
#           save_data_to_plot = FALSE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Asteraceae_",
#           format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 2. Accumulation graph of new species discoveries in the genus _Achillea_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_Achillea.png", 
                        dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 3. Accumulation graph of new species discoveries in the genus _Matricaria_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_Matricaria.png", 
                        dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 4. Accumulation graph of new species discoveries in the genus _Launaea_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_Launaea.png", 
                        dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 5. Violin graph of new species discoveries in three genera over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_all_data.png", 
                        dpi = 300)

