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
#           multi = FALSE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Crotalaria",
#           format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 1. Accumulation graph of new species discoveries in the big genus _Crotalaria_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Crotalaria.png", 
                        dpi = 300)

## ---- eval = FALSE------------------------------------------------------------
#  newdata <- powoSpecies(family = "Poaceae",
#                         genus = c("Andropogon", "Olyra", "Digitaria"),
#                         synonyms = TRUE,
#                         save = FALSE,
#                         dir = "Poaceae_results",
#                         filename = "Poaceae_spp")

## ---- eval = FALSE------------------------------------------------------------
#  accGraph(inputdf = newdata,
#           verbose = TRUE,
#           multi = TRUE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Poaceae_",
#           format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 2. Accumulation graph of new species discoveries in the genus _Andropogon_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Poaceae_Andropogon.png", 
                        dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 3. Accumulation graph of new species discoveries in the genus _Digitaria_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Poaceae_Digitaria.png", 
                        dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 4. Accumulation graph of new species discoveries in the genus _Olyra_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Poaceae_Olyra.png", 
                        dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 5. Violin graph of new species discoveries in three genera over time."----
knitr::include_graphics("figures/cumulative_discovery_Poaceae_all_data.png", 
                        dpi = 300)

