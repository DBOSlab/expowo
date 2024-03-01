## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ----eval = FALSE-------------------------------------------------------------
#  accGraph(inputdf = "Luetzelburgia_spp",
#           verbose = TRUE,
#           spp_acc = TRUE,
#           spp_changes = TRUE,
#           spp_changes_col = "genus",
#           genus_plots = FALSE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Luetzelburgia",
#           format = "jpg")
#  

## ----echo = FALSE, out.width = "600px", fig.cap = "FIGURE 1. Accumulation of formally described species in the big genus _Luetzelburgia_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Luetzelburgia.png", 
                        dpi = 300)

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
#           spp_acc = TRUE,
#           spp_changes = FALSE,
#           spp_changes_col = "genus",
#           genus_plots = TRUE,
#           save = TRUE,
#           dir = "results_accGraph",
#           filename = "cumulative_discovery_Asteraceae_",
#           format = "jpg")

## ----echo = FALSE, out.width = "600px", fig.cap = "FIGURE 2. Accumulation of formally described species in the genus _Achillea_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_Achillea.png", 
                        dpi = 300)

## ----echo = FALSE, out.width = "600px", fig.cap = "FIGURE 3. Accumulation of formally described species in the genus _Matricaria_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_Matricaria.png", 
                        dpi = 300)

## ----echo = FALSE, out.width = "600px", fig.cap = "FIGURE 4. Accumulation of formally described species in the genus _Launaea_ over time."----
knitr::include_graphics("figures/cumulative_discovery_Asteraceae_Launaea.png", 
                        dpi = 300)

