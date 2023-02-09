## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load-------------------------------------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  mapspdist <- powoSpecies(family = "Lecythidaceae",
#                           genus = "Cariniana",
#                           hybridspp = FALSE,
#                           country = NULL,
#                           verbose = TRUE,
#                           save = TRUE,
#                           dir = "results_powoSpecies/",
#                           filename = "Lecythidaceae_Cariniana")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioData")

df <- angioData[angioData$family %in% "Lecythidaceae", ]
df <- df[df$genus %in% "Cariniana", ]
                               

## ---- echo = FALSE, warning = FALSE-------------------------------------------
knitr::kable(df[-c( 2, 3, 5, 6, 7, 10, 11, 13)],
             row.names = FALSE,
             align = 'c',
             caption = "TABLE 1. A general `powoSpecies` search for mining 
             distribution of the Lecythidaceae genus _Cariniana_.")


## ---- eval = FALSE------------------------------------------------------------
#  powoMap(data = mapspdist,
#          botctrs = NULL,
#          map_div = "native_to_country",
#          multigen = TRUE,
#          verbose = FALSE,
#          vir_color = "viridis",
#          bre_color = NULL,
#          leg_title = "SR",
#          dpi = 600,
#          dir = "results_powoMap/",
#          filename = "global_richness_country_map",
#          format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 1. Global species richness of the genus _Cariniana_ at country level and colored with viridis scale."----
knitr::include_graphics("figures/global_richness_country_map_SR_Cariniana_viridis.png", 
                        dpi = 300)

## ---- eval = FALSE------------------------------------------------------------
#  data(botdivmap)
#  
#  powoMap(data = mapspdist,
#          botctrs = botdivmap,
#          map_div = "native_to_botanical_countries",
#          multigen = TRUE,
#          verbose = FALSE,
#          vir_color = "viridis",
#          bre_color = NULL,
#          leg_title = "SR",
#          dpi = 600,
#          dir = "results_powoMap/",
#          filename = "global_richness_botcountry_map",
#          format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 2. Global species richness of the genus _Cariniana_ at botanical country level and colored with viridis scale."----
knitr::include_graphics("figures/global_richness_botcountry_map_SR_Cariniana_viridis.png", dpi = 300)

## ---- eval = FALSE------------------------------------------------------------
#  
#  mapspdist <- powoSpecies(family = "Lecythidaceae",
#                           genus = NULL,
#                           hybridspp = FALSE,
#                           country = NULL,
#                           verbose = FALSE,
#                           save = FALSE,
#                           dir = "results_powoSpecies/",
#                           filename = "Lecythidaceae")

## ---- eval = FALSE------------------------------------------------------------
#  data(botdivmap)
#  
#  powoMap(data = mapspdist,
#          botctrs = botdivmap,
#          map_div = "native_to_country",
#          multigen = FALSE,
#          verbose = FALSE,
#          vir_color = "viridis",
#          bre_color = "Spectral",
#          leg_title = "SR",
#          dpi = 600,
#          dir = "results_powoMap/",
#          filename = "global_richness_botcountry_map",
#          format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 3. Global species richness of Lecythidaceae at country level and colored with viridis scale."----
knitr::include_graphics("figures/global_richness_country_map_SR_Lecythidaceae_viridis.png", dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 4. Global species richness of Lecythidaceae at country level and colored with Spectral scale."----
knitr::include_graphics("figures/global_richness_country_map_SR_Lecythidaceae_Spectral.png", dpi = 300)

## ---- eval = FALSE------------------------------------------------------------
#  data(botdivmap)
#  
#  powoMap(data = mapspdist,
#          botctrs = botdivmap,
#          map_div = "native_to_botanical_countries",
#          multigen = FALSE,
#          verbose = FALSE,
#          vir_color = "viridis",
#          bre_color = "Spectral",
#          leg_title = "SR",
#          dpi = 600,
#          dir = "results_powoMap/",
#          filename = "global_richness_botcountry_map",
#          format = "jpg")

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 5. Global species richness of Lecythidaceae at botanical country level and colored with viridis scale."----
knitr::include_graphics("figures/global_richness_botcountry_map_SR_Lecythidaceae_viridis.png", dpi = 300)

## ---- echo = FALSE, out.width = "600px", fig.cap = "FIGURE 6. Global species richness of Lecythidaceae at botanical country level and colored with Spectral scale."----
knitr::include_graphics("figures/global_richness_botcountry_map_SR_Lecythidaceae_Spectral.png", dpi = 300)

