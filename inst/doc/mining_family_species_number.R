## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ----eval = FALSE-------------------------------------------------------------
# CLM <- powoFam(family = c("Cabombaceae", "Lecythidaceae", "Martyniaceae"),
#                verbose = TRUE,
#                save = FALSE,
#                dir = "results_powoFam",
#                filename = "Camb_Lecy_Martyniaceae_diversity")

## ----echo = FALSE, warning = FALSE--------------------------------------------
utils::data("angioData")
family <- c("Cabombaceae", "Lecythidaceae", "Martyniaceae")
fams <- angioData$family[angioData$family %in% family]
sp_nbr <- as.data.frame(table(fams))
species_number <- sp_nbr$Freq
res <- data.frame(family, species_number)

knitr::kable(res,
             row.names = FALSE,
             align = 'c',
             caption = "TABLE 1. A general `powoFam` search for mining the total species number of three angiosperm families.")

## ----eval = FALSE-------------------------------------------------------------
# utils::data(POWOcodes)
# 
# ALL_fam <- powoFam(POWOcodes$family,
#                    verbose = TRUE,
#                    save = FALSE,
#                    dir = "results_powoFam",
#                    filename = "all_plants_species_number")

