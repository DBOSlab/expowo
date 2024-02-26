## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load, message=FALSE, warning=FALSE-------------------------------
library(expowo)

## ----eval = FALSE-------------------------------------------------------------
#  CLM <- powoFam(family = c("Cabombaceae", "Lecythidaceae", "Martyniaceae"),
#                 verbose = TRUE,
#                 save = FALSE,
#                 dir = "results_powoFam",
#                 filename = "Camb_Lecy_Martyniaceae_diversity")

## ----echo = FALSE, warning = FALSE--------------------------------------------
utils::data("angioData")
utils::data("POWOcodes")
family <- c("Cabombaceae", "Lecythidaceae", "Martyniaceae")
fams <- angioData$family[angioData$family %in% family]
sp_nbr <- as.data.frame(table(fams))
powo_uri <- gsub("^http", "https", POWOcodes$uri[POWOcodes$family %in% family])
kew_id <- gsub(".+[:]", "", powo_uri)
species_number <- sp_nbr$Freq
res <- data.frame(family, species_number, kew_id, powo_uri)

knitr::kable(res,
             row.names = FALSE,
             align = 'c',
             caption = "TABLE 1. A general `powoFam` search for mining the total species number of three angiosperm families.")

## ----eval = FALSE-------------------------------------------------------------
#  utils::data(POWOcodes)
#  
#  ALL_fam <- powoFam(POWOcodes$family,
#                     verbose = TRUE,
#                     save = FALSE,
#                     dir = "results_powoFam",
#                     filename = "all_angiosperms_species_number")

