## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package load-------------------------------------------------------------
library(expowo)

## ---- eval = FALSE------------------------------------------------------------
#  utils::data("POWOcodes")

## ---- eval = FALSE------------------------------------------------------------
#  utils::data("angioData")

## ---- echo = FALSE, warning = FALSE-------------------------------------------
utils::data("angioGenera")
df <- angioGenera

knitr::kable(df[-c(2, 3, 7, 8, 9, 10, 11, 12, 13)],
             row.names = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  utils::data("angioGenera")

## ---- eval = FALSE------------------------------------------------------------
#  utils::data("botregions")

