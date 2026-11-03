# Extract the top most species-rich genera of any plant family

Produces a CSV file listing the top most diverse genera of any target
botanical family of vascular plants at Plants of the World Online
(POWO).

## Usage

``` r
topGen(family,
       limit = 10,
       verbose = TRUE,
       source = "wcvp",
       save = FALSE,
       dir = "results_topGen",
       filename = "output")
```

## Arguments

- family:

  Either one family name or a vector of multiple families that is
  present in POWO.

- limit:

  A defined numerical value to limit the most diverse genera to be
  selected within each plant family. The default is to select the top
  ten richest genera.

- verbose:

  Logical, if `FALSE`, a message showing each step during the POWO
  search will not be printed in the console in full.

- source:

  Data source to be mined. The default `"wcvp"` uses the bulk data of
  the World Checklist of Vascular Plants (WCVP), the taxonomic backbone
  of POWO, which is downloaded once from Kew and cached on disk (see
  `tools::R_user_dir("expowo", "cache")`). It is refreshed only when Kew
  releases a new WCVP version. Use `"powo"` to mine the source page of
  each taxon at the POWO website instead, which is much slower and may
  be blocked by POWO.

- save:

  Logical, if `TRUE`, the search results will be saved on disk.

- dir:

  Pathway to the computer's directory, where the file will be saved
  provided that the argument `save` is set up in `TRUE`. The default is
  to create a directory named **results_topGen** and the search results
  will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **output**.

## Value

A table with the following fields: family, genus, authors,
scientific_name, species_number (counting of species within each genus),
kew_id (each genus code within Kew's databases), powo_uri (URI to access
each genus in POWO).

## See also

[`POWOcodes`](https://dboslab.github.io/expowo/reference/POWOcodes.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

topGen(family = "Lecythidaceae",
       limit = 10,
       save = FALSE,
       dir = "results_topGen",
       filename = "Lecythidaceae_top_ten")
} # }
```
