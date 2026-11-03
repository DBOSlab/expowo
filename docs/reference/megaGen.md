# Extract megadiverse genera of any plant family

Produces a CSV file listing all mega-diverse genera for any plant family
at Plants of the World Online (POWO) based on a provided numeric value
as the threshold to be considered mega-diverse. Frodin (2004) suggests
500 species as the threshold.

## Usage

``` r
megaGen(family,
        thld = 500,
        verbose = TRUE,
        source = "wcvp",
        save = FALSE,
        dir = "results_megaGen",
        filename = "output")
```

## Arguments

- family:

  Either one family name or a vector of multiple families that is
  present in POWO.

- thld:

  A defined threshold of species number for a genus to be considered
  megadiverse. The default value is 500 based on Frodin (2004).

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
  to create a directory named **results_megaGen** and the search results
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

megaGen(family = "Cyperaceae",
        thld = 500,
        save = FALSE,
        dir = "results_megaGen",
        filename = "Cyperaceae_big_genera")
} # }
```
