# Extract distribution of any plant species

Produces a CSV file listing the geographical distribution at country and
botanical subdivisions of all target species of vascular plants at
Plants of the World Online (POWO).

## Usage

``` r
powoSpDist(family,
           species = NULL,
           verbose = TRUE,
           source = "wcvp",
           save = FALSE,
           dir = "results_powoSpDist",
           filename = "output")
```

## Arguments

- family:

  Either one family name or a vector of multiple families that is
  present in POWO.

- species:

  Either one non-hybrid species name or a vector of multiple species
  that is present in POWO. If any species name is not provided, then the
  function will search any species from all accepted genera known for
  the target family.

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
  to create a directory named **results_powoSpDist** and the search
  results will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **output**.

## Value

A table with the following fields: family, genus, species, taxon_name,
authors, scientific_name, native_to_country (original distribution
according to political country), native_to_botanical_countries (original
distribution according to botanical country), introduced_to_country
(exotic distribution according to political country),
introduced_to_botanical_countries (exotic distribution according to
botanical country), kew_id (each species code within Kew's databases),
powo_uri (URI to access each species in POWO).

## See also

[`POWOcodes`](https://dboslab.github.io/expowo/reference/POWOcodes.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

powoSpDist(family = "Lecythidaceae",
           species = "Bertholletia excelsa",
           save = FALSE,
           dir = "results_powoSpDist",
           filename = "B_excelsa_distribution")
} # }
```
