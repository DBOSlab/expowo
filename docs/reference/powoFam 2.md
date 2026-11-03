# Extract species number of any plant family

Produces a CSV file listing the number of species and genera within the
target botanical families of vascular plants available at Plants of the
World Online (POWO).

## Usage

``` r
powoFam(family,
        verbose = TRUE,
        source = "wcvp",
        save = FALSE,
        dir = "results_powoFam",
        filename = "output")
```

## Arguments

- family:

  Either one family name or a vector of multiple families that is
  present in POWO.

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
  to create a directory named **results_powoFam** and the searched
  results will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **output**.

## Value

A table with the following fields: family, genus_number (counting of
genera within the specified plant family), species_number (countig of
species within each genus), kew_id (the family code used in Kew's
database), powo_uri (the URI to access the family in POWO).

## See also

[`megaGen`](https://dboslab.github.io/expowo/reference/megaGen.md)

[`topGen`](https://dboslab.github.io/expowo/reference/topGen.md)

[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`powoMap`](https://dboslab.github.io/expowo/reference/powoMap.md)

[`POWOcodes`](https://dboslab.github.io/expowo/reference/POWOcodes.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

powoFam(family = "Lecythidaceae",
        save = FALSE,
        dir = "results_powoFam",
        filename = "Lecythidaceae_spp_number")
} # }
```
