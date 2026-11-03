# Secondary function to transform data generated with powoSpecies

Produces a spreadsheet with extra data about the publication, adding the
corrected year of publication and the counting of synonyms of each
species. It is an optional function to be used after run expowo's
`powoSpecies` and to use as input data in the expowo's `accGraph`.

## Usage

``` r
get_year_pubs(inputdf = NULL,
              verbose = TRUE,
              save = FALSE,
              dir = "results_POWO_with_year_pubs",
              filename = "POWO_with_year_pubs")
```

## Arguments

- inputdf:

  A dataframe generated using `powoSpecies` function with the argument
  synonyms set as `TRUE` and containing the genus and species column and
  the associated information about the protologue of each species. The
  species name must be binomial, i.e. must contain both the genus name
  and specific epithet.

- verbose:

  Logical, if `FALSE`, a message showing each step during the POWO
  search will not be printed in the console in full.

- save:

  Logical, if `TRUE`, the transformed input POWO database will be saved
  on disk.

- dir:

  Pathway to the computer's directory, where the file will be saved
  provided that the argument `save` is set up in `TRUE`. The default is
  to create a directory named **results_POWO_with_year_pubs** and the
  search results will be saved within a subfolder named after the
  current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **POWO_with_year_pubs**.

## Value

Objects of class dataframe and saves the output on disk.

## See also

[`megaGen`](https://dboslab.github.io/expowo/reference/megaGen.md)

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`powoFam`](https://dboslab.github.io/expowo/reference/powoFam.md)

[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)

[`accGraph`](https://dboslab.github.io/expowo/reference/accGraph.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{

library(expowo)

output <- powoSpecies(family = "Fabaceae",
                      genus = NULL,
                      synonyms = TRUE,
                      country = NULL,
                      save = FALSE,
                      dir = "Fabaceae_results_powoSpecies",
                      filename = "Fabaceae_spp")


output_year_pubs <- get_year_pubs(inputdf = output,
                                  verbose = TRUE,
                                  save = FALSE,
                                  dir = "results_POWO_with_year_pubs",
                                  filename = "POWO_with_year_pubs")
} # }
```
