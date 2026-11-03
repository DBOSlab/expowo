# Create graphics of species discoveries accumulation and nomenclatural changes

Produces graphics of the dynamics of species description and historical
nomenclature changes within any family or genus provided. It is designed
to create graphics based on the dataframe resulted from `powoSpecies`.
The graph type can be selected by two arguments (spp_acc and
spp_changes) and then multiple graphs for any different taxonomic groups
within the input data can be produced automatically in a single run.

## Usage

``` r
accGraph(inputdf = NULL,
         verbose = TRUE,
         spp_acc = TRUE,
         spp_changes = TRUE,
         spp_changes_col = NULL,
         genus_plots = TRUE,
         save = FALSE,
         dir = "results_accGraph",
         filename = "cumulative_discovery_",
         format = "pdf")
```

## Arguments

- inputdf:

  A dataframe generated using `powoSpecies` function using the argument
  synonyms set as `TRUE` and containing the genus and species column and
  the associated information about the protologue of each species. The
  species name must be binomial, i.e. must contain both the genus name
  and specific epithet.

- verbose:

  Logical, if `FALSE`, a message showing each step during the POWO
  search will not be printed in the console in full.

- spp_acc:

  Logical. Setting to `FALSE` means that you do not want to create
  individual accumulation graphs for each genus present in your dataset.
  The default is `TRUE`.

- spp_changes:

  Logical. Setting to `FALSE` means that you do not want to create a
  violin plot with all data provided. The default is `TRUE`.

- spp_changes_col:

  Define the name of a column in the main input data for which the
  species changes will be considered. For example, if densities and
  jitters should be presented for each genus in the same graphic, then
  set `spp_changes_col = "genus"`. If you are plotting the dynamics of
  nomenclatural changes across the entire dataset in a single density
  graphic, then you must add a new column in your input data containing
  the same character inside all rows. In this case, ensure that you also
  keep `genus_plots = TRUE`.

- genus_plots:

  Logical. If `FALSE`, a single density and jitter plot graphic will be
  produced for the entire input data. The default is `TRUE`, which will
  result in a graphic where multiple densities and jitters will be
  created inside the graphic, depending on the chosen column.

- save:

  Logical, if `TRUE`, the search results will be saved on disk.

- dir:

  Pathway to the computer's directory, where the file will be saved
  provided that the argument `save` is set up in `TRUE`. The default is
  to create a directory named **results_accGraph** and the search
  results will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **cumulative_discovery\_**.

- format:

  A character vector related to the file format of the graph to be
  saved. The default is "pdf" to save the output in Portable Document
  Format (.pdf), but you can also choose "jpg" to save in Joint
  Photographic Experts Group (.jpg), "tiff" to save in Tag Image File
  Format (.tiff) or "png" to save in Portable Network Graphics (.png).

## Value

Objects of class c("gg", "ggplot") and saves the output on disk.

## See also

[`megaGen`](https://dboslab.github.io/expowo/reference/megaGen.md)

[`topGen`](https://dboslab.github.io/expowo/reference/topGen.md)

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`powoFam`](https://dboslab.github.io/expowo/reference/powoFam.md)

[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)

[`powoMap`](https://dboslab.github.io/expowo/reference/powoMap.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{

library(expowo)

accGraph(inputdf = "output",
         verbose = TRUE,
         spp_acc = TRUE,
         spp_changes = TRUE,
         spp_changes_col = "genus",
         genus_plots = TRUE,
         save = FALSE,
         dir = "results_accGraph",
         filename = "cumulative_discovery_Cyperaceae",
         format = "pdf")
} # }
```
