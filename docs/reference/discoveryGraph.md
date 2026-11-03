# Plot species discovery and synonymy over time

Produces a graphic with two panels: the number of species described in
each period of time, and the number of synonyms published in each
period. A species is counted in the year of its oldest homotypic name
(its basionym), i.e. the year it was discovered, not the year of its
current name. While `accGraph` shows the cumulative discovery of
species, this graphic shows when taxonomic work, i.e. new species and
renaming, was most intense.

## Usage

``` r
discoveryGraph(inputdf = NULL,
               bin = 10,
               verbose = TRUE,
               save = FALSE,
               dpi = 600,
               dir = "results_discoveryGraph",
               filename = "discovery_graph",
               format = "jpg")
```

## Arguments

- inputdf:

  A data frame generated with `powoSpecies` using `synonyms = TRUE`.

- bin:

  Number of years in each bar. The default is 10 (decades).

- verbose:

  Logical, if `FALSE`, a message showing each step will not be printed
  in the console.

- save:

  Logical, if `TRUE`, the graphic will be saved on disk.

- dpi:

  One number in the range of 72-4000 referring to the image resolution
  in dots per inch. The default is 600.

- dir:

  Pathway to the computer's directory, where the graphic will be saved
  provided that the argument `save` is `TRUE`. The default is to create
  a directory named **results_discoveryGraph** and the graphic will be
  saved within a subfolder named after the current date.

- filename:

  Name of the output file. The default is **discovery_graph**.

- format:

  The file format of the graphic: "jpg" (default), "pdf", "png" or
  "tiff".

## Value

A `ggplot` object, whose data has the number of names (`names`) in each
period (`period`) for each panel (`what`).

## See also

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`accGraph`](https://dboslab.github.io/expowo/reference/accGraph.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

fab <- powoSpecies("Fabaceae", synonyms = TRUE, verbose = FALSE)

discoveryGraph(fab,
               save = TRUE,
               dir = "results_discoveryGraph",
               filename = "Fabaceae_discovery")
} # }
```
