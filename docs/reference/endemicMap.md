# Create a global map of endemic species

Produces a global map of the number of species endemic to each botanical
country, i.e. the species native to a single botanical country (level 3)
of the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions. While `powoMap` maps where species occur, `endemicMap`
maps where species are restricted to.

## Usage

``` r
endemicMap(inputdf = NULL,
           distcol = "native_to_botanical_countries",
           breaks = c(10, 50, 100, 250, 500),
           leg_title = "Endemic species",
           verbose = TRUE,
           save = FALSE,
           dpi = 600,
           dir = "results_endemicMap",
           filename = "endemic_map",
           format = "jpg")
```

## Arguments

- inputdf:

  A data frame generated with `powoSpecies`, or any data frame with one
  species per row and a column with its native distribution in botanical
  countries separated by commas. When the input data has synonyms
  (`status` column), only accepted species are counted.

- distcol:

  Column name with the native distribution in botanical countries. The
  default is the column `native_to_botanical_countries` returned by
  `powoSpecies`.

- breaks:

  A numeric vector with the upper limits of the classes of endemic
  species shown in the map. Botanical countries without endemics and
  those with more endemics than the last limit are shown in their own
  classes. The default creates the classes 0, 1-10, 11-50, 51-100,
  101-250, 251-500 and

- leg_title:

  A character with the legend title.

- verbose:

  Logical, if `FALSE`, a message showing each step will not be printed
  in the console.

- save:

  Logical, if `TRUE`, the map will be saved on disk.

- dpi:

  One number in the range of 72-4000 referring to the image resolution
  in dots per inch. The default is 600.

- dir:

  Pathway to the computer's directory, where the map will be saved
  provided that the argument `save` is `TRUE`. The default is to create
  a directory named **results_endemicMap** and the map will be saved
  within a subfolder named after the current date.

- filename:

  Name of the output file. The default is **endemic_map**.

- format:

  The file format of the map: "jpg" (default), "pdf", "png" or "tiff".

## Value

A `ggplot` object with the map, whose data has the number of endemic
species of each botanical country in the column `endemics`. The TDWG map
of botanical countries is downloaded only once and kept at
`tools::R_user_dir("expowo", "cache")`.

## See also

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`powoMap`](https://dboslab.github.io/expowo/reference/powoMap.md)

[`introGraph`](https://dboslab.github.io/expowo/reference/introGraph.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

fab <- powoSpecies("Fabaceae", verbose = FALSE)

endemicMap(fab,
           save = TRUE,
           dir = "results_endemicMap",
           filename = "Fabaceae_endemics")
} # }
```
