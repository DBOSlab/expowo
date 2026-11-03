# Plot where species were introduced from and to

Produces a heatmap with the number of species native to each continent
(rows) that are introduced in each other continent (columns), following
the [World Geographical Scheme](https://www.tdwg.org/standards/wgsrpd/)
for Recording Plant Distributions. A species is counted once for each
pair of continents. Regions (level 2 of the scheme) can be used instead
of continents.

## Usage

``` r
introGraph(inputdf = NULL,
           level = "continent",
           verbose = TRUE,
           save = FALSE,
           dpi = 600,
           dir = "results_introGraph",
           filename = "intro_graph",
           format = "jpg")
```

## Arguments

- inputdf:

  A data frame generated with `powoSpecies`, or any data frame with the
  columns `native_to_botanical_countries` and
  `introduced_to_botanical_countries`. When the input data has synonyms
  (`status` column), only accepted species are counted.

- level:

  Either "continent" (level 1 of the scheme, the default) or "region"
  (level 2).

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
  a directory named **results_introGraph** and the graphic will be saved
  within a subfolder named after the current date.

- filename:

  Name of the output file. The default is **intro_graph**.

- format:

  The file format of the graphic: "jpg" (default), "pdf", "png" or
  "tiff".

## Value

A `ggplot` object, whose data has the number of species (`species`)
native to each continent or region (`from`) that are introduced in
another one (`to`).

## See also

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`endemicMap`](https://dboslab.github.io/expowo/reference/endemicMap.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

fab <- powoSpecies("Fabaceae", verbose = FALSE)

introGraph(fab,
           save = TRUE,
           dir = "results_introGraph",
           filename = "Fabaceae_introductions")
} # }
```
