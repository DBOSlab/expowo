# Create global maps of species richness

Produces global-scale maps of species richness at political country and
botanical country levels. Despite being originally designed to create
maps for all input data of any specified taxonomic group (genus or
family) from the search results with `powoSpecies`, the function is also
useful for any dataframe-formatted input data that has at least a column
with species and one or two columns with associated distribution in the
countries and/or botanical regions. Multiple richness maps for any
different taxonomic groups within the input data can be produced
automatically in a single run by just specifying a column name with the
associated taxonomic classification.

## Usage

``` r
powoMap(inputdf = NULL,
        botctrs = FALSE,
        distcol = NULL,
        taxclas = NULL,
        verbose = TRUE,
        save = FALSE,
        vir_color = "viridis",
        bre_color = NULL,
        leg_title = "SR",
        dpi = 600,
        dir = "results_powoMap",
        filename = "global_richness_map",
        format = "jpg")
```

## Arguments

- inputdf:

  A dataframe with a species column and the associated global
  distribution at country or botanical country level. The species name
  must be as a binomial, i.e. must contain both the genus name and
  specific epithet, but the authorship is optional. Each species must be
  as a single row with its corresponding full distribution in all
  countries and/or botanical regions within a single cell of their
  respective columns, where the country names or botanical regions are
  separated by a comma. This is, for example, the standard dataframe
  from the search results with the function `powoSpecies`.

- botctrs:

  Logical. If `TRUE`, the species richness maps will be created
  according to the botanical country subdivisions of the world. Also, a
  WGSRPD folder including the level 3 shapefile and the associated
  Brummitt's (2001) book fully describing the World Geographical Scheme
  for Recording Plant Distributions will be downloaded into the working
  directory. If you do not remove this folder or rename any of the
  contents, then the function will not download the same folder again.
  The default is `FALSE`.

- distcol:

  Column name with the full global distribution data for each species at
  political country level or the level 3 of botanical subdivision of the
  [World Geographical Scheme](https://www.tdwg.org/standards/wgsrpd/)
  for Recording Plant Distributions. If the species distribution is
  given with botanical subdivisions, then you must also change the
  argument `botctrs` to `TRUE`. If the distribution is described only by
  political country names, then set `botctrs` to `FALSE`.

- taxclas:

  A character vector with the column name for the corresponding
  taxonomic classification of each species in any higher taxonomic
  level. If provided, the function will produce, in a single run, all
  global richness maps for every distinct group within the input data.
  The default is `NULL`, then the function will generate only one global
  species richness map for the entire input data.

- verbose:

  Logical. If `FALSE`, a message showing the map creation steps will not
  be printed in the console in full.

- save:

  Logical, if `TRUE`, the global maps will be saved on disk.

- vir_color:

  A character vector with the name or code of any of the color palettes
  from [Viridis](https://CRAN.R-project.org/package=viridis) package.

- bre_color:

  A character vector with the name or code of any of the color palettes
  from [RColorBrewer](https://CRAN.R-project.org/package=RColorBrewer)
  package.

- leg_title:

  A character vector to be displayed in the output map as a legend.
  Default is to create a title called **SR**, an acronym for species
  richness.

- dpi:

  One number in the range of 72-4000 referring to the image resolution
  in the format of dots per inch in the output file. Default is to
  create an output with 600 dpi.

- dir:

  Pathway to the computer's directory, where the map file will be saved
  provided that the argument `save` is set up in `TRUE`. The default is
  to create a directory named **results_powoMap** and the search results
  will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **global_richness_map**.

- format:

  A character vector related to the file format of the global map to be
  saved. The default is "jpg" to save the output in Joint Photographic
  Experts Group (.jpg), but you can also choose "pdf" to save in
  Portable Document Format (.pdf), "tiff" to save in Tag Image File
  Format (.tiff) or "png" to save in Portable Network Graphics (.png).

## Value

One or a list of objects of class c("gg", "ggplot").

## See also

[`megaGen`](https://dboslab.github.io/expowo/reference/megaGen.md)

[`topGen`](https://dboslab.github.io/expowo/reference/topGen.md)

[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)

[`powoFam`](https://dboslab.github.io/expowo/reference/powoFam.md)

[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

mapspdist <- powoSpecies(family = "Martyniaceae",
                         hybridspp = FALSE,
                         country = NULL,
                         verbose = TRUE,
                         save = FALSE,
                         dir = "results_powoSpecies",
                         filename = "Martyniaceae_spp")

# To create multiple maps for each genus within the input data according to
# political countries.
powoMap(inputdf = mapspdist,
        botctrs = FALSE,
        distcol = "native_to_country",
        taxclas = "genus",
        verbose = FALSE,
        save = FALSE,
        vir_color = "viridis",
        bre_color = NULL,
        leg_title = "SR",
        dpi = 600,
        dir = "results_powoMap",
        filename = "global_richness_country_map",
        format = "jpg")

# To create multiple maps for each genus within the input data according to
# botanical country subdivisions.
powoMap(inputdf = mapspdist,
        botctrs = TRUE,
        distcol = "native_to_botanical_countries",
        taxclas = "genus",
        verbose = FALSE,
        save = FALSE,
        vir_color = "viridis",
        bre_color = NULL,
        leg_title = "SR",
        dpi = 600,
        dir = "results_powoMap",
        filename = "global_richness_botcountry_map",
        format = "jpg")
} # }
```
