# Extract list of species for any plant genus and family

Produces a CSV file listing all accepted species and associated
geographical distribution from any target genus or family of vascular
plants at Plants of the World Online (POWO).

## Usage

``` r
powoSpecies(family,
            genus = NULL,
            hybrid = FALSE,
            synonyms = FALSE,
            country = NULL,
            verbose = TRUE,
            source = "wcvp",
            rerun = FALSE,
            save = FALSE,
            dir = "results_powoSpecies",
            filename = "output")
```

## Arguments

- family:

  Either one family name or a vector of multiple families that is
  present in POWO.

- genus:

  Either one genus name or a vector of multiple genera that is present
  in POWO. If any genus name is not provided, then the function will
  search any species from all accepted genera known for the target
  family.

- hybrid:

  Logical, if `TRUE`, the search results will include hybrid species.

- synonyms:

  Logical, if `TRUE`, the search results will include synonyms.

- country:

  Either one country name or a vector of multiple countries. If country
  names are provided, then the function will return only the species
  that are native to such countries, according to POWO.

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

- rerun:

  Logical, if `TRUE`, a previously stopped search will continue from
  where it left off, starting with the last retrieved taxon. Please
  ensure that the 'filename' argument exactly matches the name of the
  CSV file saved from the previous search, and that the previously saved
  CSV file is located within a subfolder named after the current date.
  If it is not, please rename the date subfolder accordingly."

- save:

  Logical, if `TRUE`, the search results will be saved on disk.

- dir:

  Pathway to the computer's directory, where the file will be saved
  provided that the argument `save` is set up in `TRUE`. The default is
  to create a directory named **results_powoSpecies** and the search
  results will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **output**.

## Value

A table with the following fields: family, genus, species, taxon_name,
authors, scientific_name, publication (information about the protologue
of the species), native_to_country (original distribution according to
political country), native_to_botanical_countries (original distribution
according to botanical country), introduced_to_country (exotic
distribution according to political country),
introduced_to_botanical_countries (exotic distribution according to
botanical country), kew_id (each species code within Kew's databases),
powo_uri (the URI to access each species in POWO).

## See also

[`megaGen`](https://dboslab.github.io/expowo/reference/megaGen.md)

[`topGen`](https://dboslab.github.io/expowo/reference/topGen.md)

[`powoFam`](https://dboslab.github.io/expowo/reference/powoFam.md)

[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)

[`POWOcodes`](https://dboslab.github.io/expowo/reference/POWOcodes.md)

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

powoSpecies(family = "Martyniaceae",
            synonyms = TRUE,
            country = c("Argentina", "Brazil", "French Guiana"),
            save = FALSE,
            dir = "Martyniaceae_results_powoSpecies",
            filename = "Martyniaceae_spp")
} # }
```
