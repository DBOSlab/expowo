
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expowo

<!-- badges: start -->
<!-- badges: end -->

The main goal of the expowo package is to retrieve information on
diversity and distribution for any plant family as publicly available at
[Plants of the World Online (POWO)](https://powo.science.kew.org)

## Installation

You can install the development version of expowo from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("deborazuanny/expowo")
```

## Example

See below examples on how to use expowo functions to get basic
information on plant diversity and distribution, as well as full
checklist of species for any genera of a particular family.

``` r
library(expowo)
## basic example code
```

Note that in your computer the individual DNA alignments could be loaded
into a single list by adjusting the abovementioned code as follows:

``` r
library(expowo)

powocodes <- taxize::get_pow(c("Fabaceae", "Lecythidaceae"))
powocodes <- data.frame(powocodes)
powocodes <- cbind(family = c("Fabaceae", "Lecythidaceae"), powocodes)

resGenera <- powoGenera(powocodes$family, powocodes$uri,
                         verbose = TRUE)

write.csv(resGenera, "powo_genera_accepted_number_spp.csv", row.names=FALSE)
```

#### *1. When the DNA alignments have just a single sequence per species*

The species/sequences may be simply named as **Genus_species**, just
like the example below. The generic name could even be abbreviated but
always keeping separated from the specific epithet like **G_species**.
Note that the sequences may also be labeled by including “cf” or “aff”
between the genus and specific epithet, but they must be separated by an
underscore.

![Example with no identifiers in the
sequences](vignettes/labelling_no_identifiers.png)

## Documentation

A detailed description of the *expowo*’s full functionality will soon be
available in
[here](https://github.com/deborazuanny/expowo/tree/main/vignettes/).

## Citation

Zuanny, D. & Cardoso, D. (2022). expowo: An R package for mining plant
diversity and distribution data.
<https://github.com/deborazuanny/expowo>
