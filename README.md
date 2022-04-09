
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expowo

<!-- badges: start -->
<!-- badges: end -->

## Overview

The main goal of the expowo package is to retrieve information on
diversity and distribution for any plant family as publicly available at
[Plants of the World Online (POWO)](https://powo.science.kew.org). The
package’s basic functions starts with an object called `powocodes`, wich
can be created by using the package
[taxize](https://github.com/ropensci/taxize) or our associated data
`POWOcodes`. It includes the scientific name of all flowering plants
family and allows storing information beyond uri of each one of them.
This is the main input file to everything you can do with expowo
package. There are four main functions associated to the expowo package:
powoGenera, powoSpecies, megaGen and toptenGen. Continue reading to see
how to use them!

![How the first 15 lines of our associated data ‘POWOcodes’ should
appear in your RStudio](vignettes/POWOcodes.png)

## Installation

You can install the development version of expowo from
[GitHub](https://github.com/) using the
[devtools](https://github.com/r-lib/devtools) package with:

``` r
# install.packages("devtools")
devtools::install_github("deborazuanny/expowo")
```

## Usage

See below examples on how to use expowo functions to get basic
information on plant diversity and distribution, as well as full
checklist of species for any genera of a particular family.

#### *1. powoGenera*

Descricao da funcao e dos argumentos, como inserir cada um

##### Example of a running search with powoGenera function:

``` r
library(expowo)

powocodes <- taxize::get_pow(c("Fabaceae", "Lecythidaceae"))
powocodes <- data.frame(powocodes)
powocodes <- cbind(family = c("Fabaceae", "Lecythidaceae"), powocodes)

resGenera <- powoGenera(powocodes$family, powocodes$uri,
                         verbose = TRUE)
```

Note that you can choose between the names of individual plant families
or search for all of them just by adjusting the abovementioned code.

``` r
library(expowo)

powocodes <- taxize::get_pow(c("Fabaceae", "Lecythidaceae"))
powocodes <- data.frame(powocodes)
powocodes <- cbind(family = c("Fabaceae", "Lecythidaceae"), powocodes)

resGenera <- powoGenera(powocodes$family, powocodes$uri,
                         verbose = TRUE)
```

#### *2. powoSpecies*

With this function, you will be able to produce a dataframe of each
genera with the accepted number of species, considering hybrid species
or not, according to the data available at POWO’s database.

##### Example of a running search with powoSpecies function:

![Example with hybrid spp](vignettes/POWOcodes_b.png) ![Example with
accepted spp](vignettes/POWOcodes_c.png)

#### *3. megaGen*

In the megaGen function, our goal is to provide a data frame with the
most diverse genera of each family you set and with those genera above
the treshold that you previously defined. In the following example, we
choose the plant families “Fabaceae” and “Lecythidaceae” and considered
a megagenera those with more than 500 species, as set in the parameter
treshold.

##### Example of a running search with megaGen function:

``` r
powocodes <- taxize::get_pow(c("Fabaceae", "Lecythidaceae"))
powocodes <- data.frame(powocodes)
powocodes <- cbind(family = c("Fabaceae", "Lecythidaceae"), powocodes)

resMega <- megaGen(powocodes$family, powocodes$uri,
                    treshold = 500,
                    verbose = TRUE)
```

![Result from the example above, using the megaGen
function](vignettes/POWOcodes_d.png)

#### *4. toptenGen*

Descricao da funcao, argumentos e dica de como usar.

##### Example of a running search with toptenGen function:

``` r
powocodes <- taxize::get_pow(c("Araceae", "Lecythidaceae"))
#' powocodes <- data.frame(powocodes)
#' powocodes <- cbind(family = c("Araceae", "Lecythidaceae"), powocodes)
#'
#' resTopten <- toptenGen(powocodes$family, powocodes$uri,
#'                        verbose = TRUE)
```

![Result generated from the example above, using the toptenGen
function](vignettes/POWOcodes_e.png)

## Documentation

A detailed description of the *expowo*’s full functionality will soon be
available in
[here](https://github.com/deborazuanny/expowo/tree/main/vignettes/).

## Citation

Zuanny, D. & Cardoso, D. (2022). expowo: An R package for mining plant
diversity and distribution data.
<https://github.com/deborazuanny/expowo>
