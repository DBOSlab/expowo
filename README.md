
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expowo

An R package for mining plant checklist, diversity estimates, and
distribution data from RGB Kew’s [Plants of the World Online
(POWO)](https://powo.science.kew.org).

## Overview

The main goal of the **expowo** package is to retrieve information about
the diversity and distribution of any plant family as publicly available
at the taxonomically verified database [Plants of the World Online
(POWO)](https://powo.science.kew.org). The package is intended to
efficiently miner the content on the source html pages for any specific
genus and family. It can return a comma-separated values (CSV) file with
the number of accepted species and country-level distribution for any
genus as well as the full checklist of accepted species in any genus or
family, their authorship, original publication and global distribution.

## Before using **expowo**

#### Update R and RStudio versions

Please make sure you have installed the latest versions of both R
(<a href= "https://cran.r-project.org/bin/macosx/" target="_blank">Mac
OS</a>,
<a href= "https://cran.r-project.org/bin/windows/base/" target="_blank">Windows</a>)
and RStudio
(<a href= "https://www.rstudio.com/products/rstudio/download/" 
target="_blank">Mac OS / Windows</a>: choose the free version).

## Installation

You can install the latest development version of **expowo** from
[GitHub](https://github.com/) using the
[devtools](https://github.com/r-lib/devtools) package with the following
R code:

``` r
install.packages("devtools")
devtools::install_github("deborazuanny/expowo")
```

``` r
library(expowo)
```

OBS.: To download the development version, you will need to have the
[Git](http://git-scm.com/) software installed. And if your operating
system is Microsoft Windows, you will also need to download the
[Rtools](http://cran.r-project.org/bin/windows/Rtools/).

Otherwise, you will be able to install **expowo** more easily when it is
available on CRAN, by just running the following R code:

``` r
install.packages("expowo")
```

``` r
library(expowo)
```

## Usage

The package’s four major functions (`powoGenera`, `powoSpecies`,
`megaGen`, and `toptenGen`) require only the name of the target family
(or a vector with multiple family names) and the associated specific URI
(Uniform Resource Identifier) that identifies the html page for each
family in POWO. These four major functions work with other three
auxiliary functions (`getGenURI`, `getNumb`, and `getDist`) to mine the
plant data. Respectively, `getGenURI` mines the URI for each genus,
`getNumb` mines the total number of species within any genus, and
`getDist` does a complete search for native and introduced country-level
distribution any genus and species. To get the POWO URI for any accepted
plant family, you can either look at the data frame object `POWOcodes`
that come together with the installed **expowo** package, or you can use
the function `get_pow` from the package
[**taxize**](https://github.com/ropensci/taxize). So, the vector of URI
codes is the main input file to everything you can do with expowo
package. See below examples on how to use the **expowo**’s major
functions for mining basic information on the global plant diversity and
distribution.

#### *1. `powoGenera`: Mining all accepted genera for any angiosperm family*

This function produces a CSV file listing all genera with associated
number of accepted species and their global geographical distribution at
country level. You can also narrow down the search to focus on just a
particular genus from a particular country or a list of genera from a
list of countries (please check this
[article](https://github.com/domingoscardoso/expowo) for further
details)

##### Example of a POWO search with `powoGenera`:

``` r
library(expowo)
library(taxize)

fam <- c("Fabaceae", "Lecythidaceae")
powocodes <- cbind(family = fam,
                   data.frame(taxize::get_pow(fam)))

powoGenera(powocodes$family,
           powocodes$uri,
           verbose = TRUE,
           save = TRUE,
           dir = "results_powoGenera/",
           filename = "Fabaceae_Lecythidaceae")
```

#### *2. `powoSpecies`: Mining accepted species for any angiosperm genus or family*

With this function, you will be able to produce a CSV file of each genus
with the accepted number of species, considering hybrid species or not,
according to the data available in the POWO’s database. You can also
narrow down the search to focus on just the species from a particular
country or a list of countries, as the following example.

##### Example of a POWO search with `powoSpecies`:

``` r
library(expowo)
library(taxize)

fam <- c("Araceae", "Lecythidaceae")
powocodes <- cbind(family = fam,
                   data.frame(taxize::get_pow(fam)))
                   
powoSpecies(powocodes$family, powocodes$uri,
            hybridspp = FALSE,
            verbose = TRUE,
            save = TRUE,
            dir = "results_powoSpecies/",
            filename = "Araceae_Lecythidaceae")
```

#### *3. `megaGen`: Mining megadiverse genera for any angiosperm family*

This function is intended to produce a CSV file with the most diverse
genera of any flowering plant family, based on a specified threshold
number. In the following example, we choose the plant families
“Fabaceae” and “Lecythidaceae”, and considered a threshold of 500
species for a megadiverse genus, as set in the argument .

##### Example of a POWO search with `megaGen`:

``` r
library(expowo)
library(taxize)

fam <- c("Fabaceae", "Lecythidaceae")
powocodes <- cbind(family = fam,
                   data.frame(taxize::get_pow(fam)))

megaGen(powocodes$family, powocodes$uri,
        thld = 500,
        verbose = TRUE,
        save = TRUE,
        dir = "results_megaGen/",
        filename = "Fabaceae_Lecythidaceae")
```

#### *4. `toptenGen`: Mining the top ten most species-rich genera for any angiosperm family*

This function is relatively similar to the `megaGen`, but instead of
using any specific threshold, it saves a CSV file listing the top ten
most diverse genera of any target angiosperm family.

##### Example of a POWO search with `toptenGen`:

``` r
library(expowo)
library(taxize)

fam <- c("Araceae", "Lecythidaceae")
powocodes <- cbind(family = fam,
                   data.frame(taxize::get_pow(fam)))

toptenGen(powocodes$family, powocodes$uri,
          verbose = TRUE,
          save = TRUE,
          dir = "results_toptenGen/",
          filename = "Araceae_Lecythidaceae")
```

## Documentation

A detailed description of the **expowo**’s full functionality will soon
be available in
[here](https://github.com/deborazuanny/expowo/tree/main/vignettes/).

The **expowo** package is being continuously constructed. If you want to
make suggestions, let us know! We hope it will be helpful for your
botanical research!

## Citation

Zuanny, D. & Cardoso, D. (2022). expowo: An R package for mining plant
diversity and distribution data.
<https://github.com/deborazuanny/expowo>

<img src="vignettes/DBOSlab_logo.png" style="width:30.0%" />
