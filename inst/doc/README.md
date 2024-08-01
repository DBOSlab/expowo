  <!-- badges: start -->
  [![R-CMD-check](https://github.com/DBOSlab/expowo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DBOSlab/expowo/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/DBOSlab/expowo/branch/master/graph/badge.svg)](https://app.codecov.io/gh/DBOSlab/expowo?branch=master)
  <!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# expowo <img src="man/figures/expowo.png" align="right" alt="" width="120" />

An R package for mining species list, diversity estimates, and
distribution data for any genus or family of vascular plants from the 
World Checklist of Vascular Plants (WCVP) available through RBG
Kew’s [Plants of the World Online (POWO)](https://powo.science.kew.org).

## Overview

The main goal of the **expowo** package is to retrieve information about
the diversity and distribution of any plant family as publicly available
at the taxonomically verified database [Plants of the World Online
(POWO)](https://powo.science.kew.org). The package is intended to
efficiently mine the content inside the source HTML pages for any
specific genus and family. It can return a comma-separated values (CSV)
file with the number of accepted species and country-level distribution
for any genus as well as the full list of accepted species in any
genus or family, their authorship, original publication and global
distribution. The latests functions implemented, `powoMap` and `accGraph`, are available in the GitHub version and can automatically create global maps for any 
taxon level and accumulation graphics of species discovery, respectively.

## Before using **expowo**

#### Update R and RStudio versions

Please make sure you have installed the latest versions of both R
(<a href= "https://cran.r-project.org/bin/macosx/" target="_blank">Mac
OS</a>,
<a href= "https://cran.r-project.org/bin/windows/base/" target="_blank">Windows</a>)
and RStudio (<a href= "https://posit.co/download/rstudio-desktop/" 
target="_blank">Mac OS / Windows</a>: choose the free version).

## Installation


You can install **expowo** from [CRAN](https://CRAN.R-project.org/package=expowo) by just running the following R code:

``` r
install.packages("expowo")
```

``` r
library(expowo)
```

Otherwise, you can install the latest development version of **expowo** from
[GitHub](https://github.com/) using the
[devtools](https://github.com/r-lib/devtools) package with the following
R code:

``` coffee
install.packages("devtools")
devtools::install_github("DBOSlab/expowo")
```

``` r
library(expowo)
```

OBS.: To download the development version, you will need to have the
[Git](https://git-scm.com/) software installed. And if your operating
system is Microsoft Windows, you will also need to download the Rtools.



## Usage

The package’s major functions (`powoFam`, `powoGenera`, `powoSpecies`, 
`powoSpDist`, `megaGen`, and `topGen`) only require the name of the 
target family (or a vector with multiple family names). These major 
functions work with other minor functions (`getInfo` and `saveCSV`), with auxiliary and defensive functions to mine the 
plant data. Respectively, `getInfo` mines the taxonomic information of each genus, 
the total number of species within any genus, and does a complete search for native and introduced country-level distribution for any genus and species, while `saveCSV` fastly writes CSV files within the current date subfolder of a provided specific directory. With these extracted distributions, it is also possible to automatically generate global maps according to political country and botanical
countries with the function `powoMap`. Additionally, if you want to plot new species discoveries within any genus after downloading the data using `powoSpecies`, it is also possible to do it automatically using `accGraph`. See the examples on 
how to use the **expowo**’s functions for mining and using data regarding the global plant diversity and distribution in the ‘Articles’ section in our
[site](https://DBOSlab.github.io/expowo/).

<img src="man/figures/expowo_pkg.png" alt="" width="500" />

## Documentation

A detailed description of the **expowo**’s full functionality is
available [here](https://DBOSlab.github.io/expowo/).

The **expowo** package is being continuously constructed. If you want to
make suggestions, let us know! We hope it will be helpful for your
botanical research!

## Citation

Zuanny, D. C., B. Vilela, P. W. Moonlight, T. E. Särkinen, and D. Cardoso. 2024. 
expowo: An R package for mining global plant diversity and distribution data. 
Applications in Plant Sciences 12: e11609. <https://doi.org/10.1002/aps3.11609>

<img src="man/figures/DBOSlab_logo.png" align="left" alt="" width="120" />
