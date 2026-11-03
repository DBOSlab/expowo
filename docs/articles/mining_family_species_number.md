# Mining species number of any plant family

Here in this article, we show how to use the package’s function
`powoFam` for mining the species number for any family of angiosperms,
gymnosperms and pteridophytes. By providing a family name or a vector of
family names, the `powoFam` function visits each POWO page of the genera
within the queried family(ies) to extract the accepted species number.
Then, it sums these values to deliver the total species number within
each plant family.\

\

## Setup

Install the latest development version of **expowo** from
[GitHub](https://github.com/):

\
`#install.packages("devtools")`\
`devtools``::`[`install_github`](https://rdrr.io/pkg/devtools/man/install-deprecated.html)`(``"DBOSlab/expowo"``)`

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`expowo`](https://dboslab.github.io/expowo/)`)`

\

### Mining the total species number for any plant family

The function `powoFam` returns a data frame or saves a CSV file listing
the total species number (excluding hybrid species) for the queried
plant families.\

The example below shows how to mine the total species number by defining
a vector of three families: **Cabombaceae**, **Lecythidaceae**, and
**Martyniaceae**. The resulting output (TABLE 1) is also saved directly
in the specified directory **results_powoFam**.\

\
`CLM`` ``<-`` `[`powoFam`](https://dboslab.github.io/expowo/reference/powoFam.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Cabombaceae"``, ``"Lecythidaceae"``, ``"Martyniaceae"``)``,`\
`               verbose ``=`` ``TRUE``,`\
`               save ``=`` ``FALSE``,`\
`               dir ``=`` ``"results_powoFam"``,`\
`               filename ``=`` ``"Camb_Lecy_Martyniaceae_diversity"``)`

\

|    family     | species_number |
|:-------------:|:--------------:|
|  Cabombaceae  |       7        |
| Lecythidaceae |      381       |
| Martyniaceae  |       14       |

TABLE 1. A general `powoFam` search for mining the total species number
of three angiosperm families. {.table}

\

### Mining species number accross all plant families

To mine the species number of all families of vascular plants, we
recommend to load the dataframe-formatted object called `POWOcodes` that
comes associated with the **expowo** package. The `POWOcodes` data
object already contains the URI addresses for all plant families
recognized in the Plants of the World Online database, so you just need
to call it to the R environment.\

The example below shows how to mine the species number of all vascular
plant families by using the vector of all plant families and associated
URI addresses stored in the `POWOcodes` object.\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``POWOcodes``)`\
\
`ALL_fam`` ``<-`` `[`powoFam`](https://dboslab.github.io/expowo/reference/powoFam.md)`(``POWOcodes``$``family``,`\
`                   verbose ``=`` ``TRUE``,`\
`                   save ``=`` ``FALSE``,`\
`                   dir ``=`` ``"results_powoFam"``,`\
`                   filename ``=`` ``"all_plants_species_number"``)`

\

### Reference

POWO (2019). “Plants of the World Online. Facilitated by the Royal
Botanic Gardens, Kew. Published on the Internet. Retrieved April 2023.”
