# Mining the top richest genera of vascular plants

Here in this article, we show how to use the package’s function `topGen`
for mining the top most species rich genera for any family of vascular
plants.\

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

## Mining the top richest genera for any vascular plant family

The function `topGen` is relatively similar to the `megaGen`, but
instead of using a specific threshold of species number to be considered
a big genera, it saves a CSV file listing the top most species-rich
genera of any target vascular plant family, and their associated number
of accepted species based on an integer number set in the argument
limit. In the example below, we used the default to search for the top
ten richest genera (i.e., when the argument limit is set as NULL, the
function searches for the 10 most diverse genera within a plant family).
Some columns were omitted to display the results, and the rows show the
top ten of the three chosen angiosperm families: Bignoniaceae,
Solanaceae and Lecythidaceae. Note that the table does not have 30 rows,
what would be expected if we multiply each family (n = 3) by ten. It is
not an error, since some plant families do not have many genera, which
is the case of Begoniaceae with 2 genera.\

\
`ABL_top`` ``<-`` `[`topGen`](https://dboslab.github.io/expowo/reference/topGen.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Aristolochiaceae"``, ``"Begoniaceae"``, ``"Lecythidaceae"``)``,`\
`                  limit ``=`` ``NULL``,`\
`                  verbose ``=`` ``TRUE``,`\
`                  save ``=`` ``FALSE``,`\
`                  dir ``=`` ``"results_topGen"``,`\
`                  filename ``=`` ``"Aristo_Bego_Lecythidaceae_search"``)`

\

|      family      |     genus     | species_number |
|:----------------:|:-------------:|:--------------:|
| Aristolochiaceae | Aristolochia  |      534       |
| Aristolochiaceae |    Asarum     |      129       |
| Aristolochiaceae |    Thottea    |       45       |
| Aristolochiaceae |    Hydnora    |       7        |
| Aristolochiaceae |  Prosopanche  |       7        |
| Aristolochiaceae |   Euglypha    |       1        |
| Aristolochiaceae |   Lactoris    |       1        |
| Aristolochiaceae |    Saruma     |       1        |
|   Begoniaceae    |    Begonia    |      1992      |
|   Begoniaceae    | Hillebrandia  |       1        |
|  Lecythidaceae   |  Eschweilera  |       99       |
|  Lecythidaceae   | Barringtonia  |       72       |
|  Lecythidaceae   |   Gustavia    |       46       |
|  Lecythidaceae   |   Lecythis    |       30       |
|  Lecythidaceae   |   Couratari   |       19       |
|  Lecythidaceae   |   Foetidia    |       18       |
|  Lecythidaceae   |  Napoleonaea  |       17       |
|  Lecythidaceae   | Rhaptopetalum |       13       |
|  Lecythidaceae   |     Grias     |       12       |
|  Lecythidaceae   |   Cariniana   |       9        |

TABLE 1. A general `topGen` search to mining the top most species rich
genera for some specific angiosperm families. {.table}

\

## Mining the top richest genera accross all vascular plant families

To mine a global checklist of the top species-richest genera for all
families of vascular plants, including their associated species number,
we recommend to load the dataframe-formatted data object called
`POWOcodes` that comes associated with the **expowo** package. The
`POWOcodes` data object already contains the URI addresses for all plant
families recognized in the Plants of the World Online database, so you
just need to call it to your R environment.\

The example below shows how to mine all top most species-rich genera of
vascular plants by using the vector of all plant families, the
associated URI addresses stored in the `POWOcodes` object and the limit
of 10 genera for each family.\

\
[`data`](https://rdrr.io/r/utils/data.html)`(``POWOcodes``)`\
\
`ALL_top`` ``<-`` `[`topGen`](https://dboslab.github.io/expowo/reference/topGen.md)`(``POWOcodes``$``family``,`\
`                  limit ``=`` ``10``,`\
`                  verbose ``=`` ``TRUE``,`\
`                  save ``=`` ``FALSE``,`\
`                  dir ``=`` ``"results_topGen"``,`\
`                  filename ``=`` ``"all_toprichest_plant_genera"``)`

\

## Reference

POWO (2019). “Plants of the World Online. Facilitated by the Royal
Botanic Gardens, Kew. Published on the Internet. Retrieved April 2023.”
