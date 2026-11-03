# Mining distribution for any plant species

Here in this article, we show how to use the package’s function
`powoSpDist` for mining the entire distribution for any species of
vascular plants. Despite yielding similar outputs, `powoSpDist` and
`powoSpecies` serve distinct purposes. `powoSpecies` enables the
retrieval of all species from a designated list of families and/or
genera. In contrast, `powoSpDist` is designed to focus specifically on a
designated list of species, streamlining the process of retrieving
species distribution information. While both functions ultimately
produce the same output, their utility depends on the user’s initial
interest. If one’s interest is centered on a particular species,
`powoSpDist` offers a more efficient approach, eliminating the need to
run a code that mines data for an entire family or genus and
subsequently filter the desired species—a process that could be more
time-consuming. This targeted approach allows for a more streamlined and
resource-efficient search. The `powoSpDist` uses the argument `species`
to set the name or list of species names, and then the function access
the auxiliary functions `.getgenURI` and `getInfo` to extract their
respective distribution.\

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

## Mining the distribution for any plant species

The function `powoSpDist` returns a dataframe or saves a CSV file
listing the entire global distribution at country or botanical level for
any of the given species scientific name(s) (excluding hybrid species),
their genus, publication and authorship. The global classification of
botanical divisions follows the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions, which is already associated with each taxon’s
distribution at POWO.\

The example below shows how to mine the distribution for a specified
vector of two species. The output shown here (a table) is simplified by
removing the family and distribution according to botanical subdivision
columns.\

\
`BL_dist`` ``<-`` `[`powoSpDist`](https://dboslab.github.io/expowo/reference/powoSpDist.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Begoniaceae"``, ``"Lecythidaceae"``)``,`\
`                      species ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Hillebrandia sandwicensis"``, ``"Bertholletia excelsa"``)``,`\
`                      verbose ``=`` ``TRUE``,`\
`                      save ``=`` ``FALSE``,`\
`                      dir ``=`` ``"results_powoSpDist"``,`\
`                      filename ``=`` ``"Begoniaceae_Lecythidaceae"``)`

\

| family | taxon_name | authors | publication | native_to_country | introduced_to_country | kew_id |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| Lecythidaceae | Bertholletia excelsa | Bonpl. | F.W.H.A.von Humboldt & A.J.A.Bonpland, Pl. Aequinoct. 1: 122 (1808) | Bolivia, Brazil, Colombia, French Guiana, Guyana, Suriname, Venezuela | Caroline Is. | 591270-1 |
| Begoniaceae | Hillebrandia sandwicensis | Oliv. | Trans. Linn. Soc. London 25: 361 (1866) | United States | unknown | 106195-1 |

TABLE 1. A general `powoSpDist` search for mining distribution of two
angiosperm species. {.table style="width:100%;"}

\

## Mining distribution for all vascular plant species

To mine the distribution checklist for all accepted species of vascular
plants, you can load the dataframe-formatted data object called
`POWOcodes` that comes associated with the **expowo** package. The
`POWOcodes` data object already contains the URI addresses for all plant
families recognized in the Plants of the World Online database, you just
need to call it to your R environment.\

The example below shows how to mine the global distribution of all
accepted non hybrid species of vascular plants by using the vector of
all plant families and associated URI addresses stored in the
`POWOcodes` object. Note that the argument `species` should be as
`NULL`, so that the search is not constrained to any particular species,
i.e. the function will return global distribution for all species in all
queried families.\

\
[`data`](https://rdrr.io/r/utils/data.html)`(``POWOcodes``)`\
\
`ALL_dist`` ``<-`` `[`powoSpDist`](https://dboslab.github.io/expowo/reference/powoSpDist.md)`(``POWOcodes``$``family``,`\
`                       species ``=`` ``NULL``,`\
`                       verbose ``=`` ``TRUE``,`\
`                       save ``=`` ``FALSE``,`\
`                       dir ``=`` ``"results_powoSpDist"``,`\
`                       filename ``=`` ``"all_plant_distribution"``)`

\

## Reference

POWO (2019). “Plants of the World Online. Facilitated by the Royal
Botanic Gardens, Kew. Published on the Internet. Retrieved April 2023.”
