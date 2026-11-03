# Mining genus list for any vascular plant family

Here in this article, we show how to use the package’s function
`powoGenera` for mining all accepted genera for any family of vascular
plants available at POWO.\

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

## Mining all accepted genera for some plant families

The function `powoGenera` returns a data frame or saves a CSV file
listing all genera with associated number of accepted species and their
global geographic distribution at country or botanical level. The global
classification of botanical divisions follows the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions, which is already associated with each taxon’s
distribution in POWO. Here, we extracted the data for Aristolochiaceae,
Begoniaceae, Martyniaceae.\

\
`ABM`` ``<-`` `[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Aristolochiaceae"``, ``"Begoniaceae"``, ``"Martyniaceae"``)``,`\
`                  hybrid ``=`` ``FALSE``,`\
`                  verbose ``=`` ``FALSE``,`\
`                  save ``=`` ``FALSE``,`\
`                  dir ``=`` ``"results_powoGenera"``,`\
`                  filename ``=`` ``"Arist_Begon_Martyniaceae_search"``)`

| family | genus | authors | scientific_name | publication | species_number |
|:---|:---|:---|:---|:---|---:|
| Aristolochiaceae | Aristolochia | L. | Aristolochia L. | Sp. Pl.: 960 (1753) | 534 |
| Aristolochiaceae | Asarum | L. | Asarum L. | Sp. Pl.: 442 (1753) | 129 |
| Aristolochiaceae | Euglypha | Chodat & Hassl. | Euglypha Chodat & Hassl. | Bull. Herb. Boissier, ser. 2, 6: 138 (1906) | 1 |
| Aristolochiaceae | Hydnora | Thunb. | Hydnora Thunb. | Kongl. Vetensk. Acad. Handl. 36: 69 (1775) | 7 |
| Aristolochiaceae | Lactoris | Phil. | Lactoris Phil. | Verh. K. K. Zool.-Bot. Ges. Wien 15: 521 (1865) | 1 |
| Aristolochiaceae | Prosopanche | de Bary | Prosopanche de Bary | Abh. Naturf. Ges. Halle 10: 267 (1868) | 7 |
| Aristolochiaceae | Saruma | Oliv. | Saruma Oliv. | Hooker's Icon. Pl. 19: t. 1895 (1889) | 1 |
| Aristolochiaceae | Thottea | Rottb. | Thottea Rottb. | Nye Saml. Kongel. Danske Vidensk. Selsk. Skr. 2: 529 (1783) | 45 |
| Begoniaceae | Begonia | L. | Begonia L. | Sp. Pl.: 1056 (1753) | 1992 |
| Begoniaceae | Hillebrandia | Oliv. | Hillebrandia Oliv. | Trans. Linn. Soc. London 25: 361 (1866) | 1 |
| Martyniaceae | Craniolaria | L. | Craniolaria L. | Sp. Pl.: 618 (1753) | 2 |
| Martyniaceae | Holoregmia | Nees | Holoregmia Nees | Flora 4: 300 (1821) | 1 |
| Martyniaceae | Ibicella | (Stapf) Van Eselt. | Ibicella (Stapf) Van Eselt. | Techn. Bull. New York Agric. Exp. Sta., Geneva 149: 31 (1929) | 2 |
| Martyniaceae | Martynia | L. | Martynia L. | Sp. Pl.: 618 (1753) | 2 |
| Martyniaceae | Proboscidea | Schmidel | Proboscidea Schmidel | Icon. Pl. Anal., ed. Keller: 49 (1763) | 7 |

TABLE 1. A general `powoGenera` search for mining all accepted genera
for some specific families. {.table}

\

## Mining all accepted genera for all vascular plant families

To mine a global genus checklist with associated species number and
distribution for all families of vascular plants, we recommend to load
the dataframe-formatted data object called `POWOcodes` that comes
associated with the **expowo** package. Because the `POWOcodes` data
object already contains the URI addresses for all angiosperms,
gimnosperms and pteridophytes families recognized in the POWO database,
you just need to run the code below.\

The example below shows how to mine a global checklist of all accepted
genera of plants by using the vector of all families and associated URI
addresses stored in the `POWOcodes` object.\

\
[`data`](https://rdrr.io/r/utils/data.html)`(``POWOcodes``)`\
\
`ALL_gen`` ``<-`` `[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)`(``POWOcodes``$``family``,`\
`                      hybrid ``=`` ``TRUE``,`\
`                      verbose ``=`` ``TRUE``,`\
`                      save ``=`` ``FALSE``,`\
`                      dir ``=`` ``"results_powoGenera"``,`\
`                      filename ``=`` ``"all_plant_genera"``)`

\

Since this is a very long search that might requires days to be
completed, it may happen to loose internet connection at some point of
the queried search. But you do not need to start an entirely new search
from the beginning. By setting the `powoGenera` with `rerun = TRUE`, a
previously stopped search will continue from where it left off, starting
with the last retrieved taxon. Please ensure that the ‘filename’
argument exactly matches the name of the CSV file saved from the
previous search, and that the previously saved CSV file is located
within a subfolder named after the current date. If it is not, please
rename the date subfolder accordingly.

\

\
[`data`](https://rdrr.io/r/utils/data.html)`(``POWOcodes``)`\
\
`ALL_gen`` ``<-`` `[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)`(``POWOcodes``$``family``,`\
`                      hybrid ``=`` ``TRUE``,`\
`                      verbose ``=`` ``TRUE``,`\
`                      rerun ``=`` ``TRUE``,`\
`                      save ``=`` ``FALSE``,`\
`                      dir ``=`` ``"results_powoGenera"``,`\
`                      filename ``=`` ``"all_plant_genera"``)`

\

## Narrowing down the `powoGenera` search based on a specified country vector

You can also narrow down the search for all accepted genera of any
family so as to focus on just a particular country or a list of
countries. You just need to define a vector of country names in the
argument `country`. In the example below, see that we have originally
searched for the genera within the families Aristolochiaceae,
Lecythidaceae, Fagaceae, and Dipterocarpaceae, but the function only
returned a list of genera of Aristolochiaceae and Lecythidaceae, because
the remaining families do not have any genera recorded in the list of
countries of interest, i.e. Argentina, Brazil or French Guiana.\

\
`ADFL`` ``<-`` `[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Aristolochiaceae"``, ``"Dipterocarpaceae"``, `\
`                              ``"Fagaceae"``, ``"Lecythidaceae"``)``,`\
`                   hybrid ``=`` ``FALSE``,`\
`                   country ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Argentina"``, ``"Brazil"``, ``"French Guiana"``)``,`\
`                   verbose ``=`` ``FALSE``,`\
`                   save ``=`` ``FALSE``,`\
`                   dir ``=`` ``"results_powoGenera"``,`\
`                   filename ``=`` ``"country_constrained_search"``)`\
`  `

| family | genus | authors | scientific_name | publication | species_number |
|:---|:---|:---|:---|:---|---:|
| Aristolochiaceae | Aristolochia | L. | Aristolochia L. | Sp. Pl.: 960 (1753) | 534 |
| Aristolochiaceae | Euglypha | Chodat & Hassl. | Euglypha Chodat & Hassl. | Bull. Herb. Boissier, ser. 2, 6: 138 (1906) | 1 |
| Aristolochiaceae | Prosopanche | de Bary | Prosopanche de Bary | Abh. Naturf. Ges. Halle 10: 267 (1868) | 7 |
| Lecythidaceae | Allantoma | Miers | Allantoma Miers | Trans. Linn. Soc. London 30: 291 (1874) | 8 |
| Lecythidaceae | Asteranthos | Desf. | Asteranthos Desf. | Mem. Mus. Hist. Nat. 6: 9 (1820) | 1 |
| Lecythidaceae | Bertholletia | Bonpl. | Bertholletia Bonpl. | F.W.H.A.von Humboldt & A.J.A.Bonpland, Pl. Aequinoct. 1: 122 (1807) | 1 |
| Lecythidaceae | Cariniana | Casar. | Cariniana Casar. | Nov. Stirp. Bras. 4: 35 (1842) | 9 |
| Lecythidaceae | Corythophora | R.Knuth | Corythophora R.Knuth | H.G.A.Engler (ed.), Pflanzenr., IV, 219a: 50 (1939) | 4 |
| Lecythidaceae | Couratari | Aubl. | Couratari Aubl. | Hist. Pl. Guiane 2: 723 (1775) | 19 |
| Lecythidaceae | Couroupita | Aubl. | Couroupita Aubl. | Hist. Pl. Guiane 2: 708 (1775) | 3 |
| Lecythidaceae | Eschweilera | Mart. ex DC. | Eschweilera Mart. ex DC. | Prodr. 3: 293 (1828) | 99 |
| Lecythidaceae | Gustavia | L. | Gustavia L. | Pl. Surin. 12: 17 (1775), nom. cons. | 46 |
| Lecythidaceae | Lecythis | Loefl. | Lecythis Loefl. | Iter Hispan.: 189 (1758) | 30 |

TABLE 2. A `powoGenera` search based on a specified country vector.
{.table}

\

## Narrowing down the `powoGenera` search based on a specified genus vector

You may want to retrieve information for just one or a list of accepted
genera from a given country (or from a list of countries). Just like
before, you only need to define a vector of genus names in the argument
`genus` and a vector of country names in the argument `country`. In the
example below, see that we have again searched for just the genera
***Asarum*** and ***Bertholletia*** of the families Aristolochiaceae and
Lecythidaceae, but the function only returned the Lecythidaceae genus
***Bertholletia***, because ***Asarum*** does not occur in any of the
provided list of countries, i.e. Brazil or French Guiana.\

\
`AL`` ``<-`` `[`powoGenera`](https://dboslab.github.io/expowo/reference/powoGenera.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Aristolochiaceae"``, ``"Lecythidaceae"``)``, `\
`                 genus ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Asarum"``, ``"Bertholletia"``)``,`\
`                 hybrid ``=`` ``FALSE``,`\
`                 country ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Brazil"``, ``"French Guiana"``)``,`\
`                 verbose ``=`` ``TRUE``,`\
`                 save ``=`` ``FALSE``,`\
`                 dir ``=`` ``"results_powoGenera"``,`\
`                 filename ``=`` ``"genus_country_constrained_search"``)`

| family | genus | authors | scientific_name | publication | species_number | native_to_country |
|:---|:---|:---|:---|:---|---:|:---|
| Lecythidaceae | Bertholletia | Bonpl. | Bertholletia Bonpl. | F.W.H.A.von Humboldt & A.J.A.Bonpland, Pl. Aequinoct. 1: 122 (1807) | 1 | Bolivia, Brazil, Colombia, French Guiana, Guyana, Suriname, Venezuela |

TABLE 3. A `powoGenera` search based on specified genus and country
vectors. {.table}

\

## Reference

POWO (2019). “Plants of the World Online. Facilitated by the Royal
Botanic Gardens, Kew. Published on the Internet. Retrieved April 2023.”
