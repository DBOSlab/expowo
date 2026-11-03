# Associated data

Here in this article, we describe the data associated with `expowo`
package that are helpful to enhance performance before and while mining
the global diversity and distribution data.\

There are three associated data package, all of which can be accessed
and imported into the R environment by simply calling the `utils`
function `data`. Below, we explain how to use the data package
`POWOcodes`, `angioData`, and `botregions`.\

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

## `POWOcodes` - A complete list of vascular plant families and associated URI addresses

The `POWOcodes` data is a dataframe containing a list of families of
vascular plants available in the POWO database. The flowering plant
families are according to [APG IV](https://doi.org/10.1111/boj.12385)’s
classification. It was produced by using the functions `apgFamilies` and
`get_pow` of the package
[taxize](https://docs.ropensci.org/taxize/index.html). `POWOcodes` data
is very useful when the purpose is to query many plant families or all
of them. In addition to the names of POWO’s accepted plant families,
which are required in any search with `expowo`’s functions, this data
package has the corresponding family’s URI, which is used internally in
the function for opening a connection with each family page at Plants of
The World Online. In addition, plant families are classified into three
groups: “angiosperm”, “gymnosperm”, and “fern and lycophyte”.

To call this associated data to the R environment, use the code below:\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``"POWOcodes"``)`

\

## `angioData` - A dataframe of Angiosperm species

The `angioData` is a dataframe-formatted list of species of some
families of flowering plants and associated data, as retrieved from POWO
database using the `expowo`’s function `powoSpecies`. For publication
purposes and package size limitation, we selected some families to
include in this associated data and let the information easily
available. Currently, there are species from five plant families as
listed in Table 1. The last update of this data was at Nov 2022.\
\

TABLE 1. The plant families and species number within `angioData`.\

|    family     | species_number |      family      | species_number |
|:-------------:|:--------------:|:----------------:|:--------------:|
| Lecythidaceae |      381       | Aristolochiaceae |      727       |
| Martyniaceae  |       14       |   Cabombaceae    |       7        |
|  Begoniaceae  |      1993      |                  |                |

\

In addition to all accepted species (excluding hybrid species) in the
families above, the `angioData` also contains the publication,
authorship, and global geographic distribution at country or botanical
level for each species. The global classification of botanical divisions
follows the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions, which is already associated with each taxon’s
distribution in POWO.\

The example below shows how to call `angioData`:\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``"angioData"``)`

\

## `angioGenera` - A data frame of Angiosperm genera

The `angioGenera` comprises the genera of some families of flowering
plants and associated data as mined from POWO database using
`powoGenera`. For publication purposes and package size limitation, we
selected some families to include in this associated data and let the
information easily available. Currently, there are six plant families
(Lecythidaceae, Aristolochiaceae, Begoniaceae, Martyniaceae,
Dipterocarpaceae, and Fagaceae) as listed in Table 2. The last update of
this data was at Nov 2022.\

\

TABLE 2. The plant families and genera within `angioGenera`.\

| family | scientific_name | publication | species_number |
|:---|:---|:---|---:|
| Aristolochiaceae | Aristolochia L. | Sp. Pl.: 960 (1753) | 534 |
| Aristolochiaceae | Asarum L. | Sp. Pl.: 442 (1753) | 129 |
| Aristolochiaceae | Euglypha Chodat & Hassl. | Bull. Herb. Boissier, ser. 2, 6: 138 (1906) | 1 |
| Aristolochiaceae | Hydnora Thunb. | Kongl. Vetensk. Acad. Handl. 36: 69 (1775) | 7 |
| Aristolochiaceae | Lactoris Phil. | Verh. K. K. Zool.-Bot. Ges. Wien 15: 521 (1865) | 1 |
| Aristolochiaceae | Prosopanche de Bary | Abh. Naturf. Ges. Halle 10: 267 (1868) | 7 |
| Aristolochiaceae | Saruma Oliv. | Hooker's Icon. Pl. 19: t. 1895 (1889) | 1 |
| Aristolochiaceae | Thottea Rottb. | Nye Saml. Kongel. Danske Vidensk. Selsk. Skr. 2: 529 (1783) | 45 |
| Begoniaceae | Begonia L. | Sp. Pl.: 1056 (1753) | 1992 |
| Begoniaceae | Hillebrandia Oliv. | Trans. Linn. Soc. London 25: 361 (1866) | 1 |
| Martyniaceae | Craniolaria L. | Sp. Pl.: 618 (1753) | 2 |
| Martyniaceae | Holoregmia Nees | Flora 4: 300 (1821) | 1 |
| Martyniaceae | Ibicella (Stapf) Van Eselt. | Techn. Bull. New York Agric. Exp. Sta., Geneva 149: 31 (1929) | 2 |
| Martyniaceae | Martynia L. | Sp. Pl.: 618 (1753) | 2 |
| Martyniaceae | Proboscidea Schmidel | Icon. Pl. Anal., ed. Keller: 49 (1763) | 7 |
| Dipterocarpaceae | Anisoptera Korth. | Verh. Nat. Gesch. Ned. Bezitt., Bot.: 65 (1841) | 10 |
| Dipterocarpaceae | Cotylelobium Pierre | Fl. Forest. Cochinch.: t. 235 (1889) | 5 |
| Dipterocarpaceae | Dipterocarpus C.F.Gaertn. | Suppl. Carp. 3: 50 (1805) | 65 |
| Dipterocarpaceae | Dryobalanops C.F.Gaertn. | Suppl. Carp. 3: 49 (1805) | 7 |
| Dipterocarpaceae | Hopea Roxb. | Pl. Coromandel 3: 7 (1811), nom. cons. | 114 |
| Dipterocarpaceae | Marquesia Gilg | Bot. Jahrb. Syst. 40: 485 (1908) | 3 |
| Dipterocarpaceae | Monotes A.DC. | A.P.de Candolle, Prodr. 16(2): 623 (1868) | 23 |
| Dipterocarpaceae | Neobalanocarpus P.S.Ashton | Fl. Males. 9: 388 (1982) | 1 |
| Dipterocarpaceae | Parashorea Kurz | J. Asiat. Soc. Bengal, Pt. 2, Nat. Hist. 39: 65 (1870) | 13 |
| Dipterocarpaceae | Pseudomonotes A.C.Londoño, E.Alvarez & Forero | Brittonia 47: 230 (1995) | 1 |
| Dipterocarpaceae | Shorea Roxb. ex C.F.Gaertn. | Suppl. Carp.: 47 (1805) | 189 |
| Dipterocarpaceae | Stemonoporus Thwaites | Hooker's J. Bot. Kew Gard. Misc. 6: 67 (1854) | 26 |
| Dipterocarpaceae | Upuna Symington | Bull. Jard. Bot. Buitenzorg, ser. 3, 17: 88 (1941) | 1 |
| Dipterocarpaceae | Vateria L. | Sp. Pl.: 515 (1753) | 3 |
| Dipterocarpaceae | Vateriopsis F.Heim | Rech. Dipterocarp.: 94 (1892) | 1 |
| Dipterocarpaceae | Vatica L. | Mant. Pl. 2: 152 (1771) | 77 |
| Fagaceae | Castanea Mill. | Gard. Dict. Abr. ed. 4.: s.p. (1754) | 8 |
| Fagaceae | Castanopsis (D.Don) Spach | Hist. Nat. Veg. 11: 185 (1842), nom. cons. | 143 |
| Fagaceae | Chrysolepis Hjelmq. | Bot. Not. Suppl. 2, 1: 117 (1948) | 2 |
| Fagaceae | Fagus L. | Sp. Pl.: 997 (1753) | 12 |
| Fagaceae | Lithocarpus Blume | Bijdr. Fl. Ned. Ind.: 526 (1826) | 343 |
| Fagaceae | Notholithocarpus Manos, Cannon & S.H.Oh | Madrono 55: 188 (2008) | 1 |
| Fagaceae | Quercus L. | Sp. Pl.: 994 (1753) | 464 |
| Fagaceae | Trigonobalanus Forman | Taxon 11: 140 (1962) | 3 |
| Lecythidaceae | Allantoma Miers | Trans. Linn. Soc. London 30: 291 (1874) | 8 |
| Lecythidaceae | Asteranthos Desf. | Mem. Mus. Hist. Nat. 6: 9 (1820) | 1 |
| Lecythidaceae | Barringtonia J.R.Forst. & G.Forst. | Char. Gen. Pl.: 75 (1776), nom. cons. | 72 |
| Lecythidaceae | Bertholletia Bonpl. | F.W.H.A.von Humboldt & A.J.A.Bonpland, Pl. Aequinoct. 1: 122 (1807) | 1 |
| Lecythidaceae | Brazzeia Baill. | Bull. Mens. Soc. Linn. Paris 1: 609 (1886) | 3 |
| Lecythidaceae | Careya Roxb. | Fl. Ind. ed. 1832. 2: 636 (1832) | 3 |
| Lecythidaceae | Cariniana Casar. | Nov. Stirp. Bras. 4: 35 (1842) | 9 |
| Lecythidaceae | Chydenanthus Miers | Trans. Linn. Soc. London, Bot. 1: 111 (1875) | 1 |
| Lecythidaceae | Corythophora R.Knuth | H.G.A.Engler (ed.), Pflanzenr., IV, 219a: 50 (1939) | 4 |
| Lecythidaceae | Couratari Aubl. | Hist. Pl. Guiane 2: 723 (1775) | 19 |
| Lecythidaceae | Couroupita Aubl. | Hist. Pl. Guiane 2: 708 (1775) | 3 |
| Lecythidaceae | Crateranthus Baker f. | A.B.Rendle & al., Cat. Pl. Oban: 35 (1913) | 4 |
| Lecythidaceae | Eschweilera Mart. ex DC. | Prodr. 3: 293 (1828) | 99 |
| Lecythidaceae | Foetidia Comm. ex Lam. | Encycl. 2: 457 (1788) | 18 |
| Lecythidaceae | Grias L. | Syst. Nat. ed. 10. 2: 1075 (1759) | 12 |
| Lecythidaceae | Gustavia L. | Pl. Surin. 12: 17 (1775), nom. cons. | 46 |
| Lecythidaceae | Lecythis Loefl. | Iter Hispan.: 189 (1758) | 30 |
| Lecythidaceae | Napoleonaea P.Beauv. | Napoleone: 1 (1804) | 17 |
| Lecythidaceae | Oubanguia Baill. | Bull. Mens. Soc. Linn. Paris 2: 869 (1890) | 3 |
| Lecythidaceae | Petersianthus Merr. | Philipp. J. Sci., C 11: 200 (1916) | 2 |
| Lecythidaceae | Pierrina Engl. | Bot. Jahrb. Syst. 43: 374 (1909) | 1 |
| Lecythidaceae | Planchonia Blume | Fl. Serres Jard. Eur. 7: 24 (1851) | 8 |
| Lecythidaceae | Rhaptopetalum Oliv. | J. Linn. Soc., Bot. 8: 159 (1865) | 13 |
| Lecythidaceae | Scytopetalum Pierre ex Engl. | H.G.A.Engler & K.A.E.Prantl, Nat. Pflanzenfam., Nachtr. 1: 244 (1897) | 4 |

\

In addition to list all accepted genera in the above families, the
dataframe-formatted `angioGenera` also contains publication, authorship,
species number and global geographic distribution at country or
botanical level for each genus. The global classification of botanical
divisions follows the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions, which is already associated with each taxon’s
distribution in POWO.\

The example below shows how to call `angioGenera`:\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``"angioGenera"``)`

\

## `botregions` - Countries and associated classification of botanical divisions

The `botregions` data package is a dataframe containing all countries of
the World and associated classification of botanical divisions according
to the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions. It was built using a custim script with specific
functions from the packages `rgdal`, `ggplot2`, `broom`, `sf` and
`raster` (original script can be made available upon request). This
dataset is useful when the purpose is to query the distribution data by
both divisions of the world and for the `expowo` ’s functions convert
the country names.\

To call this associated data to your R environment, use the code below:\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``"botregions"``)`

\

## Reference

POWO (2019). “Plants of the World Online. Facilitated by the Royal
Botanic Gardens, Kew. Published on the Internet. Retrieved Nov 2022.”
