# Mining species list for any plant family

Here in this article, we show how to use the package’s function
`powoSpecies` for mining all accepted species for any genus or family of
vascular plants in the World Checklist of Vascular Plants (WCVP;
Govaerts et al., 2021) available through Kew’s Plants of The World
Online (POWO).\

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

### Mining all accepted species for any plant family

The function `powoSpecies` returns a data frame or saves a CSV file
listing all accepted species (including hybrid species or not), their
publication, authorship, and global geographic distribution at country
or botanical level for any family of vascular plants. The global
classification of botanical divisions follows the [World Geographical
Scheme](https://www.tdwg.org/standards/wgsrpd/) for Recording Plant
Distributions, which is already associated with each taxon’s
distribution in the POWO.\

The example below shows how to mine all accepted species for a specified
vector of families. The output shown here (TABLE 1) is a simplified
version, where we have removed some columns (genus, epithet and authors)
so as to focus on the display of the taxonomic and distribution data.\

\
`res`` ``<-`` `[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Cabombaceae"``, ``"Martyniaceae"``)``,`\
`                   hybrid ``=`` ``FALSE``,`\
`                   verbose ``=`` ``TRUE``,`\
`                   save ``=`` ``FALSE``,`\
`                   dir ``=`` ``"results_powoSpecies"``,`\
`                   filename ``=`` ``"Cabom_Martyniaceae_search"``)`

\

| family | scientific_name | publication | native_to_country | native_to_botanical_countries | kew_id |
|:---|:---|:---|:---|:---|:---|
| Martyniaceae | Craniolaria annua L. | Sp. Pl.: 618 (1753) | Colombia, Dominican Republic, Guyana, Venezuela, Venezuelan Antilles | Colombia, Dominican Republic, Guyana, Venezuela, Venezuelan Antilles | 675800-1 |
| Martyniaceae | Craniolaria integrifolia Cham. | Linnaea 7: 725 (1832) | Argentina, Bolivia, Brazil, Paraguay, Uruguay | Argentina Northeast, Argentina Northwest, Bolivia, Brazil Northeast, Brazil South, Brazil Southeast, Brazil West-Central, Paraguay, Uruguay | 675805-1 |
| Martyniaceae | Holoregmia viscida Nees | Flora 4: 300 (1821) | Brazil | Brazil Northeast | 327210-2 |
| Martyniaceae | Ibicella lutea (Lindl.) Van Eselt. | Techn. Bull. New York Agric. Exp. Sta., Geneva 149: 34 (1929) | Argentina, Brazil, Paraguay, Uruguay | Argentina Northeast, Argentina Northwest, Brazil Northeast, Brazil South, Brazil Southeast, Paraguay, Uruguay | 565368-1 |
| Martyniaceae | Ibicella parodii Abbiatti | Notas Mus. La Plata, Bot. 4: 458 (1939) | Argentina, Bolivia | Argentina Northeast, Argentina Northwest, Bolivia | 127285-2 |
| Martyniaceae | Martynia annua L. | Sp. Pl.: 618 (1753) | Bahamas, Belize, Costa Rica, Cuba, Dominican Republic, El Salvador, Guatemala, Haiti, Honduras, Leeward Is., Mexico, Nicaragua, Puerto Rico, Trinidad-Tobago, Windward Is. | Bahamas, Belize, Costa Rica, Cuba, Dominican Republic, El Salvador, Guatemala, Haiti, Honduras, Leeward Is., Mexico Central, Mexico Gulf, Mexico Northeast, Mexico Northwest, Mexico Southeast, Mexico Southwest, Nicaragua, Puerto Rico, Trinidad-Tobago, Windward Is. | 675840-1 |
| Martyniaceae | Martynia palmeri S.Watson | Proc. Amer. Acad. Arts 24: 66 (1889) | Mexico | Mexico Northwest | 153752-2 |
| Martyniaceae | Proboscidea althaeifolia (Benth.) Decne. | Ann. Sci. Nat., Bot., ser. 5, 3: 324 (1865) | Mexico, Peru, United States | Arizona, California, Mexico Northeast, Mexico Northwest, New Mexico, Peru, Texas | 675893-1 |
| Martyniaceae | Proboscidea diversifolia Hevly | Brittonia 21: 311 (1970) | Mexico | Mexico Southwest | 283779-2 |
| Martyniaceae | Proboscidea louisianica (Mill.) Thell. | Mem. Soc. Sci. Nat. Math. Cherbourg 38: 480 (1912) | Mexico, United States | California, Colorado, Florida, Georgia, Kansas, Kentucky, Louisiana, Mexico Central, Mexico Gulf, Mexico Northeast, Mexico Northwest, Mexico Southwest, Mississippi, Missouri, Nebraska, New Jersey, New Mexico, North Carolina, Oregon, Pennsylvania, South Carolina, South Dakota, Texas, Vermont, Virginia, Washington, Wyoming | 210335-2 |
| Martyniaceae | Proboscidea parviflora (Wooton) Wooton & Standl. | Contr. U.S. Natl. Herb. 19: 602 (1915) | Mexico, United States | Arizona, California, Mexico Central, Mexico Northeast, Mexico Northwest, Nevada, New Mexico, Texas, Utah | 210338-2 |
| Martyniaceae | Proboscidea sabulosa Correll | Rhodora 68: 426 (1966) | United States | New Mexico, Texas | 210340-2 |
| Martyniaceae | Proboscidea spicata Correll | Wrightia 4: 77 (1968) | Mexico | Mexico Northeast | 210342-2 |
| Martyniaceae | Proboscidea triloba (Schltdl. & Cham.) Decne. | Ann. Sci. Nat., Bot., ser. 5, 3: 326 (1865) | Guatemala, Mexico | Guatemala, Mexico Central, Mexico Gulf, Mexico Northeast, Mexico Southeast, Mexico Southwest | 675906-1 |
| Cabombaceae | Brasenia schreberi J.F.Gmel. | Syst. Nat. ed. 13\[bis\].: 853 (1791) | Angola, Australia, Bangladesh, Belize, Botswana, Burundi, Cameroon, Canada, China, Cuba, District of Columbia, Dominican Republic, El Salvador, Ethiopia, Guatemala, Guyana, India, Jamaica, Japan, Kenya, Mexico, Nigeria, North Korea, South Korea, Quebec, Rhode I., Russia, South Africa, Taiwan, Tanzania, Uganda, United States, Venezuela, Zaire, Zambia, Zimbabwe | Alabama, Alaska, Alberta, Amur, Angola, Arkansas, Assam, Bangladesh, Belize, Botswana, British Columbia, Burundi, California, Cameroon, China South-Central, China Southeast, Connecticut, Cuba, Delaware, District of Columbia, Dominican Republic, El Salvador, Ethiopia, Florida, Georgia, Guatemala, Guyana, Idaho, Illinois, India, Indiana, Iowa, Jamaica, Japan, Kansas, Kentucky, Kenya, Korea, Louisiana, Maine, Manitoba, Maryland, Massachusetts, Mexico Gulf, Mexico Northeast, Mexico Southeast, Mexico Southwest, Michigan, Minnesota, Mississippi, Missouri, Montana, New Brunswick, New Hampshire, New Jersey, New South Wales, New York, Newfoundland, Nigeria, North Carolina, Northern Provinces, Nova Scotia, Ohio, Oklahoma, Ontario, Oregon, Pennsylvania, Primorye, Queensland, Quebec, Rhode I., South Carolina, Taiwan, Tanzania, Tennessee, Texas, Uganda, Venezuela, Vermont, Victoria, Virginia, Washington, West Virginia, Wisconsin, Zambia, Zaire, Zimbabwe | 605270-1 |
| Cabombaceae | Cabomba aquatica Aubl. | Hist. Pl. Guiane 1: 321 (1775) | Bolivia, Brazil, Colombia, French Guiana, Guyana, Peru, Suriname, Venezuela | Bolivia, Brazil North, Brazil Northeast, Brazil Southeast, Brazil West-Central, Colombia, French Guiana, Guyana, Peru, Suriname, Venezuela | 605272-1 |
| Cabombaceae | Cabomba caroliniana A.Gray | Ann. Lyceum Nat. Hist. New York 4: 47 (1837) | Argentina, Bolivia, Brazil, District of Columbia, Mexico, Paraguay, Rhode I., United States, Uruguay | Alabama, Argentina Northeast, Argentina South, Arkansas, Bolivia, Brazil South, Brazil Southeast, Connecticut, Delaware, District of Columbia, Florida, Georgia, Illinois, Indiana, Kentucky, Louisiana, Maryland, Massachusetts, Mexico Northeast, Michigan, Mississippi, Missouri, New Hampshire, New Jersey, New York, North Carolina, Ohio, Oklahoma, Paraguay, Pennsylvania, Rhode I., South Carolina, Tennessee, Texas, Uruguay, Virginia | 273581-2 |
| Cabombaceae | Cabomba furcata Schult. & Schult.f. | Syst. Veg. 7: 1379 (1830) | Bolivia, Brazil, Colombia, Costa Rica, Cuba, Ecuador, Guyana, Honduras, Panama, Peru, Suriname, Trinidad-Tobago, Venezuela | Bolivia, Brazil North, Brazil Northeast, Brazil Southeast, Brazil West-Central, Colombia, Costa Rica, Cuba, Ecuador, Guyana, Honduras, Panama, Peru, Suriname, Trinidad-Tobago, Venezuela | 273584-2 |
| Cabombaceae | Cabomba haynesii Wiersema | Ann. Missouri Bot. Gard. 76: 1167 (1989) | Brazil, Colombia, Costa Rica, Cuba, Dominican Republic, Ecuador, El Salvador, Honduras, Jamaica, Nicaragua, Panama, Puerto Rico, Trinidad-Tobago, Venezuela | Brazil West-Central, Colombia, Costa Rica, Cuba, Dominican Republic, Ecuador, El Salvador, Honduras, Jamaica, Nicaragua, Panama, Puerto Rico, Trinidad-Tobago, Venezuela | 273585-2 |
| Cabombaceae | Cabomba palaeformis Fassett | Castanea 18: 127 (1953) | Belize, Costa Rica, Guatemala, Mexico, Nicaragua | Belize, Costa Rica, Guatemala, Mexico Central, Mexico Gulf, Mexico Northeast, Mexico Southeast, Mexico Southwest, Nicaragua | 39039-2 |
| Cabombaceae | Cabomba schwartzii Rataj | Acta Amazonica 7: 143 (1977) | Brazil | Brazil North | 128749-1 |

TABLE 1. A general `powoSpecies` search for mining all accepted species
for some specific plant families. {.table}

\

### Narrowing down the `powoSpecies` search based on a specified country vector

You can also narrow down the species checklist search of any family by
focusing on just a particular country or a list of countries. You just
need to define a vector of country names in the argument . In the
example below, note that we have originally searched for the species
within the families **Aristolochiaceae**, **Cabombaceae**, and
**Martyniaceae**, but the function returned a smaller species list,
because many species in the searched families are not recorded in any of
the specified vector of country, i.e. **Brazil** or **Colombia**.\

\
`ACM`` ``<-`` `[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Aristolochiaceae"``, ``"Cabombaceae"``, ``"Martyniaceae"``)``,`\
`                   hybrid ``=`` ``FALSE``,`\
`                   country ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Brazil"``, ``"Colombia"``)``,`\
`                   verbose ``=`` ``FALSE``,`\
`                   save ``=`` ``FALSE``,`\
`                   dir ``=`` ``"results_powoSpecies"``,`\
`                   filename ``=`` ``"country_constrained_search"``)`

\

| family | scientific_name | publication | native_to_country | native_to_botanical_countries |
|:---|:---|:---|:---|:---|
| Aristolochiaceae | Aristolochia arcuata Mast. | C.F.P.von Martius & auct. suc. (eds.), Fl. Bras. 4(2): 101 (1875) | Brazil | Brazil South, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia assisii J.Freitas, Lirio & F.Gonzalez | Phytotaxa 163: 263 (2014) | Brazil | Brazil Northeast, Brazil Southeast |
| Aristolochiaceae | Aristolochia bahiensis F.Gonzalez | Brittonia 50: 8 (1998) | Brazil | Brazil Northeast, Brazil Southeast |
| Aristolochiaceae | Aristolochia birostris Duch. | Ann. Sci. Nat., Bot., ser. 4, 2: 60 (1854) | Brazil | Brazil Northeast |
| Aristolochiaceae | Aristolochia brunneomaculata I.S.Abreu & Giul. | Sitientibus Ser. Ci. Biol. 16: 1 (2016) | Brazil | Brazil Northeast |
| Aristolochiaceae | Aristolochia burchellii Mast. | C.F.P.von Martius & auct. suc. (eds.), Fl. Bras. 4(2): 90 (1875) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia cabrerae Ahumada | Folia Bot. Geobot. Corrent. 2: 1 (1989) | Brazil | Brazil West-Central |
| Aristolochiaceae | Aristolochia chrysochlora Barb.Rodr. | Vellosia 2: 23A (1885-1888 publ.1891) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia colombiana F.Gonzalez | Revista Acad. Colomb. Ci. Exact. 90: 52 (2000) | Colombia | Colombia |
| Aristolochiaceae | Aristolochia colossifolia Hoehne | Arq. Bot. Estado Sao Paulo 2: 171 (1952) | Colombia | Colombia |
| Aristolochiaceae | Aristolochia cordigera (Klotzsch) Duch. | A.P.de Candolle, Prodr. 15(1): 455 (1864) | Brazil | Brazil North, Brazil Northeast, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia cymbifera Mart. | Nov. Gen. Sp. Pl. 1: 76 (1824) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia cynanchifolia Mart. | Nov. Gen. Sp. Pl. 1: 77 (1824) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia echinata Barb.Rodr. | Pl. Jard. Rio de Janeiro 5: 9 (1896) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia eriantha Mart. | Nov. Gen. Sp. Pl. 1: 78 (1824) | Brazil | Brazil Northeast |
| Aristolochiaceae | Aristolochia filipendulina Duch. | Ann. Sci. Nat., Bot., ser. 4, 2: 70 (1854) | Brazil | Brazil West-Central |
| Aristolochiaceae | Aristolochia gardneri Duch. | Ann. Sci. Nat., Bot., ser. 4, 2: 69 (1854) | Brazil | Brazil North, Brazil Northeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia gehrtii Hoehne | Arq. Bot. Estado Sao Paulo, n.s., f.m., 1: 45 (1939) | Brazil | Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia gigantea Mart. & Zucc. | Flora 7(1, Beil.): 139 (1824) | Brazil | Brazil Northeast, Brazil South, Brazil Southeast |
| Aristolochiaceae | Aristolochia ginzbergeri Ahumada | Darwiniana 21: 78 (1977 publ. 1978) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia gracilipedunculata F.Gonzalez | Brittonia 63: 431 (2011) | Brazil | Brazil Northeast, Brazil Southeast |
| Aristolochiaceae | Aristolochia gracilis Duch. | A.P.de Candolle, Prodr. 15(1): 460 (1864) | Brazil | Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia hilariana Duch. | A.P.de Candolle, Prodr. 15(1): 451 (1864) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia hispida Pohl ex Duch. | A.P.de Candolle, Prodr. 15(1): 498 (1864) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia huberiana S.Moore | J. Bot. 53: 6 (1915) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia huebneriana O.C.Schmidt | Repert. Spec. Nov. Regni Veg. 40: 133 (1936) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia hypoglauca Kuhlm. | Arq. Inst. Biol. Veg. 3: 45 (1936) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia insolita J.Freitas & M.Peixoto | Syst. Bot. 42: 169 (2017) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia keratuma F.Gonzalez & Pabon-Mora | Caldasia 39(1): 52 (2017) | Colombia | Colombia |
| Aristolochiaceae | Aristolochia krukoffii O.C.Schmidt | Repert. Spec. Nov. Regni Veg. 45: 53 (1938) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia labiata Willd. | Mem. Soc. Imp. Naturalistes Moscou 2: 101 (1809) | Brazil | Brazil North, Brazil Northeast, Brazil South, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia lanceolatolorata S.Moore | J. Bot. 53: 4 (1915) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia limai Hoehne | Arq. Bot. Estado Sao Paulo 1: 91 (1942) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia longispathulata F.Gonzalez | Novon 10: 371 (2000) | Brazil | Brazil Northeast |
| Aristolochiaceae | Aristolochia lorenae J.Freitas & F.Gonzalez | Syst. Bot. 44: 101 (2019) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia lutescens Duch. | A.P.de Candolle, Prodr. 15(1): 461 (1864) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia manaosensis Ahumada | Darwiniana 21: 77 (1977 publ. 1978) | Brazil | Brazil North |
| Aristolochiaceae | Aristolochia marianensis Ahumada | Hickenia 1: 171 (1979) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia medicinalis R.E.Schult. | Bot. Mus. Leafl. 21: 266 (1967) | Colombia | Colombia |
| Aristolochiaceae | Aristolochia melastoma Silva Manso ex Duch. | A.P.de Candolle, Prodr. 15(1): 461 (1864) | Brazil | Brazil North, Brazil South, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia morae F.Gonzalez | Revista Acad. Colomb. Ci. Exact. 71: 500 (1993) | Colombia | Colombia |
| Aristolochiaceae | Aristolochia nevesarmondiana Hoehne | Arch. Bot. Sao Paulo 1: 7 (1925) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia odora Steud. | Nomencl. Bot., ed. 2, 1: 133 (1840) | Brazil | Brazil Northeast, Brazil Southeast |
| Aristolochiaceae | Aristolochia papillaris Mast. | C.F.P.von Martius & auct. suc. (eds.), Fl. Bras. 4(2): 100 (1875) | Brazil | Brazil North, Brazil Northeast, Brazil South, Brazil Southeast |
| Aristolochiaceae | Aristolochia paulistana Hoehne | Arch. Bot. Sao Paulo 1: 13 (1925) | Brazil | Brazil Northeast, Brazil South, Brazil Southeast |
| Aristolochiaceae | Aristolochia pohliana Duch. | A.P.de Candolle, Prodr. 15(1): 496 (1864) | Brazil | Brazil Northeast, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia pubescens Willd. ex Duch. | A.P.de Candolle, Prodr. 15(1): 450 (1864) | Brazil | Brazil Northeast |
| Aristolochiaceae | Aristolochia raja Mart. | Nov. Gen. Sp. Pl. 1: 78 (1824) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia robertii Ahumada | Fl. Ilustr. Catar. 1(Arist.): 35 (1975) | Brazil | Brazil South |
| Aristolochiaceae | Aristolochia rumicifolia Mart. | Nov. Gen. Sp. Pl. 1: 79 (1824) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia sepicola Mast. | C.F.P.von Martius & auct. suc. (eds.), Fl. Bras. 4(2): 100 (1875) | Brazil | Brazil Northeast |
| Aristolochiaceae | Aristolochia setosa Duch. | A.P.de Candolle, Prodr. 15(1): 496 (1864) | Brazil | Brazil Northeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia smilacina Duch. | A.P.de Candolle, Prodr. 15(1): 459 (1864) | Brazil | Brazil Northeast, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia subglobosa J.Freitas, Lirio & F.Gonzalez | Phytotaxa 124: 56 (2013) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia tamnifolia (Klotzsch) Duch. | A.P.de Candolle, Prodr. 15(1): 448 (1864) | Brazil | Brazil Northeast, Brazil Southeast |
| Aristolochiaceae | Aristolochia trulliformis Mast. | C.F.P.von Martius & auct. suc. (eds.), Fl. Bras. 4(2): 101 (1875) | Brazil | Brazil North, Brazil Northeast |
| Aristolochiaceae | Aristolochia urbaniana Taub. | Beibl. Bot. Jahrb. Syst. 27: 14 (1890) | Brazil | Brazil Southeast |
| Aristolochiaceae | Aristolochia warmingii Mast. | C.F.P.von Martius & auct. suc. (eds.), Fl. Bras. 4(2): 109 (1875) | Brazil | Brazil North, Brazil Northeast, Brazil Southeast, Brazil West-Central |
| Aristolochiaceae | Aristolochia wendeliana Hoehne | Arq. Bot. Estado Sao Paulo, n.s., f.m., 1: 45 (1939) | Brazil | Brazil South, Brazil Southeast |
| Aristolochiaceae | Aristolochia zebrina J.Freitas & F.Gonzalez | Nordic J. Bot. 34: 54 (2016) | Brazil | Brazil Southeast |
| Aristolochiaceae | Prosopanche caatingicola R.F.Machado & L.P.Queiroz | Phytotaxa 75: 59 (2012) | Brazil | Brazil Northeast |
| Aristolochiaceae | Prosopanche cocuccii Tav.de Carvalho, Zachia & Mariath | Phytotaxa 521: 178 (2021) | Brazil | Brazil South |
| Aristolochiaceae | Prosopanche demogorgoni Funez | Phytotaxa 422: 94 (2019) | Brazil | Brazil South |
| Martyniaceae | Holoregmia viscida Nees | Flora 4: 300 (1821) | Brazil | Brazil Northeast |
| Cabombaceae | Cabomba schwartzii Rataj | Acta Amazonica 7: 143 (1977) | Brazil | Brazil North |

TABLE 2. A `powoSpecies` search based on a specified country vector.
{.table}

\

### Narrowing down the `powoSpecies` search based on a specified genus and country vector

You may want to retrieve information for just one or a list of accepted
genera from a given country (or from a list of countries). Just like
before, you only need to define a vector of genus names in the argument
and a vector of country names in the argument . In the example below,
see that we have searched for just the genera ***Proboscidea*** and
***Cabomba*** of the families **Martyniaceae** and **Cabombaceae**, but
the function only returned the **Cabombaceae** genus ***Cabomba***,
because ***Proboscidea*** does not occur in any of the provided list of
countries, i.e. **Argentina**, **French Guiana**, or **Brazil**.\

\
`MC`` ``<-`` `[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)`(``family ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Martyniaceae"``, ``"Cabombaceae"``)``,`\
`                  genus ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Proboscidea"``, ``"Cabomba"``)``,`\
`                  hybrid ``=`` ``FALSE``,`\
`                  country ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Argentina"``, ``"French Guiana"``, ``"Brazil"``)``,`\
`                  verbose ``=`` ``TRUE``,`\
`                  save ``=`` ``FALSE``,`\
`                  dir ``=`` ``"results_powoSpecies"``,`\
`                  filename ``=`` ``"country_and_genus_constrained_search"``)`

\

| family | taxon_name | authors | scientific_name | publication | native_to_country | native_to_botanical_countries | kew_id |
|:---|:---|:---|:---|:---|:---|:---|:---|
| Cabombaceae | Cabomba schwartzii | Rataj | Cabomba schwartzii Rataj | Acta Amazonica 7: 143 (1977) | Brazil | Brazil North | 128749-1 |

TABLE 3. A `powoSpecies` search based on specified genus and country
vectors. {.table}

\

### Mining all accepted species for all plant families

To mine a species checklist for all families of vascular plants, you
recommend to load the dataframe-formatted data object called `POWOcodes`
that comes associated with the **expowo** package. The `POWOcodes` data
object already contains the URI addresses for all plant families
recognized in the Plants of the World Online database, so you just need
to call it to your R environment.\

The example below shows how to mine a global checklist of all accepted
species of vascular plants by using the vector of all plant families and
associated URI addresses stored in the `POWOcodes` object.\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``"POWOcodes"``)`\
\
`ALL_spp`` ``<-`` `[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)`(``POWOcodes``$``family``,`\
`                       hybrid ``=`` ``TRUE``,`\
`                       verbose ``=`` ``TRUE``,`\
`                       save ``=`` ``FALSE``,`\
`                       dir ``=`` ``"results_powoSpecies"``,`\
`                       filename ``=`` ``"all_plant_species"``)`

\

Since this is a very long search that might requires hours to be
completed, it may happen to loose internet connection at some point of
the queried search. But you do not need to start an entirely new search
from the beginning. By setting the `powoSpecies` with `rerun = TRUE`, a
previously stopped search will continue from where it left off, starting
with the last retrieved taxon. Please ensure that the ‘filename’
argument exactly matches the name of the CSV file saved from the
previous search, and that the previously saved CSV file is located
within a subfolder named after the current date. If it is not, please
rename the date subfolder accordingly.

\

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``"POWOcodes"``)`\
\
`ALL_spp`` ``<-`` `[`powoSpecies`](https://dboslab.github.io/expowo/reference/powoSpecies.md)`(``POWOcodes``$``family``,`\
`                       hybrid ``=`` ``TRUE``,`\
`                       verbose ``=`` ``TRUE``,`\
`                       rerun ``=`` ``TRUE``,`\
`                       save ``=`` ``FALSE``,`\
`                       dir ``=`` ``"results_powoSpecies"``,`\
`                       filename ``=`` ``"all_plant_species"``)`

\

### Reference

POWO (2019). “Plants of the World Online. Facilitated by the Royal
Botanic Gardens, Kew. Published on the Internet. Retrieved April 2023.”
