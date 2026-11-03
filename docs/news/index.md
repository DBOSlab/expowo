# Changelog

## expowo 3.2

### Major changes

- The mining functions
  [`powoFam()`](https://dboslab.github.io/expowo/reference/powoFam.md),
  [`powoGenera()`](https://dboslab.github.io/expowo/reference/powoGenera.md),
  [`powoSpecies()`](https://dboslab.github.io/expowo/reference/powoSpecies.md),
  [`powoSpDist()`](https://dboslab.github.io/expowo/reference/powoSpDist.md),
  [`megaGen()`](https://dboslab.github.io/expowo/reference/megaGen.md)
  and [`topGen()`](https://dboslab.github.io/expowo/reference/topGen.md)
  gain a new argument `source`. The default `source = "wcvp"` reads the
  bulk data of the World Checklist of Vascular Plants (WCVP), the
  taxonomic backbone of POWO, instead of mining the source HTML page of
  each taxon at the POWO website. This was needed because POWO now
  blocks automated requests, which made every search fail with
  `HTTP status was '403 Forbidden'`.

- The WCVP file (about 90 MB) is downloaded from Kew only once and
  cached at `tools::R_user_dir("expowo", "cache")`. It is downloaded
  again only when Kew releases a new WCVP version, and the cached copy
  is used when offline. Any search after the first one runs in seconds,
  even for the largest families.

- The output tables keep exactly the same columns as in previous
  versions. The botanical countries keep the WGSRPD level-3 names used
  by POWO (e.g. “Zaire”, “Turkey”, “Yugoslavia”), so that
  [`powoMap()`](https://dboslab.github.io/expowo/reference/powoMap.md)
  and the `botregions` country lookup work as before.

- The previous mining of POWO source pages is still available with
  `source = "powo"`.

### New functions

- [`endemicMap()`](https://dboslab.github.io/expowo/reference/endemicMap.md)
  maps the number of species endemic to each botanical country (WGSRPD
  level 3), i.e. the species native to a single botanical country. While
  [`powoMap()`](https://dboslab.github.io/expowo/reference/powoMap.md)
  maps where species occur,
  [`endemicMap()`](https://dboslab.github.io/expowo/reference/endemicMap.md)
  maps where they are restricted to. The classes of the map are set with
  `breaks`.

- [`discoveryGraph()`](https://dboslab.github.io/expowo/reference/discoveryGraph.md)
  plots, in two panels, the number of species described and of synonyms
  published in each decade (or any period set with `bin`). A species is
  counted in the year of its basionym, i.e. the year it was discovered.
  It complements the cumulative curves of
  [`accGraph()`](https://dboslab.github.io/expowo/reference/accGraph.md),
  showing when taxonomic work was most intense.

- [`introGraph()`](https://dboslab.github.io/expowo/reference/introGraph.md)
  plots a heatmap with the number of species native to each continent
  (or WGSRPD region, with `level = "region"`) that are introduced in
  each other one.

- The three functions take the output of
  [`powoSpecies()`](https://dboslab.github.io/expowo/reference/powoSpecies.md),
  return a `ggplot` object and save the graphic with `save`, `dir`,
  `filename`, `format` and `dpi`, like
  [`powoMap()`](https://dboslab.github.io/expowo/reference/powoMap.md)
  and
  [`accGraph()`](https://dboslab.github.io/expowo/reference/accGraph.md).
  The TDWG map of botanical countries used by
  [`endemicMap()`](https://dboslab.github.io/expowo/reference/endemicMap.md)
  (about 4 MB) is downloaded only once and cached with the WCVP data.

- A small internal table of the 368 WGSRPD level-3 areas, with their
  names as returned by expowo and their region and continent, is built
  by `data-raw/build_wgsrpd_l3.R`. It lets
  [`introGraph()`](https://dboslab.github.io/expowo/reference/introGraph.md)
  work without any download and
  [`endemicMap()`](https://dboslab.github.io/expowo/reference/endemicMap.md)
  match the map by area code, so that botanical countries with accented
  names in the TDWG map (e.g. “Réunion”, “Zaïre”) are not missed.

### How the output is built from WCVP

- Only two WCVP tables are used: `wcvp_names.csv` and
  `wcvp_distribution.csv`. They are downloaded together in `wcvp.zip`
  from <https://sftp.kew.org/pub/data-repositories/WCVP/> and kept as a
  slim RDS file in the cache.

- **Genera**
  ([`powoGenera()`](https://dboslab.github.io/expowo/reference/powoGenera.md),
  [`powoFam()`](https://dboslab.github.io/expowo/reference/powoFam.md),
  [`megaGen()`](https://dboslab.github.io/expowo/reference/megaGen.md),
  [`topGen()`](https://dboslab.github.io/expowo/reference/topGen.md))
  are the names with `taxon_rank` “Genus” and `taxon_status` “Accepted”
  in each family, in alphabetical order. Families are searched in the
  same order as `POWOcodes`.

- **Species**
  ([`powoSpecies()`](https://dboslab.github.io/expowo/reference/powoSpecies.md),
  [`powoSpDist()`](https://dboslab.github.io/expowo/reference/powoSpDist.md))
  are the names with `taxon_rank` “Species” and `taxon_status`
  “Accepted” whose parent is one of the searched genera, in alphabetical
  order within each genus. Infraspecific taxa are not listed, as in the
  POWO genus pages.

- **species_number** is the number of accepted species in each genus,
  including hybrid species.
  [`powoFam()`](https://dboslab.github.io/expowo/reference/powoFam.md)
  sums it over the genera of each family. A genus with no accepted
  species gets 0, where the POWO search returned `NA`.

- **hybrid**: a genus or species is a hybrid when its name contains “×”
  or “+”, as in the POWO search.

- **authors** and **scientific_name** come from `taxon_authors` and
  `taxon_name`.

- **publication** joins `publication_author`, `place_of_publication`,
  `volume_and_page` and `first_published`, e.g. “H.G.A.Engler (ed.),
  Pflanzenr., IV, 219a: 31 (1939)”.

- **native_to_botanical_countries** and
  **introduced_to_botanical_countries** list the WGSRPD level-3 areas of
  each taxon, in alphabetical order. Extinct and doubtful records are
  left out. An area where the taxon is native is not also listed as
  introduced. “unknown” is used when there is no record, as before.

- **native_to_country** and **introduced_to_country** convert those
  areas to political countries with the `botregions` dataset, as in the
  POWO search. Areas missing from `botregions` keep their own name.

- The WCVP area names that changed after the POWO names (e.g. “DR
  Congo”, “Türkiye”, “Eswatini”, “Czechia-Slovakia”, “NW. Balkan Pen.”,
  “Réunion”) are translated back by their level-3 code to the names used
  by POWO and by the TDWG level-3 shapefile read in
  [`powoMap()`](https://dboslab.github.io/expowo/reference/powoMap.md).

- **synonyms = TRUE** lists the non-accepted names whose accepted name
  is one of the returned species, right after that species. Misapplied
  names are left out. Homotypic synonyms come first and then heterotypic
  ones, each in alphabetical order. The `status` is “Homotypic Synonym”
  or “Heterotypic Synonym”. The `accepted_name` is the scientific name
  of the accepted species, and the `publication` is “unknown
  publication” when WCVP has none. As in the POWO search, the species,
  authors and distribution columns of synonyms are `NA`.

- **kew_id** is the WCVP `powo_id` (the IPNI id when `powo_id` is
  empty), and **powo_uri** is built from it with the same URL as before.

- `country`, `genus`, `hybrid`, `rerun` and `save` behave as before; the
  country filter is the same function used by the POWO search, so a
  taxon is kept when it is native or introduced in any of the given
  countries.

### Minor changes

- With `source = "wcvp"`, the botanical country names are no longer
  truncated at 20 characters, the publications keep their original
  accents and the `&amp;` HTML entities are replaced by `&`.

- The distribution of each genus in
  [`powoGenera()`](https://dboslab.github.io/expowo/reference/powoGenera.md)
  also includes the distribution of all its accepted species, as in the
  POWO genus pages.

- [`powoSpDist()`](https://dboslab.github.io/expowo/reference/powoSpDist.md)
  no longer fails when none of the given species is accepted.

- The
  [`powoSpDist()`](https://dboslab.github.io/expowo/reference/powoSpDist.md)
  example and the species distribution vignette now use *Bertholletia
  excelsa*, because *Lecythis pisonis* is currently treated as a synonym
  of *Pachylecythis pisonis*.

- expowo now requires R \>= 4.0.0.

- Added a `README.Rmd` from which `README.md` is generated.

- Added three vignettes showing how to make the new graphics: mapping
  endemic species, plotting species discovery over time and plotting
  species introductions.

- The test suite now covers about 98% of the code, including the POWO
  mining (tested offline with copies of POWO pages), the WCVP download
  and cache, the maps and the graphics. A GitHub Actions workflow
  reports the coverage to Codecov.

## expowo 3.1

CRAN release: 2026-07-24

- Released on CRAN on 14 July 2026.
