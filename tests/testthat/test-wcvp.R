test_that(".wcvp_download prepares the WCVP names and distribution tables", {
  zip_file <- fake_wcvp_zip()
  expect_message(expect_message(
    wcvp <- expowo:::.wcvp_download(verbose = TRUE,
                                    url = paste0("file://", zip_file)),
    "Downloading WCVP"), "Preparing WCVP data")

  wn <- wcvp$names
  wd <- wcvp$distribution

  # Accepted genus and species plus the synonyms of accepted species are kept,
  # while unplaced names are dropped
  expect_setequal(wn$taxon_name, c("Fakea", "Fakea alba", "Fakeus albus",
                                   "Fakea nigra"))
  expect_equal(wn$publication[wn$taxon_name == "Fakea alba"],
               "C.F.P.von Martius (ed.), Sp. Pl.: 5 (1753)")
  expect_equal(wn$publication[wn$taxon_name == "Fakea"], "Sp. Pl. 1: 1 (1753)")
  expect_true(is.na(wn$publication[wn$taxon_name == "Fakeus albus"]))
  expect_true(wn$homotypic_synonym[wn$taxon_name == "Fakeus albus"])
  expect_false(any(c("ipni_id", "volume_and_page") %in% names(wn)))

  # Extinct and doubtful records are dropped and legacy area names are used
  expect_setequal(wd$area[wd$plant_name_id == 2], c("Brazil North", "Zaire"))
  expect_true(wd$introduced[wd$area == "Zaire"])
  expect_false(5 %in% wd$plant_name_id)
})

test_that(".wcvp_download quietly prepares the data when verbose = FALSE", {
  zip_file <- fake_wcvp_zip()
  expect_silent(expowo:::.wcvp_download(verbose = FALSE,
                                        url = paste0("file://", zip_file)))
})

test_that(".wcvp_load downloads WCVP when there is no cached copy", {
  cache_dir <- local_wcvp_cache()
  zip_file <- fake_wcvp_zip()
  local_mocked_bindings(.wcvp_last_modified = function() "v1",
                        .wcvp_url = paste0("file://", zip_file),
                        .package = "expowo")

  expect_message(expect_message(wcvp <- expowo:::.wcvp_load(verbose = TRUE),
                                "Downloading"), "Preparing")
  expect_true(file.exists(file.path(cache_dir, "wcvp.rds")))
  expect_equal(readLines(file.path(cache_dir, "wcvp_last_modified.txt")), "v1")
  expect_named(wcvp, c("names", "distribution"))

  # A second call in the same session reuses the loaded copy
  expect_identical(expowo:::.wcvp_load(verbose = FALSE), wcvp)
})

test_that(".wcvp_load reads the cache unless Kew releases a new version", {
  cache_dir <- local_wcvp_cache()
  dir.create(cache_dir, recursive = TRUE)
  saveRDS(list(names = "cached", distribution = NULL),
          file.path(cache_dir, "wcvp.rds"))
  writeLines("v1", file.path(cache_dir, "wcvp_last_modified.txt"))

  # Same version: read from the cache
  local_mocked_bindings(.wcvp_last_modified = function() "v1",
                        .wcvp_download = function(...) stop("no download"),
                        .package = "expowo")
  expect_message(wcvp <- expowo:::.wcvp_load(verbose = TRUE), "cache")
  expect_equal(wcvp$names, "cached")
})

test_that(".wcvp_load uses the cache when Kew cannot be reached", {
  cache_dir <- local_wcvp_cache()
  dir.create(cache_dir, recursive = TRUE)
  saveRDS(list(names = "cached", distribution = NULL),
          file.path(cache_dir, "wcvp.rds"))
  local_mocked_bindings(.wcvp_last_modified = function() NULL,
                        .wcvp_download = function(...) stop("no download"),
                        .package = "expowo")
  expect_equal(expowo:::.wcvp_load(verbose = FALSE)$names, "cached")
})

test_that(".wcvp_load refreshes the cache when there is a new WCVP version", {
  cache_dir <- local_wcvp_cache()
  dir.create(cache_dir, recursive = TRUE)
  saveRDS(list(names = "old", distribution = NULL),
          file.path(cache_dir, "wcvp.rds"))
  writeLines("v1", file.path(cache_dir, "wcvp_last_modified.txt"))
  local_mocked_bindings(.wcvp_last_modified = function() "v2",
                        .wcvp_download = function(...) {
                          list(names = "new", distribution = NULL)
                        },
                        .package = "expowo")
  expect_equal(expowo:::.wcvp_load(verbose = FALSE)$names, "new")
  expect_equal(readRDS(file.path(cache_dir, "wcvp.rds"))$names, "new")
  expect_equal(readLines(file.path(cache_dir, "wcvp_last_modified.txt")), "v2")
})

test_that(".wcvp_load stops when offline and without a cached copy", {
  local_wcvp_cache()
  local_mocked_bindings(.wcvp_last_modified = function() NULL,
                        .package = "expowo")
  expect_error(expowo:::.wcvp_load(verbose = FALSE),
               "no cached copy of WCVP")
})

test_that(".wcvp_last_modified returns the release date or NULL", {
  skip_if_offline("sftp.kew.org")
  expect_type(expowo:::.wcvp_last_modified(), "character")

  local_mocked_bindings(.wcvp_url = "https://sftp.kew.org/pub/no_such_file.zip",
                        .package = "expowo")
  expect_null(expowo:::.wcvp_last_modified())

  local_mocked_bindings(.wcvp_url = "https://no-such-host.expowo.invalid/x.zip",
                        .package = "expowo")
  expect_null(expowo:::.wcvp_last_modified())
})

test_that(".arg_check_source accepts only wcvp and powo", {
  expect_equal(expowo:::.arg_check_source("wcvp"), "wcvp")
  expect_equal(expowo:::.arg_check_source("powo"), "powo")
  expect_error(expowo:::.arg_check_source(c("wcvp", "powo")), "either")
  expect_error(expowo:::.arg_check_source(1), "either")
})

test_that(".wcvp_genera keeps the POWOcodes order and stops on unknown taxa", {
  res <- expowo:::.wcvp_genera(c("Martyniaceae", "Cabombaceae"),
                               verbose = FALSE)
  expect_equal(unique(res$family), c("Cabombaceae", "Martyniaceae"))
  expect_false("hybrid" %in% names(res))
  expect_false(is.unsorted(res$genus[res$family == "Martyniaceae"]))

  expect_error(expowo:::.wcvp_genera("Notafamilyaceae", verbose = FALSE),
               "not found in WCVP")
  expect_error(expowo:::.wcvp_genera("Martyniaceae", genus = "Notagenus",
                                     verbose = FALSE),
               "Notagenus")
})

test_that("hybrid genera are flagged with the multiplication sign", {
  res <- expowo:::.wcvp_genera("Orchidaceae", hybrid = TRUE, verbose = FALSE)
  expect_true(all(res$hybrid[grepl("×", res$genus)] == "yes"))
  expect_true(any(res$hybrid == "yes"))
  expect_true(any(res$hybrid == "no"))
})

test_that(".wcvp_species lists accepted species and flags hybrids", {
  gen <- expowo:::.wcvp_genera("Lamiaceae", genus = "Mentha", verbose = FALSE)
  spp <- expowo:::.wcvp_species(gen, hybrid = TRUE, verbose = FALSE)
  expect_true(all(spp$genus == "Mentha"))
  expect_true(any(spp$hybrid == "yes"))
  expect_false(is.unsorted(spp$taxon_name))
  expect_equal(spp$species, gsub(".*\\s", "", spp$taxon_name))

  spp_nohyb <- expowo:::.wcvp_species(gen, hybrid = FALSE, verbose = FALSE)
  expect_false("hybrid" %in% names(spp_nohyb))
  expect_equal(nrow(spp_nohyb), sum(spp$hybrid == "no"))
})

test_that(".wcvp_getInfo returns the columns of getInfo for any argument set", {
  gen <- expowo:::.wcvp_genera("Martyniaceae", hybrid = TRUE, verbose = FALSE)
  spp <- expowo:::.wcvp_species(gen, hybrid = TRUE, verbose = FALSE)

  res <- expowo:::.wcvp_getInfo(gen, hybrid = TRUE, gen_sp_nbr = TRUE,
                                verbose = FALSE)
  expect_equal(names(res), c("family", "genus", "authors", "scientific_name",
                             "publication", "species_number", "hybrid",
                             "native_to_country",
                             "native_to_botanical_countries",
                             "introduced_to_country",
                             "introduced_to_botanical_countries",
                             "kew_id", "powo_uri"))

  res <- expowo:::.wcvp_getInfo(spp, hybrid = TRUE, synonyms = TRUE,
                                verbose = FALSE)
  expect_equal(names(res)[c(7, 8, 10)], c("status", "accepted_name", "hybrid"))
  expect_equal(ncol(res), 16)
})

test_that("synonyms follow their accepted species, homotypic ones first", {
  res <- powoSpecies("Martyniaceae", synonyms = TRUE, verbose = FALSE)
  acc <- which(res$status == "Accepted")
  for (i in seq_along(acc)) {
    last <- if (i < length(acc)) acc[i + 1] - 1 else nrow(res)
    if (last > acc[i]) {
      syn <- res[(acc[i] + 1):last, ]
      expect_true(all(syn$accepted_name == res$scientific_name[acc[i]]))
      expect_false(is.unsorted(syn$status != "Homotypic Synonym"))
    }
  }
  syn <- res[res$status != "Accepted", ]
  expect_true(all(is.na(syn$species)))
  expect_true(all(is.na(syn$native_to_country)))
  expect_false(any(is.na(syn$publication)))
})

test_that("taxa without distribution records are listed as unknown", {
  wcvp <- expowo:::.wcvp_load(verbose = FALSE)
  dist <- expowo:::.wcvp_dist(c(-1L, -2L), wcvp$distribution)
  expect_equal(nrow(dist), 2)
  expect_true(all(unlist(dist) == "unknown"))
})

test_that("areas where a taxon is native are not listed as introduced", {
  distribution <- data.frame(plant_name_id = c(1L, 1L, 1L),
                             area = c("Brazil North", "Zaire", "Zaire"),
                             introduced = c(FALSE, FALSE, TRUE))
  dist <- expowo:::.wcvp_dist(1L, distribution)
  expect_equal(dist$native_to_botanical_countries, "Brazil North, Zaire")
  expect_equal(dist$native_to_country,
               "Brazil, Democratic Republic of the Congo")
  expect_equal(dist$introduced_to_country, "unknown")
})

test_that(".botdiv_lookup follows botdiv_to_countries", {
  res <- expowo:::.botdiv_lookup(c("Brazil North", "Yugoslavia", "Gambia",
                                   "Central African Repu"))
  expect_equal(unname(res[1]), "Brazil")
  expect_match(unname(res[2]), "Croatia")
  expect_equal(unname(res[3]), "Gambia")
  expect_equal(unname(res[4]), "Central African Republic")
})

test_that("a species search that returns no species keeps the columns", {
  res <- powoSpDist("Lecythidaceae", species = "Lecythis pisonis",
                    verbose = FALSE)
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 12)
})

test_that(".wcvp_powo_uri builds POWO links", {
  expect_equal(expowo:::.wcvp_powo_uri(c("1-1", NA)),
               c(paste0("http://www.plantsoftheworldonline.org/taxon/",
                        "urn:lsid:ipni.org:names:1-1"), NA))
})
