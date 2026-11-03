# Tests of the POWO mining (source = "powo") using offline copies of the POWO
# source pages built in helper-powo.R.

test_that(".powo_readLines reads any connection like readLines", {
  page <- file.path(powo_pages_dir(), "90003-1.html")
  expect_equal(expowo:::.powo_readLines(page), readLines(page))
})

test_that("powoGenera mines genera, species number and distribution", {
  local_powo_pages()
  res <- powoGenera("Martyniaceae", source = "powo", verbose = FALSE)

  expect_equal(names(res), names(angioGenera))
  expect_equal(res$genus, c("Craniolaria", "Ibicella"))
  expect_equal(res$authors, c("L.", "(Stapf) Van Eselt."))
  expect_equal(res$species_number, c(3, 2))
  expect_equal(res$publication[1], "Sp. Pl.: 618 (1753)")
  expect_equal(res$native_to_country[1],
               "Argentina, Brazil, Colombia, Democratic Republic of the Congo")
  expect_equal(res$native_to_botanical_countries[1],
               "Argentina Northeast, Brazil South, Colombia, Zaire")
  expect_equal(res$introduced_to_country, c("United Kingdom", "South Africa"))
  expect_equal(res$kew_id, c("90001-1", "90002-1"))
})

test_that("powoGenera includes hybrid genera when hybrid = TRUE", {
  local_powo_pages()
  expect_message(
    res <- powoGenera("Martyniaceae", hybrid = TRUE, source = "powo",
                      verbose = TRUE),
    "Searching distribution and sp number") |>
    suppressMessages()

  expect_equal(ncol(res), 13)
  expect_equal(res$hybrid, c("no", "no", "yes"))
  expect_true(is.na(res$species_number[3]))
  expect_equal(res$native_to_country[3], "unknown")
})

test_that("powoGenera filters genera by genus and country", {
  local_powo_pages()
  res <- powoGenera("Martyniaceae", genus = "Craniolaria",
                    country = c("Colombia", "Peru"), source = "powo",
                    verbose = FALSE)
  expect_equal(res$genus, "Craniolaria")

  expect_error(powoGenera("Martyniaceae", genus = "Notagenus",
                          source = "powo", verbose = FALSE),
               "Notagenus")
})

test_that("powoSpecies mines species, synonyms and distribution", {
  local_powo_pages()
  expect_output(
    res <- powoSpecies("Martyniaceae", synonyms = TRUE, source = "powo",
                       verbose = FALSE),
    "ERROR: Ibicella")

  expect_equal(ncol(res), 15)
  expect_equal(res$taxon_name[res$status == "Accepted"],
               c("Craniolaria annua", "Craniolaria integrifolia",
                 "Ibicella lutea", "Ibicella parodii"))

  syn <- res[res$status != "Accepted", ]
  expect_setequal(syn$status, c("Homotypic Synonym", "Heterotypic Synonym"))
  expect_setequal(syn$scientific_name, c("Martynia annua L.",
                                         "Martynia diandra Glox."))
  expect_true(all(syn$accepted_name == "Craniolaria annua L."))
  expect_setequal(syn$publication, c("Sp. Pl.: 618 (1753)",
                                     "Obs. Bot.: 14 (1785)"))
  expect_setequal(syn$kew_id, c("92001-1", "92002-1"))

  # HTML entities are replaced and failed connections are filled with NA
  expect_equal(res$publication[res$taxon_name == "Ibicella lutea"],
               "A.B.Rendle & al., Cat. Pl. Oban: 35 (1913)")
  expect_true(is.na(res$native_to_country[res$taxon_name ==
                                            "Ibicella parodii"]))
})

test_that("powoSpecies returns 13 columns without synonyms", {
  local_powo_pages()
  expect_message(
    res <- powoSpecies("Martyniaceae", genus = "Craniolaria", source = "powo",
                       verbose = TRUE),
    "Searching distribution of") |>
    suppressMessages()
  expect_equal(names(res), names(angioData))
  expect_equal(res$taxon_name, c("Craniolaria annua",
                                 "Craniolaria integrifolia"))
  expect_equal(res$introduced_to_country, c("unknown", "United Kingdom"))
})

test_that("powoSpecies includes hybrid species when hybrid = TRUE", {
  local_powo_pages()
  res <- powoSpecies("Martyniaceae", genus = "Craniolaria", hybrid = TRUE,
                     source = "powo", verbose = FALSE)
  expect_equal(ncol(res), 14)
  expect_equal(res$hybrid, c("no", "no", "yes"))
})

test_that("powoSpecies includes hybrids and synonyms together", {
  local_powo_pages()
  res <- powoSpecies("Martyniaceae", genus = "Craniolaria", hybrid = TRUE,
                     synonyms = TRUE, source = "powo", verbose = FALSE)
  expect_equal(ncol(res), 16)
  expect_equal(sum(res$status == "Accepted"), 3)
})

test_that("powoSpecies keeps genera without species pages", {
  local_powo_pages()
  expect_output(
    res <- powoSpecies("Martyniaceae", hybrid = TRUE, source = "powo",
                       verbose = FALSE),
    "ERROR")
  expect_true(any(is.na(res$taxon_name)))
})

test_that("powoSpecies filters species by country and lists synonyms", {
  local_powo_pages()
  expect_output(
    res <- powoSpecies("Martyniaceae", genus = "Craniolaria", synonyms = TRUE,
                       country = c("Colombia", "Japan"), source = "powo",
                       verbose = TRUE) |>
      suppressMessages(),
    "There is no taxa occurring in the countries below")
  expect_equal(res$taxon_name, c("Craniolaria annua", "Martynia diandra",
                                 "Martynia annua"))

  expect_output(
    res <- powoSpecies("Martyniaceae", genus = "Craniolaria",
                       country = "Japan", source = "powo",
                       verbose = TRUE) |>
      suppressMessages(),
    "Your search returned an empty data frame")
  expect_equal(nrow(res), 0)
})

test_that("powoSpecies follows the accepted family of a family synonym", {
  local_powo_pages()
  expect_message(
    res <- powoSpecies("Cabombaceae", source = "powo", verbose = FALSE),
    "Cabombaceae is under synonym of Fakeaceae") |>
    suppressMessages()
  expect_equal(res$family, "Fakeaceae")
  expect_equal(res$taxon_name, "Fakegenus fakeus")
})

test_that("powoFam, megaGen and topGen count species from POWO pages", {
  local_powo_pages()

  fam <- powoFam("Martyniaceae", source = "powo", verbose = FALSE)
  expect_equal(fam$genus_number, 2)
  expect_equal(fam$species_number, 5)
  expect_equal(fam$kew_id, "77126797-1")

  expect_message(
    mega <- megaGen("Martyniaceae", thld = 3, source = "powo",
                    verbose = TRUE),
    "Searching sp number") |>
    suppressMessages()
  expect_equal(mega$genus, "Craniolaria")

  top <- topGen("Martyniaceae", limit = 1, source = "powo", verbose = FALSE)
  expect_equal(top$genus, "Craniolaria")
})

test_that("genera without species number are searched again", {
  local_powo_pages()
  df <- data.frame(family = "Martyniaceae", genus = "× Martynella",
                   kew_id = "90003-1",
                   powo_uri = paste0("http://www.plantsoftheworldonline.org/",
                                     "taxon/urn:lsid:ipni.org:names:90003-1"))
  res <- expowo:::.getsppNumb(df, verbose = FALSE)
  expect_true(is.na(res$species_number))
})

test_that("powoSpDist returns the distribution of the given species", {
  local_powo_pages()
  res <- powoSpDist("Martyniaceae", species = "Craniolaria annua",
                    source = "powo", verbose = FALSE)
  expect_equal(ncol(res), 12)
  expect_equal(res$native_to_botanical_countries, "Colombia, Venezuela")

  expect_output(
    res <- powoSpDist("Martyniaceae", source = "powo", verbose = FALSE),
    "ERROR")
  expect_equal(nrow(res), 4)
})

test_that("botdiv_to_countries converts botanical countries to countries", {
  x <- list("Brazil North, Brazil South, Central African Repu, Yugoslavia")
  res <- expowo:::botdiv_to_countries(x, 1)
  # As in POWO searches, the countries of a single botanical country are kept
  # together before sorting
  expect_equal(res, paste("Bosnia and Herzegovina, Croatia, Macedonia,",
                          "Montenegro, Serbia, Slovenia, Brazil,",
                          "Central African Republic"))
  expect_equal(expowo:::botdiv_to_countries(list("<p>"), 1), "unknown")
})
