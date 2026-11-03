test_that("get_year_pubs extracts the year of each name and basionym", {
  spp <- powoSpecies("Lecythidaceae", synonyms = TRUE, verbose = FALSE)
  res <- get_year_pubs(spp, verbose = FALSE) |>
    expect_output("Column names match")

  expect_true(all(c("year", "year_basionym", "year_changed",
                    "number_synonyms") %in% names(res)))
  expect_type(res$year, "double")
  acc <- res[res$status == "Accepted", ]
  expect_true(all(acc$year_changed %in% c("yes", "no", NA)))
  expect_true(all(acc$year_basionym <= acc$year, na.rm = TRUE))
  expect_true(all(acc$number_synonyms >= 0))
})

test_that("get_year_pubs handles names without synonyms and saves results", {
  dir <- withr::local_tempdir()
  spp <- powoSpecies("Martyniaceae", synonyms = TRUE, verbose = FALSE)
  expect_output(
    res <- get_year_pubs(spp, save = TRUE, dir = dir, filename = "years",
                         verbose = FALSE),
    "Column names match")
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "years.csv")))
  expect_equal(res$year[res$taxon_name == "Martynia annua"], 1753)
})

test_that("get_year_pubs corrects the year of conserved names", {
  spp <- data.frame(family = "Fakeaceae", genus = "Fakea",
                    species = c("alba", "nigra"),
                    taxon_name = c("Fakea alba", "Fakea nigra"),
                    authors = "L.",
                    scientific_name = c("Fakea alba L.", "Fakea nigra L."),
                    status = "Accepted", accepted_name = NA,
                    publication = c("Sp. Pl.: 1 (1753), nom. cons.",
                                    "Sp. Pl.: 2 (1753)"),
                    native_to_country = "Brazil",
                    native_to_botanical_countries = "Brazil North",
                    introduced_to_country = "unknown",
                    introduced_to_botanical_countries = "unknown",
                    kew_id = "1-1", powo_uri = NA)
  expect_output(res <- get_year_pubs(spp, verbose = FALSE))
  expect_equal(res$year, c(1753, 1753))
})
