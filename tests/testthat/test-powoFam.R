test_that("powoFam counts genera and species of each family", {
  res <- powoFam(c("Martyniaceae", "Cabombaceae"), verbose = FALSE)
  expect_equal(names(res), c("family", "genus_number", "species_number",
                             "kew_id", "powo_uri"))
  expect_equal(res$family, c("Cabombaceae", "Martyniaceae"))

  gen <- powoGenera(c("Martyniaceae", "Cabombaceae"), verbose = FALSE)
  expect_equal(res$genus_number, as.numeric(table(gen$family)[res$family]))
  expect_equal(res$species_number,
               as.numeric(tapply(gen$species_number, gen$family,
                                 sum)[res$family]))
})

test_that("powoFam replaces family synonyms and saves the results", {
  dir <- withr::local_tempdir()
  expect_message(
    res <- powoFam("Leguminosae", save = TRUE, dir = dir,
                   filename = "fabaceae"),
    "Writing")
  expect_equal(res$family, "Fabaceae")
  expect_gt(res$species_number, 20000)
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "fabaceae.csv")))
})
