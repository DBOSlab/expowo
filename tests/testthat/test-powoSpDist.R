test_that("powoSpDist returns the distribution of the given species", {
  res <- powoSpDist("Lecythidaceae", species = "Bertholletia excelsa",
                    verbose = FALSE)
  expect_equal(names(res), setdiff(names(angioData), "publication"))
  expect_equal(nrow(res), 1)
  expect_match(res$native_to_country, "Brazil")
})

test_that("powoSpDist returns all species of a family", {
  dir <- withr::local_tempdir()
  res <- powoSpDist("Martyniaceae", save = TRUE, dir = dir,
                    filename = "martyniaceae", verbose = FALSE)
  expect_equal(nrow(res), nrow(powoSpecies("Martyniaceae", verbose = FALSE)))
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "martyniaceae.csv")))
})
