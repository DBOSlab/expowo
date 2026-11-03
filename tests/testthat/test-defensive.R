test_that("numeric arguments are checked", {
  expect_error(megaGen("Martyniaceae", thld = "500"),
               "thld should be a numeric value")
  expect_error(topGen("Martyniaceae", limit = "10"),
               "limit should be a integer numeric value")
})

test_that("dir must be a character and loses its trailing slash", {
  expect_error(powoFam("Martyniaceae", dir = 1),
               "dir should be a character")
  expect_equal(expowo:::.arg_check_dir("results/"), "results")
  expect_equal(expowo:::.arg_check_dir("results"), "results")
})

test_that("a new search does not overwrite a previously saved file", {
  dir <- withr::local_tempdir()
  folder <- file.path(dir, format(Sys.time(), "%d%b%Y"))
  dir.create(folder)
  writeLines("family", file.path(folder, "output.csv"))

  expect_error(powoSpecies("Martyniaceae", dir = dir, filename = "output"),
               "rerun = FALSE")
  expect_error(powoGenera("Martyniaceae", dir = dir, filename = "output"),
               "rerun = FALSE")
})

test_that("a rerun search needs the previously saved file", {
  dir <- withr::local_tempdir()
  expect_error(powoSpecies("Martyniaceae", rerun = TRUE, dir = dir,
                           filename = "output"),
               "rerun = TRUE")
  expect_error(powoGenera("Martyniaceae", rerun = TRUE, dir = dir,
                          filename = "output"),
               "rerun = TRUE")
})

test_that("only supported image formats are accepted", {
  expect_error(expowo:::.arg_check_format("bmp"), "format names")
  expect_silent(expowo:::.arg_check_format(c("pdf", "PNG")))
})

test_that("family synonyms are replaced by the accepted family", {
  expect_equal(expowo:::.arg_check_family(c("Leguminosae", "Compositae",
                                            "Martyniaceae")),
               c("Fabaceae", "Asteraceae", "Martyniaceae"))
})

test_that("input data of accGraph and get_year_pubs are checked", {
  expect_output(expowo:::.arg_check_inputdf("a"), "Input is not a dataframe")
  expect_output(expowo:::.arg_check_inputdf(data.frame(x = 1)),
                "column names do not match")
  expect_output(expowo:::.arg_check_inputdf(
    powoSpecies("Martyniaceae", synonyms = TRUE, verbose = FALSE)),
    "Column names match")
})

test_that("input data of powoMap need species binomials", {
  df <- data.frame(name = "Craniolaria annua", dist = "Brazil, Peru")
  res <- expowo:::.arg_check_data_map(df, "dist")
  expect_equal(names(res)[1], "taxon_name")
  expect_equal(res$taxon_name, "Craniolaria annua")

  expect_error(expowo:::.arg_check_data_map(data.frame(name = "craniolaria",
                                                       dist = "Brazil"),
                                            "dist"),
               "binomial")
})
