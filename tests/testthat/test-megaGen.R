test_that("megaGen works for one family with no genus larger than the default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen("Martyniaceae",
                    "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1",
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for one family with any genus larger than the default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen("Begoniaceae",
                    "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(res_ex$species_number, 1931)
  expect_equal(nrow(res_ex), 1)
})

test_that("megaGen works for more than one family with no genus larger than the default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen(c("Amborellaceae", "Martyniaceae"),
                    c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                      "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1"),
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for more than one family with any genus larger than the default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen(c("Begoniaceae", "Piperaceae"),
                    c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                      "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126695-1"),
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})

test_that("megaGen works for one family with no genus larger than a specified threshold, thld = 300", {
  res_ex <- megaGen("Martyniaceae",
                    "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1",
                    thld = 300,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for more than one family with no genus larger than a specified threshold, thld = 300", {
  res_ex <- megaGen(c("Amborellaceae", "Martyniaceae"),
                    c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                      "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1"),
                    thld = 300,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for one family with any genus larger than a specified threshold, thld = 1000", {
  res_ex <- megaGen("Begoniaceae",
                    "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                    thld = 1000,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(res_ex$species_number, 1931)
  expect_equal(nrow(res_ex), 1)
})

test_that("megaGen works for more than one family with any genus larger than a specified threshold, thld = 1000", {
  res_ex <- megaGen(c("Begoniaceae", "Piperaceae"),
                    c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                      "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126695-1"),
                    thld = 1000,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})

test_that("megaGen output errors for wrong inputs", {
  expect_error(megaGen("Lecythidaceae",
                       c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30000348-2"),
                       verbose = FALSE,
                       save = FALSE))
  expect_error(megaGen(c("Capparaceae", "Lecythidaceae"),
                       "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                       verbose = FALSE,
                       save = FALSE))
  expect_error(megaGen("Capparaceae",
                       "http://powo.science.kew.org/taxon/30001562-2",
                       verbose = FALSE,
                       save = FALSE))
})
