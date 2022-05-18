test_that("toptenGen works for one family with less than 10 genera", {
  res_ex <- toptenGen("Begoniaceae",
                      "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 2)
})

test_that("toptenGen works for more than one family with less than 10 genera", {
  res_ex <- toptenGen(c("Amborellaceae", "Begoniaceae"),
                      c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                        "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1"),
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})

test_that("toptenGen works for one family with more than 10 genera", {
  res_ex <- toptenGen("Lecythidaceae",
                      "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30000348-2",
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 10)
})

test_that("toptenGen works for more than one family with more than 10 genera each", {
  res_ex <- toptenGen(c("Capparaceae", "Lecythidaceae"),
                      c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                        "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30000348-2"),
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 20)
})

test_that("toptenGen output errors for wrong inputs", {
  expect_error(toptenGen("Lecythidaceae",
                         c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                           "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30000348-2"),
                         verbose = FALSE,
                         save = FALSE))
  expect_error(toptenGen(c("Capparaceae", "Lecythidaceae"),
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                         verbose = FALSE,
                         save = FALSE))
  expect_error(toptenGen("Capparaceae",
                         "http://powo.science.kew.org/taxon/30001562-2",
                         verbose = FALSE,
                         save = FALSE))
})
