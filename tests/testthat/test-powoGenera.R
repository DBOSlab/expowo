test_that("powoGenera works for one family search and no specified genus or country vectors, genus = NULL & country = NULL", {
  res_ex <- powoGenera("Martyniaceae",
                       "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1",
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 5)
})

test_that("powoGenera works for more than one family and no specified genus orcountry vectors, genus = NULL & country = NULL", {
  res_ex <- powoGenera(c("Amborellaceae", "Begoniaceae"),
                       c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1"),
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 3)
})

test_that("powoGenera works for one family search and a specified genus vector, genus = Begonia & country = NULL", {
  res_ex <- powoGenera("Begoniaceae",
                       "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                       genus = "Begonia",
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 1)
})

test_that("powoGenera works for more than one family and a specified genus vector, genus = c(Amborella, Begonia⁠) & country = NULL", {
  res_ex <- powoGenera(c("Amborellaceae", "Begoniaceae"),
                       c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1"),
                       genus = c("Amborella", "Begonia"),
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 2)
})

test_that("powoGenera works for one family search and a specified country vector, genus = NULL & country = Brazil", {
  res_ex <- powoGenera("Martyniaceae",
                       "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1",
                       country = "Brazil",
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 3)
})

test_that("powoGenera works for more than one family and a specified country vector, genus = NULL & country = BrazilL", {
  res_ex <- powoGenera(c("Amborellaceae", "Begoniaceae"),
                       c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1"),
                       country = "Brazil",
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 1)
})

test_that("powoGenera works for one family search and specified genus and country vectors, genus = Holoregmia & country = Brazil", {
  res_ex <- powoGenera("Martyniaceae",
                       "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1",
                       genus = "Holoregmia",
                       country = "Brazil",
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 1)
})

test_that("powoGenera works for more than one family and specified genus and country vectors, genus = c(Begonia, Holoregmia⁠) & country = NULL", {
  res_ex <- powoGenera(c("Begoniaceae", "Martyniaceae"),
                       c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1",
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126797-1"),
                       genus = c("Begonia", "Holoregmia"),
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(is.numeric(res_ex[,6]), TRUE)
  expect_equal(ncol(res_ex), 12)
  expect_equal(nrow(res_ex), 2)
})

test_that("powoGenera output errors for wrong inputs", {
  expect_error(powoGenera("Lecythidaceae",
                          c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                            "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30000348-2"),
                          verbose = FALSE,
                          save = FALSE))
  expect_error(powoGenera(c("Capparaceae", "Lecythidaceae"),
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                          verbose = FALSE,
                          save = FALSE))
  expect_error(powoGenera("Capparaceae",
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                          genus = "Caparis",
                          verbose = FALSE,
                          save = FALSE))
})
