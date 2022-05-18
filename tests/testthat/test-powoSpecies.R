test_that("powoSpecies works for one family search and no specified genus or country vectors, genus = NULL & country = NULL", {
  res_ex <- powoSpecies("Amborellaceae",
                       "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 1)
})

test_that("powoSpecies works for more than one family and no specified genus or country vectors, genus = NULL & country = NULL", {
  res_ex <- powoSpecies(c("Amborellaceae", "Hydatellaceae"),
                       c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                         "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:8-1"),
                       verbose = FALSE,
                       save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 14)
})

test_that("powoSpecies works for one family search and a specified genus vector, genus = Brasenia & country = NULL", {
  res_ex <- powoSpecies("Cabombaceae",
                        "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126578-1",
                        genus = "Brasenia",
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 1)
})

test_that("powoSpecies works for more than one family and a specified genus vector, genus = c(Brasenia, Hillebrandia) & country = NULL", {
  res_ex <- powoSpecies(c("Cabombaceae", "Begoniaceae"),
                        c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126578-1",
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126658-1"),
                        genus = c("Brasenia", "Hillebrandia"),
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 2)
})

test_that("powoSpecies works for one family search and a specified country vector, genus = NULL & country = Brazil", {
  res_ex <- powoSpecies("Cabombaceae",
                        "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126578-1",
                        country = "Brazil",
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 5)
})

test_that("powoSpecies works for more than one family and a specified country vector, genus = NULL & country = New Caledonia", {
  res_ex <- powoSpecies(c("Amborellaceae", "Cabombaceae"),
                        c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126578-1"),
                        country = "New Caledonia",
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 1)
})

test_that("powoSpecies works for more than one family and specified genus and country vectors, genus = c(Amborella, Cabomba) & country = c(Brazil, New Caledonia", {
  res_ex <- powoSpecies(c("Amborellaceae", "Cabombaceae"),
                        c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126703-1",
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126578-1"),
                        genus = c("Amborella", "Cabomba"),
                        country = c("Brazil", "New Caledonia"),
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 6)
})

test_that("powoSpecies works for one family search and a specified hybridspp vector, hybridspp = TRUE", {
  res_ex <- powoSpecies("Cabombaceae",
                        "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126578-1",
                        hybridspp = TRUE,
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 14)
  expect_equal(nrow(res_ex), 7)
})

test_that("powoSpecies output errors for wrong inputs", {
  expect_error(powoSpecies("Lecythidaceae",
                          c("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                            "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30000348-2"),
                          verbose = FALSE,
                          save = FALSE))
  expect_error(powoSpecies(c("Capparaceae", "Lecythidaceae"),
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                          verbose = FALSE,
                          save = FALSE))
  expect_error(powoSpecies("Capparaceae",
                          "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30001562-2",
                          genus = "Caparis",
                          verbose = FALSE,
                          save = FALSE))
})
