test_that("powoSpecies works for one family search and no specified genus or country vectors, genus = NULL & country = NULL", {
  res_ex <- powoSpecies("Amborellaceae",
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
                        hybridspp = TRUE,
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 14)
  expect_equal(nrow(res_ex), 7)
})
