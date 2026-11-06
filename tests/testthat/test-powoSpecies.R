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

  # Amborella trichopoda is native and Cabomba caroliniana is introduced
  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 13)
  expect_equal(nrow(res_ex), 2)
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
                        hybrid = TRUE,
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 14)
  expect_equal(nrow(res_ex), 7)
})

test_that("powoSpecies returns synonyms with the same columns as the POWO search", {
  res_ex <- powoSpecies("Martyniaceae",
                        synonyms = TRUE,
                        country = c("Argentina", "Brazil", "French Guiana"),
                        verbose = FALSE,
                        save = FALSE)

  expect_equal(names(res_ex), c("family", "genus", "species", "taxon_name",
                                "authors", "scientific_name", "status",
                                "accepted_name", "publication",
                                "native_to_country",
                                "native_to_botanical_countries",
                                "introduced_to_country",
                                "introduced_to_botanical_countries",
                                "kew_id", "powo_uri"))
  expect_true(all(res_ex$status %in% c("Accepted", "Homotypic Synonym",
                                       "Heterotypic Synonym")))
  expect_true(all(!is.na(res_ex$accepted_name[res_ex$status != "Accepted"])))
})

test_that("powoSpecies rejects an unknown source", {
  expect_error(powoSpecies("Amborellaceae", source = "gbif"),
               "should be either")
})

test_that("searches do not create objects in the global environment", {
  rm(list = intersect(c("POWOcodes", "botregions"), ls(globalenv())),
     envir = globalenv())

  powoSpecies("Martyniaceae", country = "Brazil", verbose = FALSE)
  powoFam("Martyniaceae", verbose = FALSE)
  local_powo_pages()
  expect_output(powoSpecies("Martyniaceae", source = "powo", verbose = FALSE),
                "ERROR")

  expect_false(exists("POWOcodes", envir = globalenv(), inherits = FALSE))
  expect_false(exists("botregions", envir = globalenv(), inherits = FALSE))
})
