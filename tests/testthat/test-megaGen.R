test_that("megaGen works for one family with no genus larger than the
          default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen("Martyniaceae",
                    thld = 500,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for one family with any genus larger than the default
          threshold of 500 species, thld = NULL", {
  res_ex <- megaGen("Begoniaceae",
                    thld = 500,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(nrow(res_ex), 1)
})

test_that("megaGen works for more than one family with no genus larger than the
          default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen(c("Amborellaceae", "Martyniaceae"),
                    thld = 500,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for more than one family with any genus larger than the
          default threshold of 500 species, thld = NULL", {
  res_ex <- megaGen(c("Begoniaceae", "Piperaceae"),
                    thld = 500,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})

test_that("megaGen works for one family with no genus larger than a specified
          threshold, thld = 300", {
  res_ex <- megaGen("Martyniaceae",
                    thld = 300,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for more than one family with no genus larger than a
          specified threshold, thld = 300", {
  res_ex <- megaGen(c("Amborellaceae", "Martyniaceae"),
                    thld = 300,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(length(res_ex[,2])==0, TRUE)
  expect_equal(length(res_ex[,4])==0, TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 0)
})

test_that("megaGen works for one family with any genus larger than a specified
          threshold, thld = 1000", {
  res_ex <- megaGen("Begoniaceae",
                    thld = 1000,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(nrow(res_ex), 1)
})

test_that("megaGen works for more than one family with any genus larger than a
          specified threshold, thld = 1000", {
  res_ex <- megaGen(c("Begoniaceae", "Piperaceae"),
                    thld = 1000,
                    verbose = FALSE,
                    save = FALSE)

  expect_equal(class(res_ex), "data.frame")
  expect_equal(is.character(res_ex[,2]), TRUE)
  expect_equal(is.character(res_ex[,4]), TRUE)
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})
