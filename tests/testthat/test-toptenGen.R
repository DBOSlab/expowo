test_that("toptenGen works for one family with less than 10 genera", {
  res_ex <- toptenGen("Begoniaceae",
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 2)
})

test_that("toptenGen works for more than one family with less than 10 genera", {
  res_ex <- toptenGen(c("Amborellaceae", "Begoniaceae"),
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})

test_that("toptenGen works for one family with more than 10 genera", {
  res_ex <- toptenGen("Lecythidaceae",
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 10)
})

test_that("toptenGen works for more than one family with more than 10 genera each", {
  res_ex <- toptenGen(c("Capparaceae", "Lecythidaceae"),
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 20)
})
