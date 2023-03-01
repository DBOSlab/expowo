test_that("topGen works for one family with less than 10 genera", {
  res_ex <- topGen("Begoniaceae",
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 2)
})

test_that("topGen works for more than one family with less than 10 genera", {
  res_ex <- topGen(c("Amborellaceae", "Begoniaceae"),
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 3)
})

test_that("topGen works for one family with more than 10 genera", {
  res_ex <- topGen("Lecythidaceae",
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 10)
})

test_that("topGen works for more than one family with more than 10 genera each", {
  res_ex <- topGen(c("Capparaceae", "Lecythidaceae"),
                      verbose = FALSE,
                      save = FALSE)

  expect_equal(class(res_ex)[4], "data.frame")
  expect_equal(ncol(res_ex), 7)
  expect_equal(nrow(res_ex), 20)
})
