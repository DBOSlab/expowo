test_that("saveCSV writes the CSV within a current date folder", {
  dir <- file.path(withr::local_tempdir(), "results")
  df <- data.frame(family = "Martyniaceae", genus = c("Craniolaria", "Ibicella"))

  expect_message(saveCSV(df, dir = dir, filename = "test"), "Writing")
  file <- file.path(dir, format(Sys.time(), "%d%b%Y"), "test.csv")
  expect_true(file.exists(file))
  expect_equal(read.csv(file), df)

  # Appending does not write the header again
  saveCSV(df, dir = dir, filename = "test", append = TRUE, verbose = FALSE)
  expect_equal(nrow(read.csv(file)), 4)
})

test_that("saveCSV does nothing when save = FALSE", {
  dir <- file.path(withr::local_tempdir(), "results")
  saveCSV(data.frame(x = 1), dir = dir, save = FALSE)
  expect_false(dir.exists(dir))
})

test_that("saveCSV uses a given folder name", {
  dir <- withr::local_tempdir()
  folder <- file.path(dir, "myfolder")
  saveCSV(data.frame(x = 1), dir = dir, foldername = folder,
          filename = "out", verbose = FALSE)
  expect_true(file.exists(file.path(folder, "out.csv")))
})
