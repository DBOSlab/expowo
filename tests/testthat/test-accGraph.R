# accGraph draws polygons with the deprecated `size` aesthetic of ggplot2
# (through PupillometryR); keep those deprecation warnings out of the tests
local_quiet_graph <- function(env = parent.frame()) {
  withr::local_options(warn = -1, .local_envir = env)
}

spp <- powoSpecies("Martyniaceae", synonyms = TRUE, verbose = FALSE)

test_that("accGraph plots species accumulation for each genus", {
  local_quiet_graph()
  expect_output(
    p <- accGraph(spp, spp_changes = FALSE, verbose = FALSE),
    "Column names match")
  expect_type(p, "list")
  expect_length(p, length(unique(spp$genus[spp$status == "Accepted"])))
  expect_s3_class(p[[1]], "ggplot")
})

test_that("accGraph plots species accumulation for all data together", {
  local_quiet_graph()
  expect_output(
    p <- accGraph(spp, spp_changes = FALSE, genus_plots = FALSE,
                  verbose = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("accGraph plots nomenclatural changes", {
  local_quiet_graph()
  expect_output(
    p <- accGraph(spp, spp_acc = FALSE, spp_changes_col = "genus",
                  verbose = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("accGraph accepts data already processed by get_year_pubs", {
  local_quiet_graph()
  years <- suppressWarnings(capture.output(
    df <- get_year_pubs(spp, verbose = FALSE)))
  expect_output(
    p <- accGraph(df, spp_acc = FALSE, spp_changes_col = "genus",
                  verbose = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("accGraph saves all plots", {
  local_quiet_graph()
  dir <- file.path(withr::local_tempdir(), "graphs")
  folder <- file.path(dir, format(Sys.time(), "%d%b%Y"))
  expect_output(
    suppressMessages(
      accGraph(spp, spp_changes_col = "genus", save = TRUE, dir = dir,
               format = "pdf")))
  expect_true(file.exists(file.path(folder,
                                    "cumulative_discovery__Craniolaria.pdf")))
  expect_true(file.exists(file.path(folder,
                                    "cumulative_discovery_all_data.pdf")))

  dir <- file.path(withr::local_tempdir(), "graphs")
  folder <- file.path(dir, format(Sys.time(), "%d%b%Y"))
  expect_output(
    expect_message(
      accGraph(spp, spp_changes = FALSE, genus_plots = FALSE, save = TRUE,
               dir = dir, format = "pdf"),
      "Writing accumulation plot"))
  expect_true(file.exists(
    file.path(folder, "cumulative_discovery__fully_plotted_data.pdf")))

  dir <- file.path(withr::local_tempdir(), "graphs")
  expect_output(
    accGraph(spp, spp_acc = FALSE, spp_changes_col = "genus", save = TRUE,
             dir = dir, format = "pdf", verbose = FALSE))
  expect_true(dir.exists(dir))
})
