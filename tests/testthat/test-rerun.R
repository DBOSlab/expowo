# A rerun continues a stopped search from the last taxon saved on disk

save_partial <- function(df, dir, filename, n) {
  folder <- file.path(dir, format(Sys.time(), "%d%b%Y"))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(df[seq_len(n), ], file.path(folder,
                                                 paste0(filename, ".csv")))
  file.path(folder, paste0(filename, ".csv"))
}

test_that("powoSpecies continues a stopped WCVP search", {
  dir <- withr::local_tempdir()
  full <- powoSpecies("Lecythidaceae", verbose = FALSE)
  file <- save_partial(full, dir, "lecy", 200)

  res <- powoSpecies("Lecythidaceae", rerun = TRUE, save = TRUE, dir = dir,
                     filename = "lecy", verbose = FALSE)
  expect_equal(res$kew_id, full$kew_id[201:nrow(full)])
  expect_equal(nrow(read.csv(file)), nrow(full))
})

test_that("powoSpecies continues a stopped WCVP search with synonyms", {
  dir <- withr::local_tempdir()
  full <- powoSpecies("Lecythidaceae", synonyms = TRUE, verbose = FALSE)
  last <- max(which(full$status == "Accepted")[1:150])
  save_partial(full, dir, "lecy", last)

  res <- powoSpecies("Lecythidaceae", synonyms = TRUE, rerun = TRUE,
                     dir = dir, filename = "lecy", verbose = FALSE)
  expect_equal(res$scientific_name[1],
               full$scientific_name[full$status == "Accepted"][151])
})

test_that("powoGenera continues a stopped WCVP search from the last genus", {
  dir <- withr::local_tempdir()
  full <- powoGenera("Orchidaceae", verbose = FALSE)
  save_partial(full, dir, "orchids", 200)

  res <- powoGenera("Orchidaceae", rerun = TRUE, dir = dir,
                    filename = "orchids", verbose = FALSE)
  expect_equal(res$genus[1], full$genus[200])
  expect_equal(nrow(res), nrow(full) - 199)
})

test_that("powoSpecies and powoGenera continue a stopped POWO search", {
  local_powo_pages()
  dir <- withr::local_tempdir()

  full <- powoSpecies("Martyniaceae", genus = "Craniolaria", source = "powo",
                      verbose = FALSE)
  save_partial(full, dir, "spp", 1)
  res <- powoSpecies("Martyniaceae", genus = "Craniolaria", rerun = TRUE,
                     source = "powo", dir = dir, filename = "spp",
                     verbose = FALSE)
  expect_equal(res$taxon_name, "Craniolaria integrifolia")

  full <- powoGenera("Martyniaceae", source = "powo", verbose = FALSE)
  save_partial(full, dir, "gen", 1)
  res <- powoGenera("Martyniaceae", rerun = TRUE, source = "powo", dir = dir,
                    filename = "gen", verbose = FALSE)
  expect_equal(res$genus, c("Craniolaria", "Ibicella"))
})

test_that("POWO searches are saved in chunks", {
  local_powo_pages()
  dir <- withr::local_tempdir()
  res <- powoGenera("Martyniaceae", source = "powo", save = TRUE, dir = dir,
                    filename = "gen", verbose = FALSE)
  file <- file.path(dir, format(Sys.time(), "%d%b%Y"), "gen.csv")
  expect_equal(nrow(read.csv(file)), nrow(res))
})
