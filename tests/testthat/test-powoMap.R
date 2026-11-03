# powoMap maps `world_plant$Freq` inside aes(), which ggplot2 discourages;
# keep that warning out of the tests
local_quiet_map <- function(env = parent.frame()) {
  withr::local_options(warn = -1, .local_envir = env)
}

spp <- powoSpecies("Martyniaceae", verbose = FALSE)

test_that("powoMap maps species richness across political countries", {
  local_quiet_map()
  p <- powoMap(spp, distcol = "native_to_country", verbose = FALSE,
               dir = withr::local_tempdir())
  expect_s3_class(p, "ggplot")
  expect_true("Freq" %in% names(p$data))
  expect_equal(max(p$data$Freq), max(table(unlist(
    strsplit(spp$native_to_country, ", ")))))

  p <- powoMap(spp, distcol = "native_to_country", vir_color = NULL,
               bre_color = "Spectral", verbose = FALSE,
               dir = withr::local_tempdir())
  expect_s3_class(p, "ggplot")
})

test_that("powoMap makes one map for each taxonomic group", {
  local_quiet_map()
  p <- powoMap(spp, distcol = "native_to_country", taxclas = "genus",
               verbose = FALSE, dir = withr::local_tempdir())
  expect_type(p, "list")
  expect_named(p, unique(spp$genus))

  p <- powoMap(spp, distcol = "native_to_country", taxclas = "genus",
               vir_color = NULL, bre_color = "Spectral", verbose = FALSE,
               dir = withr::local_tempdir())
  expect_s3_class(p[[1]], "ggplot")
})

test_that("powoMap saves maps in any colour palette", {
  local_quiet_map()
  dir <- withr::local_tempdir()
  folder <- file.path(dir, format(Sys.time(), "%d%b%Y"))
  expect_message(
    powoMap(spp, distcol = "native_to_country", save = TRUE, dpi = 72,
            dir = dir, filename = "map", format = "png"),
    "Saving")
  expect_true(file.exists(file.path(folder, "map_SR_viridis.png")))

  expect_message(
    powoMap(spp, distcol = "native_to_country", save = TRUE, dpi = 72,
            vir_color = NULL, bre_color = "Spectral",
            dir = dir, filename = "map", format = "png"),
    "Saving")
  expect_true(file.exists(file.path(folder, "map_SR_Spectral.png")))

  suppressMessages(
    powoMap(spp[spp$genus == "Martynia", ], distcol = "native_to_country",
            taxclas = "genus", save = TRUE, dpi = 72, dir = dir,
            filename = "map", format = "png"))
  expect_true(file.exists(file.path(folder, "map_SR_Martynia_viridis.png")))

  suppressMessages(
    powoMap(spp[spp$genus == "Martynia", ], distcol = "native_to_country",
            taxclas = "genus", save = TRUE, dpi = 72, vir_color = NULL,
            bre_color = "Spectral", dir = dir, filename = "map",
            format = "png"))
  expect_true(file.exists(file.path(folder, "map_SR_Martynia_Spectral.png")))
})

test_that("powoMap maps species richness across botanical countries", {
  local_quiet_map()
  skip_if_offline("github.com")
  # The WGSRPD shapefile is downloaded into the working directory
  withr::local_dir(withr::local_tempdir())
  p <- suppressMessages(
    powoMap(spp, botctrs = TRUE, distcol = "native_to_botanical_countries",
            verbose = FALSE, dir = "maps"))
  expect_s3_class(p, "ggplot")
  expect_true(file.exists("WGSRPD/level3/level3.shp"))
  expect_gt(max(p$data$Freq), 0)
})
