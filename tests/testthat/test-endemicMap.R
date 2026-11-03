spp <- powoSpecies("Lecythidaceae", synonyms = TRUE, verbose = FALSE)

test_that("endemicMap counts the species native to a single botanical country", {
  skip_if_offline("raw.githubusercontent.com")
  p <- endemicMap(spp, verbose = FALSE)
  expect_s3_class(p, "ggplot")

  acc <- spp[spp$status == "Accepted", ]
  areas <- strsplit(acc$native_to_botanical_countries, ", ")
  single <- unlist(areas[lengths(areas) == 1])
  single <- single[single != "unknown"]
  expect_equal(sum(p$data$endemics), length(single))
  expect_equal(p$data$endemics[p$data$botanical_country %in%
                                 "Brazil Southeast"],
               sum(single == "Brazil Southeast"))
  expect_equal(levels(p$data$class),
               c("0", "1-10", "11-50", "51-100", "101-250", "251-500",
                 "> 500"))
})

test_that("endemicMap matches every botanical country of the WCVP output", {
  skip_if_offline("raw.githubusercontent.com")
  df <- data.frame(native_to_botanical_countries =
                     c("Reunion", "Zaire", "Suriname", "Galapagos",
                       "Juan Fernandez Is.", "Turkey", "Brazil North"))
  expect_silent(p <- endemicMap(df, verbose = FALSE))
  expect_equal(sum(p$data$endemics), 7)

  expect_message(endemicMap(data.frame(native_to_botanical_countries =
                                         c("Atlantis", "Brazil North")),
                            verbose = TRUE),
                 "not found in the map: Atlantis")
})

test_that("endemicMap uses custom classes and colours", {
  skip_if_offline("raw.githubusercontent.com")
  p <- endemicMap(spp, breaks = c(1, 5), leg_title = "Endemics",
                  verbose = FALSE)
  expect_equal(levels(p$data$class), c("0", "1", "2-5", "> 5"))
  expect_equal(p$scales$get_scales("fill")$name, "Endemics")
})

test_that("endemicMap saves the map", {
  skip_if_offline("raw.githubusercontent.com")
  dir <- withr::local_tempdir()
  expect_message(endemicMap(spp, save = TRUE, dpi = 72, dir = dir,
                            filename = "lecy", format = "png"),
                 "Writing 'lecy.png'")
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "lecy.png")))
})

test_that("endemicMap downloads the map of botanical countries only once", {
  skip_if_offline("raw.githubusercontent.com")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  df <- data.frame(native_to_botanical_countries = "Brazil North")
  expect_message(endemicMap(df), "Downloading the TDWG map")
  expect_no_message(endemicMap(df, verbose = FALSE))
  expect_true(file.exists(file.path(tools::R_user_dir("expowo", "cache"),
                                    "level3", "level3.shp")))
})

test_that("endemicMap checks its arguments", {
  expect_error(endemicMap(NULL), "should be a data frame")
  expect_error(endemicMap(data.frame(x = 1)),
               "lacks the column\\(s\\) native_to_botanical_countries")
  expect_error(endemicMap(spp, breaks = c(50, 10)), "increasing numbers")
  expect_error(endemicMap(spp, breaks = "10"), "increasing numbers")
  expect_error(endemicMap(spp, breaks = c(0, 10)), "increasing numbers")
  expect_error(endemicMap(spp, format = "bmp"), "format names")
})
