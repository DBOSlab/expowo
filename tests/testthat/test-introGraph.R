# Tiny species list: species A is native to Brazil and introduced in Europe
# and Australia; species B is introduced within its own continent only, but
# in another region (Southwestern to Middle Europe)
fake_spp <- data.frame(
  native_to_botanical_countries = c("Brazil North, Peru", "Spain",
                                    "Brazil South", "Kenya"),
  introduced_to_botanical_countries = c("Spain, Great Britain, Queensland",
                                        "Germany", "unknown", NA))

test_that("introGraph counts each species once per pair of continents", {
  expect_message(p <- introGraph(fake_spp), "1 species are introduced")
  expect_s3_class(p, "ggplot")

  d <- p$data
  expect_equal(sum(d$species), 2)
  expect_equal(d$species[d$from == "Southern America" & d$to == "Europe"], 1)
  expect_equal(d$species[d$from == "Southern America" &
                           d$to == "Australasia"], 1)
  expect_false(any(d$from == d$to))
  expect_setequal(levels(d$from), c("Australasia", "Europe",
                                    "Southern America"))
})

test_that("introGraph works at the level of regions", {
  p <- introGraph(fake_spp, level = "region", verbose = FALSE)
  d <- p$data
  expect_equal(d$species[d$from == "Brazil" & d$to == "Southwestern Europe"],
               1)
  expect_equal(d$species[d$from == "Western South America" &
                           d$to == "Northern Europe"], 1)
  expect_equal(d$species[d$from == "Southwestern Europe" &
                           d$to == "Middle Europe"], 1)
})

test_that("introGraph uses accepted species of WCVP data and saves", {
  spp <- powoSpecies("Martyniaceae", synonyms = TRUE, verbose = FALSE)
  dir <- withr::local_tempdir()
  expect_message(p <- introGraph(spp, save = TRUE, dpi = 72, dir = dir,
                                 filename = "marty", format = "png"),
                 "Writing 'marty.png'")
  expect_gt(sum(p$data$species), 0)
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "marty.png")))

  p <- introGraph(spp, level = "region", save = TRUE, dpi = 72, dir = dir,
                  filename = "marty_regions", format = "png",
                  verbose = FALSE)
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "marty_regions.png")))
})

test_that("introGraph checks its arguments", {
  expect_error(introGraph(NULL), "should be a data frame")
  expect_error(introGraph(fake_spp[, 1, drop = FALSE]),
               "introduced_to_botanical_countries")
  expect_error(introGraph(fake_spp, level = "country"), "continent")
  expect_error(introGraph(fake_spp, format = "bmp"), "format names")
  expect_error(introGraph(fake_spp[3:4, ]), "None of the species")
  expect_error(introGraph(fake_spp[2, ]),
               "None of the species in the input data is introduced in")
})
