# Tiny species list: Fakea alba was described in 1805 as Fakeus albus
fake_spp <- data.frame(
  status = c("Accepted", "Homotypic Synonym", "Heterotypic Synonym",
             "Accepted", "Accepted"),
  scientific_name = c("Fakea alba (Mill.) L.", "Fakeus albus Mill.",
                      "Fakea nivea L.", "Fakea nigra L.", "Fakea rubra L."),
  accepted_name = c(NA, "Fakea alba (Mill.) L.", "Fakea alba (Mill.) L.",
                    NA, NA),
  publication = c("Sp. Pl.: 1 (2001)", "Gard. Dict.: 2 (1805)",
                  "Sp. Pl.: 3 (1912)", "Sp. Pl.: 4 (1803)",
                  "unknown publication"))

test_that("discoveryGraph counts species by the year of their basionym", {
  p <- discoveryGraph(fake_spp, verbose = FALSE)
  expect_s3_class(p, "ggplot")

  new_spp <- p$data[p$data$what == levels(p$data$what)[1], ]
  synonyms <- p$data[p$data$what == levels(p$data$what)[2], ]
  # Fakea alba counts in the 1800s (its basionym), not in the 2000s
  expect_equal(new_spp$period, 1800)
  expect_equal(new_spp$names, 2)
  expect_equal(synonyms$period, c(1800, 1910))
  expect_equal(synonyms$names, c(1, 1))
})

test_that("discoveryGraph groups names in periods of any length", {
  expect_message(p <- discoveryGraph(fake_spp, bin = 50), "3 accepted species")
  expect_equal(unique(p$data$period), c(1800, 1900))
  expect_equal(p$labels$y, "Names per 50 years")
  expect_equal(p$layers[[2]]$data$label, c("1800-1849: 2", "1800-1849: 1"))
})

test_that("discoveryGraph works with WCVP data and saves the graphic", {
  spp <- powoSpecies("Martyniaceae", synonyms = TRUE, verbose = FALSE)
  dir <- withr::local_tempdir()
  expect_message(p <- discoveryGraph(spp, save = TRUE, dpi = 72, dir = dir,
                                     filename = "marty", format = "png"),
                 "Writing 'marty.png'")
  expect_equal(sum(p$data$names[p$data$what == levels(p$data$what)[1]]),
               sum(spp$status == "Accepted"))
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"),
                                    "marty.png")))
})

test_that("discoveryGraph checks its arguments", {
  expect_error(discoveryGraph(NULL), "should be a data frame")
  expect_error(discoveryGraph(fake_spp[, -1]),
               "synonyms = TRUE")
  expect_error(discoveryGraph(fake_spp, bin = 0), "number of years")
  expect_error(discoveryGraph(fake_spp, bin = "10"), "number of years")
  expect_error(discoveryGraph(fake_spp, format = "bmp"), "format names")

  no_years <- fake_spp
  no_years$publication <- "unknown publication"
  expect_error(discoveryGraph(no_years, verbose = FALSE),
               "No year of publication")
})
