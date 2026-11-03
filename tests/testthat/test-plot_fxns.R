test_that("the level-3 table names every botanical country returned by expowo", {
  l3 <- expowo:::wgsrpd_l3
  expect_equal(nrow(l3), 368)
  expect_false(anyDuplicated(l3$code) > 0)
  expect_false(anyDuplicated(l3$botanical_country) > 0)
  expect_true(all(grepl("^[ -~]+$", l3$botanical_country)))
  expect_setequal(unique(l3$continent),
                  c("Europe", "Africa", "Temperate Asia", "Tropical Asia",
                    "Australasia", "Pacific", "Northern America",
                    "Southern America", "Antarctic"))

  spp <- powoSpecies(c("Fabaceae", "Orchidaceae"), verbose = FALSE)
  areas <- unique(unlist(expowo:::.split_areas(
    c(spp$native_to_botanical_countries,
      spp$introduced_to_botanical_countries))))
  expect_true(all(areas %in% l3$botanical_country))
})

test_that(".pub_year extracts the year of publication", {
  expect_equal(expowo:::.pub_year(c("Sp. Pl.: 618 (1753)",
                                    "Bull. Soc. 6: 191 (1914 publ. 1915)",
                                    "unknown publication", NA)),
               c(1753, 1914, NA, NA))
})

test_that(".split_areas drops unknown and empty areas", {
  expect_equal(expowo:::.split_areas(c("Brazil North, Peru", "unknown", NA)),
               list(c("Brazil North", "Peru"), character(0), character(0)))
})

test_that(".accepted_only keeps accepted names only when there is a status", {
  df <- data.frame(status = c("Accepted", "Homotypic Synonym"), x = 1:2)
  expect_equal(expowo:::.accepted_only(df)$x, 1)
  expect_equal(nrow(expowo:::.accepted_only(df[, "x", drop = FALSE])), 2)
})
