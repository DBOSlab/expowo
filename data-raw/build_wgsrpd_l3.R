# Builds the internal table of WGSRPD level-3 areas (R/sysdata.rda), with the
# area names exactly as expowo returns them (legacy POWO names, in ASCII) and
# their continent. Run from the package folder:
#   Rscript data-raw/build_wgsrpd_l3.R [path/to/wcvp_distribution.csv]
# Without a path, WCVP is downloaded from Kew.

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  dir <- tempfile("wcvp")
  dir.create(dir)
  zip_file <- file.path(dir, "wcvp.zip")
  options(timeout = 1800)
  utils::download.file(expowo:::.wcvp_url, zip_file, mode = "wb")
  utils::unzip(zip_file, files = "wcvp_distribution.csv", exdir = dir)
  args <- file.path(dir, "wcvp_distribution.csv")
}

dist <- data.table::fread(args[1], sep = "|", quote = "", encoding = "UTF-8",
                          na.strings = "",
                          select = c("continent_code_l1", "continent",
                                     "region_code_l2", "region",
                                     "area_code_l3", "area"),
                          data.table = FALSE)
l3 <- unique(dist[!is.na(dist$area_code_l3), ])
l3 <- l3[order(l3$area_code_l3), ]

# Same naming as .wcvp_download and .wcvp_dist
tf <- l3$area_code_l3 %in% names(expowo:::.wcvp_legacy_areas)
l3$area[tf] <- expowo:::.wcvp_legacy_areas[l3$area_code_l3[tf]]
l3$area <- iconv(l3$area, from = "UTF-8", to = "ASCII//TRANSLIT")
stopifnot(!anyNA(l3$area), !any(grepl("['\"`^~]", l3$area)),
          !anyDuplicated(l3$area_code_l3))

continents <- c("1" = "Europe", "2" = "Africa", "3" = "Temperate Asia",
                "4" = "Tropical Asia", "5" = "Australasia", "6" = "Pacific",
                "7" = "Northern America", "8" = "Southern America",
                "9" = "Antarctic")

wgsrpd_l3 <- data.frame(code = l3$area_code_l3,
                        botanical_country = l3$area,
                        region = l3$region,
                        continent = unname(continents[
                          as.character(l3$continent_code_l1)]),
                        stringsAsFactors = FALSE)

usethis::use_data(wgsrpd_l3, internal = TRUE, overwrite = TRUE)
message(nrow(wgsrpd_l3), " level-3 areas saved in R/sysdata.rda")
