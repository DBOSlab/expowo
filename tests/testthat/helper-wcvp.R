# Tiny WCVP release, with the same file layout as wcvp.zip from Kew, used to
# test the download and preparation of WCVP data offline.
fake_wcvp_zip <- function(dir = tempfile("fakewcvp")) {
  dir.create(dir)

  names_cols <- c("plant_name_id", "ipni_id", "taxon_rank", "taxon_status",
                  "family", "genus_hybrid", "genus", "species_hybrid",
                  "species", "infraspecific_rank", "infraspecies",
                  "parenthetical_author", "primary_author",
                  "publication_author", "place_of_publication",
                  "volume_and_page", "first_published",
                  "nomenclatural_remarks", "geographic_area",
                  "lifeform_description", "climate_description", "taxon_name",
                  "taxon_authors", "accepted_plant_name_id",
                  "basionym_plant_name_id", "replaced_synonym_author",
                  "homotypic_synonym", "parent_plant_name_id", "powo_id",
                  "hybrid_formula", "reviewed")
  row <- function(id, rank, status, genus, species, name, authors, accepted,
                  parent, powo_id, homotypic = "", pub_author = "",
                  place = "Sp. Pl.", vol = " 1: 1", year = "(1753)") {
    x <- stats::setNames(rep("", length(names_cols)), names_cols)
    x[c("plant_name_id", "ipni_id", "taxon_rank", "taxon_status", "family",
        "genus", "species", "publication_author", "place_of_publication",
        "volume_and_page", "first_published", "taxon_name", "taxon_authors",
        "accepted_plant_name_id", "homotypic_synonym",
        "parent_plant_name_id", "powo_id")] <-
      c(id, powo_id, rank, status, "Fakeaceae", genus, species, pub_author,
        place, vol, year, name, authors, accepted, homotypic, parent, powo_id)
    paste(x, collapse = "|")
  }
  wcvp_names <- c(
    paste(names_cols, collapse = "|"),
    row("1", "Genus", "Accepted", "Fakea", "", "Fakea", "L.", "1", "", "1-1"),
    row("2", "Species", "Accepted", "Fakea", "alba", "Fakea alba", "L.", "2",
        "1", "2-1", pub_author = "C.F.P.von Martius (ed.)", vol = ": 5"),
    row("3", "Species", "Synonym", "Fakeus", "albus", "Fakeus albus", "Mill.",
        "2", "", "3-1", homotypic = "T", place = "", vol = "", year = ""),
    row("4", "Species", "Misapplied", "Fakea", "nigra", "Fakea nigra",
        "auct.", "2", "", "4-1"),
    row("5", "Species", "Unplaced", "Fakea", "dubia", "Fakea dubia", "Hort.",
        "", "", "5-1"))

  wcvp_distribution <- c(
    paste("plant_locality_id", "plant_name_id", "continent_code_l1",
          "continent", "region_code_l2", "region", "area_code_l3", "area",
          "introduced", "extinct", "location_doubtful", sep = "|"),
    "1|2|8|S AMERICA|84|Brazil|BZN|Brazil North|0|0|0",
    "2|2|2|AFRICA|23|W-C Trop Africa|ZAI|DR Congo|1|0|0",
    "3|2|1|EUROPE|10|N Europe|GRB|Great Britain|0|1|0",
    "4|2|1|EUROPE|10|N Europe|IRE|Ireland|0|0|1",
    "5|1|8|S AMERICA|84|Brazil|BZN|Brazil North|0|0|0",
    "6|5|8|S AMERICA|84|Brazil|BZS|Brazil South|0|0|0")

  writeLines(wcvp_names, file.path(dir, "wcvp_names.csv"), useBytes = TRUE)
  writeLines(wcvp_distribution, file.path(dir, "wcvp_distribution.csv"))
  zip_file <- file.path(dir, "wcvp.zip")
  utils::zip(zip_file,
             files = file.path(dir, c("wcvp_names.csv",
                                      "wcvp_distribution.csv")),
             flags = "-j -q")
  return(zip_file)
}

# Keep the session copy of WCVP untouched while a test changes the cache
local_wcvp_cache <- function(env = parent.frame()) {
  cache_dir <- tempfile("expowo_cache")
  old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_dir)
  session_wcvp <- expowo:::.expowo_env$wcvp
  rm(list = intersect("wcvp", ls(expowo:::.expowo_env)),
     envir = expowo:::.expowo_env)
  withr::defer({
    if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else
      Sys.setenv(R_USER_CACHE_DIR = old)
    assign("wcvp", session_wcvp, envir = expowo:::.expowo_env)
    unlink(cache_dir, recursive = TRUE)
  }, envir = env)
  return(tools::R_user_dir("expowo", which = "cache"))
}
