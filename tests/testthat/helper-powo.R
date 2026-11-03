# Offline copies of POWO source pages used to test the POWO mining functions
# (source = "powo"). The pages are built here and written to a temporary
# folder, each file named after the IPNI id at the end of the taxon URI. They
# reproduce the HTML patterns that the POWO mining functions look for.

.powo_link <- function(id, name, authors) {
  paste0("<li class=\"x\"><a href=\"/taxon/urn:lsid:ipni.org:names:", id,
         "\"><em lang='la'>", name, "</em> ", authors, "</a></li>")
}

.powo_taxon <- function(native = NULL, introduced = NULL, publication = NULL,
                        includes = NULL, synonyms = NULL, links = NULL) {
  c("<html><body>",
    if (!is.null(includes)) {
      paste0("<p>Includes ", includes, " Accepted Species</p>")
    },
    if (!is.null(native)) {
      paste0("<h3>Native to:</h3>  <p class=\"p\">  ", native, "  </p>")
    },
    if (!is.null(introduced)) {
      paste0("<h3>Introduced into:</h3>  <p class=\"p\">  ", introduced,
             "  </p>")
    },
    if (!is.null(publication)) {
      paste0("<div>First published in ", publication,
             "  </div><div>end</div>")
    },
    synonyms,
    links,
    "</body></html>")
}

.powo_synonyms <- function(homotypic, heterotypic) {
  li <- function(x) {
    paste0("<li><a class=\"p-regular teal-link\" ",
           "href=\"/taxon/urn:lsid:ipni.org:names:", x[1], "\">",
           "<em lang='la'>", x[2], "</em> ", x[3], "</a> in ",
           "<span class='p-s'>", x[4], "</span></li>")
  }
  c(paste0("<p>Has ", length(homotypic) + length(heterotypic),
           " Synonyms</p>"),
    "<div class=\"taxon-section__content\">",
    paste0("<h4 class=\"heading-4\">Homotypic Synonyms</h4><ul>",
           paste(vapply(homotypic, li, ""), collapse = ""), "</ul>"),
    paste0("<h4 class=\"heading-4\">Heterotypic Synonyms</h4><ul>",
           paste(vapply(heterotypic, li, ""), collapse = "")),
    "  </ul>",
    "  </div><div>footer</div>")
}

.powo_pages <- function() {
  list(
    # Martyniaceae family page
    "77126797-1" = c(
      "<html><body>",
      .powo_link("77126797-1", "Martyniaceae", "Horan."),
      .powo_link("90001-1", "Craniolaria", "L."),
      .powo_link("90002-1", "Ibicella", "(Stapf) Van Eselt."),
      .powo_link("90003-1", "× Martynella", "Hort."),
      "</body></html>"),
    # Cabombaceae page pointing to an accepted family
    "77126578-1" = c(
      "<html><body>",
      "<p>    This is a synonym of</p>",
      paste0("<p class=\"x\"><a href=\"/taxon/urn:lsid:ipni.org:names:",
             "77999999-1\"><em lang='la'>Fakeaceae</em> Fake</a></p>"),
      "</body></html>"),
    "77999999-1" = c("<html><body>",
                     .powo_link("90004-1", "Fakegenus", "Fake"),
                     "</body></html>"),
    # Genus pages
    "90001-1" = .powo_taxon(
      native = "Argentina Northeast, Brazil South, Colombia, Zaire",
      introduced = "Great Britain",
      publication = "Sp. Pl.: 618 (1753)",
      includes = 3,
      links = c(.powo_link("90001-1", "Craniolaria", "L."),
                .powo_link("91001-1", "Craniolaria annua", "L."),
                .powo_link("91002-1", "Craniolaria integrifolia", "Cham."),
                .powo_link("91003-1", "Craniolaria × hybrida", "Hort."))),
    "90002-1" = .powo_taxon(
      native = "Argentina Northeast, Brazil South",
      introduced = "KwaZulu-Natal",
      publication = paste("Techn. Bull. New York Agric. Exp. Sta., Geneva",
                          "149: 31 (1929)"),
      includes = 2,
      links = c(.powo_link("91004-1", "Ibicella lutea", "(Lindl.) Van Eselt."),
                .powo_link("91005-1", "Ibicella parodii", "Van Eselt."))),
    "90003-1" = "<html><body><p>No data</p></body></html>",
    "90004-1" = .powo_taxon(
      native = "New Caledonia",
      publication = "Fake J. 1: 1 (2000)",
      includes = 1,
      links = .powo_link("91006-1", "Fakegenus fakeus", "Fake")),
    # Species pages
    "91001-1" = .powo_taxon(
      native = "Colombia, Venezuela",
      publication = "Sp. Pl.: 618 (1753)",
      synonyms = .powo_synonyms(
        homotypic = list(c("92001-1", "Martynia annua", "L.",
                           "Sp. Pl.: 618 (1753)")),
        heterotypic = list(c("92002-1", "Martynia diandra", "Glox.",
                             "Obs. Bot.: 14 (1785)")))),
    "91002-1" = .powo_taxon(native = "Argentina Northeast, Brazil South, Zaire",
                            introduced = "Great Britain",
                            publication = "Linnaea 7: 725 (1832)"),
    "91003-1" = .powo_taxon(native = "Brazil North",
                            publication = "Hort. J. 2: 3 (1900)"),
    "91004-1" = .powo_taxon(
      native = "Argentina Northeast, Brazil South",
      introduced = "KwaZulu-Natal",
      publication = "A.B.Rendle &amp; al., Cat. Pl. Oban: 35 (1913)"),
    # 91005-1 has no page on purpose, to test a failed connection to POWO
    "91006-1" = .powo_taxon(native = "New Caledonia",
                            publication = "Fake J. 1: 2 (2000)")
  )
}

# Folder with the POWO pages, written once per test run
powo_pages_dir <- function() {
  dir <- file.path(tempdir(), "expowo_powo_pages")
  if (!dir.exists(dir)) {
    dir.create(dir)
    pages <- .powo_pages()
    # Write the UTF-8 bytes as they are, whatever the locale (e.g. the
    # multiplication sign of hybrid names)
    for (id in names(pages)) {
      writeLines(enc2utf8(pages[[id]]), file.path(dir, paste0(id, ".html")),
                 useBytes = TRUE)
    }
  }
  return(dir)
}

fake_powo_readLines <- function(con, ...) {
  id <- sub(".*[:]", "", con)
  page <- file.path(powo_pages_dir(), paste0(id, ".html"))
  # Pages missing on purpose simulate a failed connection to POWO
  if (!file.exists(page)) {
    stop(paste0("cannot open the connection to '", con, "'"))
  }
  readLines(page, encoding = "UTF-8", warn = FALSE)
}

local_powo_pages <- function(env = parent.frame()) {
  local_mocked_bindings(.powo_readLines = fake_powo_readLines,
                        .package = "expowo",
                        .env = env)
}
