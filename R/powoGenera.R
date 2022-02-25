#' Extracts list of genera from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a data frame listing all genera, and associated accepted
#' species and geographical distribution, from URI addresses of angiosperm
#' families at [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' powoGenera(family, uri)
#'
#' @param family Either a single family name or a vector of multiple families
#' that are present in POWO.
#'
#' @param uri one or multiple URI addresses for each family to be searched in POWO.
#'
#' @return Table in data frame format.
#'
#' @seealso \code{\link{powoSpecies}}
#'
#' @examples
#' \dontrun{
#' powocodes <- taxize::get_pow(c("Fabaceae", "Lecythidaceae"))
#' powocodes <- data.frame(powocodes)
#' powocodes <- cbind(family = c("Fabaceae", "Lecythidaceae"), powocodes)
#'
#' resGenera <- powoGenera(powocodes$family, powocodes$uri,
#'                         verbose = TRUE)
#'
#' write.csv(resGenera, "powo_genera_accepted_number_spp.csv", row.names=FALSE)
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#@importFrom progress progress_bar
#'
#' @export
#'

powoGenera <- function(family, uri,
                       verbose = TRUE) {

  powo_codes <- data.frame(family = family,
                           uri = uri)

  # POWO search for the number of genera in each family
  powo_fams_uri <- list()
  list_fams <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause 300 seconds of um pause every 500th search,
    # because POWO website cannot permit constant search
    if (i%%500 == 0) {
      Sys.sleep(300)
    }
    # Adding a counter to identify each running search
    if (verbose) {
    print(paste0("Searching... ",
                 powo_codes$family[i], " ", i, "/", length(powo_codes$family)))
    }

    powo_fams_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8", warn = F)

    temp <- grepl("<li><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]", powo_fams_uri[[i]])
    powo_genus_uri <- powo_fams_uri[[i]][temp]
    temp <- !grepl("aceae|oideae|meta\\sproperty|meta\\sname|Compositae|Cruciferae|Gramineae|Guttiferae|Labiatae|Leguminosae|Palmae|Umbelliferae", powo_genus_uri)

    list_fams[[i]] <- data.frame(temp_genus_uri = powo_genus_uri[temp],
                                 family = powo_codes$family[i],
                                 genus = NA,
                                 authors = NA,
                                 genus_author = NA,
                                 powo_uri = NA)

    # Filling in each column
    list_fams[[i]][["temp_genus_uri"]] <- gsub(".*<li><a href[=]\"", "", list_fams[[i]][["temp_genus_uri"]])
    list_fams[[i]][["powo_uri"]] <- paste("http://www.plantsoftheworldonline.org", gsub("\".+", "", list_fams[[i]][["temp_genus_uri"]]), sep = "")

    list_fams[[i]][["authors"]] <- gsub(".*em>", "", list_fams[[i]][["temp_genus_uri"]])
    list_fams[[i]][["authors"]] <- gsub("<.*", "", list_fams[[i]][["authors"]])
    list_fams[[i]][["authors"]] <- gsub("^\\s", "", list_fams[[i]][["authors"]])
    list_fams[[i]][["genus"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "", list_fams[[i]][["temp_genus_uri"]])
    list_fams[[i]][["genus_author"]] <- paste(list_fams[[i]][["genus"]], list_fams[[i]][["authors"]])

    # Select specific columns of interest
    list_fams[[i]] <- list_fams[[i]] %>% select("family", "genus", "authors", "genus_author", "powo_uri")

  }
  names(list_fams) <- powo_codes$family

  # Combining all dataframes from the list of each family search
  if (length(list_fams) == 1) {
    df <- list_fams[[1]]
  } else {
    df <- list_fams[[1]]
    for (i in 2:length(list_fams)) {
      df <- rbind(df, list_fams[[i]])
    }
  }


  # Extract number of species and distribution using auxiliary function getDist
  df <- getDist(df,
                listspp = TRUE,
                verbose = verbose)


  # Select specific columns of interest
  df <- df %>% select("family",
                      "genus",
                      "authors",
                      "genus_author",
                      "publication",
                      "no_species",
                      "native_to_country",
                      "native_to_botanical_countries",
                      "introduced_to_country",
                      "introduced_to_botanical_countries",
                      "powo_uri")



  return(df)
}


