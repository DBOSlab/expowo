#' Extracts list of species from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a data frame listing all accepted species and associated
#' geographical distribution, from URI addresses of genera of angiosperm
#' families at [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' powoSpecies(family, genus, uri)
#'
#' @param family Either a single family name or a vector of multiple families
#' that are present in POWO.
#'
#' @param genus Either a single genus name or a vector of multiple genera
#' that are present in POWO.
#'
#' @param uri one or multiple URI addresses for each genus to be searched in POWO.
#'
#' @return Table in data frame format
#'
#' @seealso \code{\link{powoGenera}}
#'
#' @examples
#' \dontrun{
#' powocodes <- taxize::get_pow(c("Araceae", "Lecythidaceae"))
#' powocodes <- data.frame(powocodes)
#' powocodes <- cbind(family = c("Araceae", "Lecythidaceae"), powocodes)
#'
#' resGenera <- powoGenera(powocodes$family, powocodes$uri,
#'                         verbose = TRUE)
#'
#' resSpecies <- powoSpecies(resGenera$family, resGenera$genus, resGenera$powo_uri,
#'                           hybridspp = FALSE,
#'                           verbose = TRUE)
#'
#' write.csv(resSpecies, "powo_genera_list_accepted_spp.csv", row.names=FALSE)
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

powoSpecies <- function(family, genus, uri,
                        hybridspp = FALSE,
                        verbose = TRUE) {

  powo_codes <- data.frame(family = family,
                           genus = genus,
                           uri = uri)

  # POWO search for the number of genera in each family
  powo_genus_uri <- list()
  list_genus <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause 300 seconds of um pause every 500th search,
    # because POWO website cannot permite constant search
    if (i%%500 == 0) {
      Sys.sleep(300)
    }
    # Adding a counter to identify each running search
    if (verbose) {
      print(paste0("Searching spp list of... ",
                   powo_codes$genus[i], " ",
                   powo_codes$family[i], " ", i, "/", length(powo_codes$family)))
    }

    powo_genus_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8", warn = F)

    temp <- grepl("<li><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]", powo_genus_uri[[i]])
    powo_spp_uri <- powo_genus_uri[[i]][temp]

    list_genus[[i]] <- data.frame(temp_spp_uri = powo_spp_uri,
                                  family = powo_codes$family[i],
                                  genus = powo_codes$genus[i],
                                  species = NA,
                                  taxon_name = NA,
                                  authors = NA,
                                  scientific_name = NA,
                                  hybrid = NA,
                                  kew_id = NA,
                                  powo_uri = NA)

    # Filling in each column
    list_genus[[i]][["temp_spp_uri"]] <- gsub(".*<li><a href[=]\"", "", list_genus[[i]][["temp_spp_uri"]])
    list_genus[[i]][["powo_uri"]] <- paste("http://www.plantsoftheworldonline.org", gsub("\".+", "", list_genus[[i]][["temp_spp_uri"]]), sep = "")
    list_genus[[i]][["kew_id"]] <- gsub(".+[:]", "", list_genus[[i]][["powo_uri"]])

    list_genus[[i]][["species"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "", list_genus[[i]][["temp_spp_uri"]])
    list_genus[[i]][["species"]] <- gsub(".*\\s", "", list_genus[[i]][["species"]])

    list_genus[[i]][["authors"]] <- gsub(".*em>", "", list_genus[[i]][["temp_spp_uri"]])
    list_genus[[i]][["authors"]] <- gsub("<.*", "", list_genus[[i]][["authors"]])
    list_genus[[i]][["authors"]] <- gsub("^\\s", "", list_genus[[i]][["authors"]])
    list_genus[[i]][["taxon_name"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "", list_genus[[i]][["temp_spp_uri"]])
    list_genus[[i]][["taxon_name"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "", list_genus[[i]][["temp_spp_uri"]])
    list_genus[[i]][["scientific_name"]] <- paste(list_genus[[i]][["taxon_name"]], list_genus[[i]][["authors"]])

    # Select specific columns of interest

    list_genus[[i]] <- list_genus[[i]] %>% select("family", "genus", "species",
                                                  "taxon_name","authors",
                                                  "scientific_name","hybrid",
                                                  "kew_id", "powo_uri")

    # Remove any possible generic synomym from  the retrieved list
    list_genus[[i]] <- list_genus[[i]][grepl("\\s", list_genus[[i]]$taxon_name), ]

    tf <- grepl("[+]|\\s×\\s", list_genus[[i]]$taxon_name)
    list_genus[[i]]$hybrid[tf] <- "yes"
    list_genus[[i]]$hybrid[!tf] <- "no"

    if (hybridspp == FALSE) {
      list_genus[[i]] <- list_genus[[i]] %>% filter(hybrid == "no") %>% select(-"hybrid")
    }

  }
  names(list_genus) <- powo_codes$genus

  # Combining all dataframes from the list of each family search
  df <- list_genus[[1]]
  for (i in 2:length(list_genus)) {
    df <- rbind(df, list_genus[[i]])
  }

  # Extract distribution using auxiliary function getDist
  df <- getDist(df,
                listspp = FALSE,
                verbose = verbose)


  # Select specific columns of interest
  if (hybridspp == FALSE) {
    df <- df %>% select("family",
                        "genus",
                        "species",
                        "taxon_name",
                        "authors",
                        "scientific_name",
                        "publication",
                        "native_to_country",
                        "native_to_botanical_countries",
                        "introduced_to_country",
                        "introduced_to_botanical_countries",
                        "kew_id",
                        "powo_uri")
  } else {
    df <- df %>% select("family",
                        "genus",
                        "species",
                        "taxon_name",
                        "authors",
                        "scientific_name",
                        "publication",
                        "hybrid",
                        "native_to_country",
                        "native_to_botanical_countries",
                        "introduced_to_country",
                        "introduced_to_botanical_countries",
                        "kew_id",
                        "powo_uri")

  }

  return(df)
}

