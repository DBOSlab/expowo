#' Extract list of species distribution from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing the geographical distribution
#' of all target species of flowering plants available at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoSpDist(family, species = NULL,
#'             verbose = TRUE, save = FALSE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families
#' that is present in POWO.
#'
#' @param species Either one non-hybrid species name or a vector of multiple
#' species that are present in POWO. If any species name is not provided, then
#' the function will search any species from all accepted genera known for the
#' target family.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be
#' printed in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the search results will not be saved
#' on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoSpDist** and the search
#' results will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#'  a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' powoSpDist(family = "Lecythidaceae",
#'            species = "Lecythis pisonis",
#'            verbose = TRUE,
#'            save = FALSE,
#'            dir = "results_powoSpDist/",
#'            filename = "L_pisonis_distribution")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

powoSpDist <- function(family,
                       species = NULL,
                       verbose = TRUE,
                       save = FALSE,
                       dir = "results_powoSpDist/",
                       filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # dir check.
  dir <- .arg_check_dir(dir)

  # Extracting the uri of each plant family using associated data POWOcodes
  utils::data("POWOcodes", package = "expowo")
  powo_codes_fam <- dplyr::filter(POWOcodes, family %in% .env$family)

  # Extracting the genus name and POWO search for the genus URI in each family
  # using auxiliary function getGenURI.
  genus <- unique(gsub("\\s.+", "", species))
  resGenera <- getGenURI(powo_codes_fam,
                         genus = genus,
                         verbose = verbose)

  powo_codes <- data.frame(family = resGenera$family,
                           genus = resGenera$genus,
                           uri = resGenera$powo_uri)
  # POWO search for the species or species list in each genus of flowering
  # plants.
  powo_genus_uri <- list()
  list_genus <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause 300 seconds of um pause every 500th search,
    # because POWO website may crash when searching uninterruptedly.
    if (i%%500 == 0) {
      Sys.sleep(300)
    }
    # Adding a counter to identify each running search.
    if (verbose) {
      print(paste0("Searching spp list of... ",
                   powo_codes$genus[i], " ",
                   powo_codes$family[i], " ",
                   i, "/", length(powo_codes$family)))
    }

    powo_genus_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8",
                                     warn = F)
    url_ext <- "><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]"
    temp <- grepl(url_ext, powo_genus_uri[[i]])
    powo_spp_uri <- powo_genus_uri[[i]][temp]

    if (length(powo_spp_uri) == 0) {
      powo_spp_uri = "unknown"
    }

    list_genus[[i]] <- data.frame(temp_spp_uri = powo_spp_uri,
                                  family = powo_codes$family[i],
                                  genus = powo_codes$genus[i],
                                  species = NA,
                                  taxon_name = NA,
                                  authors = NA,
                                  scientific_name = NA,
                                  kew_id = NA,
                                  powo_uri = NA)

    if (!"unknown" %in% powo_spp_uri) {

      # Filling in each column.
      list_genus[[i]][["temp_spp_uri"]] <-
        gsub(".*><a href[=]\"", "", list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["powo_uri"]] <-
        paste("http://www.plantsoftheworldonline.org",
              gsub("\".+", "", list_genus[[i]][["temp_spp_uri"]]), sep = "")
      list_genus[[i]][["kew_id"]] <-
        gsub(".+[:]", "", list_genus[[i]][["powo_uri"]])

      list_genus[[i]][["species"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                           list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["species"]] <- gsub(".*\\s", "",
                                           list_genus[[i]][["species"]])

      list_genus[[i]][["authors"]] <- gsub(".*em>", "",
                                           list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["authors"]] <- gsub("<.*", "",
                                           list_genus[[i]][["authors"]])
      list_genus[[i]][["authors"]] <- gsub("^\\s", "",
                                           list_genus[[i]][["authors"]])
      list_genus[[i]][["taxon_name"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                              list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["taxon_name"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                              list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["scientific_name"]] <-
        paste(list_genus[[i]][["taxon_name"]], list_genus[[i]][["authors"]])



      # Remove any possible generic synonym from the retrieved list.
      list_genus[[i]] <- list_genus[[i]][grepl("\\s",
                                               list_genus[[i]]$taxon_name), ]
    }

    # Select specific columns of interest.
    list_genus[[i]] <- list_genus[[i]] %>% select("family", "genus", "species",
                                                  "taxon_name","authors",
                                                  "scientific_name",
                                                  "kew_id", "powo_uri")

  }
  names(list_genus) <- powo_codes$genus

  # Combining all dataframes from the list of each search.
  df <- list_genus[[1]]
  if (length(list_genus) > 1) {
    for (i in 2:length(list_genus)) {
      df <- rbind(df, list_genus[[i]])
    }
  }

  df <- df[df$taxon_name %in% species, ]

  # Extract distribution using auxiliary function getDist.
  df <- getDist(df,
                listspp = FALSE,
                verbose = verbose)

  # Select specific columns of interest.
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


  # Saving the dataframe if param save is TRUE.
  .save_df(save, dir, filename, df)

  return(df)

}

