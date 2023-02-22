#' Extract list of species from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing all accepted species and
#' associated geographical distribution from any target genus or family of
#' flowering plants at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoSpecies(family, genus = NULL, hybridspp = FALSE, country = NULL,
#'             verbose = TRUE, save = FALSE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families
#' that is present in POWO.
#'
#' @param genus Either one genus name or a vector of multiple genera
#' that are present in POWO. If any genus name is not provided, then the
#' function will search any species from all accepted genera known for the
#' target family.
#'
#' @param hybridspp Logical, if \code{TRUE}, the search results will include
#' hybrid species.
#'
#' @param country Either one country name or a vector of multiple countries.
#' If country names are provided, then the function will return only the species
#' that are native to such countries, according to POWO.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be
#' printed in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the search results will not be saved
#' on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoSpecies** and the search
#' results will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. The default is to
#' create a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{powoGenera}}
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' powoSpecies(family = "Martyniaceae",
#'             hybridspp = FALSE,
#'             country = c("Argentina", "Brazil", "French Guiana"),
#'             verbose = TRUE,
#'             save = FALSE,
#'             dir = "results_powoSpecies/",
#'             filename = "Martyniaceae_spp")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

powoSpecies <- function(family,
                        genus = NULL,
                        hybridspp = FALSE,
                        country = NULL,
                        verbose = TRUE,
                        save = FALSE,
                        dir = "results_powoSpecies/",
                        filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # dir check
  dir <- .arg_check_dir(dir)

  # Extracting the uri of each plant family using associated data POWOcodes
  utils::data("POWOcodes", package = "expowo")
  powo_codes_fam <- dplyr::filter(POWOcodes, family %in% .env$family)

  # POWO search for the genus URI in each family using auxiliary function
  # getGenURI.
  resGenera <- getGenURI(powo_codes_fam,
                         genus = genus,
                         verbose = verbose)

  powo_codes <- data.frame(family = resGenera$family,
                           genus = resGenera$genus,
                           uri = resGenera$powo_uri)
  # POWO search for the list of accepted species in each genus of flowering
  # plants.
  powo_genus_uri <- list()
  list_genus <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause 300 seconds of um pause every 500th search,
    # because POWO website may crash when searching uninterruptedly.
    if (i%%500 == 0) {
      Sys.sleep(300)
    }
    # Adding a counter to identify each running search
    if (verbose) {
      print(paste0("Searching spp list of... ",
                   powo_codes$genus[i], " ",
                   powo_codes$family[i], " ",
                   i, "/", length(powo_codes$family)))
    }

    powo_genus_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8",
                                     warn = F)

    temp <-
      grepl("><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]",
                  powo_genus_uri[[i]])
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
                                  hybrid = NA,
                                  kew_id = NA,
                                  powo_uri = NA)

    if (!"unknown" %in% powo_spp_uri) {

      # Filling in each column
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



      # Remove any possible generic synonym from the retrieved list
      list_genus[[i]] <- list_genus[[i]][grepl("\\s",
                                               list_genus[[i]]$taxon_name), ]
    }

    # Select specific columns of interest
    list_genus[[i]] <- list_genus[[i]] %>% select("family", "genus", "species",
                                                  "taxon_name","authors",
                                                  "scientific_name","hybrid",
                                                  "kew_id", "powo_uri")

    # Replace hybrid symbol
    list_genus[[i]]$taxon_name <- gsub("\u00D7", "x",
                                       list_genus[[i]]$taxon_name)

    # Identify hybrid species
    tf <- grepl("[+]|\\sx\\s", list_genus[[i]]$taxon_name)
    list_genus[[i]]$hybrid[tf] <- "yes"
    list_genus[[i]]$hybrid[!tf] <- "no"

    if (hybridspp == FALSE) {
      list_genus[[i]] <- list_genus[[i]] %>%
        filter(list_genus[[i]]$hybrid == "no") %>%
        select(-"hybrid")
    }

  }
  names(list_genus) <- powo_codes$genus

  # Combining all dataframes from the list of each family/genus search
  df <- list_genus[[1]]
  if (length(list_genus) > 1) {
    for (i in 2:length(list_genus)) {
      df <- rbind(df, list_genus[[i]])
    }
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

  # If a vector of country names is provided, then remove any species that do
  # not occur in the given country. The temp vector is logical (TRUE or FALSE)
  # and shows which species/row should be kept in the search given the provided
  # country vector.
  if (!is.null(country)) {
    temp <- vector()
    for (i in seq_along(df$native_to_country)) {
      tt <- gsub("^\\s", "",
                 strsplit(df$native_to_country[i], ",")[[1]]) %in% country
      if (any(tt)) {
        temp[i] <- TRUE
      } else {
        temp[i] <- FALSE
      }

    }

    # The following conditions is just to show/print how the df will be
    # subsetted according to the provided country vector.
    if (verbose) {
      if (any(temp)) {
        tl <- list()
        for (i in seq_along(country)) {
          tv <- vector()
          for (l in seq_along(df$native_to_country)) {
            tv[l] <-
              country[i] %in% gsub("^\\s", "",
                                   strsplit(df$native_to_country[l], ",")[[1]])
          }
          if (length(which(tv == TRUE)) == 0) {
            tl[[i]] <- FALSE
          }
          if (length(which(tv == TRUE)) != 0 &
              length(which(tv == TRUE)) < length(tv)) {
            tl[[i]] <- TRUE
          }
          if (length(which(tv == TRUE)) == length(tv)) {
            tl[[i]] <- TRUE
          }
        }
        cv <- country[unlist(tl)]

        if (length(country[country %in% cv]) != length(country)) {
          cat(paste("Your search returned species with distribution only in the
                    following countries:\n", "\n",

                    paste(country[country %in% cv], collapse = ", "), "\n",
                    "\n",

                    "There is no species occurring in the countries below:\n",
                    "\n",

                    paste(country[!country %in% cv], collapse = ", "), "\n",
                    "\n",

                    "Check whether any species does not occur in the countries
                    above either because:\n",
                    "1. The species indeed does not occur in the provided
                    country vector;\n",
                    "2. The country name is written with any typo;\n",
                    "3. Any country name in the country vector is not written
                    in English language.\n", "\n"))
        }

      } else {
        cat(paste("Your search returned an empty data frame either because:\n",
                  "1. No species occurs in the provided country vector;\n",
                  "2. The country vector has any typo;\n",
                  "3. Any country name in the country vector is not written in
                  English language."))
      }
    }

    # Subset the searched genera according to the country vector.
    if (verbose) {
      if(length(df$genus[temp]) != length(temp)) {
        cat(paste("Genera listed below were removed from the original search
                  because they are not native to any of the given country
                  vector:\n", "\n",
                  df$genus[!temp]))
      }
    }
    df <- df[temp, ]

  }

  # Saving the dataframe if param save is TRUE.
  .save_df(save, dir, filename, df)

  return(df)

}

