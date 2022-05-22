#' Get list of species from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing all accepted species and
#' associated geographical distribution from any target genus or family of
#' flowering plants at
#' [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' powoSpecies(family, uri, genus = NULL, hybridspp = FALSE, country = NULL,
#'             verbose = TRUE, save = TRUE, dir, filename)
#'
#' @param family Either a single family name or a vector of multiple families
#' that are present in POWO.
#'
#' @param uri URI address for each family associated to the target genus to be
#' searched in POWO.
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
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **output**.
#'
#' @return Table in .csv format and saves the output on disk.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#' library(taxize)
#'
#' fam <- c("Araceae", "Lecythidaceae")
#' powocodes <- cbind(family = fam,
#'                    data.frame(taxize::get_pow(fam)))
#'
#' powoSpecies(powocodes$family, powocodes$uri,
#'             hybridspp = FALSE,
#'             country = c("Argentina", "Brazil", "French Guiana"),
#'             verbose = TRUE,
#'             save = TRUE,
#'             dir = "results_powoSpecies/",
#'             filename = "Araceae_Lecythidaceae")
#'
#' ## Searching for all species and associated global distribution
#' ## in any or all flowering plant genera, by using the URI addresses
#' ## within the POWOcodes data file
#'
#' data(POWOcodes)
#'
#' powoSpecies(POWOcodes$family, POWOcodes$uri,
#'             hybridspp = TRUE,
#'             verbose = TRUE,
#'             save = TRUE,
#'             dir = "results_powoSpecies/",
#'             filename = "all_angiosperm_species")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#'
#' @export
#'

powoSpecies <- function(family, uri,
                        genus = NULL,
                        hybridspp = FALSE,
                        country = NULL,
                        verbose = TRUE,
                        save = TRUE,
                        dir = "results_powoSpecies/",
                        filename = "output") {

  if(length(family) != length(uri)) {
    stop(paste("Any family or URI is missing."))
  }

  data("POWOcodes")
  uri_log <- uri %in% POWOcodes$uri
  uri_log <- which(uri_log == FALSE)
  if(length(uri_log) >= 1) {
    stop(paste("Any family's URI address is incomplete or misspelled and cannot
               open connection with POWO website."))
  }

  powo_codes_fam <- data.frame(family = family,
                               uri = uri)

  # POWO search for the genus URI in each family using auxiliary function
  # getGenURI
  resGenera <- getGenURI(powo_codes_fam,
                         genus = genus,
                         verbose = verbose)

  powo_codes <- data.frame(family = resGenera$family,
                           genus = resGenera$genus,
                           uri = resGenera$powo_uri)

  # POWO search for the list of accepted species in each genus of flowering
  # plants
  powo_genus_uri <- list()
  list_genus <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause 300 seconds of um pause every 500th search,
    # because POWO website may crash when searching uninterruptedly
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

    temp<-grepl("<li><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]",
                powo_genus_uri[[i]])
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
    list_genus[[i]][["temp_spp_uri"]] <- gsub(".*<li><a href[=]\"", "",
                                              list_genus[[i]][["temp_spp_uri"]])
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

    # Select specific columns of interest
    list_genus[[i]] <- list_genus[[i]] %>% select("family", "genus", "species",
                                                  "taxon_name","authors",
                                                  "scientific_name","hybrid",
                                                  "kew_id", "powo_uri")

    # Remove any possible generic synomym from  the retrieved list
    list_genus[[i]] <- list_genus[[i]][grepl("\\s",
                                             list_genus[[i]]$taxon_name), ]

    tf <- grepl("[+]|\\s×\\s", list_genus[[i]]$taxon_name)
    list_genus[[i]]$hybrid[tf] <- "yes"
    list_genus[[i]]$hybrid[!tf] <- "no"

    if (hybridspp == FALSE) {
      list_genus[[i]] <- list_genus[[i]] %>% filter(hybrid == "no") %>%
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
  # country vector
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
    # subsetted according to the provided country vector
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

    # Subset the searched genera according to the country vector
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

  if (save) {
    # Create a new directory to save the results with current date
    if (!dir.exists(dir)) {
      dir.create(dir)
      todaydate <- format(Sys.time(), "%d %b %Y")
      folder_name <- paste0(dir, gsub(" ", "", todaydate))
      print(paste0("Writing '", folder_name, "' on disk."))
      dir.create(folder_name) # If there is no directory... make one!
    } else {
      # If directory was created during a previous search, get its name to save
      # results
      folder_name <- paste0(dir, gsub(" ", "", format(Sys.time(), "%d %b %Y")))
    }
    # Create and save the spreadsheet in .csv format
    fullname <- paste0(folder_name, "/", filename, ".csv")
    print(paste0("Writing the spreadsheet '", filename, ".csv' on disk."))
    data.table::fwrite(df,
                       file = fullname,
                       sep = ",",
                       row.names = FALSE,
                       col.names = TRUE)
  }

  return(df)

}

