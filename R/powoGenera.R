#' Extracts list of genera from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a data frame listing all genera with associated number
#' of accepted species and geographical distribution, from URI addresses of angiosperm
#' families at [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' powoGenera(family, uri, genus, country)
#'
#' @param family Either a single family name or a vector of multiple families
#' that are present in POWO.
#'
#' @param uri one or multiple URI addresses for each family to be searched in POWO.
#'
#' @param genus Either a single genus name or a vector of multiple genera
#' that are present in POWO. If you do not provide any genus name, then the function
#' will search all accepted genera known for the family names provided.
#'
#' @param country Either a single country name or a vector of multiple countries.
#' If you provide any country name, then the function will return only the genera
#' with native distribution known to any of the provided country names, as available
#' in POWO.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be printed
#' in the console in full.
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
#'
#' This time providing a vector with the genus of interest
#' resGenera <- powoGenera(powocodes$family, powocodes$uri,
#'                         genus = c("Luetzelburgia", "Bertholletia"),
#'                         country = c("Argentina", "Brazil", "French Guiana"),
#'                         verbose = TRUE)
#'
#' write.csv(resGenera, "powo_genera_accepted_number_spp.csv", row.names=FALSE)
#'
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

powoGenera <- function(family, uri,
                       genus = NULL,
                       country = NULL,
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

    # Visiting POWO source page for each entry family
    powo_fams_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8", warn = F)

    # Find whether the family exists by searching a pattern for constituent accepted genera
    tt <- grepl("\\s{2,}This is a synonym of", powo_fams_uri[[i]])
    temp <- powo_fams_uri[[i]][tt]

    if (length(temp) != 0) {

      tt <- grepl("<a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]", powo_fams_uri[[i]])

      powo_fams_uri[[i]][tt] <- gsub(".*org[:]names[:]", "", powo_fams_uri[[i]][tt])
      new_powo_fam_uri <- gsub("\".*", "", powo_fams_uri[[i]][tt])
      new_fam_name <- gsub(".*[=]", "", powo_fams_uri[[i]][tt])
      new_fam_name <- gsub("<.*", "", new_fam_name)
      new_fam_name <- gsub(".*>", "", new_fam_name)
      print(paste(powo_codes$family[i], "is under synonym of",  new_fam_name))


      if (any(!new_fam_name %in% powo_codes$family)) {
        print(paste("Searching genera now under", paste0(new_fam_name, "...")))
        new_powo_fam_uri <- paste("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:", new_powo_fam_uri, sep = "")

        # Visiting POWO source page again for the accepted family name of an originally
        # entered family that is under synonym
        powo_fams_uri[[i]] <- readLines(new_powo_fam_uri, encoding = "UTF-8", warn = F)

        tt <- grepl("\\s{2,}This is a synonym of", powo_fams_uri[[i]])
        temp <- powo_fams_uri[[i]][tt]

        powo_codes$family[i] <- new_fam_name

      } else {
        powo_codes <- powo_codes[!powo_codes$family %in% powo_codes$family[i], ]
      }

    }

    if (length(temp) == 0) {
      # The temp vector get the uri for each genus within the family
      temp <- grepl("<li><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]", powo_fams_uri[[i]])
      powo_genus_uri <- powo_fams_uri[[i]][temp]
      # The following subset within the retrieved genus uri within the temp vector
      # is to exclude any possible uri of family-level synonyms
      temp <- !grepl("aceae|oideae|meta\\sproperty|meta\\sname|Compositae|Cruciferae|Gramineae|Guttiferae|Labiatae|Leguminosae|Palmae|Umbelliferae", powo_genus_uri)

      list_fams[[i]] <- data.frame(temp_genus_uri = powo_genus_uri[temp],
                                   family = powo_codes$family[i],
                                   genus = NA,
                                   authors = NA,
                                   scientific_name = NA,
                                   kew_id = NA,
                                   powo_uri = NA)

      # Filling in each column
      list_fams[[i]][["temp_genus_uri"]] <- gsub(".*<li><a href[=]\"", "", list_fams[[i]][["temp_genus_uri"]])
      list_fams[[i]][["powo_uri"]] <- paste("http://www.plantsoftheworldonline.org", gsub("\".+", "", list_fams[[i]][["temp_genus_uri"]]), sep = "")
      list_fams[[i]][["kew_id"]] <- gsub(".+[:]", "", list_fams[[i]][["powo_uri"]])

      list_fams[[i]][["authors"]] <- gsub(".*em>", "", list_fams[[i]][["temp_genus_uri"]])
      list_fams[[i]][["authors"]] <- gsub("<.*", "", list_fams[[i]][["authors"]])
      list_fams[[i]][["authors"]] <- gsub("^\\s", "", list_fams[[i]][["authors"]])
      list_fams[[i]][["genus"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "", list_fams[[i]][["temp_genus_uri"]])
      list_fams[[i]][["scientific_name"]] <- paste(list_fams[[i]][["genus"]], list_fams[[i]][["authors"]])

      # Select specific columns of interest
      list_fams[[i]] <- list_fams[[i]] %>% select("family", "genus", "authors", "scientific_name", "kew_id", "powo_uri")
    }

    # ending the main for loop
  }
  names(list_fams) <- powo_codes$family

  # Combining all dataframes from the list of each family search
  # Each dataframe includes the retrieved genus uri for each searched family
  if (length(list_fams) == 1) {
    df <- list_fams[[1]]
  } else {
    df <- list_fams[[1]]
    for (i in 2:length(list_fams)) {
      df <- rbind(df, list_fams[[i]])
    }
  }

  # If a vector of genus names is provided, then remove all other genera in the
  # family during the next search steps
  if (!is.null(genus)) {

    temp <- df$genus %in% genus

    if (length(which(temp == TRUE)) == length(genus)) {

      df <- df[temp, ]

    } else {

      stop(paste("Any genus in the provided genus vector might have a typo or is not present in POWO.\n",
                 "Please correct the following names in your genus vector according to POWO:\n", "\n",

                 paste(genus[!genus %in% df$genus], collapse = ", "), "\n", "\n",

                 "Find help also at DBOSLab-UFBA:\n",
                 "Débora Zuanny, deborazuanny@gmail.com\n",
                 "Domingos Cardoso, cardosobot@gmail.com"))
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
                      "scientific_name",
                      "publication",
                      "species_number",
                      "native_to_country",
                      "native_to_botanical_countries",
                      "introduced_to_country",
                      "introduced_to_botanical_countries",
                      "kew_id",
                      "powo_uri")


  # If a vector of country names is provided, then remove any genera that do not
  # occur in the given country. The temp vector is logical (TRUE or FALSE) and
  # shows which genus/row should be kept in the search given the provided country vector
  if (!is.null(country)) {

    temp <- vector()

    for (i in seq_along(df$native_to_country)) {

      tt <- gsub("^\\s", "", strsplit(df$native_to_country[i], ",")[[1]]) %in% country

      if (any(tt)) {
        temp[i] <- TRUE
      } else {
        temp[i] <- FALSE
      }

    }

    # The following conditions is just to show/print how the df will be subsetted
    # according to the provided country vector
    if (any(temp)) {
      tl <- list()
      for (i in seq_along(country)) {
        tv <- vector()
        for (l in seq_along(df$native_to_country)) {
          tv[l] <- country[i] %in% gsub("^\\s", "", strsplit(df$native_to_country[l], ",")[[1]])
        }
        if (length(which(tv == TRUE)) == 0) {
          tl[[i]] <- FALSE
        }
        if (length(which(tv == TRUE)) != 0 & length(which(tv == TRUE)) < length(tv)) {
          tl[[i]] <- TRUE
        }
        if (length(which(tv == TRUE)) == length(tv)) {
          tl[[i]] <- TRUE
        }
      }
      cv <- country[unlist(tl)]

      if (length(country[country %in% cv]) != length(country)) {
        cat(paste("Your search returned genera with distribution only in the following countries:\n", "\n",

                  paste(country[country %in% cv], collapse = ", "), "\n", "\n",

                  "There is no genus occurring in the countries below:\n", "\n",

                  paste(country[!country %in% cv], collapse = ", "), "\n", "\n",

                  "Check whether any genus does not occur in the countries above either because:\n",
                  "1. The genus indeed does not occur in the provided country vector;\n",
                  "2. The country name is written with any typo;\n",
                  "3. Any country name in the country vector is not written in English language.\n", "\n"))
      }

    } else {

      cat(paste("Your search returned an empty data frame either because:\n",
                "1. No genus occurs in the provided country vector;\n",
                "2. The country vector has any typo;\n",
                "3. Any country name in the country vector is not written in English language."))

    }

    # Subset the searched genera according to the country vector
    if(length(df$genus[temp]) != length(temp)) {
      cat(paste("Genera listed below were removed from the original search because they are not native to any of the given country vector:\n", "\n",
                df$genus[!temp]))
    }
    df <- df[temp, ]

  }

  return(df)

}
