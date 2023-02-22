#' Extract list of genera from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing all genera with associated number
#' of accepted species and geographical distribution for any angiosperm
#' family at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoGenera(family, genus = NULL, country = NULL,
#'            verbose = TRUE, save = FALSE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param genus Either one genus name or a vector of multiple genera
#' that are present in POWO. If any genus name is not provided, then the
#' function will search all accepted genera known for the target family.
#'
#' @param country Either one country name or a vector of multiple countries.
#' If country names are provided, then the function will return only the genera
#' that are native to such countries, according to POWO.
#'
#' @param verbose Logical, if \code{FALSE}, the searched results will not be
#' printed in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the searched results will not be saved
#' on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoGenera** and the searched
#' results will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. The default is to
#' create a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' powoGenera(family = "Lecythidaceae",
#'            verbose = TRUE,
#'            save = FALSE,
#'            dir = "results_powoGenera/",
#'            filename = "Lecythidaceae_genera")
#'
#' powoGenera(family = "Lecythidaceae",
#'            genus = "Bertholletia",
#'            country = c("Argentina", "Brazil", "French Guiana"),
#'            verbose = TRUE,
#'            save = FALSE,
#'            dir = "results_powoGenera/",
#'            filename = "Lecythidaceae_bycountry")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

powoGenera <- function(family,
                       genus = NULL,
                       country = NULL,
                       verbose = TRUE,
                       save = FALSE,
                       dir = "results_powoGenera/",
                       filename = "output") {

  # Family check for synonym
  family <- .arg_check_family(family)

  # dir check
  dir <- .arg_check_dir(dir)

  # Extracting the uri of each plant family using associated data POWOcodes
  utils::data("POWOcodes", package = "expowo")
  powo_codes_fam <- dplyr::filter(POWOcodes, family %in% .env$family)

  # POWO search for the genus URI in each family using auxiliary function
  # getGenURI.
  df <- getGenURI(powo_codes_fam,
                  genus = genus,
                  verbose = verbose)

  # Extract distribution and number of species using auxiliary function getDist.
  df <- getDist(df,
                listspp = TRUE,
                verbose = verbose)

  # Select specific columns of interest.
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
  # shows which genus/row should be kept in the search given the provided
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
            tv[l] <- country[i] %in%
              gsub("^\\s", "", strsplit(df$native_to_country[l], ",")[[1]])
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
          cat(paste("Your search returned genera with distribution only in the
                    following countries:\n", "\n",

                    paste(country[country %in% cv], collapse = ", "), "\n",
                    "\n",

                    "There is no genus occurring in the countries below:\n",
                    "\n",

                    paste(country[!country %in% cv], collapse = ", "), "\n",
                    "\n",

                    "Check whether any genus does not occur in the countries
                    above either because:\n",
                    "1. The genus indeed does not occur in the provided country
                    vector;\n",
                    "2. The country name is written with any typo;\n",
                    "3. Any country name in the country vector is not written in
                    English language.\n", "\n"))
        }

      } else {
        cat(paste("Your search returned an empty data frame either because:\n",
                  "1. No genus occurs in the provided country vector;\n",
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
