#' Get the species number of any plant family from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing the number of species within the
#' target botanical families of flowering plants available at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoFam(family, uri,
#'         verbose = TRUE, save = TRUE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families that
#' are present in POWO.
#'
#' @param uri URI address for each family to be searched in POWO.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be
#' printed in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the search results will not be saved.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoFam** and the searched results
#' will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **output**.
#'
#' @return Table in .csv format that is saved on disk.
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{toptenGen}}
#' @seealso \code{\link{powoGenera}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoMap}}
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#' library(taxize)
#'
#' fam <- c("Araceae", "Sapotaceae")
#' powocodes <- cbind(family = fam,
#'                    data.frame(taxize::get_pow(fam)))
#'
#' powoFam(powocodes$family, powocodes$uri,
#'         verbose = TRUE,
#'         save = TRUE,
#'         dir = "results_powoFam/",
#'         filename = "Araceae_Sapotaceae")
#'
#' ## Searching for the number of species in any or all flowering plant
#' ## families, by using the URI addresses within the POWOcodes data file.
#'
#' data(POWOcodes)
#'
#' powoFam(POWOcodes$family, POWOcodes$uri,
#'         verbose = TRUE,
#'         save = TRUE,
#'         dir = "results_powoFam/",
#'         filename = "all_angiosperms_species_number")
#' }
#'
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

powoFam <- function(family, uri,
                    verbose = TRUE,
                    save = TRUE,
                    dir = "results_powoFam/",
                    filename = "output") {

  # family and URI check.
  .arg_check_fam_uri(family, uri)

  # dir check.
  dir <- .arg_check_dir(dir)

  # Placing input data into dataframe.
  powo_codes_fam <- data.frame(family = family,
                               uri = uri)

  # POWO search for the genus URI in each family using auxiliary function
  # getGenURI.
  df <- getGenURI(powo_codes_fam,
                  genus = NULL,
                  verbose = verbose)

  # Extract number of species using auxiliary function getNumb.
  df <- getNumb(df,
                verbose = verbose)

  # Enforce transformation to numeric values.
  df$species_number <- as.numeric(df$species_number)

  # Select specific columns of interest and the most diverse genera.
  df_temp <- data.frame(family = powo_codes_fam$family,
                        species_number = NA,
                        kew_id = gsub(".+[:]", "", powo_codes_fam$uri),
                        powo_uri = powo_codes_fam$uri)

  for (i in seq_along(df_temp$family)) {
    tf <- df$family %in% df_temp$family[i]
    df_temp$species_number[i] <- sum(df$species_number[tf])
  }

  df <- df_temp


  # Saving the dataframe if param save is TRUE.
  if (save) {
    # Create a new directory to save the results (spreadsheet in .csv format)
    # with current date.
    .save_df(dir, filename, df)
  }

  return(df)
}
