#' Extract species number of any plant family
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing the number of species and genera
#' within the target botanical families of flowering plants available at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoFam(family,
#'         verbose = TRUE,
#'         save = FALSE,
#'         dir = "results_powoFam",
#'         filename = "output")
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoFam** and the searched results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return A table with the following fields: family, genus_number (counting of
#' genera within the specified plant family), species_number (countig of species
#' within each genus), kew_id (the family code used in Kew's database),
#' powo_uri (the URI to access the family in POWO).
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoGenera}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoMap}}
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' powoFam(family = "Lecythidaceae",
#'         save = FALSE,
#'         dir = "results_powoFam",
#'         filename = "Lecythidaceae_spp_number")
#' }
#'
#' @importFrom data.table fwrite
#' @importFrom utils data
#' @importFrom dplyr filter
#'
#' @export
#'

powoFam <- function(family,
                    verbose = TRUE,
                    save = FALSE,
                    dir = "results_powoFam",
                    filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # dir check
  dir <- .arg_check_dir(dir)

  # Extracting the uri of each plant family using associated data POWOcodes
  utils::data("POWOcodes", package = "expowo")
  powo_codes_fam <- dplyr::filter(POWOcodes, family %in% .env$family)

  # Search POWO for the genus URI within corresponding plant family
  df <- .getgenURI(family = family,
                   genus = NULL,
                   hybrid = FALSE,
                   verbose = verbose)

  # Extract number of species in each genus of the queried families
  df <- .getsppNumb(df,
                    verbose = verbose)

  # Get genus and species number for each family
  df_temp <- data.frame(family = powo_codes_fam$family,
                        genus_number = NA,
                        species_number = NA,
                        kew_id = gsub(".+[:]", "", powo_codes_fam$uri),
                        powo_uri = powo_codes_fam$uri)

  for (i in seq_along(df_temp$family)) {
    tf <- df$family %in% df_temp$family[i]
    df_temp$species_number[i] <- sum(df$species_number[tf])
    df_temp$genus_number[i] <- as.numeric(length(df$genus[tf]))
  }
  df <- df_temp

  # Save the search results if param save is TRUE
  saveCSV(df,
          dir = dir,
          filename = filename,
          verbose = verbose,
          append = FALSE,
          save = save,
          foldername = NULL)

  return(df)
}
