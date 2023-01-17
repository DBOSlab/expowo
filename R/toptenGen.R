#' Get top ten most species-rich genera
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing the top ten most diverse genera
#' of any target botanical family of flowering plants at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' toptenGen(family, uri,
#'           verbose = TRUE, save = TRUE, dir, filename)
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
#' is to create a directory named **results_toptenGen** and the searched results
#' will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **output**.
#'
#' @return Table in .csv format that is saved on disk.
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
#' toptenGen(powocodes$family, powocodes$uri,
#'           verbose = TRUE,
#'           save = TRUE,
#'           dir = "results_toptenGen/",
#'           filename = "Araceae_Lecythidaceae")
#'
#' ## Searching for the top ten most diverse genera
#' ## in any or all flowering plant families, by using
#' ## the URI addresses within the POWOcodes data file
#'
#' data(POWOcodes)
#'
#' toptenGen(POWOcodes$family, POWOcodes$uri,
#'         verbose = TRUE,
#'         save = TRUE,
#'         dir = "results_toptenGen/",
#'         filename = "toptendiverse_plant_genera")
#' }
#'
#' @importFrom dplyr arrange desc filter group_by select slice
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

toptenGen <- function(family, uri,
                      verbose = TRUE,
                      save = TRUE,
                      dir = "results_toptenGen/",
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

  # Select specific columns of interest and the most diverse genera.
  df$species_number <- as.numeric(df$species_number)
  df <- df %>% select("family",
                      "genus",
                      "authors",
                      "scientific_name",
                      "species_number",
                      "kew_id",
                      "powo_uri") %>%
    arrange(desc(df$species_number)) %>%  # Displaying in the descending order
    group_by(family) %>%                  # to filter in each family
    slice(1:10)                           # the top ten richest genera.


  # Saving the dataframe if param save is TRUE.
  if (save) {
    # Create a new directory to save the results (spreadsheet in .csv format)
    # with current date.
    .save_df(dir, filename, df)
  }

  return(df)
}
