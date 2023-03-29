#' Extract megadiverse genera of any plant family
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing all mega-diverse genera for any plant
#' family at [Plants of the World Online (POWO)](https://powo.science.kew.org/)
#' based on a provided numeric value as the threshold to be considered
#' mega-diverse. Frodin (2004) suggests 500 species as the threshold.
#'
#' @usage
#' megaGen(family,
#'         thld = 500,
#'         verbose = TRUE,
#'         save = FALSE,
#'         dir = "results_megaGen",
#'         filename = "output")
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param thld A defined threshold of species number for a genus to be
#' considered megadiverse. The default value is 500 based on Frodin (2004).
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_megaGen** and the search results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#'
#' library(expowo)
#'
#' megaGen(family = "Cyperaceae",
#'         thld = 500,
#'         save = FALSE,
#'         dir = "results_megaGen",
#'         filename = "Cyperaceae_big_genera")
#' }
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

megaGen <- function(family,
                    thld = 500,
                    verbose = TRUE,
                    save = FALSE,
                    dir = "results_megaGen",
                    filename = "output") {


  # family check for synonym
  family <- .arg_check_family(family)

  # thld check
  .arg_check_thld(thld)

  # dir check
  dir <- .arg_check_dir(dir)

  # Search POWO for the genus URI within corresponding plant family
  df <- .getgenURI(family = family,
                   genus = NULL,
                   hybrid = FALSE,
                   verbose = verbose)

  # Extract number of species in each genus of the queried families
  df <- .getsppNumb(df,
                    verbose = verbose)

  # Select specific columns of interest and the megadiverse genera by a
  # threshold.
  if(is.null(thld)) thld <- 500
  df <- df %>% select("family",
                      "genus",
                      "authors",
                      "scientific_name",
                      "species_number",
                      "kew_id",
                      "powo_uri") %>%
    filter(df$species_number >= thld)

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
