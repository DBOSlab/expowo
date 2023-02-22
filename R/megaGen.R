#' Extract megadiverse genera from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing all mega-diverse genera for
#' any angiosperm family at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/) based on
#' a provided numeric value as the threshold to be considered mega-diverse.
#' Frodin (2004) in Taxon suggests 500 species as the threshold.
#'
#' @usage
#' megaGen(family, thld = 500, verbose = TRUE, save = FALSE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param thld A defined threshold of species number for a genus to be
#' considered megadiverse. The default value is 500 based on Frodin (2004) in
#' Taxon.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be
#' printed in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the searched results will not be saved
#' on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_megaGen** and the searched results
#' will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \donttest{
#'
#' library(expowo)
#'
#' megaGen(family = "Cyperaceae",
#'         thld = 500,
#'         verbose = TRUE,
#'         save = FALSE,
#'         dir = "results_megaGen/",
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
                    dir = "results_megaGen/",
                    filename = "output") {


  # family check for synonym
  family <- .arg_check_family(family)

  # thld check
  .arg_check_thld(thld)

  # dir check
  dir <- .arg_check_dir(dir)

  # Extracting the uri of each plant family using associated data POWOcodes
  utils::data("POWOcodes", package = "expowo")
  powo_codes_fam <- dplyr::filter(POWOcodes, family %in% .env$family)

  # POWO search for the genus URI in each family using auxiliary function
  # getGenURI.
  df <- getGenURI(powo_codes_fam,
                  genus = NULL,
                  verbose = verbose)

  # Extract number of species using auxiliary function getNumb
  df <- getNumb(df,
                verbose = verbose)

  # Select specific columns of interest and the megadiverse genera by a
  # threshold.
  if(is.null(thld)) thld <- 500
  df$species_number <- as.numeric(df$species_number)
  df <- df %>% select("family",
                      "genus",
                      "authors",
                      "scientific_name",
                      "species_number",
                      "kew_id",
                      "powo_uri") %>%
    filter(df$species_number >= thld)

  # Saving the dataframe if param save is TRUE.
  .save_df(save, dir, filename, df)

  return(df)
}
