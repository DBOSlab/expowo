#' Get megadiverse genera from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing all megadiverse genera for
#' any angiosperm family at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#' A numeric value should define the threshold to be considered a megadiverse
#' genus. Frodin, D.G.(2004) in Taxon suggests 500 species as the threshold for
#' a megadiverse genus.
#'
#' @usage
#' megaGen(family, uri, thld = NULL,
#'         verbose = TRUE, save = TRUE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families that
#' are present in POWO.
#'
#' @param uri URI address for each family to be searched in POWO.
#'
#' @param thld A defined threshold of species number for a genus to be
#' considered megadiverse. If no threshold number is provided, the function will
#' consider a value of 500 based on Frodin, D.G.(2004) in Taxon.
#'
#' @param verbose Logical, if \code{FALSE}, the searched results will not be
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
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **output**.
#'
#' @return Table in .csv format that is saved on disk.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#'
#' library(expowo)
#' library(taxize)
#'
#' fam <- c("Fabaceae", "Lecythidaceae")
#' powocodes <- cbind(family = fam,
#'                    data.frame(taxize::get_pow(fam)))
#'
#' megaGen(powocodes$family, powocodes$uri,
#'         thld = 500,
#'         verbose = TRUE,
#'         save = TRUE,
#'         dir = "results_megaGen/",
#'         filename = "Fabaceae_Lecythidaceae")
#'
#' ## Searching for all megadiverse angiosperm genera
#' ## in any or all families, by using the URI addresses
#' ## within the POWOcodes data file
#'
#' data(POWOcodes)
#'
#' megaGen(POWOcodes$family, POWOcodes$uri,
#'         thld = 500,
#'         verbose = TRUE,
#'         save = TRUE,
#'         dir = "results_megaGen/",
#'         filename = "megadiverse_plant_genera")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

megaGen <- function(family, uri,
                    thld = NULL,
                    verbose = TRUE,
                    save = TRUE,
                    dir = "results_megaGen/",
                    filename = "output") {


  # Family and URI check
  .arg_check_fam_uri(family, uri)

  # thld check
  .arg_check_thld(thld)

  # dir check
  dir <- .arg_check_dir(dir)

  # Placing input data into dataframe
  powo_codes_fam <- data.frame(family = family,
                               uri = uri)

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

  # Saving the dataframe if param save is TRUE
  if (save) {
    # Create a new directory to save the results (spreadsheet in .csv format)
    # with current date.
    .save_df(dir, filename, df)
  }

  return(df)
}
