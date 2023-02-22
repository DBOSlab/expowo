#' Extract the top most species-rich genera
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing the top most diverse genera
#' of any target botanical family of flowering plants at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' topGen(family, limit = 10, verbose = TRUE, save = FALSE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param limit A defined numerical value to limit the most diverse genera to be
#' selected within each plant family. The default is to select the top ten
#' richest genera.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be
#' printed in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the search results will not be saved on
#' disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_topGen** and the searched results
#' will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#'  a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' topGen(family = "Lecythidaceae",
#'        limit = 10,
#'        verbose = TRUE,
#'        save = FALSE,
#'        dir = "results_topGen/",
#'        filename = "Lecythidaceae_top_ten")
#' }
#'
#' @importFrom dplyr arrange desc filter group_by select slice
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

topGen <- function(family,
                   limit = 10,
                   verbose = TRUE,
                   save = FALSE,
                   dir = "results_topGen/",
                   filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # limit check
  .arg_check_limit(limit)

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

  # Select specific columns of interest and the most diverse genera
  if(is.null(limit)) limit <- 10
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
    slice(1:limit)                        # the top richest genera.


  # Saving the dataframe if param save is TRUE.
  .save_df(save, dir, filename, df)

  return(df)
}
