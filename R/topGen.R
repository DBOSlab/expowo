#' Extract the top most species-rich genera of any plant family
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing the top most diverse genera
#' of any target botanical family of vascular plants at
#' Plants of the World Online (POWO).
#'
#' @usage
#' topGen(family,
#'        limit = 10,
#'        verbose = TRUE,
#'        source = "wcvp",
#'        save = FALSE,
#'        dir = "results_topGen",
#'        filename = "output")
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param limit A defined numerical value to limit the most diverse genera to be
#' selected within each plant family. The default is to select the top ten
#' richest genera.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param source Data source to be mined. The default \code{"wcvp"} uses the
#' bulk data of the World Checklist of Vascular Plants (WCVP), the taxonomic
#' backbone of POWO, which is downloaded once from Kew and cached on disk (see
#' \code{tools::R_user_dir("expowo", "cache")}). It is refreshed only when Kew
#' releases a new WCVP version. Use \code{"powo"} to mine the source page of
#' each taxon at the POWO website instead, which is much slower and may be
#' blocked by POWO.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_topGen** and the search results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return A table with the following fields: family, genus, authors,
#' scientific_name, species_number (counting of species within each genus),
#' kew_id (each genus code within Kew's databases), powo_uri (URI to access each
#' genus in POWO).
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' topGen(family = "Lecythidaceae",
#'        limit = 10,
#'        save = FALSE,
#'        dir = "results_topGen",
#'        filename = "Lecythidaceae_top_ten")
#' }
#'
#' @importFrom dplyr arrange desc filter group_by select slice
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#'
#' @export
#'

topGen <- function(family,
                   limit = 10,
                   verbose = TRUE,
                   source = "wcvp",
                   save = FALSE,
                   dir = "results_topGen",
                   filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # limit check
  .arg_check_limit(limit)

  # dir check
  dir <- .arg_check_dir(dir)

  # source check
  source <- .arg_check_source(source)

  if (source == "wcvp") {
    # Get the accepted genera and species number from WCVP bulk data
    df <- .wcvp_genera(family = family,
                       genus = NULL,
                       hybrid = FALSE,
                       verbose = verbose)
    df <- .wcvp_sppNumb(df,
                        verbose = verbose)
  } else {
    # Search POWO for the genus URI within corresponding plant family
    df <- .getgenURI(family = family,
                     genus = NULL,
                     hybrid = FALSE,
                     verbose = verbose)

    # Extract number of species in each genus of the queried families
    df <- .getsppNumb(df,
                      verbose = verbose)
  }

  # Select specific columns of interest and the most diverse genera
  if (is.null(limit)) limit <- 10

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
