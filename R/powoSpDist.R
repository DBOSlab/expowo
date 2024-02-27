#' Extract distribution of any plant species
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing the geographical distribution at
#' country and botanical subdivisions of all target species of vascular plants
#' at [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoSpDist(family,
#'            species = NULL,
#'            verbose = TRUE,
#'            save = FALSE,
#'            dir = "results_powoSpDist",
#'            filename = "output")
#'
#' @param family Either one family name or a vector of multiple families
#' that is present in POWO.
#'
#' @param species Either one non-hybrid species name or a vector of multiple
#' species that is present in POWO. If any species name is not provided, then
#' the function will search any species from all accepted genera known for the
#' target family.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoSpDist** and the search results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return A table with the following fields: family, genus, species,
#' taxon_name, authors, scientific_name, native_to_country (original
#' distribution according to political country),
#' native_to_botanical_countries (original distribution according to botanical
#' country), introduced_to_country (exotic distribution according to political
#' country), introduced_to_botanical_countries (exotic distribution according to
#' botanical country), kew_id (each species code within Kew's databases),
#' powo_uri (URI to access each species in POWO).
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' powoSpDist(family = "Lecythidaceae",
#'            species = "Lecythis pisonis",
#'            save = FALSE,
#'            dir = "results_powoSpDist",
#'            filename = "L_pisonis_distribution")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom utils data
#'
#' @export
#'

powoSpDist <- function(family,
                       species = NULL,
                       verbose = TRUE,
                       save = FALSE,
                       dir = "results_powoSpDist",
                       filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # dir check
  dir <- .arg_check_dir(dir)

  # Search POWO for the genus URI within corresponding plant family
  genus <- unique(gsub("\\s.+", "", species))
  if (length(genus) == 0){
    genus = NULL
  }
  powo_codes <- .getgenURI(family = family,
                           genus = genus,
                           hybrid = FALSE,
                           verbose = verbose)

  # Search POWO URI for all accepted species in each plant genus
  df <- .getsppURI(powo_codes,
                   splist = TRUE,
                   hybrid = FALSE,
                   verbose = verbose)

  if (!is.null(species)) {
    df <- df[df$taxon_name %in% species, ]
  }

  # Extract full information for each taxon
  df <- getInfo(df = df,
                hybrid = FALSE,
                synonyms = FALSE,
                country = NULL,
                gen_sp_nbr = FALSE,
                verbose = verbose,
                dfsize = nrow(df))

  df <- df %>% select(-"publication")

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
