#' Extract list of species for any plant genus and family
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing all accepted species and
#' associated geographical distribution from any target genus or family of
#' flowering plants at
#' [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoSpecies(family,
#'             genus = NULL,
#'             hybrid = FALSE,
#'             synonyms = FALSE,
#'             country = NULL,
#'             verbose = TRUE,
#'             rerun = FALSE,
#'             save = FALSE,
#'             dir = "results_powoSpecies",
#'             filename = "output")
#'
#' @param family Either one family name or a vector of multiple families
#' that is present in POWO.
#'
#' @param genus Either one genus name or a vector of multiple genera
#' that is present in POWO. If any genus name is not provided, then the
#' function will search any species from all accepted genera known for the
#' target family.
#'
#' @param hybrid Logical, if \code{TRUE}, the search results will include
#' hybrid species.
#'
#' @param synonyms Logical, if \code{TRUE}, the search results will include
#' synonyms.
#'
#' @param country Either one country name or a vector of multiple countries.
#' If country names are provided, then the function will return only the species
#' that are native to such countries, according to POWO.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param rerun Logical, if \code{TRUE}, a previously stopped search will continue
#' from where it left off, starting with the last retrieved taxon. Please ensure
#' that the 'filename' argument exactly matches the name of the CSV file saved
#' from the previous search, and that the previously saved CSV file is located
#' within a subfolder named after the current date. If it is not, please rename
#' the date subfolder accordingly."
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoSpecies** and the search results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return A table with the following fields: family, genus, species, taxon_name,
#' authors, scientific_name, publication (information about the protologue of
#' the species), native_to_country (original distribution according to
#' political country), native_to_botanical_countries (original distribution
#' according to botanical country), introduced_to_country (exotic distribution
#' according to political country), introduced_to_botanical_countries (exotic
#' distribution according to botanical country), kew_id (each species code
#' within Kew's databases), powo_uri (the URI to access each species in POWO).
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{powoGenera}}
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' powoSpecies(family = "Martyniaceae",
#'             synonyms = TRUE,
#'             country = c("Argentina", "Brazil", "French Guiana"),
#'             save = FALSE,
#'             dir = "Martyniaceae_results_powoSpecies",
#'             filename = "Martyniaceae_spp")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom tibble add_row
#' @importFrom flora remove.authors
#' @importFrom R.utils countLines
#' @importFrom utils data read.csv
#'
#' @export
#'

powoSpecies <- function(family,
                        genus = NULL,
                        hybrid = FALSE,
                        synonyms = FALSE,
                        country = NULL,
                        verbose = TRUE,
                        rerun = FALSE,
                        save = FALSE,
                        dir = "results_powoSpecies",
                        filename = "output") {

  # family check for synonym
  family <- .arg_check_family(family)

  # dir check
  dir <- .arg_check_dir(dir)

  # Check 'rerun = FALSE' to ensure there is no previously saved CSV file
  # with the same name as the current filename
  .arg_check_run(dir, filename, rerun)

  # rerun check
  .arg_check_rerun(dir, filename, rerun)

  # Search POWO for the genus URI within corresponding plant family
  powo_codes <- .getgenURI(family = family,
                           genus = genus,
                           hybrid = hybrid,
                           verbose = verbose)

  # Keep only taxa that were not retrieved in previously stopped run
  if (rerun) {
    last_gen <- .powo_rerun(powo_codes,
                            sp_uri = FALSE,
                            dir, filename)
    pos <- which(powo_codes$genus %in% last_gen$genus):nrow(powo_codes)
    powo_codes <- powo_codes[pos, ]
  }

  # Search POWO URI for all accepted species in each plant genus
  df <- .getsppURI(powo_codes,
                   splist = TRUE,
                   hybrid = hybrid,
                   verbose = verbose)

  # Keep only taxa that were not retrieved in previously stopped run
  if (rerun) {
    df <- .powo_rerun(df,
                      sp_uri = TRUE,
                      dir, filename)
  }

  # Make folder name to save search results
  foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))

  # Extract full information for each taxon
  # The function searches for a set of up to 500 taxa and then save mined data
  chunk <- 500
  n <- nrow(df)
  r  <- rep(1:ceiling(n/chunk), each=chunk)[1:n]
  dfchunk <- split(df, r)
  for (i in 1:length(dfchunk)) {

    dfchunk[[i]] <- getInfo(df = dfchunk[[i]],
                            hybrid = hybrid,
                            synonyms = synonyms,
                            country = country,
                            gen_sp_nbr = FALSE,
                            verbose = verbose,
                            dfsize = n)

    # Save the search results if param save is TRUE
    saveCSV(dfchunk[[i]],
            dir = dir,
            filename = filename,
            verbose = verbose,
            append = TRUE,
            save = save,
            foldername = foldername)
  }

  df <- dfchunk[[1]]
  if (length(dfchunk) > 1) {
    for (i in 2:length(dfchunk)) {
      df <- rbind(df, dfchunk[[i]])
    }
  }

  return(df)
}
