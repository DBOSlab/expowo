#' Extract list of genera for any plant family
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a CSV file listing all genera with associated number
#' of accepted species and geographical distribution for any angiosperm family
#' at [Plants of the World Online (POWO)](https://powo.science.kew.org/).
#'
#' @usage
#' powoGenera(family,
#'            genus = NULL,
#'            hybrid = FALSE,
#'            country = NULL,
#'            verbose = TRUE,
#'            rerun = FALSE,
#'            save = FALSE,
#'            dir = "results_powoGenera",
#'            filename = "output")
#'
#' @param family Either one family name or a vector of multiple families that
#' is present in POWO.
#'
#' @param genus Either one genus name or a vector of multiple genera
#' that is present in POWO. If any genus name is not provided, then the
#' function will search all accepted genera known for the target family.
#'
#' @param hybrid Logical, if \code{TRUE}, the search results will include
#' hybrid genera.
#'
#' @param country Either one country name or a vector of multiple countries.
#' If country names are provided, then the function will return only the genera
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
#' is to create a directory named **results_powoGenera** and the search results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @return Table in .csv format.
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' powoGenera(family = "Lecythidaceae",
#'            verbose = TRUE,
#'            save = TRUE,
#'            dir = "results_powoGenera",
#'            filename = "Lecythidaceae_genera")
#'
#' powoGenera(family = "Lecythidaceae",
#'            genus = "Bertholletia",
#'            country = c("Argentina", "Brazil", "French Guiana"),
#'            verbose = TRUE,
#'            save = TRUE,
#'            dir = "results_powoGenera",
#'            filename = "Lecythidaceae_search")
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#' @importFrom R.utils countLines
#' @importFrom utils data
#'
#' @export
#'

powoGenera <- function(family,
                       genus = NULL,
                       hybrid = FALSE,
                       country = NULL,
                       verbose = TRUE,
                       rerun = FALSE,
                       save = FALSE,
                       dir = "results_powoGenera",
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
  df <- .getgenURI(family = family,
                   genus = genus,
                   hybrid = hybrid,
                   verbose = verbose)

  # Keep only taxa that were not retrieved in previously stopped run
  if (rerun) {
    last_gen <- .powo_rerun(df,
                            sp_uri = FALSE,
                            dir, filename)
    pos <- which(df$genus %in% last_gen$genus):nrow(df)
    df <- df[pos, ]
  }

  # Create folder name to save search results
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
                            synonyms = FALSE,
                            country = country,
                            gen_sp_nbr = TRUE,
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
