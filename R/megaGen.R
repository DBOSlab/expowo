#' Get megadiverse genera from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing all megadiverse genera
#' any angiosperm family at [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#' A numeric value should define the threshold to be considered a megadiverse genus.
#' [Frodin (2004)](https://doi.org/10.2307/4135449) suggests 500 species
#' as the threshold for megadiverse genera.
#'
#' @usage
#' megaGen(family, uri, thld = NULL,
#'         verbose = TRUE, save = TRUE, dir, filename)
#'
#' @param family Either one family name or a vector of multiple families that are
#' present in POWO.
#'
#' @param uri URI address for each family to be searched in POWO.
#'
#' @param thld A defined threshold of species number for a genus to be considered
#' megadiverse.
#'
#' @param verbose Logical, if \code{FALSE}, the searched results will not be printed
#' in the console in full.
#'
#' @param save Logical, if \code{FALSE}, the searched results will not be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_megaGen** and the searched results
#' will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. Default is to create a file
#' entitled **output**.
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
#'
#' @export
#'

megaGen <- function(family, uri,
                    thld = NULL,
                    verbose = TRUE,
                    save = TRUE,
                    dir = "results_megaGen/",
                    filename = "output") {


  powo_codes_fam <- data.frame(family = family,
                               uri = uri)

  # POWO search for the genus URI in each family using auxiliary function getGenURI
  df <- getGenURI(powo_codes_fam,
                  genus = NULL,
                  verbose = verbose)

  # Extract number of species using auxiliary function getNumb
  df <- getNumb(df,
                verbose = verbose)

  # Select specific columns of interest and the megadiverse genera by a threshold
  df$species_number <- as.numeric(df$species_number)
  df <- df %>% select("family",
                      "genus",
                      "authors",
                      "scientific_name",
                      "species_number",
                      "kew_id",
                      "powo_uri") %>%
    filter(species_number >= thld)


  if (save) {
    # Create a new directory to save the results with current date
    if (!dir.exists(dir)) {
      dir.create(dir)
      todaydate <- format(Sys.time(), "%d %b %Y")
      folder_name <- paste0(dir, gsub(" ", "", todaydate))
      print(paste0("Writing '", folder_name, "' on disk."))
      dir.create(folder_name) # If there is no directory... make one!
    } else {
      # If directory was created during a previous search, get its name to save results
      folder_name <- paste0(dir, gsub(" ", "", format(Sys.time(), "%d %b %Y")))
    }
    # Create and save the spreadsheet in .csv format
    fullname <- paste0(folder_name, "/", filename, ".csv")
    print(paste0("Writing the spreadsheet '", filename, ".csv' on disk."))
    data.table::fwrite(df,
                       file = fullname,
                       sep = ",",
                       row.names = FALSE,
                       col.names = TRUE)
  }

  return(df)
}
