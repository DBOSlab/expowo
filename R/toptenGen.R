#' Get top ten most species-rich genera
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing the top ten most diverse genera
#' of any target botanical family of flowering plants at
#' [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
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

  if(length(family) != length(uri)) {
    stop(paste("Any family or URI is missing."))
  }
  data("POWOcodes")
  uri_log <- uri %in% POWOcodes$uri
  uri_log <- which(uri_log == FALSE)
  if(length(uri_log) >= 1) {
    stop(paste("Any family's URI address is incomplete or misspelled and cannot
               open connection with POWO website."))
  }

  powo_codes_fam <- data.frame(family = family,
                               uri = uri)

  # POWO search for the genus URI in each family using auxiliary function
  # getGenURI
  df <- getGenURI(powo_codes_fam,
                  genus = NULL,
                  verbose = verbose)

  # Extract number of species using auxiliary function getNumb
  df <- getNumb(df,
                verbose = verbose)

  # Select specific columns of interest and the most diverse genera
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
    slice(1:10)                           # the top ten richest genera


  if (save) {
    # Create a new directory to save the results with current date
    if (!dir.exists(dir)) {
      dir.create(dir)
      todaydate <- format(Sys.time(), "%d %b %Y")
      folder_name <- paste0(dir, gsub(" ", "", todaydate))
      print(paste0("Writing '", folder_name, "' on disk."))
      dir.create(folder_name) # If there is no directory... make one!
    } else {
      # If directory was created during a previous search, get its name to save
      # results
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
