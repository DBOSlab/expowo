#' Fastly save CSV file within a current date folder
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Uses \code{fwrite} to fastly write a CSV file within a current
#' date subfolder of a provided specific directory.
#'
#' @usage
#' saveCSV(df,
#'         dir = "results",
#'         filename = "output",
#'         verbose = TRUE,
#'         append = FALSE,
#'         save = TRUE,
#'         foldername = NULL)
#'
#' @param df Any input list of same length vectors (e.g. \code{data.frame} and
#' \code{data.table}).
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results** and the input \code{data.frame}
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **output**.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing the saving process
#' will not be printed in the console in full.
#'
#' @param append Logical, if \code{TRUE}, the file is opened in append mode and
#' column names (header row) are not written.
#'
#' @param save Logical, if \code{FALSE}, the input \code{data.frame} will be not
#' be saved on disk.
#'
#' @param foldername A parameter only for internal use with
#' [expowo](https://dboslab.github.io/expowo/)'s main functions. You must keep
#' this always as \code{NULL}.
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' res <- powoGenera(family = "Lecythidaceae",
#'                   verbose = TRUE,
#'                   save = FALSE)
#'
#' saveCSV(res,
#'         dir = "results_powoGenera",
#'         filename = "Lecythidaceae_search",
#'         append = FALSE)
#'}
#'
#' @export
#'

saveCSV <- function (df,
                     dir = "results",
                     filename = "output",
                     verbose = TRUE,
                     append = FALSE,
                     save = TRUE,
                     foldername = NULL) {

  # Make folder name to save search results
  if(is.null(foldername)) {
    foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))
  }

  # Save the data frame if param save is TRUE
  if (save) {
    # Create a new directory to save the results with current date
    # If there is no directory... make one!
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    if (!dir.exists(foldername)) {
      dir.create(foldername)
    }
    # Create and save the spreadsheet in .csv format
    fullname <- paste0(foldername, "/", filename, ".csv")
    if (verbose) {
      message(paste0("Writing spreadsheet '", filename, ".csv' within '",
                     foldername, "' on disk."))
    }
    data.table::fwrite(df,
                       file = fullname,
                       sep = ",",
                       row.names = FALSE,
                       col.names = !file.exists(fullname),
                       append = append)
  }
}
