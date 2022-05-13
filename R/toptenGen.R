#' Extracts top ten genera with highest species diversity
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing the top ten most diverse genera
#' of any target botanical family associated to the URI addresses of angiosperm
#' families at [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' toptenGen(dir, filename, family, uri,
#'           verbose = TRUE, save = TRUE)
#'
#' @param dir Pathway to computer`s directory, where the file will be saved if the
#' param "save" is setted up in \code{TRUE}. Default is to create a directory
#' named "results_toptenGen/".
#'
#' @param filename Name of the final output file. Default is to create a file entitled "output".
#'
#' @param family Either a single family name or a vector of multiple families
#' that are present in POWO.
#'
#' @param uri one or multiple URI addresses for each family to be searched in POWO.
#'
#' @param verbose Logical, if \code{FALSE}, the search results will not be printed
#' in the console in full. Defaults is TRUE.
#'
#' @param save Logical, if \code{FALSE}, the search results will not be saved.
#' Defaults is TRUE.
#'
#' @return Table in .csv format and saves the output on disk.
#'
#' @examples
#' \dontrun{
#' powocodes <- taxize::get_pow(c("Araceae", "Lecythidaceae"))
#' powocodes <- data.frame(powocodes)
#' powocodes <- cbind(family = c("Araceae", "Lecythidaceae"), powocodes)
#'
#'toptenGen(dir = "results_toptenGen/",
#'          filename = "Araceae_Lecythidaceae",
#'          powocodes$family,
#'          powocodes$uri,
#'          verbose = TRUE,
#'          save = TRUE)
#'}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom data.table fwrite
#'
#' @export
#'

toptenGen <- function(dir = "results_toptenGen/",
                      filename = "output",
                      family, uri,
                      verbose = TRUE,
                      save = TRUE) {

  powo_codes <- data.frame(family = family,
                           uri = uri)

  # POWO search for the number of genera in each family
  powo_fams_uri <- list()
  list_fams <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause 300 seconds of um pause every 500th search,
    # because POWO website cannot permit constant search
    if (i%%500 == 0) {
      Sys.sleep(300)
    }
    # Adding a counter to identify each running search
    if (verbose) {
      print(paste0("Searching... ",
                   powo_codes$family[i], " ", i, "/", length(powo_codes$family)))
    }

    powo_fams_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8", warn = F)

    temp <- grepl("<li><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]", powo_fams_uri[[i]])
    powo_genus_uri <- powo_fams_uri[[i]][temp]
    temp <- !grepl("aceae|oideae|meta\\sproperty|meta\\sname|Compositae|Cruciferae|Gramineae|Guttiferae|Labiatae|Leguminosae|Palmae|Umbelliferae", powo_genus_uri)

    list_fams[[i]] <- data.frame(temp_genus_uri = powo_genus_uri[temp],
                                 family = powo_codes$family[i],
                                 genus = NA,
                                 authors = NA,
                                 scientific_name = NA,
                                 powo_uri = NA)

    # Filling in each column
    list_fams[[i]][["temp_genus_uri"]] <- gsub(".*<li><a href[=]\"", "", list_fams[[i]][["temp_genus_uri"]])
    list_fams[[i]][["powo_uri"]] <- paste("http://www.plantsoftheworldonline.org", gsub("\".+", "", list_fams[[i]][["temp_genus_uri"]]), sep = "")

    list_fams[[i]][["authors"]] <- gsub(".*em>", "", list_fams[[i]][["temp_genus_uri"]])
    list_fams[[i]][["authors"]] <- gsub("<.*", "", list_fams[[i]][["authors"]])
    list_fams[[i]][["authors"]] <- gsub("^\\s", "", list_fams[[i]][["authors"]])
    list_fams[[i]][["genus"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "", list_fams[[i]][["temp_genus_uri"]])
    list_fams[[i]][["scientific_name"]] <- paste(list_fams[[i]][["genus"]], list_fams[[i]][["authors"]])

    # Select specific columns of interest
    list_fams[[i]] <- list_fams[[i]] %>% select("family", "genus", "authors", "scientific_name", "powo_uri")

  }
  names(list_fams) <- powo_codes$family

  # Combining all dataframes from the list of each family search
  if (length(list_fams) == 1) {
    df <- list_fams[[1]]
  } else {
    df <- list_fams[[1]]
    for (i in 2:length(list_fams)) {
      df <- rbind(df, list_fams[[i]])
    }
  }


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
                      "powo_uri") %>%
       arrange(desc(species_number))  %>%     # displaying in the decreasing order
       group_by(family) %>%               # to search for each family
       slice(1:10)                        # filtering the top ten richest genera

  if(save = TRUE){
    # Create a new directory to save the results with current date
    if(!dir.exists(dir)) {
      todaydate <- format(Sys.time(), "%d %b %Y")
      folder_name <- paste0(dir, gsub(" ", "", todaydate))
      message(paste0("Writing ", folder_name, "on disk."))
      dir.create(folder_name) } #if there is no directory... make one!

    # Creating and saving the spreadsheet in .csv format
    fullname <- paste0(dir, filename, ".csv")
    message(paste0("Writing the spreadsheet ", fullname, "on disk."))
    fwrite(df,
           fullname,
           sep = ",",
           row.names = FALSE,
           col.names = TRUE)
  }

  return(df)
}



