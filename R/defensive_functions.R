# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Debora Zuanny & Domingos Cardoso

#_______________________________________________________________________________
# Check if the threshold input for megaGen is a numeric value
.arg_check_thld <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"numeric" %in% class_x) {
    stop(paste0("The argument thld should be a numeric value, not '",
                class_x, "'."),
         call. = FALSE)
  }
}

#_______________________________________________________________________________
# Check if the threshold input for topGen is a numeric value
.arg_check_limit <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"numeric" %in% class_x) {
    stop(paste0("The argument limit should be a integer numeric value, not '",
                class_x, "'."),
         call. = FALSE)
  }
}

#_______________________________________________________________________________
# Check if the dir input is "character" type and if it has a "/" in the end
.arg_check_dir <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"character" %in% class_x) {
    stop(paste0("The argument dir should be a character, not '", class_x, "'."),
         call. = FALSE)
  }
  if (grepl("[/]$", x)) {
    x <- gsub("[/]$", "", x)
  }
  return(x)
}

#_______________________________________________________________________________
# Check if the previously search CSV file is within the correct subfolder so as
# to do a renun search
.arg_check_run <- function(dir, filename, rerun) {

  csv_file <- paste0(paste0(dir, "/", format(Sys.time(), "%d%b%Y")),
                     "/", filename, ".csv")
  if (rerun == FALSE) {
    if (file.exists(csv_file)) {
      stop(paste0("by setting 'rerun = FALSE', you have selected a completely
                   new search mode to run. However, it appears that a CSV file
                   with the same name as your current filename has been detected
                   in the results folder, from a previous search. Please ensure
                   that the filename argument is different from the CSV
                   filename from previous search:\n
                   (i) rename the CSV filename from previous search;\n
                   (ii) or move the previously saved CSV file from outside the
                   subfolder named by the today date '",
                   format(Sys.time(), "%d%b%Y"), "'."),
           call. = FALSE)
    }
  }
}

#_______________________________________________________________________________
# Check if the previously search CSV file is within the correct subfolder so as
# to do the renun search
.arg_check_rerun <- function(dir, filename, rerun) {

  csv_file <- paste0(paste0(dir, "/", format(Sys.time(), "%d%b%Y")),
                     "/", filename, ".csv")

  if (rerun) {
    if (!file.exists(csv_file)) {
      stop(paste0("By setting 'rerun = TRUE', you have selected a rerun search
                   mode. However, the CSV file saved during the previous search
                   was not found in the results folder. Please ensure that:\n
                   (i) the filename argument is exactly as the CSV filename from
                   previous search;\n
                   (ii) the previously saved CSV file is within a subfolder
                   named after the today date '",
                   format(Sys.time(), "%d%b%Y"), "'. If it is not, please rename
                   the date subfolder accordingly\n
                   (iii) or set 'rerun = FALSE' for a new run from beginning."),
           call. = FALSE)
    }
  }
}

#_______________________________________________________________________________
# Function to check if the input data for powoMap is a dataframe-formatted
# object generated from powoSpecies.
.arg_check_data_map <- function (inputdf, distcol) {

  # Change column names if the input dataframe is different from powoSpecies
  tf <- is.null(which(grepl(",\\s", inputdf[[distcol]]) == TRUE))
  if (any(tf)) {
    stop("Make sure the input data has each species as a single row with
          its corresponding full distribution in all countries and/or
          botanical regions within a single cell of their respective
          columns, where the country names or botanical regions are
          separated by a comma.")
  }

  # Identify the column with the species scientific binomial and rename it to
  # "taxon_name".
  temp <- lapply(inputdf[1, ],
                 function(x) grepl("^[[:upper:]][[:lower:]]+\\s[[:lower:]]+$|
                                   ^[[:upper:]][[:lower:]]+\\s[[:lower:]]+\\s",
                                   x))
  tf <- unlist(temp)[which(unlist(temp) == TRUE)]
  if (any(tf)) {
    if(!any(names(tf) %in% "taxon_name")) {
      tf <- names(inputdf) %in% names(tf)[1]
      names(inputdf)[tf] <- "taxon_name"
      # Clean the authorship if they are present within the species binomials
      inputdf[[names(inputdf)[tf]]] <- sub("^(\\w+\\s+\\w+).*", "\\1",
                                           inputdf[[names(inputdf)[tf]]])
    }
  } else {
    stop("Make sure the input data has the column with species names as
          a binomial, i.e. containing both the genus name and specific
          epithet. Having or not the taxon authorship within the same
          column of species binomials is NOT an issue.")
  }

  return(inputdf)
}

#_______________________________________________________________________________
# Function to check if the format argument for powoMap is any of jpg, pdf, tiff,
# or png
.arg_check_format <- function (format) {
  format_supported <- c("jpg", "jpeg", "pdf", "tif", "tiff", "png")
  format_supported <- append(format_supported, toupper(format_supported))
  tf <- format %in% format_supported
  if(any(!tf)) {
    stop(paste0("Make sure the format argument has any of the following
                format names:\n\n",
                paste0(format_supported, collapse = ", ")))
  }
}

#_______________________________________________________________________________
# Function to check if any family name provided is a synonym and does not
# have data available.

.arg_check_family <- function (family) {
  rep_str = c("Compositae" = "Asteraceae", "Palmae" = "Arecaceae",
              "Leguminosae" = "Fabaceae", "Guttiferae" = "Clusiaceae",
              "Cruciferae" = "Brassicaceae", "Umbelliferae" = "Apiaceae",
              "Labiatae" = "Lamiaceae", "Graminae" = "Poaceae",
              "Capparidaceae" = "Capparaceae", "Avicenniaceae"= "Acanthaceae",
              "Ficoidaceae" = "Aizoaceae", "Mesembryanthemaceae" = "Aizoaceae",
              "Tetragoniaceae" = "Aizoaceae",
              "Limnocharitaceae" = "Alismataceae",
              "Chenopodiaceae" = "Amaranthaceae",
              "Alliaceae" = "Amaryllidaceae", "Asclepiadaceae" = "Apocynaceae",
              "Periplocaceae" = "Apocynaceae", "Lemnaceae" = "Araceae",
              "Hydnoraceae" = "Aristolochiaceae",
              "Lactoridaceae" = "Aristolochiaceae",
              "Agavaceae" = "Asparagaceae", "Dracaenaceae" = "Asparagaceae",
              "Eriospermaceae" = "Asparagaceae",
              "Hyacinthaceae" = "Asparagaceae", "Aloaceae" = "Asphodelaceae",
              "Hemerocallidaceae" = "Asphodelaceae",
              "Xanthorrhoeaceae" = "Asphodelaceae",
              "Cochlospermaceae" = "Bixaceae",
              "Hoplestigmataceae" = "Boraginaceae",
              "Hydrophyllaceae" = "Boraginaceae",
              "Haptanthaceae" = "Buxaceae", "Lobeliaceae" = "Campanulaceae",
              "Dipsacaceae" = "Caprifoliaceae",
              "Valerianaceae" = "Caprifoliaceae",
              "Illecebraceae" = "Caryophyllaceae",
              "Brexiaceae" = "Celastraceae", "Hippocrateaceae" = "Celastraceae",
              "Malesherbiaceae" = "Celastraceae",
              "Parnassiaceae" = "Celastraceae",
              "Cuscutaceae" = "Convolvulaceae", "Alangiaceae" = "Cornaceae",
              "Chailletiaceae" = "Dichapetalaceae",
              "Taccaceae" = "Dioscoreaceae", "Mimosaceae" = "Fabaceae",
              "Melianthaceae" = "Francoaceae", "Vivianiaceae" = "Francoaceae",
              "Najadaceae" = "Hydrocharitaceae",
              "Pterostemonaceae" = "Iteaceae",
              "Barringtoniaceae" = "Lecythidaceae",
              "Scytopetalaceae" = "Lecythidaceae",
              "Sonneratiaceae" = "Lythraceae", "Trapaceae" = "Lythraceae",
              "Bombacaceae" = "Malvaceae", "Byttneriaceae" = "Malvaceae",
              "Sterculiaceae" = "Malvaceae", "Tiliaceae" = "Malvaceae",
              "Heteropyxidaceae" = "Myrtaceae",
              "Octoknemataceae" = "Olacaceae",
              "Cyclocheilaceae" = "Orobanchaceae",
              "Nesogenaceae" = "Orobanchaceae", "Fumariaceae" = "Papaveraceae",
              "Turneraceae" = "Passifloraceae", "Oliniaceae" = "Penaeaceae",
              "Medusandraceae" = "Peridiscaceae",
              "Hymenocardiaceae" = "Phyllanthaceae",
              "Callitrichaceae" = "Plantaginaceae",
              "Zannichelliaceae" = "Potamogetonaceae",
              "Myrsinaceae" = "Primulaceae",
              "Theophrastaceae" = "Primulaceae",
              "Anarthriaceae" = "Restionaceae",
              "Centrolepidaceae" = "Restionaceae",
              "Rhipogonaceae" = "Ripogonaceae",
              "Ptaeroxylaceae" = "Rutaceae",
              "Flacourtiaceae" = "Salicaceae",
              "Samydaceae" = "Salicaceae", "Viscaceae" = "Santalaceae",
              "Illiciaceae" = "Schisandraceae",
              "Pellicieraceae" = "Tetrameristaceae",
              "Cecropiaceae" = "Urticaceae",
              "Adoxaceae" = "Viburnaceae", "Ampelidaceae" = "Vitaceae",
              "Leeaceae" = "Vitaceae", "Balanitaceae" = "Zygophyllaceae")
  tf <- family %in% names(rep_str)
  if(any(tf)) {
    for (i in seq_along(family[tf])) {
      family[tf][i] <- rep_str[names(rep_str) %in% family[tf][i]]}
  }
  return(family)
}
