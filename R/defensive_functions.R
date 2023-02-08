# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Debora Zuanny & Domingos Cardoso

#_______________________________________________________________________________
# Function to check if any family or URI is missing
.arg_check_fam_uri <- function (family, uri) {

  if(length(family) != length(uri)) {
    stop(paste("Any family or URI is missing."))
  }

  utils::data("POWOcodes")
  uri_log <- uri %in% POWOcodes$uri
  uri_log <- which(uri_log == FALSE)
  if(length(uri_log) >= 1) {
    stop(paste("Any family's URI address is incomplete or misspelled and cannot
               open connection with POWO website."))
  }
}


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
.arg_check_dir<- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"character" %in% class_x) {
    stop(paste0("The argument dir should be a character, not '", class_x, "'."),
         call. = FALSE)
  }
  if (!grepl("[/]$", x)) {
    x <- paste0(x, "/")
  }
  return(x)
}


#_______________________________________________________________________________
# Function to check if the input data for powoMap is a dataframe-formatted
# object generated from powoSpecies.
.arg_check_data_map <- function (data) {
  col_names <- c("family", "genus", "species", "taxon_name", "authors",
                 "native_to_country", "native_to_botanical_countries",
                 "kew_id", "powo_uri")
  tf <- col_names %in% names(data)
  if(any(!tf)) {
    stop(paste0("Make sure you have used an input data generated from
                powoSpecies.\n\n",
               paste0("The input data must have at least the following
                      columns:\n\n"),
                paste0(col_names[c(1,2, 3, 6, 7)], collapse = ", ")))
  }
}


#_______________________________________________________________________________
# Function to check if the format argument for powoMap is any of jpg, pdf, tiff,
# or png
.arg_check_format <- function (format) {
  format_supported <- c("jpg", "jpeg", "pdf", "tif", "tiff", "png")
  format_supported <- append(format_supported, toupper(format_supported))
  tf <- format %in% format_supported
  if(any(!tf)) {
    stop(paste0("Make sure the format argument has any of the
                following format names:\n\n",
                paste0(format_supported, collapse = ", ")))
  }
}


#_______________________________________________________________________________
# Function to check if any family name provided is a synonymous and does not
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
}
