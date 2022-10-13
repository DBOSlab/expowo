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


