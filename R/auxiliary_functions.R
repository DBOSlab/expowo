# Small functions to support main functions
# Author: Debora Zuanny & Domingos Cardoso


#_______________________________________________________________________________
# Function to save the results in CSV format in an specific directory
.save_df <- function (save, dir, filename, df) {

  # Saving the dataframe if param save is TRUE.
  if (save) {
  # Create a new directory to save the results with current date
  # If there is no directory... make one!
  todaydate <- format(Sys.time(), "%d%b%Y")
  folder_name <- paste0(dir, todaydate)
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  # Create and save the spreadsheet in .csv format
  fullname <- paste0(folder_name, "/", filename, ".csv")
  print(paste0("Writing the spreadsheet '", filename, ".csv' within '",
               folder_name, "' on disk."))
  data.table::fwrite(df,
                     file = fullname,
                     sep = ",",
                     row.names = FALSE,
                     col.names = TRUE)
  }
}


#_______________________________________________________________________________
# Secondary function to find related country for each botanical subdivision.
.botdiv_to_countries <- function(x, i) {

  # Read data of botanical subdivisions and convert non-ASCII chars
  # botregions <- read.csv("dataraw/botanical_country_subdivisions.csv")
  # botregions$country <- iconv(botregions$country,
  #                             from = "UTF-8",
  #                             to = "ASCII//TRANSLIT")
  # botregions$botanical_division <- iconv(botregions$botanical_division,
  #                                        from = "UTF-8",
  #                                        to = "ASCII//TRANSLIT")
  # write.csv(botregions, "dataraw/botanical_country_subdivisions.csv",
  #           row.names=FALSE)

  # Read data of botanical subdivisions.
  utils::data("botregions", package = "expowo")

  # Find duplicated botanical regions
  # botregions$botanical_division[duplicated(botregions$botanical_division)]

  temp <- strsplit(x[[i]], ", ")[[1]]

  for (n in seq_along(temp)) {
    tf <- botregions$botanical_division %in% temp[n]
    if (length(botregions$country[tf]) == 0) {

      # use strtrim to limit the length of each character/botanical region
      # because POWO has limited chars.
      bot_temp <- strtrim(botregions$botanical_division, 20)
      tf <- bot_temp %in% temp[n]
      if (length(botregions$country[tf]) > 0) {

        temp[n] <- botregions$country[tf]
      }
    } else {
      if (any(!botregions$country[tf] %in% temp[n])) {

        if (length(botregions$country[tf]) > 1) {
          temp[n] <- paste(botregions$country[tf], collapse = ", ")
        } else {
          temp[n] <- botregions$country[tf]
        }
      }
    }
  }
  temp <- sort(unique(temp))

  x[[i]] <- paste(temp, collapse = ", ")

  g <- grepl("<", x[[i]])
  if (any(g)) {
    x[[i]][g] <- "unknown"
  }

  return(x[[i]])
}


#_______________________________________________________________________________
# Filling with NA some lists that returned NULL results
.fill_NA <- function(x) {
  temp <- lapply(x, is.null)
  x[unlist(temp)] <- NA
  temp <- lapply(x, function(x) length(x) == 0)
  x[unlist(temp)] <- NA
  return(x)
}

