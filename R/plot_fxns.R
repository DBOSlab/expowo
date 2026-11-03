# Auxiliary functions shared by endemicMap, discoveryGraph and introGraph
# Author: Debora Zuanny & Domingos Cardoso

# Colours: a blue sequential ramp for counts, plus blue and orange for two
# series and a light grey for zero values.
.expowo_blues <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf",
                   "#184f95", "#0d366b")
.expowo_cols <- c(blue = "#2a78d6", orange = "#eb6834", zero = "#f0efec",
                  ink = "#0b0b0b", ink2 = "#52514e", grid = "#e9e8e4")

.level3_url <- "https://raw.githubusercontent.com/tdwg/wgsrpd/master/level3/"

#_______________________________________________________________________________
# Function to check that the input data has the columns needed by a function
.arg_check_columns <- function(inputdf, columns, hint = NULL) {
  if (!is.data.frame(inputdf)) {
    stop("The argument inputdf should be a data frame, such as the output of ",
         "powoSpecies.", call. = FALSE)
  }
  missing_cols <- columns[!columns %in% names(inputdf)]
  if (length(missing_cols) > 0) {
    stop(paste0("The input data lacks the column(s) ",
                paste(missing_cols, collapse = ", "), ".", hint),
         call. = FALSE)
  }
}

#_______________________________________________________________________________
# Function to keep only accepted names, when the input data has synonyms
.accepted_only <- function(inputdf) {
  if ("status" %in% names(inputdf)) {
    inputdf <- inputdf[inputdf$status %in% "Accepted", ]
  }
  return(inputdf)
}

#_______________________________________________________________________________
# Function to split comma-separated distribution cells into vectors of areas
.split_areas <- function(x) {
  lapply(strsplit(as.character(x), ",\\s*"),
         function(a) a[!is.na(a) & !a %in% c("unknown", "")])
}

#_______________________________________________________________________________
# Function to get the year of publication, e.g. "Sp. Pl.: 618 (1753)" -> 1753
.pub_year <- function(x) {
  year <- rep(NA_real_, length(x))
  tf <- grepl("[(][0-9]{4}", x)
  year[tf] <- as.numeric(sub(".*?[(]([0-9]{4}).*", "\\1", x[tf]))
  return(year)
}

#_______________________________________________________________________________
# Function to load the TDWG level-3 shapefile of botanical countries. The four
# shapefile files (about 4 MB) are downloaded only once into the cache folder.
# Areas are named by their level-3 code, exactly as returned by expowo.
.wgsrpd_level3 <- function(verbose = TRUE) {
  shp_dir <- file.path(tools::R_user_dir("expowo", which = "cache"), "level3")
  files <- file.path(shp_dir, paste0("level3.", c("shp", "shx", "dbf", "prj")))

  if (!all(file.exists(files))) {
    if (verbose) {
      message("Downloading the TDWG map of botanical countries (only once)...")
    }
    dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
    for (f in files) {
      utils::download.file(paste0(.level3_url, basename(f)), f, mode = "wb",
                           quiet = TRUE)
    }
  }

  world <- sf::st_read(files[1], quiet = TRUE)
  world <- world[world$LEVEL3_COD != "ANT", ]
  world$botanical_country <-
    wgsrpd_l3$botanical_country[match(world$LEVEL3_COD, wgsrpd_l3$code)]

  return(world)
}

#_______________________________________________________________________________
# Theme of the expowo graphics
.theme_expowo <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = .expowo_cols[["grid"]],
                                               linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = .expowo_cols[["ink2"]]),
      axis.title = ggplot2::element_text(colour = .expowo_cols[["ink2"]]),
      strip.text = ggplot2::element_text(colour = .expowo_cols[["ink"]],
                                         face = "bold", hjust = 0),
      legend.text = ggplot2::element_text(colour = .expowo_cols[["ink2"]]),
      legend.title = ggplot2::element_text(colour = .expowo_cols[["ink2"]]),
      plot.margin = ggplot2::margin(10, 14, 10, 10))
}

#_______________________________________________________________________________
# Function to save a graphic within a current date folder
.save_graph <- function(p, dir, filename, format, dpi, width, height,
                        verbose) {
  foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  if (!dir.exists(foldername)) {
    dir.create(foldername)
  }
  fullname <- paste0(foldername, "/", filename, ".", format)
  if (verbose) {
    message(paste0("Writing '", filename, ".", format, "' within '",
                   foldername, "' on disk."))
  }
  ggplot2::ggsave(fullname, p, dpi = dpi, bg = "white", width = width,
                  height = height, units = "in")
  invisible(fullname)
}
