#' Create global maps of species richness
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces global-scale maps of species richness at political
#' country and botanical country levels. Despite being originally designed to
#' create maps for all input data of any specified taxonomic group
#' (genus or family) from the search results with \code{powoSpecies}, the
#' function is also useful for any dataframe-formatted input data that has at
#' least a column with species and one or two columns with associated
#' distribution in the countries and/or botanical regions. Multiple richness
#' maps for any different taxonomic groups within the input data can be produced
#' automatically in a single run by just specifying a column name with the
#' associated taxonomic classification.
#'
#' @usage
#' powoMap(inputdf = NULL,
#'         botctrs = FALSE,
#'         distcol = NULL,
#'         taxclas = NULL,
#'         verbose = TRUE,
#'         save = FALSE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_map",
#'         format = "jpg")
#'
#' @param inputdf A dataframe with a species column and the associated global
#' distribution at country or botanical country level. The species name must be
#' as a binomial, i.e. must contain both the genus name and specific epithet,
#' but the authorship is optional. Each species must be as a single row with its
#' corresponding full distribution in all countries and/or botanical regions
#' within a single cell of their respective columns, where the country names or
#' botanical regions are separated by a comma. This is, for example, the
#' standard dataframe from the search results with the
#' function \code{powoSpecies}.
#'
#' @param botctrs Logical. If \code{TRUE}, the species richness maps will be
#' created according to the botanical country subdivisions of the world. Also, a
#' WGSRPD folder including the level 3 shapefile and the associated Brummitt's
#' (2001) book fully describing the World Geographical Scheme for Recording
#' Plant Distributions will be downloaded into the working directory. If you do
#' not remove this folder or rename any of the contents, then the function will
#' not download the same folder again. The default is \code{FALSE}.
#'
#' @param distcol Column name with the full global distribution data for each
#' species at political country level or the level 3 of botanical subdivision of
#' the [World Geographical Scheme](https://www.tdwg.org/standards/wgsrpd/)
#' for Recording Plant Distributions. If the species distribution is given with
#' botanical subdivisions, then you must also change the argument \code{botctrs}
#' to \code{TRUE}. If the distribution is described only by political country
#' names, then set \code{botctrs} to \code{FALSE}.
#'
#' @param taxclas A character vector with the column name for the corresponding
#' taxonomic classification of each species in any higher taxonomic level. If
#' provided, the function will produce, in a single run, all global richness
#' maps for every distinct group within the input data. The default is
#' \code{NULL}, then the function will generate only one global species richness
#' map for the entire input data.
#'
#' @param verbose Logical. If \code{FALSE}, the map creation steps will not be
#' printed in the console in full.
#'
#' @param save Logical. If \code{FALSE}, the global maps will not be saved
#' on disk.
#'
#' @param vir_color A character vector with the name or code of any of the color
#' palettes from [Viridis](https://CRAN.R-project.org/package=viridis) package.
#'
#' @param bre_color A character vector with the name or code of any of the color
#' palettes from [RColorBrewer](https://CRAN.R-project.org/package=RColorBrewer)
#' package.
#'
#' @param leg_title A character vector to be displayed in the output map as a
#' legend. Default is to create a title called **SR**, an acronym for species
#' richness.
#'
#' @param dpi One number in the range of 72-4000 referring to the image
#' resolution in the format of dots per inch in the output file. Default is to
#' create an output with 600 dpi.
#'
#' @param dir Pathway to the computer's directory, where the map file will be
#' saved provided that the argument \code{save} is set up in \code{TRUE}. The
#' default is to create a directory named **results_powoMap/** and the search
#' results will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. The default is to
#' create a file entitled **global_richness_map**.
#'
#' @param format A character vector related to the file format of the global
#' map to be saved. The default is "jpg" to save the output in Joint
#' Photographic Experts Group (.jpg), but you can also choose "pdf" to save in
#' Portable Document Format (.pdf), "tiff" to save in Tag Image File Format
#' (.tiff) or "png" to save in Portable Network Graphics (.png).
#'
#' @return One or a list of objects of class c("gg", "ggplot").
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{powoGenera}}
#'
#' @examples
#' \donttest{
#' library(expowo)
#'
#' mapspdist <- powoSpecies(family = "Lecythidaceae",
#'                          hybridspp = FALSE,
#'                          country = NULL,
#'                          verbose = TRUE,
#'                          save = FALSE,
#'                          dir = "results_powoSpecies/",
#'                          filename = "Lecythidaceae")
#'
#' # To create multiple maps for each genus within the input data according to
#' # political countries.
#' powoMap(inputdf = mapspdist,
#'         botctrs = FALSE,
#'         distcol = "native_to_country",
#'         taxclas = "genus",
#'         verbose = FALSE,
#'         save = FALSE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_country_map",
#'         format = "jpg")
#'
#' # To create multiple maps for each genus within the input data according to
#' # botanical country subdivisions.
#' powoMap(inputdf = mapspdist,
#'         botctrs = TRUE,
#'         distcol = "native_to_botanical_countries",
#'         taxclas = "genus",
#'         verbose = FALSE,
#'         save = FALSE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_botcountry_map",
#'         format = "jpg")
#'}
#'
#' @importFrom ggplot2 ggplot aes unit element_text geom_sf theme_void theme
#' @importFrom ggplot2 ggtitle scale_fill_viridis_c scale_fill_gradientn ggsave
#' @importFrom magrittr "%>%"
#' @importFrom rnaturalearth ne_countries
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp merge
#' @importFrom utils download.file unzip
#'
#' @export
#'

powoMap <- function(inputdf = NULL,
                    botctrs = FALSE,
                    distcol = NULL,
                    taxclas = NULL,
                    verbose = TRUE,
                    save = FALSE,
                    vir_color = "viridis",
                    bre_color = NULL,
                    leg_title = "SR",
                    dpi = 600,
                    dir = "results_powoMap/",
                    filename = "global_richness_map",
                    format = "jpg") {

  # Data check
  inputdf <- .arg_check_data_map(inputdf, distcol)

  # Format check
  .arg_check_format(format)

  # Dir check
  dir <- .arg_check_dir(dir)

  # If the distribution is according to botanical subdivisions, load global map
  if (botctrs) {

    # Check if the folder /WGSRPD is in any directory within the user's
    # machine.
    if (!dir.exists("WGSRPD")){

      # Download level 3 of botanical subdivision of the World
      download.file(url = "https://github.com/tdwg/wgsrpd/archive/master.zip",
                    destfile = "wgsrpd-master.zip")

      # Unzip the .zip file
      unzip(zipfile = "wgsrpd-master.zip")
      unlink("wgsrpd-master.zip")

      file.rename("wgsrpd-master", "WGSRPD")
      file.rename("WGSRPD/109-488-1-ED", "WGSRPD/book-1-ED")
      file.rename("WGSRPD/README.md", "WGSRPD/README.txt")

      # Remove the downloaded .zip file and other folders which will not be
      # useful for plotting.
      unlink("WGSRPD/level1", recursive = TRUE)
      unlink("WGSRPD/level2", recursive = TRUE)
      unlink("WGSRPD/level4", recursive = TRUE)
      unlink("WGSRPD/geojson", recursive = TRUE)
    }

    # Read the botanical divisions map of level 3
    world <- sp::st_read("WGSRPD/level3/level3.shp")

    # Remove Antarctica
    world <- world[!world$LEVEL3_NAM %in% "Antarctica", ]

  } else {
    # Load the world map from rnaturalearth
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    # Remove Antarctica
    world <- world[!world$admin %in% "Antarctica", ]

    # Replacing the non-ASCII char in the world$admin column
    world$admin <- gsub("\u00e7", "c", world$admin)

    # Harmonize country names between the species list and World sf dataframe
    inputdf <- .harmonize_ctr(inputdf, distcol)
  }

  # Create a new directory to save the plot
  # If there is no directory... make one!
  todaydate <- format(Sys.time(), "%d%b%Y")
  folder_name <- paste0(dir, todaydate)
  fullname <- paste0(folder_name, "/", filename, ".", format)
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }

  # Making the global map of species richness for an entire group, i.e. all
  # data available in the table (e.g. all families, a single family or a genus).
  if (is.null(taxclas)) {

    # Making world map with species richness across countries/botanical
    # subdivisions.
    world_plant <- .get_SR(inputdf, world, botctrs, distcol, verbose)

    p <- .ggplot_map(world_plant,
                     taxclas,
                     tax = NULL,
                     leg_title,
                     vir_color,
                     bre_color)
    if (save) {
      .save_map(p,
                taxclas,
                tax = NULL,
                verbose,
                vir_color,
                bre_color,
                fullname,
                dpi,
                format)
    }
    return(p)
  }

  # Making the maps for each taxonomic group separately
  if (!is.null(taxclas)) {
    tax <- unique(inputdf[[taxclas]])
    p <- list()
    for (i in seq_along(tax)) {
      temp_inputdf <- inputdf %>% filter(inputdf[[taxclas]] == tax[i])

      # Making world map with species richness across countries/botanical
      # subdivisions.
      world_plant <- .get_SR(temp_inputdf, world, botctrs, distcol, verbose)

      p[[i]] <- .ggplot_map(world_plant,
                            taxclas,
                            tax = tax[i],
                            leg_title,
                            vir_color,
                            bre_color)
      if (save) {
        .save_map(p[[i]],
                  taxclas,
                  tax = tax[i],
                  verbose,
                  vir_color,
                  bre_color,
                  fullname,
                  dpi,
                  format)
      }
    }
    names(p) <- tax
    return(p)
  }
}


# Auxiliary function to make world map with species richness across
# countries/botanical subdivisions.
.get_SR <- function(df, world, botctrs, distcol, verbose) {

  country_data <- list()

  for (i in seq_along(df[[distcol]])) {
    country_data[[i]] <- gsub("^\\s", "", strsplit(df[[distcol]][i], ",")[[1]])
  }
  names(country_data) <- df$taxon_name

  sp_div <- unlist(country_data)
  names(sp_div) <- gsub("[0-9]", "", names(sp_div))

  country_data <- data.frame(table(sp_div))
  names(country_data)[1] <- "countries"

  # Merge the plant distribution data with global map
  world_plant <- sp::merge(world, country_data,
                           by.x = ifelse(!botctrs,
                                         "admin", "LEVEL3_NAM"),
                           by.y = "countries",
                           all.x = T)

  # Replacing NA values for zero.
  world_plant$Freq[is.na(world_plant$Freq)] = 0

  return(world_plant)
}


# Auxiliary function to create maps in different styles.
.ggplot_map <- function(world_plant,
                        taxclas,
                        tax = NULL,
                        leg_title,
                        vir_color,
                        bre_color) {

  p <- ggplot2::ggplot(data = world_plant) +
    ggplot2::geom_sf(ggplot2::aes(fill = world_plant$Freq), colour = "gray60",
                     size = 0.05) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = c(0.2, 0.3),
                   legend.title = ggplot2::element_text(size = 6,
                                                        face = "bold"),
                   legend.text = ggplot2::element_text(size = 4),
                   legend.key.size = ggplot2::unit(0.3, "cm")) +
    ggplot2::ggtitle("")

  if (is.null(taxclas)) {
    if (!is.null(vir_color)) {
      p <- p + ggplot2::scale_fill_viridis_c(name = leg_title,
                                             direction = -1,
                                             option = vir_color,
                                             limits = c(0,
                                                        max(world_plant$Freq)))
      return(p)
    }
    if (!is.null(bre_color)) {
      bre_col <- rev(RColorBrewer::brewer.pal(9, bre_color))
      p <- p +
        ggplot2::scale_fill_gradientn(name = leg_title,
                                      colours = bre_col,
                                      limits = c(0, max(world_plant$Freq)),
                                      na.value =  "white")
      return(p)
    }
  }

  # Create the leg title of scale bar and modify taxonomic name to italic and
  # bold.
  if (!is.null(taxclas)) {
    tax_name <- eval(bquote(expression(bolditalic(.(tax))~bold(.(leg_title)))))
    if (!is.null(vir_color)) {
      p <- p +
        ggplot2::scale_fill_viridis_c(name = tax_name,
                                      direction = -1,
                                      option = vir_color,
                                      limits = c(0, max(world_plant$Freq)))
      return(p)
    }
    if (!is.null(bre_color)) {
      bre_col <- rev(RColorBrewer::brewer.pal(9, bre_color))
      p <- p +
        ggplot2::scale_fill_gradientn(name = tax_name,
                                      colours = bre_col,
                                      limits = c(0, max(world_plant$Freq)),
                                      na.value = "white")
      return(p)
    }
  }
}


# Auxiliary function to save the maps
.save_map <- function(p,
                      taxclas,
                      tax,
                      verbose,
                      vir_color,
                      bre_color,
                      fullname,
                      dpi,
                      format) {

  # Set dimensions of the images to be saved
  w = 9.26
  h = 4.85
  u = "in"

  if (is.null(taxclas)) {
    if (!is.null(vir_color)) {
      if(verbose) {
        cat(paste("Saving", w, "x", h, u, "image\n"))
      }
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_",
                                                       vir_color, ".",
                                                       format), fullname),
                      p, dpi = dpi, bg = "white",
                      width = w, height = h, units = u)
    }
    if (!is.null(bre_color)) {
      if(verbose) {
        cat(paste("Saving", w, "x", h, u, "image\n"))
      }
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_",
                                                       bre_color, ".",
                                                       format), fullname),
                      p, dpi = dpi, bg = "white",
                      width = w, height = h, units = u)
    }
  }

  # Create the leg title of scale bar and modify taxonomic name to italic and bold
  if (!is.null(taxclas)) {
    if (!is.null(vir_color)) {
      if(verbose) {
        cat(paste("Saving", w, "x", h, u, "image\n"))
      }
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_", tax,
                                                       "_", vir_color, ".",
                                                       format), fullname),
                      p, dpi = dpi, bg = "white",
                      width = w, height = h, units = u)
    }
    if (!is.null(bre_color)) {
      if(verbose) {
        cat(paste("Saving", w, "x", h, u, "image\n"))
      }
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_", tax,
                                                       "_", bre_color, ".",
                                                       format), fullname),
                      p, dpi = dpi, bg = "white",
                      width = w, height = h, units = u)
    }
  }
}


# Auxiliary function to harmonize country names between the species list and
# World sf dataframe.
.harmonize_ctr <- function(inputdf, distcol) {
  ctr <- c("Bahamas", "United States Minor Outlying Islands",
           "Bonaire, Saint Eustatius and Saba", "Bouvet Island",
           "Christmas Island", "Clipperton Island", "Cocos Islands",
           "Cote d'Ivoire",
           "French Southern Territories",
           "Micronesia", "French Guiana", "Palestina",
           "Gibraltar", "Guadeloupe", "Guinea-Bissau",
           "Hong Kong", "Macao", "Martinique", "Mayotte",
           "Reunion", "Saint-Barthelemy", "Saint-Martin",
           "Serbia",
           "South Georgia and the South Sandwich Islands", "Spratly islands",
           "Svalbard and Jan Mayen", "Tanzania", "Tokelau",
           "Tuvalu", "United States",
           "Vatican City", "Virgin Islands, U.S.", "Paracel Islands")

  ctr_change <- c("The Bahamas", "United States of America",
                  "Netherlands", "Norway",
                  "Australia", "France", "Australia",
                  "Ivory Coast",
                  "French Southern and Antarctic Lands",
                  "Federated States of Micronesia", "France", "Palestine",
                  "United Kingdom", "France", "Guinea Bissau",
                  "Hong Kong S.A.R.", "Macao S.A.R", "France", "France",
                  "France", "Saint Barthelemy", "Saint Martin",
                  "Republic of Serbia",
                  "South Georgia and South Sandwich Islands", "China",
                  "Norway", "United Republic of Tanzania", "New Zealand",
                  "United Kingdom", "United States of America",
                  "Vatican", "United States Virgin Islands", "China")

  for (i in seq_along(ctr)) {
    inputdf[[distcol]] <- gsub(ctr[i], ctr_change[i], inputdf[[distcol]])
  }
  return(inputdf)
}
