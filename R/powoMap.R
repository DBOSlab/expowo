#' Mapping species distribution from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a global-scale map associated with all accepted
#' species and their geographical distribution from any target genus or family
#' of flowering plants at
#' [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' powoMap(data = NULL,
#'         map_div = "native_to_country",
#'         multigen = FALSE,
#'         verbose = TRUE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_map",
#'         format = "pdf")
#'
#' @param data A data frame created using the function \code{powoSpecies} with
#' species' global distribution.
#'
#' @param map_div One of two columns of native distribution from the input data
#' created using \code{powoSpecies} function. It has to be "native_to_country"
#' associated to political country, and "native_to_botanical_countries"
#' according to the level 3 of botanical subdivision of the World, from
#' [World Geographical Scheme for Recording Plant Distributions]
#' (https://www.tdwg.org/standards/wgsrpd/).
#'
#' @param multigen Logical, if \code{TRUE}, the map results will include
#' more than one genus.
#'
#' @param verbose Logical, if \code{FALSE}, the map creation steps will not be
#' printed in the console in full.
#'
#' @param vir_color A character vector with the name or code of one of the color
#' palettes from [Viridis](https://CRAN.R-project.org/package=viridis) package.
#'
#' @param bre_color A character vector with the name or code of one of the color
#' palettes from [RColorBrewer](https://CRAN.R-project.org/package=RColorBrewer)
#' package.
#'
#' @param leg_title A character vector to be displayed in the output map as a
#' legend. Default is to create one title called **SR**, which means species
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
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **global_richness_map**.
#'
#' @param format A character vector related to the file format that the global
#' map will be saved, which can be, e.g., a Portable Document Format (.pdf),
#' Tag Image File Format (.tiff), or Joint Photographic Experts Group (.jpeg).
#' Default is to save the output in **jpg** format.
#'
#' @return Global map in .pdf format and saves the output on disk.
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{toptenGen}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{powoGenera}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#' library(taxize)
#'
#' powocodes <- cbind(family = "Lecythidaceae",
#'                    data.frame(taxize::get_pow("Lecythidaceae")))
#'
#' mapspdist <- powoSpecies(powocodes$family, powocodes$uri,
#'                          hybridspp = FALSE,
#'                          country = NULL,
#'                          verbose = TRUE,
#'                          save = TRUE,
#'                          dir = "results_powoSpecies/",
#'                          filename = "Lecythidaceae")
#'
#'# To create a map according to political countries
#' powoMap(data = mapspdist,
#'         botctrs = NULL,
#'         map_div = "native_to_country",
#'         multigen = TRUE,
#'         verbose = FALSE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_country_map",
#'         format = "jpg")
#'
#'# To create a map according to botanical countries subdivision
#' data(botdivmap)
#' powoMap(data = mapspdist,
#'         botctrs = botdivmap,
#'         map_div = "native_to_botanical_countries",
#'         multigen = TRUE,
#'         verbose = FALSE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_botcountry_map",
#'         format = "jpg")
#'
#'}
#'
#' @importFrom ggplot2 ggplot aes unit element_text geom_sf theme_void theme
#' @importFrom ggplot2 ggtitle scale_fill_viridis_c scale_fill_gradientn ggsave
#' @importFrom magrittr "%>%"
#' @importFrom rnaturalearth ne_countries
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp merge
#'
#' @export
#'

powoMap <- function(data = NULL,
                    botctrs = NULL,
                    map_div = "native_to_country",
                    multigen = FALSE,
                    verbose = TRUE,
                    vir_color = "viridis",
                    bre_color = NULL,
                    leg_title = "SR",
                    dpi = 600,
                    dir = "results_powoMap/",
                    filename = "global_richness_map",
                    format = "jpg") {
  #

  # Load global map
  if (map_div == "native_to_country") {
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    # Remove Antarctica
    world <- world[!world$admin %in% "Antarctica", ]

    # Harmonize country names between the species list and World sf dataframe
    # Crosscheck according to rnaturalearth site - Tuvalu, Bonaire
    ctr <- c("Bahamas", "United States Minor Outlying Islands",
             "Bonaire, Saint Eustatius and Saba", "Bouvet Island",
             "Christmas Island", "Clipperton Island", "Cocos Islands",
             "Curacao", "Cote d'Ivoire",
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
                    "Curaçao", "Ivory Coast",
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
      data[[map_div]] <- gsub(ctr[i], ctr_change[i], data[[map_div]])
    }

  }

  if (map_div == "native_to_botanical_countries") {
    world <- botctrs
  }

  #_____________________________________________________________________________
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

  #_____________________________________________________________________________
  # Plotting the global map of species richness for an entire group, i.e. all
  # data available in the table (e.g. all families, a single family or a genus).
  if (multigen == FALSE) {

    # Making world map with species richness across countries/botanical
    # subdivisions.
    world_plant <- .get_SR(data, world, map_div, verbose)

    # Making the maps and saving them as figures in different styles.
    .save_map(world_plant,
              multigen,
              gen = NULL,
              leg_title,
              vir_color,
              bre_color,
              fullname,
              dpi,
              format)
  }

  #_____________________________________________________________________________
  # Plotting figures for each genus separately.
  if (multigen == TRUE) {

    gen <- unique(data$genus)

    for (i in seq_along(gen)) {
      temp_data <- data %>% filter(genus == gen[i])

      # Making world map with species richness across countries/botanical
      # subdivisions.
      world_plant <- .get_SR(temp_data, world, map_div, verbose)

      # Making the maps and saving them as figures in different styles.
      .save_map(world_plant,
                multigen,
                gen = gen[i],
                leg_title,
                vir_color,
                bre_color,
                fullname,
                dpi,
                format)
    }
  }

}


# Auxiliary function to make world map with species richness across
# countries/botanical subdivisions.
.get_SR <- function(df, world, map_div, verbose) {

  country_data <- list()

  for (i in seq_along(df[[map_div]])) {
    country_data[[i]] <- gsub("^\\s", "", strsplit(df[[map_div]][i], ",")[[1]])
  }
  names(country_data) <- df$taxon_name

  sp_div <- unlist(country_data)
  names(sp_div) <- gsub("[0-9]", "", names(sp_div))

  country_data <- data.frame(table(sp_div))
  names(country_data)[1] <- "countries"

  # Merge the plant distribution data with global map.
  world_plant <- sp::merge(world, country_data,
                           by.x = ifelse(map_div == "native_to_country",
                                         "admin", "LEVEL3_NAM"),
                           by.y = "countries",
                           all.x = T)

  # Replacing NA values for zero.
  world_plant$Freq[is.na(world_plant$Freq)] = 0

  return(world_plant)
}

#_______________________________________________________________________________
# Auxiliary function to create maps and saving them as figures in different
# styles.
.save_map <- function(world_plant,
                      multigen,
                      gen,
                      leg_title,
                      vir_color,
                      bre_color,
                      fullname,
                      dpi,
                      format) {

  p <- ggplot2::ggplot(data = world_plant) +
    ggplot2::geom_sf(ggplot2::aes(fill = Freq), colour = "gray60", size = 0.1) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = c(0.2, 0.3),
                   legend.title = ggplot2::element_text(size = 6,
                                                        face = "bold"),
                   legend.text = ggplot2::element_text(size = 4),
                   legend.key.size = ggplot2::unit(0.3, "cm")) +
    ggplot2::ggtitle("")

  if (multigen == FALSE) {
    if (!is.null(vir_color)) {
      p <- p + ggplot2::scale_fill_viridis_c(name = leg_title,
                                             option = vir_color,
                                             limits = c(0,
                                                        max(world_plant$Freq)))
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_",
                                                       vir_color, ".",
                                                       format), fullname),
                      p, dpi=dpi, bg="white")
    }
    if (!is.null(bre_color)) {
      color1 <- rev(RColorBrewer::brewer.pal(9, bre_color))
      p <- p +
        ggplot2::scale_fill_gradientn(name = leg_title,
                                      colours = color1,
                                      limits = c(0, max(world_plant$Freq)),
                                      na.value =  "white")
        ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_",
                                                         bre_color, ".",
                                                         format), fullname),
                        p, dpi=dpi, bg="white")
    }
  }

  # Create the leg title of scale bar and modify gen name to italic and bold
  name1 <- eval(bquote(expression(bolditalic(.(gen)) ~ bold(.(leg_title)))))
  if (multigen == TRUE) {
    if (!is.null(vir_color)) {
      p <- p +
        ggplot2::scale_fill_viridis_c(name = name1,
                                      option = vir_color,
                                      limits = c(0, max(world_plant$Freq)))
        ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_", gen,
                                                         "_", vir_color, ".",
                                                         format), fullname),
                        p, dpi=dpi, bg="white")
    }
    if (!is.null(bre_color)) {
      color1 <- rev(RColorBrewer::brewer.pal(9, bre_color))
      p <- p +
        ggplot2::scale_fill_gradientn(name = name1,
                                      colours = color1,
                                      limits = c(0, max(world_plant$Freq)),
                                      na.value = "white")
        ggplot2::ggsave(gsub(paste0(".", format), paste0("_SR_", gen,
                                                         "_", bre_color, ".",
                                                         format), fullname),
                        p, dpi=dpi, bg="white")
    }
  }
}
