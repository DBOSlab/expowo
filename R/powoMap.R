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
#'         SR_PD = "SR",
#'         multigen = FALSE,
#'         verbose = TRUE,
#'         vir_color = "viridis",
#'         bre_color = NULL,
#'         map_title = "(a)",
#'         leg_title = "SR",
#'         dpi = 600,
#'         dir = "results_powoMap/",
#'         filename = "global_richness_map",
#'         format = "pdf")
#'
#' @param data a vector of multiple families
#' that are present in POWO.
#'
#' @param uri URI address for each family associated to the target genus to be
#' searched in POWO.
#'
#' @param genus Either one genus name or a vector of multiple genera
#' that are present in POWO. If any genus name is not provided, then the
#' function will search any species from all accepted genera known for the
#' target family.
#'
#' @param hybridspp Logical, if \code{TRUE}, the search results will include
#' hybrid species.
#'
#' @param country Either one country name or a vector of multiple countries.
#' If country names are provided, then the function will return only the species
#' that are native to such countries, according to POWO.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_powoSpecies** and the search
#' results will be saved within a subfolder named by the current date.
#'
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **output**.
#'
#' @param filename Name of the output file to be saved. Default is to create a
#' file entitled **output**.
#'
#' @seealso \code{\link{powoSpecies}}
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
#'  powoMap(mapspdist,
#'          map_div = "native_to_country",
#'          SR_PD = "SR",
#'          multigen = FALSE,
#'          verbose = FALSE,
#'          vir_color = "viridis",
#'          bre_color = NULL,
#'          map_title = "(a)",
#'          leg_title = "SR",
#'          dpi = 600,
#'          dir = "results_powoMap/",
#'          filename = "global_richness_map",
#'          format = "pdf")
#'}
#'
#' @importFrom ggplot2 ggplot aes unit element_text geom_sf theme_void theme ggtitle scale_fill_viridis_c scale_fill_gradientn ggsave
#' @importFrom magrittr "%>%"
#' @importFrom rnaturalearth ne_countries
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp merge
#' @importFrom V.PhyloMaker phylo.maker
#' @importFrom picante pd
#'
#' @export
#'

powoMap <- function(data = NULL,
                    map_div = "native_to_country",
                    SR_PD = "SR",
                    multigen = FALSE,
                    verbose = TRUE,
                    vir_color = "viridis",
                    bre_color = NULL,
                    map_title = "(a)",
                    leg_title = "SR",
                    dpi = 600,
                    dir = "results_powoMap/",
                    filename = "global_richness_map",
                    format = "pdf") {


  # Load global map
  if (map_div == "native_to_country") {
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    # Remove Antarctica
    world <- world[!world$admin %in% "Antarctica", ]

    # Harmonize country names between the species list and World sf dataframe
    unique(botregions$country)[!unique(botregions$country) %in% world$admin]
    ctr <- c("United States", "United States Minor Outlying Islands")
    ctr_change <- c("United States of America", "United States of America")
    for (i in seq_along(ctr)) {
      data[[map_div]] <- gsub(ctr[i], ctr_change[i], data[[map_div]])
    }

  }

  if (map_div == "native_to_botanical_countries") {
    data(botdivmap)
    world <- botdivmap
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

  #_______________________________________________________________________________
  # Plotting the global map of species richness for an entire group, i.e. all data
  # available in the table (e.g. all families, a single family or a genus)
  if (multigen == FALSE) {

    # Making world map with species richness across countries/botanical subdivisions
    world_plant <- .get_SR_PD(data, world, map_div, SR_PD, verbose)

    # Making the maps and saving them as figures in different styles
    .save_map(world_plant,
              multigen,
              gen = NULL,
              map_title,
              leg_title,
              vir_color,
              bre_color,
              fullname,
              SR_PD,
              dpi,
              format)
  }

  #_______________________________________________________________________________
  # Plotting figures for each genus separately
  if (multigen == TRUE) {

    gen <- unique(data$genus)

    for (i in seq_along(gen)) {
      temp_data <- data %>% filter(genus == gen[i])

      # Making world map with species richness across countries/botanical subdivisions
      world_plant <- .get_SR_PD(temp_data, world, map_div, SR_PD, verbose)

      # Making the maps and saving them as figures in different styles
      .save_map(world_plant,
                multigen,
                gen = gen[i],
                map_title,
                leg_title,
                vir_color,
                bre_color,
                fullname,
                SR_PD,
                dpi,
                format)
    }
  }

}


# Auxiliary function to make world map with species richness across countries/botanical subdivisions
.get_SR_PD <- function(df, world, map_div, SR_PD, verbose) {

  country_data <- list()

  for (i in seq_along(df[[map_div]])) {
    country_data[[i]] <- gsub("^\\s", "", strsplit(df[[map_div]][i], ",")[[1]])
  }
  names(country_data) <- df$taxon_name

  sp_div <- unlist(country_data)
  names(sp_div) <- gsub("[0-9]", "", names(sp_div))

  if (SR_PD == "SR") {
    country_data <- data.frame(table(sp_div))
    names(country_data)[1] <- "countries"
  }

  if (SR_PD == "PD") {
    country_data <- data.frame(countries = sort(unique(sp_div)),
                               Freq = NA)

    for (i in seq_along(country_data$countries)) {
      # Get species pool in each country
      spplist <- unique(names(sp_div)[sp_div %in% country_data$countries[i]])

      # Create input data for building the community phylogeny with V.PhyloMaker
      spplist <- data.frame(species = spplist,
                            genus = gsub("\\s.*", "", spplist),
                            family = df$family[df$taxon_name %in% spplist])

      if (verbose) {
        print(paste0("Estimating PD for ",
                     country_data$countries[i], "... ",
                     i, "/", length(country_data$countries), " countries"))
      }

      # Build community phylogeny for each country
      tree <- V.PhyloMaker::phylo.maker(sp.list = spplist,
                                        tree = GBOTB.extended, nodes = nodes.info.1,
                                        scenarios = "S3")

      # To keep any basal trichotomy/polytomy but forcing the tree as rooted:
      # is.rooted(tree$scenario.3)
      tree$scenario.3$root.edge <- 0

      # Create input matrix data for calculating PD with picante
      sppmat <- t(matrix(rep(1, length(spplist$species))))
      rownames(sppmat) <- country_data$countries[i]
      colnames(sppmat) <- gsub("\\s", "_", spplist$species)

      # Calculate PD
      phylod <- picante::pd(sppmat, tree$scenario.3, include.root = TRUE)

      country_data$Freq[i] <- round(phylod$PD, 2)
    }
  }

  # Merge the plant distribution data with global map
  world_plant <- sp::merge(world, country_data,
                           by.x = "admin",
                           by.y = "countries",
                           all.x = T)
  # Replacing NA values for zero
  world_plant$Freq[is.na(world_plant$Freq)] = 0

  return(world_plant)
}


# Auxiliary function to create maps and saving them as figures in different styles
.save_map <- function(world_plant,
                      multigen,
                      gen,
                      map_title,
                      leg_title,
                      vir_color,
                      bre_color,
                      fullname,
                      SR_PD,
                      dpi,
                      format) {

  p <- ggplot2::ggplot(data = world_plant) +
    ggplot2::geom_sf(ggplot2::aes(fill = Freq), colour = "gray60", size = 0.1) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = c(0.2, 0.3),
                   legend.title = ggplot2::element_text(size = 6, face = "bold"),
                   legend.text = ggplot2::element_text(size = 4),
                   legend.key.size = ggplot2::unit(0.3, "cm")) +
    ggplot2::ggtitle(map_title)

  if (multigen == FALSE) {
    if (!is.null(vir_color)) {
      p <- p + ggplot2::scale_fill_viridis_c(name = leg_title,
                                             option = vir_color,
                                             limits = c(0, max(world_plant$Freq)))
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_", SR_PD, "_", vir_color,
                                                       ".", format), fullname),
                      p, dpi=dpi, bg="white")
    }
    if (!is.null(bre_color)) {
      p <- p + ggplot2::scale_fill_gradientn(name = leg_title,
                                             colours = rev(RColorBrewer::brewer.pal(9, bre_color)),
                                             limits = c(0, max(world_plant$Freq)),
                                             na.value =  "white")
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_", SR_PD, "_", bre_color,
                                                       ".", format), fullname),
                      p, dpi=dpi, bg="white")
    }
  }

  if (multigen == TRUE) {
    if (!is.null(vir_color)) {
      p <- p + ggplot2::scale_fill_viridis_c(name = paste(gen, leg_title),
                                             option = vir_color,
                                             limits = c(0, max(world_plant$Freq)))
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_", SR_PD, "_", gen, "_",
                                                       vir_color, ".", format), fullname),
                      p, dpi=dpi, bg="white")
    }
    if (!is.null(bre_color)) {
      p <- p + ggplot2::scale_fill_gradientn(name = paste(gen, leg_title),
                                             colours = rev(RColorBrewer::brewer.pal(9, bre_color)),
                                             limits = c(0, max(world_plant$Freq)),
                                             na.value = "white")
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_", SR_PD, "_", gen, "_",
                                                       bre_color, ".", format), fullname),
                      p, dpi=dpi, bg="white")
    }
  }
}



