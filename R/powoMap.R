#' Get list of species from POWO
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description It produces a CSV file listing all accepted species and
#' associated geographical distribution from any target genus or family of
#' flowering plants at
#' [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/).
#'
#' @usage
#' powoSpecies(family, uri, genus = NULL, hybridspp = FALSE, country = NULL,
#'             verbose = TRUE, save = TRUE, dir, filename)
#'
#' @param family Either a single family name or a vector of multiple families
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
#' @return Table in .csv format and saves the output on disk.
#'
#' @seealso \code{\link{POWOcodes}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#' library(taxize)
#'
#' fam <- c("Araceae", "Lecythidaceae")
#' powocodes <- cbind(family = fam,
#'                    data.frame(taxize::get_pow(fam)))
#'
#' powoSpecies(powocodes$family, powocodes$uri,
#'             hybridspp = FALSE,
#'             country = c("Argentina", "Brazil", "French Guiana"),
#'             verbose = TRUE,
#'             save = TRUE,
#'             dir = "results_powoSpecies/",
#'             filename = "Araceae_Lecythidaceae")
#'
#' ## Searching for all species and associated global distribution
#' ## in any or all flowering plant genera, by using the URI addresses
#' ## within the POWOcodes data file
#'
#' data(POWOcodes)
#'
#' powoSpecies(POWOcodes$family, POWOcodes$uri,
#'             hybridspp = TRUE,
#'             verbose = TRUE,
#'             save = TRUE,
#'             dir = "results_powoSpecies/",
#'             filename = "all_angiosperm_species")
#'}
#'
#' @importFrom ggplot2 ggplot aes unit
#' @importFrom magrittr "%>%"
#' @importFrom rnaturalearth ne_countries
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp merge
#'
#' @export
#'

setwd("G:/Mestrado_UFBA/Extracao_dados_mestrado/Tabelas")
# Load plant data
list.files()
data <- read.csv("powo_Species_megagenFabaceae_accepted_spp.csv")

names(data)

tf <- grepl("(", data$publication)
data$protologue <- "NA"
data$protologue[tf]
map_div= "native_to_botanical_countries"


powoMap(data,
        map_div = "native_to_country",
        multigen = TRUE,
        vir_color = "viridis",
        bre_color = NULL,
        map_title = "(a)",
        leg_title = "SR",
        dpi = 600,
        dir = "results_powoMap/",
        filename = "global_richness_map",
        format = "pdf")


powoMap <- function(data = NULL,
                    map_div = "native_to_country",
                    multigen = FALSE,
                    vir_color = "viridis",
                    bre_color = NULL,
                    map_title = "(a)",
                    leg_title = "SR",
                    dpi = 600,
                    dir = "results_powoMap/",
                    filename = "global_richness_map",
                    format = "pdf") {


  # Load global map
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # Remove Antarctica
  world <- world[!world$admin %in% "Antarctica", ]


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
    world.plant <- .get_country_data(data, world, map_div)

    # Making the maps and saving them as figures in different styles
    p <- ggplot2::ggplot(data = world.plant) +
      ggplot2::geom_sf(aes(fill = Freq), colour = "gray60", size = 0.1) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = c(0.2, 0.3),
                     legend.title = element_text(size = 6, face = "bold"),
                     legend.text = element_text(size = 4),
                     legend.key.size = unit(0.3, "cm")) +
      ggplot2::ggtitle(map_title)

    if (!is.null(vir_color)) {
      p <- p + ggplot2::scale_fill_viridis_c(name = leg_title, option = vir_color, trans = "sqrt")
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_", vir_color, ".", format), fullname), p, dpi=dpi, bg="transparent")
    }

    if (!is.null(bre_color)) {
      p <- p + scale_fill_gradientn(name = leg_title,
                                    colours = rev(RColorBrewer::brewer.pal(9, bre_color)),
                                    na.value=  "white")
      ggplot2::ggsave(gsub(paste0(".", format), paste0("_", bre_color, ".", format), fullname), p, dpi=dpi, bg="transparent")
    }

  }


  #_______________________________________________________________________________
  # Plotting figures for each genus separately
  if (multigen == TRUE) {

    gen <- unique(data$genus)

    for (i in seq_along(gen)) {
      temp_data <- data %>% filter(genus == gen[i])

      # Making world map with species richness across countries/botanical subdivisions
      world.plant <- .get_country_data(temp_data, world, map_div)

      # Making the maps and saving them as figures in different styles
      p <- ggplot2::ggplot(data = world.plant) +
        ggplot2::geom_sf(aes(fill = Freq), colour = "gray60", size = 0.1) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = c(0.2, 0.3),
                       legend.title = element_text(size = 6, face = "bold"),
                       legend.text = element_text(size = 4),
                       legend.key.size = unit(0.3, "cm")) +
        ggplot2::ggtitle(map_title)

      if (!is.null(vir_color)) {
        p <- p + ggplot2::scale_fill_viridis_c(name = paste(gen[i], leg_title), option = vir_color, trans = "sqrt")
        ggplot2::ggsave(gsub(paste0(".", format), paste0("_", gen[i], "_", vir_color, ".", format), fullname), p, dpi=dpi, bg="transparent")
      }

      if (!is.null(bre_color)) {
        p <- p + scale_fill_gradientn(name = paste(gen[i], leg_title),
                                      colours=rev(RColorBrewer::brewer.pal(9, bre_color)),
                                      na.value="white")
        ggplot2::ggsave(gsub(paste0(".", format), paste0("_", gen[i], "_", bre_color, ".", format), fullname), p, dpi=dpi, bg="transparent")
      }
    }

  }

}


# Auxiliary function to make world map with species richness across countries/botanical subdivisions
.get_country_data <- function (data, world, map_div) {
  country.data.list <- list()
  for (i in seq_along(data[[map_div]])) {
    country.data.list[[i]] <- gsub("^\\s", "", strsplit(data[[map_div]][i], ",")[[1]])
  }
  country.data.list <- data.frame(table(unlist(country.data.list)))
  names(country.data.list)[1] <- "countries"

  # Merge the plant distribution data with global map
  world.plant <- sp::merge(world, country.data.list,
                           by.x="admin",
                           by.y="countries",
                           all.x=T)
  # Replacing NA values for zero
  world.plant$Freq[is.na(world.plant$Freq)] = 0

  return(world.plant)
}

