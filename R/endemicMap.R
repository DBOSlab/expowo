#' Create global and regional maps of endemic species
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a map of the number of species endemic to each
#' botanical country, i.e. the species native to a single botanical country
#' (level 3) of the
#' [World Geographical Scheme](https://www.tdwg.org/standards/wgsrpd/)
#' for Recording Plant Distributions. It comes with
#' the option to display the entire world or one continent according to the
#' Level 1 continental scheme of the WGSRPD. While \code{powoMap} maps where
#' species occur, \code{endemicMap} maps where species are restricted to.
#'
#' @usage
#' endemicMap(inputdf = NULL,
#'            distcol = "native_to_botanical_countries",
#'            breaks = c(10, 50, 100, 250, 500),
#'            leg_title = "Endemic species",
#'            region = "world",
#'            verbose = TRUE,
#'            save = FALSE,
#'            dpi = 600,
#'            dir = "results_endemicMap",
#'            filename = "endemic_map",
#'            format = "jpg")
#'
#' @param inputdf A data frame generated with \code{powoSpecies}, or any data
#' frame with one species per row and a column with its native distribution in
#' botanical countries separated by commas. When the input data has synonyms
#' (\code{status} column), only accepted species are counted.
#'
#' @param distcol Column name with the native distribution in botanical
#' countries. The default is the column \code{native_to_botanical_countries}
#' returned by \code{powoSpecies}.
#'
#' @param breaks Numeric vector with the upper limits of the classes of
#' endemic species shown in the map. Botanical countries without endemics and
#' those with more endemics than the last limit are shown in their own classes.
#' The default creates the classes 0, 1-10, 11-50, 51-100, 101-250, 251-500 and
#' more than 500.
#'
#' @param leg_title A character with the legend title.
#'
#' @param region Map extent. Available options are \code{"world"},
#' \code{"europe"}, \code{"africa"}, \code{"asia_temperate"},
#' \code{"asia_tropical"}, \code{"australasia"}, \code{"pacific"},
#' \code{"northern_america"}, \code{"southern_america"} and
#' \code{"antarctic"}.
#'
#' @param verbose Logical. If \code{FALSE}, messages are not printed.
#'
#' @param save Logical. If \code{TRUE}, the map is saved to disk.
#'
#' @param dpi One number in the range of 72-4000 referring to the image
#' resolution in dots per inch. The default is 600.
#'
#' @param dir Pathway to the computer's directory, where the map will be saved
#' provided that the argument \code{save} is \code{TRUE}. The default is to
#' create a directory named **results_endemicMap** and the map will be saved
#' within a subfolder named after the current date.
#'
#' @param filename Name of the output file. The default is **endemic_map**.
#'
#' @param format Output format of the map: \code{"jpg"}, \code{"pdf"},
#' \code{"png"} or \code{"tiff"}.
#'
#' @return A \code{ggplot} object with the map, whose data has the number of
#' endemic species of each botanical country in the column \code{endemics}.
#' The TDWG map of botanical countries is downloaded only once and kept at
#' \code{tools::R_user_dir("expowo", "cache")}.
#'
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoMap}}
#' @seealso \code{\link{introGraph}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' fab <- powoSpecies("Fabaceae", verbose = FALSE)
#'
#' endemicMap(fab,
#'            save = FALSE,
#'            dir = "results_endemicMap",
#'            filename = "Fabaceae_endemics")
#' }

#'
#' @export
#'

endemicMap <- function(inputdf = NULL,
                       distcol = "native_to_botanical_countries",
                       breaks = c(10, 50, 100, 250, 500),
                       leg_title = "Endemic species",
                       region = "world",
                       verbose = TRUE,
                       save = FALSE,
                       dpi = 600,
                       dir = "results_endemicMap",
                       filename = "endemic_map",
                       format = "jpg") {

  # Input data check
  .arg_check_columns(inputdf, distcol)

  # Format and directory checks
  .arg_check_format(format)
  dir <- .arg_check_dir(dir)

  # Breaks check
  if (!is.numeric(breaks) || length(breaks) < 1 ||
      is.unsorted(breaks, strictly = TRUE) || breaks[1] < 1) {
    stop("The argument breaks should be increasing numbers of 1 or more, ",
      "such as c(10, 50, 100).", call. = FALSE)
  }

  # Continental map options
  regions <- list(
    world = list(continent = NULL, xlim = c(-180, 180), ylim = c(-90, 90)),
    europe = list(continent = "Europe", xlim = c(-25, 70), ylim = c(27, 72)),
    africa = list(continent = "Africa", xlim = c(-25, 75), ylim = c(-40, 40)),
    asia_temperate = list(continent = "Temperate Asia", xlim = c(25, 180),
                          ylim = c(20, 82)),
    asia_tropical = list(continent = "Tropical Asia", xlim = c(60, 155),
                         ylim = c(-12, 35)),
    australasia = list(continent = "Australasia", xlim = c(110, 180),
                       ylim = c(-50, -8)),
    pacific = list(continent = "Pacific", xlim = c(-180, 180),
                   ylim = c(-35, 35)),
    northern_america = list(continent = "Northern America", xlim = c(-180, -50),
                            ylim = c(5, 85)),
    southern_america = list(continent = "Southern America", xlim = c(-95, -30),
                            ylim = c(-60, 30)),
    antarctic = list(continent = "Antarctic", xlim = c(-180, 180),
                     ylim = c(-90, -60)))

  if (!is.character(region) || length(region) != 1 ||
      !region %in% names(regions)) {
    stop("The argument region should be one of: ",
         paste(names(regions), collapse = ", "), ".", call. = FALSE)}

  region_info <- regions[[region]]

  # Count species native to a single botanical country
  df <- .accepted_only(inputdf)
  areas <- .split_areas(df[[distcol]])
  endemic_area <- vapply(areas[lengths(areas) == 1], `[`, "", 1)
  counts <- table(endemic_area)

  # Load the complete WGSRPD Level-3 map
  world <- .wgsrpd_level3(verbose = verbose)

  # Keep the complete list for checking genuinely missing areas
  all_map_areas <- world$botanical_country

  # Report only areas absent from the global WGSRPD map
  missing_areas <- setdiff(names(counts), all_map_areas)

  if (length(missing_areas) > 0 && verbose) {
    message("Botanical countries not found in the map: ",
      paste(missing_areas, collapse = ", "))}

  # Filter the map by the selected Level-1 continent
  if (!is.null(region_info$continent)) {
    selected_codes <- wgsrpd_l3$code[
      wgsrpd_l3$continent == region_info$continent]

    world <- world[world$LEVEL3_COD %in% selected_codes,
                   , drop = FALSE]}

  world$endemics <- as.numeric(counts[world$botanical_country])
  world$endemics[is.na(world$endemics)] <- 0

  # Classes of endemic species
  lower <- c(1, breaks[-length(breaks)] + 1)

  labels <- c("0", ifelse(lower == breaks, breaks, paste0(lower, "-", breaks)),
    paste0("> ", breaks[length(breaks)]))

  world$class <- cut(world$endemics, c(-Inf, 0, breaks, Inf), labels = labels)

  fills <- c(.expowo_cols[["zero"]], scales::gradient_n_pal(.expowo_blues)(
    seq(0, 1, length.out = length(labels) - 1)))

  names(fills) <- labels

  p <- ggplot2::ggplot(world) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$class), colour = "white",
      linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = fills, name = leg_title, drop = FALSE) +
    ggplot2::coord_sf(xlim = region_info$xlim, ylim = region_info$ylim,
      expand = FALSE, datum = NA) +
    .theme_expowo() +
    ggplot2::theme(legend.position = "bottom", legend.title.position = "top",
      panel.grid.major = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 1, label.position = "bottom",
        keywidth = ggplot2::unit(1.2, "cm"),
        keyheight = ggplot2::unit(0.3, "cm")))

  if (save) {.save_graph(p, dir, filename, format, dpi, width = 9.26,
                         height = 5.2, verbose = verbose)}

  return(p)
}
