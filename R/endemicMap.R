#' Create a global map of endemic species
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a global map of the number of species endemic to each
#' botanical country, i.e. the species native to a single botanical country
#' (level 3) of the
#' [World Geographical Scheme](https://www.tdwg.org/standards/wgsrpd/)
#' for Recording Plant Distributions. While \code{powoMap} maps where species
#' occur, \code{endemicMap} maps where species are restricted to.
#'
#' @usage
#' endemicMap(inputdf = NULL,
#'            distcol = "native_to_botanical_countries",
#'            breaks = c(10, 50, 100, 250, 500),
#'            leg_title = "Endemic species",
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
#' @param breaks A numeric vector with the upper limits of the classes of
#' endemic species shown in the map. Botanical countries without endemics and
#' those with more endemics than the last limit are shown in their own classes.
#' The default creates the classes 0, 1-10, 11-50, 51-100, 101-250, 251-500 and
#' more than 500.
#'
#' @param leg_title A character with the legend title.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step will
#' not be printed in the console.
#'
#' @param save Logical, if \code{TRUE}, the map will be saved on disk.
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
#' @param format The file format of the map: "jpg" (default), "pdf", "png" or
#' "tiff".
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
#'            save = TRUE,
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
                       verbose = TRUE,
                       save = FALSE,
                       dpi = 600,
                       dir = "results_endemicMap",
                       filename = "endemic_map",
                       format = "jpg") {

  # inputdf check
  .arg_check_columns(inputdf, distcol)

  # breaks check
  if (!is.numeric(breaks) || length(breaks) < 1 ||
      is.unsorted(breaks, strictly = TRUE) || breaks[1] < 1) {
    stop("The argument breaks should be increasing numbers of 1 or more, ",
         "such as c(10, 50, 100).", call. = FALSE)
  }

  # format check
  .arg_check_format(format)

  # dir check
  dir <- .arg_check_dir(dir)

  # Count the species native to a single botanical country
  df <- .accepted_only(inputdf)
  areas <- .split_areas(df[[distcol]])
  endemic_area <- vapply(areas[lengths(areas) == 1], `[`, "", 1)
  counts <- table(endemic_area)

  world <- .wgsrpd_level3(verbose = verbose)

  missing_areas <- setdiff(names(counts), world$botanical_country)
  if (length(missing_areas) > 0 && verbose) {
    message("Botanical countries not found in the map: ",
            paste(missing_areas, collapse = ", "))
  }
  world$endemics <- as.numeric(counts[world$botanical_country])
  world$endemics[is.na(world$endemics)] <- 0

  # Classes of endemic species
  lower <- c(1, breaks[-length(breaks)] + 1)
  labels <- c("0", ifelse(lower == breaks, breaks, paste0(lower, "-", breaks)),
              paste0("> ", breaks[length(breaks)]))
  world$class <- cut(world$endemics, c(-Inf, 0, breaks, Inf), labels = labels)
  fills <- c(.expowo_cols[["zero"]],
             scales::gradient_n_pal(.expowo_blues)(
               seq(0, 1, length.out = length(labels) - 1)))
  names(fills) <- labels

  p <- ggplot2::ggplot(world) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$class), colour = "white",
                     linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = fills, name = leg_title,
                               drop = FALSE) +
    ggplot2::coord_sf(expand = FALSE, datum = NA) +
    .theme_expowo() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title.position = "top",
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1, label.position = "bottom",
      keywidth = ggplot2::unit(1.2, "cm"),
      keyheight = ggplot2::unit(0.3, "cm")))

  if (save) {
    .save_graph(p, dir, filename, format, dpi, width = 9.26, height = 5.2,
                verbose = verbose)
  }

  return(p)
}
