#' Plot where species were introduced from and to
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a heatmap with the number of species native to each
#' continent (rows) that are introduced in each other continent (columns),
#' following the
#' [World Geographical Scheme](https://www.tdwg.org/standards/wgsrpd/)
#' for Recording Plant Distributions. A species is counted once for each pair
#' of continents. Regions (level 2 of the scheme) can be used instead of
#' continents.
#'
#' @usage
#' introGraph(inputdf = NULL,
#'            level = "continent",
#'            verbose = TRUE,
#'            save = FALSE,
#'            dpi = 600,
#'            dir = "results_introGraph",
#'            filename = "intro_graph",
#'            format = "jpg")
#'
#' @param inputdf A data frame generated with \code{powoSpecies}, or any data
#' frame with the columns \code{native_to_botanical_countries} and
#' \code{introduced_to_botanical_countries}. When the input data has synonyms
#' (\code{status} column), only accepted species are counted.
#'
#' @param level Either "continent" (level 1 of the scheme, the default) or
#' "region" (level 2).
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step will
#' not be printed in the console.
#'
#' @param save Logical, if \code{TRUE}, the graphic will be saved on disk.
#'
#' @param dpi One number in the range of 72-4000 referring to the image
#' resolution in dots per inch. The default is 600.
#'
#' @param dir Pathway to the computer's directory, where the graphic will be
#' saved provided that the argument \code{save} is \code{TRUE}. The default is
#' to create a directory named **results_introGraph** and the graphic will be
#' saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file. The default is **intro_graph**.
#'
#' @param format The file format of the graphic: "jpg" (default), "pdf",
#' "png" or "tiff".
#'
#' @return A \code{ggplot} object, whose data has the number of species
#' (\code{species}) native to each continent or region (\code{from}) that are
#' introduced in another one (\code{to}).
#'
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{endemicMap}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' fab <- powoSpecies("Fabaceae", verbose = FALSE)
#'
#' introGraph(fab,
#'            save = TRUE,
#'            dir = "results_introGraph",
#'            filename = "Fabaceae_introductions")
#' }
#'
#' @export
#'

introGraph <- function(inputdf = NULL,
                       level = "continent",
                       verbose = TRUE,
                       save = FALSE,
                       dpi = 600,
                       dir = "results_introGraph",
                       filename = "intro_graph",
                       format = "jpg") {

  # inputdf check
  .arg_check_columns(inputdf, c("native_to_botanical_countries",
                                "introduced_to_botanical_countries"))

  # level check
  if (!identical(level, "continent") && !identical(level, "region")) {
    stop("The argument level should be either \"continent\" or \"region\".",
         call. = FALSE)
  }

  # format check
  .arg_check_format(format)

  # dir check
  dir <- .arg_check_dir(dir)

  # Continent or region of each botanical country
  area_group <- stats::setNames(wgsrpd_l3[[level]],
                                wgsrpd_l3$botanical_country)
  to_group <- function(x) {
    lapply(.split_areas(x), function(a) unique(stats::na.omit(area_group[a])))
  }

  df <- .accepted_only(inputdf)
  native <- to_group(df$native_to_botanical_countries)
  introduced <- to_group(df$introduced_to_botanical_countries)
  tf <- lengths(native) > 0 & lengths(introduced) > 0
  if (!any(tf)) {
    stop("None of the species in the input data has both native and ",
         "introduced distribution.", call. = FALSE)
  }

  pairs <- do.call(rbind, Map(function(n, i, sp) {
    expand.grid(from = n, to = i, species = sp, stringsAsFactors = FALSE)
  }, native[tf], introduced[tf], which(tf)))
  pairs <- pairs[pairs$from != pairs$to, ]
  if (nrow(pairs) == 0) {
    stop("None of the species in the input data is introduced in another ",
         level, ".", call. = FALSE)
  }

  groups <- unique(wgsrpd_l3[[level]][order(wgsrpd_l3$code)])
  groups <- sort(groups[groups %in% c(pairs$from, pairs$to)])
  counts <- as.data.frame(table(from = factor(pairs$from, levels = groups),
                                to = factor(pairs$to, levels = groups)),
                          responseName = "species")
  counts <- counts[counts$from != counts$to, ]
  counts$label <- ifelse(counts$species > 0,
                         format(counts$species, big.mark = ",", trim = TRUE),
                         "")
  counts$dark <- sqrt(counts$species) > 0.6 * sqrt(max(counts$species))

  if (verbose) {
    message(length(unique(pairs$species)), " species are introduced in ",
            "another ", level, ".")
  }

  p <- ggplot2::ggplot(counts, ggplot2::aes(x = .data$to, y = .data$from,
                                            fill = .data$species)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label,
                                    colour = .data$dark),
                       size = ifelse(level == "continent", 3.2, 2.2)) +
    ggplot2::scale_fill_gradientn(colours = .expowo_blues, trans = "sqrt",
                                  name = "Species", labels = scales::comma) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white",
                                            `FALSE` = .expowo_cols[["ink"]]),
                                 guide = "none") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = "Introduced in", y = "Native to") +
    .theme_expowo() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 35,
                                                       hjust = 1))

  if (save) {
    size <- ifelse(level == "continent", 7, 14)
    .save_graph(p, dir, filename, format, dpi, width = size + 1.5,
                height = size, verbose = verbose)
  }

  return(p)
}
