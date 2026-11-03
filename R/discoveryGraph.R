#' Plot species discovery and synonymy over time
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces a graphic with two panels: the number of species
#' described in each period of time, and the number of synonyms published in
#' each period. A species is counted in the year of its oldest homotypic name
#' (its basionym), i.e. the year it was discovered, not the year of its current
#' name. While \code{accGraph} shows the cumulative discovery of species, this
#' graphic shows when taxonomic work, i.e. new species and renaming, was most
#' intense.
#'
#' @usage
#' discoveryGraph(inputdf = NULL,
#'                bin = 10,
#'                verbose = TRUE,
#'                save = FALSE,
#'                dpi = 600,
#'                dir = "results_discoveryGraph",
#'                filename = "discovery_graph",
#'                format = "jpg")
#'
#' @param inputdf A data frame generated with \code{powoSpecies} using
#' \code{synonyms = TRUE}.
#'
#' @param bin Number of years in each bar. The default is 10 (decades).
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
#' to create a directory named **results_discoveryGraph** and the graphic will be
#' saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file. The default is
#' **discovery_graph**.
#'
#' @param format The file format of the graphic: "jpg" (default), "pdf",
#' "png" or "tiff".
#'
#' @return A \code{ggplot} object, whose data has the number of names
#' (\code{names}) in each period (\code{period}) for each panel (\code{what}).
#'
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{accGraph}}
#'
#' @examples
#' \dontrun{
#' library(expowo)
#'
#' fab <- powoSpecies("Fabaceae", synonyms = TRUE, verbose = FALSE)
#'
#' discoveryGraph(fab,
#'                save = TRUE,
#'                dir = "results_discoveryGraph",
#'                filename = "Fabaceae_discovery")
#' }
#'
#' @export
#'

discoveryGraph <- function(inputdf = NULL,
                           bin = 10,
                           verbose = TRUE,
                           save = FALSE,
                           dpi = 600,
                           dir = "results_discoveryGraph",
                           filename = "discovery_graph",
                           format = "jpg") {

  # inputdf check
  .arg_check_columns(inputdf, c("status", "scientific_name", "accepted_name",
                                "publication"),
                     hint = " Please use powoSpecies with synonyms = TRUE.")

  # bin check
  if (!is.numeric(bin) || length(bin) != 1 || bin < 1) {
    stop("The argument bin should be a number of years, such as 10.",
         call. = FALSE)
  }

  # format check
  .arg_check_format(format)

  # dir check
  dir <- .arg_check_dir(dir)

  df <- inputdf
  df$year <- .pub_year(df$publication)

  # A species is discovered in the year of its oldest homotypic name
  acc <- df[df$status %in% "Accepted", ]
  homo <- df[df$status %in% "Homotypic Synonym" & !is.na(df$year), ]
  basionym_year <- tapply(homo$year, homo$accepted_name, min)
  acc$discovery <- suppressWarnings(
    pmin(acc$year, as.numeric(basionym_year[acc$scientific_name]),
         na.rm = TRUE))

  what <- c("New species described (year of the basionym)",
            "Synonyms published")
  count_period <- function(year, panel) {
    year <- year[!is.na(year)]
    if (length(year) == 0) return(NULL)
    period <- floor(year / bin) * bin
    d <- as.data.frame(table(period = period), stringsAsFactors = FALSE)
    data.frame(period = as.numeric(d$period), names = as.numeric(d$Freq),
               what = panel)
  }
  counts <- rbind(count_period(acc$discovery, what[1]),
                  count_period(df$year[!df$status %in% "Accepted"], what[2]))
  if (is.null(counts)) {
    stop("No year of publication was found in the input data.", call. = FALSE)
  }
  counts$what <- factor(counts$what, levels = what)

  # Label the busiest period of each panel
  peaks <- do.call(rbind, lapply(split(counts, counts$what, drop = TRUE),
                                 function(x) x[which.max(x$names), ]))
  if (bin == 10) {
    period_label <- paste0(peaks$period, "s")
  } else {
    period_label <- paste0(peaks$period, "-", peaks$period + bin - 1)
  }
  peaks$label <- paste0(period_label, ": ",
                        format(peaks$names, big.mark = ",", trim = TRUE))

  if (verbose) {
    message(nrow(acc), " accepted species and ",
            sum(!df$status %in% "Accepted"), " synonyms in the input data.")
  }

  p <- ggplot2::ggplot(counts, ggplot2::aes(x = .data$period + bin / 2,
                                            y = .data$names,
                                            fill = .data$what)) +
    ggplot2::geom_col(width = bin * 0.8) +
    ggplot2::geom_text(data = peaks, ggplot2::aes(label = .data$label),
                       vjust = -0.5, size = 3.2,
                       colour = .expowo_cols[["ink2"]]) +
    ggplot2::facet_wrap(~ what, ncol = 1, scales = "free_y", drop = FALSE) +
    ggplot2::scale_fill_manual(values = unname(.expowo_cols[c("blue",
                                                              "orange")]),
                               guide = "none", drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(
                                  mult = c(0, 0.18))) +
    ggplot2::labs(x = "Year of publication",
                  y = paste("Names per",
                            ifelse(bin == 10, "decade",
                                   paste(bin, "years")))) +
    .theme_expowo() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  if (save) {
    .save_graph(p, dir, filename, format, dpi, width = 8, height = 6,
                verbose = verbose)
  }

  return(p)
}
