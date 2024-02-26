#' Create graphics of species discoveries accumulation and nomenclatural changes
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces graphics of the dynamics of species description and
#' historical nomenclature changes within any family or genus provided. It is
#' designed to create graphics based on the dataframe resulted from
#' \code{powoSpecies}. The graph type can be selected by two arguments
#' (spp_acc and spp_changes) and then multiple graphs for any different
#' taxonomic groups within the input data can be produced automatically in a
#' single run.
#'
#' @usage
#' accGraph(inputdf = NULL,
#'          verbose = TRUE,
#'          spp_acc = TRUE,
#'          spp_changes = TRUE,
#'          spp_changes_col = NULL,
#'          genus_plots = TRUE,
#'          save = FALSE,
#'          dir = "results_accGraph",
#'          filename = "cumulative_discovery_",
#'          format = "pdf")
#'
#' @param inputdf A dataframe generated using \code{powoSpecies} function using
#' the argument synonyms set as \code{TRUE} and containing the genus and species
#' column and the associated information about the protologue of each species.
#' The species name must be binomial, i.e. must contain both the genus name and
#' specific epithet.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param spp_acc Logical. Setting to \code{FALSE} means that you do not want to
#' create individual accumulation graphs for each genus present in your dataset.
#' The default is \code{TRUE}.
#'
#' @param spp_changes Logical. Setting to \code{FALSE} means that you do not want
#' to create a violin plot with all data provided. The default is \code{TRUE}.
#'
#' @param spp_changes_col Define the name of a column in the main input data
#' for which the species changes will be considered. For example, if densities
#' and jitters should be presented for each genus in the same graphic, then set
#' \code{spp_changes_col = "genus"}. If you are plotting the dynamics of
#' nomenclatural changes across the entire dataset in a single density graphic,
#' then you must add a new column in your input data containing the same character
#' inside all rows. In this case, ensure that you also keep \code{genus_plots = TRUE}.
#'
#' @param genus_plots Logical. If \code{FALSE}, a single density and jitter plot
#' graphic will be produced for the entire input data. The default is \code{TRUE},
#' which will result in a graphic where multiple densities and jitters will be
#' created inside the graphic, depending on the chosen column.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved
#' provided that the argument \code{save} is set up in \code{TRUE}. The default
#' is to create a directory named **results_accGraph** and the search results
#' will be saved within a subfolder named after the current date.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled **cumulative_discovery_**.
#'
#' @param format A character vector related to the file format of the graph
#' to be saved. The default is "pdf" to save the output in Portable Document
#' Format (.pdf), but you can also choose "jpg" to save in Joint Photographic
#' Experts Group (.jpg), "tiff" to save in Tag Image File Format (.tiff) or
#' "png" to save in Portable Network Graphics (.png).
#'
#' @return Objects of class c("gg", "ggplot") and saves the output on disk.
#'
#' @seealso \code{\link{megaGen}}
#' @seealso \code{\link{topGen}}
#' @seealso \code{\link{powoSpecies}}
#' @seealso \code{\link{powoFam}}
#' @seealso \code{\link{powoGenera}}
#' @seealso \code{\link{powoMap}}
#'
#' @examples
#' \dontrun{
#'
#' library(expowo)
#'
#' accGraph(inputdf = "output",
#'          verbose = TRUE,
#'          spp_acc = TRUE,
#'          spp_changes = TRUE,
#'          spp_changes_col = "genus",
#'          genus_plots = TRUE,
#'          save = FALSE,
#'          dir = "results_accGraph",
#'          filename = "cumulative_discovery_Cyperaceae",
#'          format = "pdf")
#' }
#'
#' @importFrom cowplot save_plot
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes theme_bw theme stat_bin element_blank ylab
#' @importFrom ggplot2 geom_point scale_colour_viridis_d scale_fill_viridis_d
#' @importFrom ggplot2 scale_color_manual scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 margin element_rect guides guide_legend annotate
#' @importFrom ggplot2 position_nudge position_jitter xlab scale_colour_manual
#' @importFrom ggplot2 geom_histogram
#' @importFrom PupillometryR geom_flat_violin
#' @importFrom viridisLite viridis
#' @importFrom plyr round_any
#'
#' @export
#'

accGraph <- function(inputdf = NULL,
                     verbose = TRUE,
                     spp_acc = TRUE,
                     spp_changes = TRUE,
                     spp_changes_col = NULL,
                     genus_plots = TRUE,
                     save = FALSE,
                     dir = "results_accGraph",
                     filename = "cumulative_discovery_",
                     format = "pdf") {

  # format check
  .arg_check_format(format)

  # dir check
  dir <- .arg_check_dir(dir)

  # Get name of folder and file name if the results are intended to be saved
  foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))
  fullname <- paste0(foldername, "/", filename, ".", format)

  # inputdf check if it is a dataframe and if was generated by powoSpecies
  .arg_check_inputdf(inputdf)

  # Transform the input df to get year of publication and number of synonyms
  # ff they do not have yet
  df <- inputdf
  tf <- names(df) %in% c("number_synonyms", "year", "year_basionym", "year_changed")
  if (length(which(tf)) == 0) {
  df <- get_year_pubs(inputdf = df,
                      verbose = FALSE,
                      save = FALSE)
  }


  # Plotting the graphs
  if (spp_acc) {

    # Input df with only the accepted species and associated basionym year
    df_accepted <- df %>%
      filter(df$status == "Accepted") %>%
      select(-c("status", "accepted_name"))

    # Plotting the accumulation figures
    # Selecting colors
    cols <- c("#180F3EFF", "#F1605DFF")

    # Single plot for the entire input data
    if (genus_plots == FALSE) {
      tax_group <- "fully_plotted_data"
      p <- list()
      l <- length(df_accepted$species)

      # p <- ggplot(df_accepted,
      #                  aes(df_accepted$year,
      #                      colour = df_accepted$year_changed)) +

      p <- ggplot(df_accepted) +
        stat_bin(aes(x = year, y = cumsum(..count..), colour = cols[1]),
                 geom = "step", bins = 80) +
        stat_bin(aes(x = year_basionym, y = cumsum(..count..), colour = cols[2]),
                 geom = "step", bins=80) +

        #stat_ecdf(linewidth = 1.5, alpha=0.6, geom = "step", pad = FALSE) +
        theme_bw() +
        xlab(expression(bold("Year of publication of name"))) +
        ylab(expression(bold("Accumulation of species discovery"))) +
        theme(legend.title = element_blank()) +
        scale_colour_manual(values = cols, labels = c("accepted name", "basionym")) +

        scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
        scale_x_continuous(breaks = c(1753, 1800, 1850, 1900, 1950, 2000,
                                      max(df$year[!is.na(df$year)]))) +
        theme(axis.title.y = element_text(size = 14, margin = margin(0,12,0,0))) +
        theme(axis.text.x = element_text(size = 14)) +
        theme(axis.text.y = element_text(size = 14)) +
        theme(legend.title = element_text(size = 14)) +
        theme(legend.position = c(0.2, 0.8),
              legend.key = element_rect(linewidth=10, linetype='blank'),
              legend.text = element_text(size=14),
              legend.key.size = unit(1, "cm"),
              legend.title = element_text(size=14)) +
        guides(alpha ='none', colour=guide_legend("publication year"),
               size = 14) +
        annotate("text",
                 x = max(df$year[!is.na(df$year)]),
                 y = plyr::round_any(l, 5, ceiling),
                 label = paste(l, " spp."),
                 colour = "black", alpha = 0.6)

      if (save) {

        if (!dir.exists(dir)) {
          dir.create(dir)
        }
        if (!dir.exists(foldername)) {
          dir.create(foldername)
        }

        cowplot::save_plot(gsub(paste0(".", format),
                                paste0(tax_group, ".", format),
                                fullname),
                           p,
                           ncol = 1, nrow = 1,
                           base_height = 8.5,
                           base_aspect_ratio = 1.3,
                           base_width = NULL)
      }
    }

    # Multiple individual plots for each genus
    if (genus_plots) {

      if (save) {

        if (!dir.exists(dir)) {
          dir.create(dir)
        }
        if (!dir.exists(foldername)) {
          dir.create(foldername)
        }

      }

      tax_group <- unique(df_accepted$genus)
      p <- list()
      for (i in seq_along(tax_group)) {

        temp_df_accepted <- df_accepted[df_accepted$genus %in% tax_group[i], ]
        l <- length(temp_df_accepted$species)

        p[[i]] <- ggplot(df_accepted) +
          stat_bin(aes(x = year, y = cumsum(..count..), colour = cols[1]),
                   geom = "step", bins = 80) +
          stat_bin(aes(x = year_basionym, y = cumsum(..count..), colour = cols[2]),
                   geom = "step", bins=80) +
          theme_bw() +
          xlab(expression(bold("Year of publication of name"))) +
          ylab(eval(bquote(expression(bold("Accumulation of species discovery in")
                                      ~bolditalic(.(tax_group[i])))))) +
          theme(legend.title = element_blank()) +
          scale_colour_manual(values = cols, labels = c("accepted name", "basionym")) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                             labels = scales::percent_format(accuracy = 1)) +
          scale_x_continuous(breaks = c(1753, 1800, 1850, 1900, 1950, 2000,
                                        max(df$year[!is.na(df$year)]))) +
          theme(axis.title.y = element_text(size = 14, margin = margin(0,12,0,0))) +
          theme(axis.text.x = element_text(size = 14)) +
          theme(axis.text.y = element_text(size = 14)) +
          theme(legend.title = element_text(size = 14)) +
          theme(legend.position = c(0.2, 0.8),
                legend.key = element_rect(linewidth = 10, linetype = 'blank'),
                legend.text = element_text(size = 14),
                legend.key.size = unit(1, "cm"),
                legend.title = element_text(size = 14)) +
          guides(alpha = 'none', colour=guide_legend("publication year"),
                 size = 14) +
          annotate("text",
                   x = max(df$year[!is.na(df$year)]),
                   y = plyr::round_any(l, 5, ceiling),
                   label = paste(l, " spp."),
                   colour = "black", alpha=0.6)

        if (save) {
          cowplot::save_plot(gsub(paste0(".", format),
                                  paste0(tax_group[i], ".", format),
                                  fullname),
                             p[[i]],
                             ncol = 1, nrow = 1,
                             base_height = 8.5,
                             base_aspect_ratio = 1.3,
                             base_width = NULL)
        }
      }
    }
  }

  if (spp_changes) {
    # Plotting the species changes in a violin plot

    tf <- !df$status %in% "Accepted"
    df$genus[tf] <- gsub("\\s.*", "", df$accepted_name[tf])
    #df$status[tf] <- "Synonym"
    df$accepted_name[!tf] <- df$scientific_name[!tf]

    df$number_synonyms[is.na(df$number_synonyms)] <- 0

    # Plotting the graph
    p <- ggplot(df,
                aes(x = df[[spp_changes_col]], y = df$year,
                    size = df$number_synonyms,
                    fill = factor(df$status, levels = c("Homotypic Synonym",
                                                        "Heterotypic Synonym",
                                                        "Accepted")))) +
      PupillometryR::geom_flat_violin(position = position_nudge(x = .25, y = 0),
                                      trim = FALSE,
                                      alpha = .6,
                                      size = .1,
                                      na.rm = TRUE) +
      geom_point(aes(fill = df$status, size = df$number_synonyms),
                 position = position_jitter(width = .1, height = .05),
                 shape = 21,
                 alpha = .6,
                 stroke = .1,
                 colour = "black",
                 na.rm = TRUE,
                 show.legend = TRUE) +

      scale_fill_viridis_d(option = "E") +
      scale_colour_viridis_d(option = "E") +
      theme_bw() +
      ylab(expression(bold("Year of publication of name"))) +
      xlab(expression(bold("Changes in species nomenclature over time"))) +
      theme(legend.title = element_blank()) +
      scale_y_continuous(breaks=c(1753, 1800, 1850, 1900, 1950, 2000,
                                  max(df$year[!is.na(df$year)]))) +
      theme(axis.text.x=element_text(size = 12, face = "italic")) +
      theme(axis.title.y=element_text(size = 14)) +
      guides(fill = guide_legend(order = 1),
             size = guide_legend(order = 2))

    if (save) {

      if (!dir.exists(dir)) {
        dir.create(dir)
      }
      if (!dir.exists(foldername)) {
        dir.create(foldername)
      }

      cowplot::save_plot(gsub(paste0(".", format),
                              paste0("all_data", ".", format),
                              fullname),
                         p,
                         ncol = 1, nrow = 1,
                         base_height = 8.5,
                         base_aspect_ratio = 1.3,
                         base_width = NULL)
    }
  }

  return(p)
}
