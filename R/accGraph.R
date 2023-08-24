#' Create graphs depicting the accumulation of species discoveries
#'
#' @author Debora Zuanny & Domingos Cardoso
#'
#' @description Produces graphs of accumulation of species discovery for each
#' genus and a flat violin graph for all data provided. It is designed to
#' create graphs based on the search results from \code{powoSpecies}. Multiple
#' graphs for any different taxonomic groups within the input data can be
#' produced automatically in a single run, and can generate a violin plot
#' comparing the distribution of data of all genera included.
#'
#' @usage
#' accGraph(inputdf = NULL,
#'          verbose = TRUE,
#'          multi = TRUE,
#'          save = FALSE,
#'          dir = "results_accGraph",
#'          filename = "cumulative_discovery_",
#'          format = "pdf")
#'
#' @param inputdf A dataframe generated using \code{powoSpecies} function and
#' containing the genus and species column and the associated information about
#' the protologue of each species. The species name must be binomial, i.e.
#' must contain both the genus name and specific epithet.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing each step during
#' the POWO search will not be printed in the console in full.
#'
#' @param multi Logical. Setting to \code{FALSE} means that your dataset contains
#' only one genus and the function will not create a violin plot.
#' The default is \code{TRUE}.
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
#' @return Objects of class c("gg", "ggplot") or graphics and saves the output
#' on disk.
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
#'          multi = FALSE,
#'          save = FALSE,
#'          dir = "results_accGraph",
#'          filename = "cumulative_discovery_Cyperaceae",
#'          format = "pdf")
#' }
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes theme_bw theme stat_ecdf element_blank ylab
#' @importFrom ggplot2 geom_point scale_colour_viridis_d scale_fill_viridis_d
#' @importFrom ggplot2 scale_color_manual scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 margin element_rect guides guide_legend annotate
#' @importFrom ggplot2 position_nudge position_jitter
#' @importFrom PupillometryR geom_flat_violin
#' @importFrom cowplot save_plot
#' @importFrom scales show_col
#' @importFrom tibble add_column
#' @importFrom viridisLite viridis
#'
#' @export
#'

accGraph <- function(inputdf = NULL,
                     verbose = TRUE,
                     multi = TRUE,
                     save = FALSE,
                     dir = "results_accGraph",
                     filename = "cumulative_discovery_",
                     format = "pdf") {

  # format check
  .arg_check_format(format)

  # dir check
  dir <- .arg_check_dir(dir)

  # Fill in with NAs the empty cells
  df <- data.frame(apply(inputdf, 2, function(x) gsub("^$", NA, x)))

  # Creating new columns for the year and corrected year of publication
  df <- df %>% tibble::add_column(year = NA, .after = "publication")
  df <- df %>% tibble::add_column(year_corrected = NA, .after = "year")
  df <- df %>% tibble::add_column(number_synonyms = NA, .after = "accepted_name")

  # Extracting the year from the full publication information
  # Clean examples such as "non C. caerulea Jacq.", ", non Engelh. (1898)"
  df$publication <- gsub(",\\snon\\s[[:upper:]].*", "",  df$publication)

  # Get examples of not validly published names
  tf <- grepl("[0-9][)],\\s", df$publication)
  p <- unique(gsub(".*[)],\\s", "", df$publication[tf]))
  tf_inval <- !grepl(paste0(p, collapse = "|"), df$publication)

  df$year[tf_inval] <- gsub(",\\scontrary.*", "", df$publication[tf_inval])
  df$year[tf_inval] <- gsub(".*[(]|[)].*|.*.\\s|.*[.]", "", df$year[tf_inval])
  df$year[tf_inval] <- gsub("unknown\\spublication|publication", "unknown",
                            df$year[tf_inval])
  df$year[!tf_inval] <- gsub(".*[(]|,.*", "", df$publication[!tf_inval])
  df$year[!tf_inval] <- gsub(".*[.]\\s|[)].*", "", df$year[!tf_inval])
  df$year[!tf_inval] <- gsub("unknown\\spublication|publication", "unknown",
                             df$year[!tf_inval])

  # Extracting the correct year for species with synonyms
  # Fill corrected year for species without synonyms
  tf_accepted <- df$status %in% "Accepted"

  # Get the accepted names for species without any synonym
  tftf <- !df$scientific_name[tf_accepted] %in% df$accepted_name
  df$year_corrected[tf_accepted][tftf] <- df$year[tf_accepted][tftf]

  # Count the number of synonyms
  df$number_synonyms[tf_accepted][tftf] <- 0
  df$number_synonyms[tf_accepted][!tftf] <- table(df$accepted_name)

  # Extracting the correct year for species with synonyms
  n_au <- unique(df$accepted_name[!is.na(df$accepted_name)])
  for (i in seq_along(n_au)) {

    tf <- df$scientific_name[tf_accepted] %in% n_au[i]
    year_acc <- df$year[tf_accepted][tf]

    tftf <- df$accepted_name[tf_inval] %in% n_au[i]
    temp <- df[tf_inval, ][tftf, ]
    temp <- temp[!grepl("\\svar[.]|\\sf[.]\\s|\\ssubsp[.]\\s",
                        temp$scientific_name), ]
    if(length(temp$year) == 0) {
      year_syn <- NA
    } else {
      year_syn <- min(temp$year)
    }

    if (year_acc < year_syn |
        is.na(year_syn)) {
      df$year_corrected[tf_accepted][tf] <- year_acc
    } else {
      df$year_corrected[tf_accepted][tf] <- year_syn
    }

  }

  # Correct the publication year for "nomina conservanda" examples
  tf <- grepl(", nom. cons.", df$publication[tf_accepted])
  df$year[tf_accepted][tf] <- gsub(".*[(]|[)],\\snom[.] cons[.]*", "",
                                   df$publication[tf_accepted][tf])
  df$year_corrected[tf_accepted][tf] <- gsub(".*[(]|[)],\\snom[.] cons[.]*", "",
                                             df$publication[tf_accepted][tf])

  # Identify each change in the corrected year of species discovery
  df$year <- as.numeric(df$year)
  tf <- is.na(df$year_corrected)
  df$year_corrected[!tf] <- as.numeric(df$year_corrected[!tf])

  df$year <- as.numeric(df$year)
  df$year_corrected <- as.numeric(df$year_corrected)

  tf <- df$year == df$year_corrected

  df <- df %>% tibble::add_column(year_changed = NA, .after = "year_corrected")
  df$year_changed[tf] <- "no"
  df$year_changed[!tf] <- "yes"

  # Final spreadsheet including the input df with only the accepted species and
  # associated corrected year
  df_accepted <- df %>%
    filter(df$status == "Accepted") %>%
    select(-c("status", "accepted_name"))

  # Create a new directory to save the plots
  # If there is no directory... make one!
  foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))
  fullname <- paste0(foldername, "/", filename, ".", format)
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  if (!dir.exists(foldername)) {
    dir.create(foldername)
  }

  # saving the new spreadsheet with the data to plot
  saveCSV(df_accepted,
          dir = dir,
          filename = "data_to_plot",
          verbose = verbose,
          save = save,
          foldername = foldername)

  # Plotting the figures
  # Selecting colors
  scales::show_col(viridis(10, option = "magma"))
  cols <- c("#180F3EFF", "#F1605DFF")

  tax_group <- unique(df_accepted$genus)
  for (i in seq_along(tax_group)) {

    temp_df_accepted <- df_accepted[df_accepted$genus %in% tax_group[i], ]
    l <- length(temp_df_accepted$species)

    plot <- ggplot(temp_df_accepted,
                   aes(temp_df_accepted$year,
                       colour = temp_df_accepted$year_changed))+
      stat_ecdf(linewidth = 1.5, alpha=0.6, geom = "step") +
      theme_bw() +
      ylab(eval(bquote(expression(bold("Accumulation of species discovery in")
                                  ~bolditalic(.(tax_group[i])))))) +
      theme(axis.title.x=element_blank()) +
      scale_color_manual(values = cols) +
      scale_y_continuous(breaks=scales::pretty_breaks(n=5),
                         labels=scales::percent_format(accuracy = 1)) +
      scale_x_continuous(breaks=c(1753, 1800, 1850, 1900, 1950, 2000,
                                  max(df$year[!is.na(df$year)]))) +
      theme(axis.title.y=element_text(size=14, margin=margin(0,12,0,0))) +
      theme(axis.text.x=element_text(size=14)) +
      theme(axis.text.y=element_text(size=14)) +
      theme(legend.title = element_text(size=14)) +
      theme(legend.position = c(0.2, 0.8),
            legend.key = element_rect(linewidth=10, linetype='blank'),
            legend.text = element_text(size=14),
            legend.key.size = unit(1, "cm"),
            legend.title = element_text(size=14)) +
      guides(alpha='none', colour=guide_legend("Publication year corrected"),
             size=14) +
      annotate("text", x = 2020, y = 1.02,
               label = paste(l, " spp."),
               colour = "black", alpha=0.6)

    if(save){
      cowplot::save_plot(gsub(paste0(".", format),
                              paste0(tax_group[i], ".", format),
                              fullname),
                         plot,
                         ncol = 1, nrow = 1,
                         base_height = 8.5,
                         base_aspect_ratio = 1.3,
                         base_width = NULL)
    }
  }

  if (multi){
    # Plotting the figures for all data
    tf <- !df$status %in% "Accepted"
    df$genus[tf] <- gsub("\\s.*", "", df$accepted_name[tf])
    df$status[tf] <- "Synonym"
    df$accepted_name[!tf] <- df$scientific_name[!tf]

    df$number_synonyms[is.na(df$number_synonyms)] <- 0

    source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

    p <- ggplot(df,
                aes(x = df$genus, y = df$year, fill = df$status,
                    size = df$number_synonyms))+
      geom_flat_violin(position = position_nudge(x = .25, y = 0),
                       trim = FALSE,
                       alpha = .6,
                       lwd = 0.1) +
      geom_point(position = position_jitter(width = .1, height = 0.05),
                 shape =  21,
                 alpha = .6,
                 stroke = 0.0001,
                 show.legend = TRUE,
                 colour="black",
                 aes(fill = df$status, size = df$number_synonyms)) +

      scale_fill_viridis_d(option = "E") +
      scale_colour_viridis_d(option = "E") +
      theme_bw() +
      ylab("Changes in species nomenclature over the years") +
      theme(axis.title.x = element_blank(),
            legend.title = element_blank()) +
      scale_y_continuous(breaks=c(1753, 1800, 1850, 1900, 1950, 2000,
                                  max(df$year[!is.na(df$year)]))) +
      theme(axis.text.x=element_text(size = 12, face = "italic")) +
      theme(axis.title.y=element_text(size = 14))

    if (save){
      cowplot::save_plot(gsub(paste0(".", format),
                              paste0("all_data", ".", format),
                              fullname),
                         p,
                         ncol = 1, nrow = 1,
                         base_height = 8.5,
                         base_aspect_ratio = 1.3,
                         base_width = NULL)  }
  }

  return(p)
}
