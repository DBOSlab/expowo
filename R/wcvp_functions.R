# Auxiliary functions to mine the World Checklist of Vascular Plants (WCVP)
# bulk data, which is the taxonomic backbone of POWO. They are used when
# source = "wcvp" (the default) and return exactly the same output structure
# as the POWO scraping functions.
# Author: Debora Zuanny & Domingos Cardoso

# Session cache for the loaded WCVP tables
.expowo_env <- new.env(parent = emptyenv())

.wcvp_url <- "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip"

# WGSRPD level-3 names that differ between the current WCVP release and the
# legacy names used by POWO and by the TDWG level-3 shapefile (powoMap). The
# legacy names keep the botregions country lookup and the maps working.
.wcvp_legacy_areas <- c(CKI = "Cocos (Keeling) I.",
                        CPI = "C. American Pacific Is.",
                        CZE = "Czechoslovakia",
                        FOR = "Foroyar",
                        GAL = "Galapagos",
                        JNF = "Juan Fernandez Is.",
                        KGZ = "Kirgizistan",
                        LEE = "Leeward Is. AB Ant",
                        PAN = "Panama",
                        QUE = "Quebec",
                        REU = "Reunion",
                        SUD = "Sudan",
                        SWZ = "Swaziland",
                        TUE = "Turkey-in-Europe",
                        TUR = "Turkey",
                        YAK = "Yakutskiya",
                        YUG = "Yugoslavia",
                        ZAI = "Zaire")

#_______________________________________________________________________________
# Function to check the source argument
.arg_check_source <- function(source) {
  if (!is.character(source) || length(source) != 1 ||
      !source %in% c("wcvp", "powo")) {
    stop("The argument source should be either \"wcvp\" or \"powo\".",
         call. = FALSE)
  }
  return(source)
}

#_______________________________________________________________________________
# Function to load the WCVP names and distribution tables. The WCVP zip file is
# downloaded only once and stored in the user cache directory as a slim RDS
# file, which is refreshed whenever Kew releases a new WCVP version.
.wcvp_load <- function(verbose = TRUE) {

  if (!is.null(.expowo_env$wcvp)) {
    return(.expowo_env$wcvp)
  }

  cache_dir <- tools::R_user_dir("expowo", which = "cache")
  rds_file <- file.path(cache_dir, "wcvp.rds")
  stamp_file <- file.path(cache_dir, "wcvp_last_modified.txt")

  remote_stamp <- .wcvp_last_modified()
  local_stamp <- if (file.exists(stamp_file)) readLines(stamp_file, warn = F)

  if (file.exists(rds_file) &&
      (is.null(remote_stamp) || identical(remote_stamp, local_stamp))) {
    if (verbose) {
      message("Loading WCVP data from the cache...")
    }
    wcvp <- readRDS(rds_file)

  } else {
    if (is.null(remote_stamp) && !file.exists(rds_file)) {
      stop(paste("Unable to reach the WCVP download at", .wcvp_url,
                 "and there is no cached copy of WCVP. Please check your",
                 "internet connection."),
           call. = FALSE)
    }
    wcvp <- .wcvp_download(verbose = verbose)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(wcvp, rds_file)
    writeLines(remote_stamp, stamp_file)
  }

  .expowo_env$wcvp <- wcvp

  return(wcvp)
}

#_______________________________________________________________________________
# Function to get the date of the latest WCVP release
.wcvp_last_modified <- function() {
  headers <- tryCatch(curlGetHeaders(.wcvp_url),
                      error = function(e) NULL)
  if (is.null(headers) || attr(headers, "status") != 200) {
    return(NULL)
  }
  stamp <- grep("^last-modified:", headers, ignore.case = TRUE, value = TRUE)
  if (length(stamp) == 0) {
    return("unknown")
  }
  return(trimws(gsub("^last-modified:", "", stamp[1], ignore.case = TRUE)))
}

#_______________________________________________________________________________
# Function to download WCVP and keep only the data used by expowo
.wcvp_download <- function(verbose = TRUE,
                           url = .wcvp_url) {

  if (verbose) {
    message("Downloading WCVP from Kew (about 90 MB, only once)...")
  }

  temp_dir <- tempfile("wcvp")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  zip_file <- file.path(temp_dir, "wcvp.zip")

  old_timeout <- options(timeout = max(1800, getOption("timeout")))
  on.exit(options(old_timeout), add = TRUE)
  utils::download.file(url = url,
                       destfile = zip_file,
                       mode = "wb",
                       quiet = !verbose)
  utils::unzip(zip_file,
               files = c("wcvp_names.csv", "wcvp_distribution.csv"),
               exdir = temp_dir)

  if (verbose) {
    message("Preparing WCVP data...")
  }

  names_cols <- c("plant_name_id", "ipni_id", "taxon_rank", "taxon_status",
                  "family", "genus_hybrid", "genus", "species",
                  "publication_author", "place_of_publication",
                  "volume_and_page", "first_published", "taxon_name", "taxon_authors", "accepted_plant_name_id",
                  "homotypic_synonym", "parent_plant_name_id", "powo_id")
  # strip.white = FALSE keeps the leading space of volume_and_page, which is
  # needed to rebuild the publication as displayed in POWO.
  wcvp_names <- data.table::fread(file.path(temp_dir, "wcvp_names.csv"),
                                  sep = "|", quote = "", encoding = "UTF-8",
                                  na.strings = "", strip.white = FALSE,
                                  select = names_cols,
                                  colClasses = list(character = names_cols),
                                  data.table = FALSE, showProgress = FALSE)
  wcvp_names$plant_name_id <- as.integer(wcvp_names$plant_name_id)
  wcvp_names$accepted_plant_name_id <-
    as.integer(wcvp_names$accepted_plant_name_id)
  wcvp_names$parent_plant_name_id <-
    as.integer(wcvp_names$parent_plant_name_id)
  wcvp_names$homotypic_synonym <- wcvp_names$homotypic_synonym %in%
    c("T", "True", "TRUE")

  # Build the protologue as displayed in POWO, e.g. "Sp. Pl.: 960 (1753)" or
  # "H.G.A.Engler (ed.), Pflanzenr., IV, 219a: 31 (1939)"
  publ <- paste0(ifelse(is.na(wcvp_names$publication_author), "",
                        paste0(wcvp_names$publication_author, ", ")),
                 ifelse(is.na(wcvp_names$place_of_publication), "",
                        wcvp_names$place_of_publication),
                 ifelse(is.na(wcvp_names$volume_and_page), "",
                        wcvp_names$volume_and_page),
                 ifelse(is.na(wcvp_names$first_published), "",
                        paste0(" ", wcvp_names$first_published)))
  publ <- trimws(publ)
  publ[publ == ""] <- NA
  wcvp_names$publication <- publ
  wcvp_names <- wcvp_names[, !names(wcvp_names) %in%
                             c("publication_author", "place_of_publication",
                               "volume_and_page", "first_published")]

  # POWO links each taxon by its POWO id, which is the IPNI id for most names
  tf <- is.na(wcvp_names$powo_id)
  wcvp_names$powo_id[tf] <- wcvp_names$ipni_id[tf]
  wcvp_names <- wcvp_names[, names(wcvp_names) != "ipni_id"]

  # Keep accepted genera and species, plus every synonym of an accepted species
  acc <- wcvp_names$taxon_status == "Accepted" &
    wcvp_names$taxon_rank %in% c("Genus", "Species")
  acc_spp <- wcvp_names$plant_name_id[acc &
                                        wcvp_names$taxon_rank == "Species"]
  syn <- wcvp_names$taxon_status != "Accepted" &
    wcvp_names$accepted_plant_name_id %in% acc_spp
  wcvp_names <- wcvp_names[acc | syn, ]

  dist_cols <- c("plant_name_id", "area_code_l3", "area", "introduced",
                 "extinct", "location_doubtful")
  wcvp_dist <- data.table::fread(file.path(temp_dir, "wcvp_distribution.csv"),
                                 sep = "|", quote = "", encoding = "UTF-8",
                                 na.strings = "", select = dist_cols,
                                 data.table = FALSE, showProgress = FALSE)
  wcvp_dist$plant_name_id <- as.integer(wcvp_dist$plant_name_id)
  wcvp_dist <- wcvp_dist[wcvp_dist$plant_name_id %in%
                           wcvp_names$plant_name_id[acc[acc | syn]] &
                           !is.na(wcvp_dist$area), ]

  # Native and introduced records as listed in POWO, i.e. excluding extinct
  # and doubtful occurrences
  wcvp_dist <- wcvp_dist[wcvp_dist$extinct == 0 &
                           wcvp_dist$location_doubtful == 0, ]

  # Use the legacy WGSRPD level-3 names, as displayed in POWO
  tf <- wcvp_dist$area_code_l3 %in% names(.wcvp_legacy_areas)
  wcvp_dist$area[tf] <- .wcvp_legacy_areas[wcvp_dist$area_code_l3[tf]]

  wcvp_dist <- data.frame(plant_name_id = wcvp_dist$plant_name_id,
                          area = wcvp_dist$area,
                          introduced = wcvp_dist$introduced == 1)

  return(list(names = wcvp_names, distribution = wcvp_dist))
}

#_______________________________________________________________________________
# Function to get the accepted genera of each family, in the same structure as
# returned by .getgenURI
.wcvp_genera <- function(family,
                         genus = NULL,
                         hybrid = FALSE,
                         verbose = TRUE) {

  wcvp <- .wcvp_load(verbose = verbose)
  wn <- wcvp$names

  # Keep the family order of POWOcodes, as in the POWO search
  utils::data("POWOcodes", package = "expowo")
  fams <- POWOcodes$family[POWOcodes$family %in% family]
  fams <- c(fams, unique(family[!family %in% fams]))

  missing_fams <- fams[!fams %in% wn$family]
  if (length(missing_fams) > 0) {
    stop(paste("The following families were not found in WCVP:",
               paste(missing_fams, collapse = ", ")),
         call. = FALSE)
  }

  list_fams <- list()
  for (i in seq_along(fams)) {
    if (verbose) {
      message(paste0("Searching... ", fams[i], " ", i, "/", length(fams)))
    }
    gen <- wn[wn$family == fams[i] &
                wn$taxon_rank == "Genus" &
                wn$taxon_status == "Accepted", ]
    gen <- gen[order(gen$genus), ]

    list_fams[[i]] <- data.frame(family = gen$family,
                                 genus = gen$taxon_name,
                                 authors = gen$taxon_authors,
                                 scientific_name = paste(gen$taxon_name,
                                                         gen$taxon_authors),
                                 hybrid = ifelse(grepl("\u00D7|[+]",
                                                       gen$taxon_name),
                                                 "yes", "no"),
                                 kew_id = gen$powo_id,
                                 powo_uri = .wcvp_powo_uri(gen$powo_id),
                                 plant_name_id = gen$plant_name_id)
  }
  df <- do.call(rbind, list_fams)

  if (hybrid == FALSE) {
    df <- df[df$hybrid == "no", names(df) != "hybrid"]
  }

  # If a vector of genus names is provided, then remove all other genera in the
  # family during the subsequent search steps.
  if (!is.null(genus)) {
    temp <- df$genus %in% genus
    if (all(genus %in% df$genus)) {
      df <- df[temp, ]
    } else {

      stop(paste("Any genus in the provided genus vector might have a typo or is
                 not present in POWO.\n",
                 "Please correct the following names in your genus vector
                 according to POWO:\n", "\n",

                 paste(genus[!genus %in% df$genus], collapse = ", "), "\n",
                 "\n",

                 "Find help also at DBOSLab-UFBA:\n",
                 "Debora Zuanny, deborazuanny@gmail.com\n",
                 "Domingos Cardoso, cardosobot@gmail.com"))
    }
  }

  rownames(df) <- NULL

  return(df)
}

#_______________________________________________________________________________
# Function to get the accepted species of each genus, in the same structure as
# returned by .getsppURI
.wcvp_species <- function(powo_codes,
                          hybrid = FALSE,
                          verbose = TRUE) {

  wn <- .wcvp_load(verbose = verbose)$names

  spp <- wn[wn$taxon_rank == "Species" &
              wn$taxon_status == "Accepted" &
              wn$parent_plant_name_id %in% powo_codes$plant_name_id, ]

  # Keep the genus order of the search and the alphabetical order of species
  pos <- match(spp$parent_plant_name_id, powo_codes$plant_name_id)
  spp <- spp[order(pos, spp$taxon_name), ]
  pos <- match(spp$parent_plant_name_id, powo_codes$plant_name_id)

  df <- data.frame(family = powo_codes$family[pos],
                   genus = powo_codes$genus[pos],
                   species = gsub(".*\\s", "", spp$taxon_name),
                   taxon_name = spp$taxon_name,
                   authors = spp$taxon_authors,
                   scientific_name = paste(spp$taxon_name, spp$taxon_authors),
                   status = rep(NA, nrow(spp)),
                   accepted_name = rep(NA, nrow(spp)),
                   hybrid = ifelse(grepl("[+]|\u00D7", spp$taxon_name),
                                   "yes", "no"),
                   kew_id = spp$powo_id,
                   powo_uri = .wcvp_powo_uri(spp$powo_id),
                   plant_name_id = spp$plant_name_id)

  if (hybrid == FALSE) {
    df <- df[df$hybrid == "no", names(df) != "hybrid"]
  }

  rownames(df) <- NULL

  return(df)
}

#_______________________________________________________________________________
# Function to count the accepted species of each genus, as .getsppNumb does
.wcvp_sppNumb <- function(df,
                          verbose = TRUE) {

  wn <- .wcvp_load(verbose = verbose)$names

  spp <- wn$parent_plant_name_id[wn$taxon_rank == "Species" &
                                   wn$taxon_status == "Accepted"]
  df$species_number <- as.numeric(tabulate(match(spp, df$plant_name_id),
                                           nbins = nrow(df)))

  return(df)
}

#_______________________________________________________________________________
# Function to add publication, distribution and synonyms, returning the same
# columns as getInfo
.wcvp_getInfo <- function(df,
                          hybrid = FALSE,
                          synonyms = FALSE,
                          country = NULL,
                          gen_sp_nbr = FALSE,
                          verbose = TRUE) {

  wcvp <- .wcvp_load(verbose = verbose)
  wn <- wcvp$names

  if (gen_sp_nbr) {
    df <- .wcvp_sppNumb(df, verbose = verbose)
  }

  df$publication <- wn$publication[match(df$plant_name_id, wn$plant_name_id)]
  df$status <- rep("Accepted", nrow(df))

  # Native and introduced distribution. The distribution of a genus, as in
  # POWO, also includes the distribution of all its accepted species.
  distribution <- wcvp$distribution
  if (gen_sp_nbr) {
    spp <- wn[wn$taxon_rank == "Species" &
                wn$taxon_status == "Accepted" &
                wn$parent_plant_name_id %in% df$plant_name_id, ]
    spp_dist <- distribution[distribution$plant_name_id %in%
                               spp$plant_name_id, ]
    spp_dist$plant_name_id <-
      spp$parent_plant_name_id[match(spp_dist$plant_name_id,
                                     spp$plant_name_id)]
    distribution <- rbind(distribution[distribution$plant_name_id %in%
                                         df$plant_name_id, ], spp_dist)
  }
  dist <- .wcvp_dist(df$plant_name_id, distribution)
  df$native_to_country <- dist$native_to_country
  df$native_to_botanical_countries <- dist$native_to_botanical_countries
  df$introduced_to_country <- dist$introduced_to_country
  df$introduced_to_botanical_countries <- dist$introduced_to_botanical_countries

  # Synonyms are listed right after their accepted species
  if (synonyms) {
    df <- .wcvp_synonyms(df, wn)
  }

  # Select specific columns of interest, in the same order of getInfo
  if (gen_sp_nbr) {
    cols <- c("family", "genus", "authors", "scientific_name", "publication",
              "species_number", "hybrid")
  } else {
    cols <- c("family", "genus", "species", "taxon_name", "authors",
              "scientific_name", "status", "accepted_name", "publication",
              "hybrid")
  }
  cols <- c(cols, "native_to_country", "native_to_botanical_countries",
            "introduced_to_country", "introduced_to_botanical_countries",
            "kew_id", "powo_uri")
  if (hybrid == FALSE) {
    cols <- cols[cols != "hybrid"]
  }
  df <- df[, cols]

  # If a vector of country names is provided, then remove any taxon that does
  # not occur in the given country.
  if (!is.null(country)) {
    df <- country_filter(df,
                         country = country,
                         synonyms = synonyms,
                         verbose = verbose)
  }

  # Removing columns when 'gen_sp_nbr = FALSE'
  if (gen_sp_nbr == FALSE) {
    if (all(is.na(df$accepted_name))) {
      df <- df %>% select(-c("status", "accepted_name"))
    }
  }

  rownames(df) <- NULL

  return(df)
}

#_______________________________________________________________________________
# Secondary function to summarize the distribution of each taxon into the
# comma-separated strings of botanical countries and political countries
.wcvp_dist <- function(ids, distribution) {

  distribution <- distribution[distribution$plant_name_id %in% ids, ]
  nat <- distribution[!distribution$introduced, ]
  intr <- distribution[distribution$introduced, ]

  # Botanical countries for each taxon, alphabetically sorted as in POWO
  nat_bot <- lapply(split(nat$area, factor(nat$plant_name_id, levels = ids)),
                    function(x) sort(unique(x)))
  intr_bot <- lapply(split(intr$area, factor(intr$plant_name_id, levels = ids)),
                     function(x) sort(unique(x)))
  # Areas where the taxon is native are not listed as introduced
  intr_bot <- mapply(setdiff, intr_bot, nat_bot, SIMPLIFY = FALSE)

  # Related country of each botanical country, following botdiv_to_countries
  areas <- unique(distribution$area)
  bot_ctr <- .botdiv_lookup(areas)

  collapse_bot <- function(x) {
    if (length(x) == 0) return("unknown")
    paste(x, collapse = ", ")
  }
  collapse_ctr <- function(x) {
    if (length(x) == 0) return("unknown")
    paste(sort(unique(bot_ctr[x])), collapse = ", ")
  }
  to_ascii <- function(x) {
    iconv(unlist(x, use.names = F), from = "UTF-8", to = "ASCII//TRANSLIT")
  }

  data.frame(
    native_to_country = to_ascii(lapply(nat_bot, collapse_ctr)),
    native_to_botanical_countries = to_ascii(lapply(nat_bot, collapse_bot)),
    introduced_to_country = to_ascii(lapply(intr_bot, collapse_ctr)),
    introduced_to_botanical_countries = to_ascii(lapply(intr_bot,
                                                        collapse_bot)))
}

#_______________________________________________________________________________
# Secondary function to find related country for each botanical subdivision,
# vectorised version of the lookup used in botdiv_to_countries
.botdiv_lookup <- function(areas) {

  utils::data("botregions", package = "expowo")

  res <- areas
  for (n in seq_along(areas)) {
    tf <- botregions$botanical_division %in% areas[n]
    if (length(botregions$country[tf]) == 0) {
      tf <- strtrim(botregions$botanical_division, 20) %in% areas[n]
    }
    if (length(botregions$country[tf]) > 0 &&
        any(!botregions$country[tf] %in% areas[n])) {
      res[n] <- paste(botregions$country[tf], collapse = ", ")
    }
  }
  names(res) <- areas

  return(res)
}

#_______________________________________________________________________________
# Secondary function to add the synonyms right after each accepted species, as
# get_synonyms does
.wcvp_synonyms <- function(df, wn) {

  syn <- wn[wn$taxon_status != "Accepted" &
              wn$taxon_status != "Misapplied" &
              wn$accepted_plant_name_id %in% df$plant_name_id, ]
  if (nrow(syn) == 0) {
    return(df)
  }

  # Homotypic synonyms first and then heterotypic ones, as listed in POWO
  syn <- syn[order(match(syn$accepted_plant_name_id, df$plant_name_id),
                   !syn$homotypic_synonym, syn$taxon_name), ]
  pos <- match(syn$accepted_plant_name_id, df$plant_name_id)

  df_syn <- df[pos, ]
  df_syn[, setdiff(names(df_syn), c("family", "plant_name_id"))] <- NA
  df_syn$scientific_name <- trimws(paste(syn$taxon_name,
                                         ifelse(is.na(syn$taxon_authors), "",
                                                syn$taxon_authors)))
  df_syn$genus <- gsub("\\s.*", "", syn$taxon_name)
  df_syn$taxon_name <- syn$taxon_name
  df_syn$accepted_name <- df$scientific_name[pos]
  df_syn$status <- ifelse(syn$homotypic_synonym,
                          "Homotypic Synonym", "Heterotypic Synonym")
  df_syn$publication <- ifelse(is.na(syn$publication),
                               "unknown publication", syn$publication)
  df_syn$kew_id <- syn$powo_id
  df_syn$powo_uri <- .wcvp_powo_uri(syn$powo_id)

  # Interleave each accepted species with its synonyms
  df <- rbind(df, df_syn)
  ord <- c(seq_len(nrow(df) - nrow(df_syn)), pos)
  df <- df[order(ord, c(rep(0, length(ord) - length(pos)), seq_along(pos))), ]

  return(df)
}

#_______________________________________________________________________________
# Secondary function to build the POWO URI of each taxon
.wcvp_powo_uri <- function(powo_id) {
  ifelse(is.na(powo_id), NA,
         paste0("http://www.plantsoftheworldonline.org/",
                "taxon/urn:lsid:ipni.org:names:", powo_id))
}
