# Auxiliary functions to support main functions
# Author: Debora Zuanny & Domingos Cardoso

#_______________________________________________________________________________
# Function to get genus URI
.getgenURI <- function(family = family,
                       genus = NULL,
                       hybrid = hybrid,
                       verbose = verbose) {

  # Extract the uri of each plant family using associated data POWOcodes
  utils::data("POWOcodes", package = "expowo")
  powo_codes <- dplyr::filter(POWOcodes, family %in% .env$family)

  powo_fams_uri <- list()
  list_fams <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a counter to identify each running search
    if (verbose) {
      message(paste0("Searching... ",
                     powo_codes$family[i], " ", i, "/",
                     length(powo_codes$family)))
    }

    # Visiting the POWO source page for each entry family
    #powo_fams_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8", warn = F)

    set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
    powo_fams_uri[[i]] <- readLines(textConnection(content(GET(powo_codes$uri[i]), as="text")), encoding="UTF-8", warn=F)
    # Find whether the family exists by searching a pattern for constituent
    # accepted genera.
    tt <- grepl("\\s{2,}This is a synonym of", powo_fams_uri[[i]])
    temp <- powo_fams_uri[[i]][tt]

    if (length(temp) != 0) {

      tt <- grepl("><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]",
                  powo_fams_uri[[i]])

      powo_fams_uri[[i]][tt] <- gsub(".*org[:]names[:]", "",
                                     powo_fams_uri[[i]][tt])
      new_powo_fam_uri <- gsub("\".*", "", powo_fams_uri[[i]][tt])
      new_fam_name <- gsub(".*[=]", "", powo_fams_uri[[i]][tt])
      new_fam_name <- gsub("<.*", "", new_fam_name)
      new_fam_name <- gsub(".*>", "", new_fam_name)
      message(paste(powo_codes$family[i], "is under synonym of",  new_fam_name))

      if (any(!new_fam_name %in% powo_codes$family)) {
        message(paste("Searching genera now under",
                      paste0(new_fam_name, "...")))
        new_powo_fam_uri <-
          paste("https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:",
                new_powo_fam_uri, sep = "")

        # Visiting POWO source page again for the accepted family name of an
        # originally entered family that is under synonym.
        #powo_fams_uri[[i]] <- readLines(new_powo_fam_uri, encoding = "UTF-8", warn = F)
        set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
        powo_fams_uri[[i]] <- readLines(textConnection(content(GET(new_powo_fam_uri), as="text")), encoding="UTF-8", warn=F)

        tt <- grepl("\\s{2,}This is a synonym of", powo_fams_uri[[i]])
        temp <- powo_fams_uri[[i]][tt]
        powo_codes$family[i] <- new_fam_name

      } else {
        powo_codes <- powo_codes[!powo_codes$family %in% powo_codes$family[i], ]
      }

    }

    if (length(temp) == 0) {
      # The temp vector get the URI for each genus within the family
      url_xx <- "><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]"
      temp <- grepl(url_xx, powo_fams_uri[[i]])
      powo_genus_uri <- powo_fams_uri[[i]][temp]
      # The following subset within the retrieved genus uri within the temp
      # vector is to exclude any possible IRI of family-level synonyms.
      temp <- !grepl("aceae|oideae|meta\\sproperty|meta\\sname|Compositae|
                     Cruciferae|Gramineae|Guttiferae|Labiatae|Leguminosae|
                     Palmae|Umbelliferae", powo_genus_uri)

      list_fams[[i]] <- data.frame(temp_genus_uri = powo_genus_uri[temp],
                                   family = powo_codes$family[i],
                                   genus = NA,
                                   authors = NA,
                                   scientific_name = NA,
                                   hybrid = NA,
                                   kew_id = NA,
                                   powo_uri = NA)

      # Filling retrieved information in each column
      list_fams[[i]][["temp_genus_uri"]] <-
        gsub(".*><a href[=]\"", "", list_fams[[i]][["temp_genus_uri"]])
      list_fams[[i]][["powo_uri"]] <-
        paste("http://www.plantsoftheworldonline.org",
              gsub("\".+", "", list_fams[[i]][["temp_genus_uri"]]), sep = "")
      list_fams[[i]][["kew_id"]] <- gsub(".+[:]", "",
                                         list_fams[[i]][["powo_uri"]])
      list_fams[[i]][["authors"]] <- gsub(".*em>", "",
                                          list_fams[[i]][["temp_genus_uri"]])
      list_fams[[i]][["authors"]] <- gsub("<.*", "",
                                          list_fams[[i]][["authors"]])
      list_fams[[i]][["authors"]] <- gsub("^\\s", "",
                                          list_fams[[i]][["authors"]])
      list_fams[[i]][["genus"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                        list_fams[[i]][["temp_genus_uri"]])
      list_fams[[i]][["scientific_name"]] <- paste(list_fams[[i]][["genus"]],
                                                   list_fams[[i]][["authors"]])
      # Select specific columns of interest
      list_fams[[i]] <- list_fams[[i]] %>% select("family", "genus", "authors",
                                                  "scientific_name", "hybrid",
                                                  "kew_id", "powo_uri")
    }

    # Identify hybrid taxon
    tf <- grepl("\u00D7", list_fams[[i]]$genus)
    list_fams[[i]]$hybrid[tf] <- "yes"
    list_fams[[i]]$hybrid[!tf] <- "no"

    if (hybrid == FALSE) {
      list_fams[[i]] <- list_fams[[i]] %>%
        filter(list_fams[[i]]$hybrid == "no") %>%
        select(-"hybrid")
    }

    # Pause for 300 seconds right after every 500th search,
    # because POWO website may crash when searching uninterruptedly.
    if (i %% 500 == 0) {
      Sys.sleep(300)
    }
    # Ending the main for loop
  }
  names(list_fams) <- powo_codes$family

  # Combining all dataframes from the list of each family search
  # Each dataframe includes the retrieved genus URI for each searched family
  df <- list_fams[[1]]
  if (length(list_fams) > 1) {
    for (i in 2:length(list_fams)) {
      df <- rbind(df, list_fams[[i]])
    }
  }

  # If a vector of genus names is provided, then remove all other genera in the
  # family during the subsequent search steps.
  if (!is.null(genus)) {
    temp <- df$genus %in% genus
    if (length(which(temp == TRUE)) == length(genus)) {
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

  return(df)
}

#_______________________________________________________________________________
# Function to get species URI
.getsppURI <- function(powo_codes,
                       splist = TRUE,
                       hybrid = hybrid,
                       verbose = verbose) {

  powo_genus_uri <- list()
  list_genus <- list()
  for (i in seq_along(powo_codes$powo_uri)) {
    # Adding a counter to identify each running search
    if (verbose) {
      message(paste0("Searching sp list of... ",
                     powo_codes$genus[i], " ",
                     powo_codes$family[i], " ",
                     i, "/", length(powo_codes$family)))
    }

    #powo_genus_uri[[i]] <- readLines(powo_codes$powo_uri[i], encoding = "UTF-8", warn = F)
    set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
    powo_genus_uri[[i]] <- readLines(textConnection(content(GET(powo_codes$powo_uri[i]), as="text")), encoding="UTF-8", warn=F)

    temp <- grepl("><a href[=]\"/taxon/urn[:]lsid[:]ipni[.]org[:]names[:]",
                  powo_genus_uri[[i]])
    powo_spp_uri <- powo_genus_uri[[i]][temp]

    if (length(powo_spp_uri) == 0) {
      powo_spp_uri = "unknown"
    }

    list_genus[[i]] <- data.frame(temp_spp_uri = powo_spp_uri,
                                  family = powo_codes$family[i],
                                  genus = powo_codes$genus[i],
                                  species = NA,
                                  taxon_name = NA,
                                  authors = NA,
                                  scientific_name = NA,
                                  status = NA,
                                  accepted_name = NA,
                                  hybrid = NA,
                                  kew_id = NA,
                                  powo_uri = NA)

    if (!"unknown" %in% powo_spp_uri) {
      # Filling in each column
      list_genus[[i]][["temp_spp_uri"]] <-
        gsub(".*><a href[=]\"", "", list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["powo_uri"]] <-
        paste("http://www.plantsoftheworldonline.org",
              gsub("\".+", "", list_genus[[i]][["temp_spp_uri"]]), sep = "")
      list_genus[[i]][["kew_id"]] <-
        gsub(".+[:]", "", list_genus[[i]][["powo_uri"]])
      list_genus[[i]][["species"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                           list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["species"]] <- gsub(".*\\s", "",
                                           list_genus[[i]][["species"]])
      list_genus[[i]][["authors"]] <- gsub(".*em>", "",
                                           list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["authors"]] <- gsub("<.*", "",
                                           list_genus[[i]][["authors"]])
      list_genus[[i]][["authors"]] <- gsub("^\\s", "",
                                           list_genus[[i]][["authors"]])
      list_genus[[i]][["taxon_name"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                              list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["taxon_name"]] <- gsub(".*\\slang[=]'la'>|<[/]em>.*", "",
                                              list_genus[[i]][["temp_spp_uri"]])
      list_genus[[i]][["scientific_name"]] <-
        paste(list_genus[[i]][["taxon_name"]], list_genus[[i]][["authors"]])

      # Remove any possible generic synonym from the retrieved list
      list_genus[[i]] <- list_genus[[i]][grepl("\\s",
                                               list_genus[[i]]$taxon_name), ]
    }

    # Select specific columns of interest
    list_genus[[i]] <- list_genus[[i]] %>% select("family", "genus", "species",
                                                  "taxon_name", "authors",
                                                  "scientific_name", "status",
                                                  "accepted_name", "hybrid",
                                                  "kew_id", "powo_uri")

    if (splist) {
      # Identify hybrid taxon
      tf <- grepl("[+]|\u00D7", list_genus[[i]]$taxon_name)
      list_genus[[i]]$hybrid[tf] <- "yes"
      list_genus[[i]]$hybrid[!tf] <- "no"

      if (hybrid == FALSE) {
        list_genus[[i]] <- list_genus[[i]] %>%
          filter(list_genus[[i]]$hybrid == "no") %>%
          select(-"hybrid")
      }
    }
    # Pause for 300 seconds right after every 500th search,
    # because POWO website may crash when searching uninterruptedly.
    if (i %% 500 == 0) {
      Sys.sleep(300)
    }
  }
  names(list_genus) <- powo_codes$genus

  # Combining all data frames from the list of each family/genus search
  df <- list_genus[[1]]
  if (length(list_genus) > 1) {
    for (i in 2:length(list_genus)) {
      df <- rbind(df, list_genus[[i]])
    }
  }

  return(df)
}

#_______________________________________________________________________________
# Function to get species number and reopen connection for lost query
.getsppNumb <- function(df,
                        verbose = verbose) {

  # Extract number of species in each genus of the queried families
  df <- .getNumb(df,
                 verbose = verbose)

  # Try open a new connection for those lost during the first search
  df <- .reopen_query(df,
                      verbose = verbose)

  return(df)
}

#_______________________________________________________________________________
# Function to get species number
.getNumb <- function(df,
                     verbose = verbose) {

  l_uri <- length(df$powo_uri)
  # Creating empty lists to save data of interest during all search
  list_spp <- list_html <- list_grepl <- vector("list", length = l_uri)

  for (i in seq_along(df$powo_uri)) {
    # The tryCatch function helps skipping error in for-loop.
    tryCatch({

      #list_html[[i]] <- readLines(paste(df$powo_uri[i]), warn = F)
      list_html[[i]] <- readLines(textConnection(content(GET(paste(df$powo_uri[i])), as="text")), encoding="UTF-8", warn=F)
      # Adding a counter to identify each running search
      tf <- df$powo_uri == df$powo_uri[i]
      if (verbose) {
        gen <- df$genus[tf]
        fam <- df$family[tf]
        message(paste0("Searching sp number of... ",
                       gen, " ",
                       fam, " ", i, "/",
                       length(list_spp)))
      }

      list_grepl[[i]] <- grepl(">Includes\\s", list_html[[i]])
      list_spp[[i]] <- gsub(".*>Includes\\s", "",
                            list_html[[i]][list_grepl[[i]]])
      list_spp[[i]] <- gsub("\\sAccepted.+", "",
                            list_spp[[i]][grepl("\\sAccepted\\s",
                                                list_spp[[i]])])

      # The function below will print any search error (e.g. site address of a
      # specific genus is not opening for some reason).
    }, error = function(e) {cat(paste("ERROR:", df$genus[tf], df$family[tf]),
                                conditionMessage(e), "\n")})

    # Pause for 300 seconds right after every 500th search,
    # because POWO website may crash when searching uninterruptedly.
    if (i %% 500 == 0) {
      Sys.sleep(300)
    }
  }

  # Filling in with "NA" those genera for which the search failed to open the
  # POWO site.
  temp <- lapply(list_spp, is.null)
  list_spp[unlist(temp)] <- NA
  temp <- lapply(list_spp, function(x) length(x) == 0)
  list_spp[unlist(temp)] <- NA

  # Extracting the number of species from the list during the POWO searching.
  df$species_number <- unlist(list_spp, use.names = F)
  df$species_number <- as.numeric(df$species_number)

  return(df)
}

#_______________________________________________________________________________
# Function reopen a new connection for those lost during the first search
.reopen_query <- function(df, verbose = verbose) {
  tf <- is.na(df$species_number)
  if (any(tf)) {
    df_temp <- .getNumb(df[tf, ],
                        verbose = verbose)
    for (i in seq_along(df_temp$kew_id)) {
      tf <- df$kew_id %in% df_temp$kew_id[i]
      df$species_number[tf] <- df_temp$species_number[i]
    }
  }
  return(df)
}

#_______________________________________________________________________________
# Function to apply a continuation search from the last searched taxon
.powo_rerun <- function (df, sp_uri = FALSE, dir, filename) {
  csv_file <- paste0(paste0(dir, "/", format(Sys.time(), "%d%b%Y")),
                     "/", filename, ".csv")
  # Read headers only
  column_names <- as.vector(t(utils::read.csv(csv_file, header=FALSE,
                                              colClasses='character', nrows=1)))
  # Then read last n lines
  ltokeep <- 100
  nL <- R.utils::countLines(csv_file)
  df_temp <- read.csv(csv_file, header=FALSE, col.names=column_names,
                      skip=nL-ltokeep)
  status <- df_temp$status
  if (any(names(df_temp) %in% "status")) {
    df_temp <- df_temp %>% filter(status == "Accepted")
    l_row <- nrow(df_temp)
    last_gen <- df_temp$genus[l_row]
  } else {
    l_row <- nrow(df_temp)
    last_gen <- df_temp$genus[l_row]
  }

  if (sp_uri) {
    pos <- (which(df$kew_id %in% df_temp$kew_id[l_row])+1):nrow(df)
    df <- df[pos, ]
  } else {
    df <- df[df$genus %in% last_gen, ]
  }

  return(df)
}
