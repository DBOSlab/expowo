# Auxiliary function to get distribution, protologue, synonym and species number

getInfo <- function(df,
                    hybrid = hybrid,
                    synonyms = synonyms,
                    country = country,
                    gen_sp_nbr = gen_sp_nbr,
                    verbose = verbose,
                    dfsize = NULL) {

  # Creating empty lists to save data of interest during all search
  list_data <- get_tax_data(df,
                            synonyms = synonyms,
                            gen_sp_nbr = gen_sp_nbr,
                            verbose = verbose,
                            dfsize = dfsize)

  # Try open a new connection for those lost during the first search
  tf <- unlist(lapply(list_data[[1]], is.null))
  if (any(tf)) {
    list_data_temp <- get_tax_data(df[tf, ],
                                   synonyms = synonyms,
                                   gen_sp_nbr = gen_sp_nbr,
                                   verbose = verbose,
                                   dfsize = dfsize)
    for (i in seq_along(list_data)) {
      list_data[[i]][tf] <- list_data_temp[[i]]
    }
  }

  # Filling with NA each list that has returned species search with NULL results
  list_data <- lapply(list_data, .fill_NA)

  if (gen_sp_nbr) {
    # Extracting species number
    df$species_number <- as.numeric(unlist(list_data[[6]], use.names = F))
  }

  # Extracting the protologue
  df$publication <- NA
  df$publication <- unlist(list_data[[5]], use.names = F)
  df$status <- "Accepted"

  # Extracting native and introduced distribution
  df$native_to_country <- NA
  df$native_to_botanical_countries <- NA
  df$introduced_to_country <- NA
  df$introduced_to_botanical_countries <- NA
  begindiscol = 12
  enddiscol = 15
  n = 11
  if (hybrid) {
    begindiscol = 13
    enddiscol = 16
    n = 12
  }
  if (gen_sp_nbr) {
    begindiscol = 10
    enddiscol = 13
    n = 9
  }
  for (i in begindiscol:enddiscol) {
    df[[i]] <- unlist(list_data[[i-n]], use.names = F)

    # Removing any non-ASCII chars from the columns of distribution data
    df[[i]] <- iconv(df[[i]],
                     from = "UTF-8", to = "ASCII//TRANSLIT")
  }

  # Extracting synonyms
  if (synonyms) {
    df <- get_synonyms(df, list_data[[6]])
  }

  # Select specific columns of interest
  if (gen_sp_nbr == FALSE &
      hybrid == FALSE) {
    df <- df %>% select("family",
                        "genus",
                        "species",
                        "taxon_name",
                        "authors",
                        "scientific_name",
                        "status",
                        "accepted_name",
                        "publication",
                        "native_to_country",
                        "native_to_botanical_countries",
                        "introduced_to_country",
                        "introduced_to_botanical_countries",
                        "kew_id",
                        "powo_uri")
  }
  if (gen_sp_nbr == FALSE &
      hybrid == TRUE) {
    df <- df %>% select("family",
                        "genus",
                        "species",
                        "taxon_name",
                        "authors",
                        "scientific_name",
                        "status",
                        "accepted_name",
                        "publication",
                        "hybrid",
                        "native_to_country",
                        "native_to_botanical_countries",
                        "introduced_to_country",
                        "introduced_to_botanical_countries",
                        "kew_id",
                        "powo_uri")
  }
  if (gen_sp_nbr == TRUE &
      hybrid == FALSE) {
    df <- df %>% select("family",
                        "genus",
                        "authors",
                        "scientific_name",
                        "publication",
                        "species_number",
                        "native_to_country",
                        "native_to_botanical_countries",
                        "introduced_to_country",
                        "introduced_to_botanical_countries",
                        "kew_id",
                        "powo_uri")
  }
  if (gen_sp_nbr == TRUE &
      hybrid == TRUE) {
    df <- df %>% select("family",
                        "genus",
                        "authors",
                        "scientific_name",
                        "publication",
                        "species_number",
                        "hybrid",
                        "native_to_country",
                        "native_to_botanical_countries",
                        "introduced_to_country",
                        "introduced_to_botanical_countries",
                        "kew_id",
                        "powo_uri")
  }

  # If a vector of country names is provided, then remove any taxon that does
  # not occur in the given country. The temp vector is logical (TRUE or FALSE)
  # and shows which taxon/row should be kept in the search given the provided
  # country vector.
  if (!is.null(country)) {
    df <- country_filter(df,
                         country = country,
                         synonyms = synonyms,
                         verbose = verbose)
  }

  # Removing columns when 'gen_sp_nbr = FALSE'
  if (gen_sp_nbr == FALSE) {
    tf <- which(is.na(df$accepted_name) == TRUE)
    if (length(tf) == length(df$accepted_name)) {
      df <- df %>% select(-c("status", "accepted_name"))
    }
  }

  return(df)
}

#_______________________________________________________________________________
# Secondary function to visit the POWO page of each species
get_tax_data <- function(df,
                         synonyms = synonyms,
                         gen_sp_nbr = gen_sp_nbr,
                         verbose = verbose,
                         dfsize = dfsize) {

  l_uri <- length(df$powo_uri)

  if (gen_sp_nbr) {
    sp_nbr <- vector("list", length = l_uri)
  }

  dist_nat <- dist_nat_bot <- dist_intr <- dist_intr_bot <-
    html <- tf_sp_nbr <- publ <- syns <-
    vector("list", length = l_uri)

  for (i in seq_along(df$powo_uri)) {
    # The tryCatch function helps skipping error in for-loop
    tryCatch({
      tf <- df$powo_uri == df$powo_uri[i]
      gen <- df$genus[tf]
      fam <- df$family[tf]
      if (!is.na(df$powo_uri[i])) {
        #html[[i]] <- readLines(paste(df$powo_uri[i]), warn = F)
        httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
        html[[i]] <- readLines(textConnection(httr::content(httr::GET(paste(df$powo_uri[i])), as="text")), encoding="UTF-8", warn=F)

        # Adding a counter to identify each running search.
        if (gen_sp_nbr) {
          if (verbose) {
            message(paste0("Searching distribution and sp number of... ",
                           gen, " ",
                           fam, " ", i, "/",
                           length(sp_nbr)))
          }
        } else {
          if (verbose) {
            ng <- gen
            ng <- ng[!is.na(ng)]
            ns <- df$species[tf]
            ns <- ns[!is.na(ns)]
            nf <- fam
            nf <- nf[!is.na(nf)]
            message(paste0("Searching distribution of... ",
                           ng, " ",
                           ns, " ",
                           nf, " ", i, "/",
                           dfsize))
          }
        }
        # Extracting species number
        if (gen_sp_nbr) {
          tf_sp_nbr[[i]] <- grepl(">Includes\\s", html[[i]])
          sp_nbr[[i]] <- gsub(".*>Includes\\s", "",
                              html[[i]][tf_sp_nbr[[i]]])
          sp_nbr[[i]] <- gsub("\\sAccepted.+", "",
                              sp_nbr[[i]][grepl("\\sAccepted\\s", sp_nbr[[i]])])
        }
        # Extracting native and introduced distribution
        dist_nat[[i]] <-
          gsub(".*Native\\sto[:]<[/]h3>\\s+<p\\sclass[=]\"p\">\\s+", "",
               paste0(html[[i]], collapse = ""))
        dist_nat[[i]] <- gsub("\\s+<.+", "", dist_nat[[i]])
        dist_nat[[i]] <- gsub("\\s{2,}", " ", dist_nat[[i]])

        dist_intr[[i]] <-
          gsub(".*Introduced\\sinto[:]</h3>\\s+<p\\sclass[=]\"p\">\\s+", "",
               paste0(html[[i]], collapse = ""))
        dist_intr[[i]] <- gsub("\\s+<.+", "", dist_intr[[i]])
        dist_intr[[i]] <- gsub("\\s{2,}", " ", dist_intr[[i]])

        # Extracting the protologue
        publ[[i]] <- gsub(".*First\\spublished\\sin\\s+", "",
                          paste0(html[[i]], collapse = ""))
        publ[[i]] <- gsub("\\s+</div>.+", "", publ[[i]])
        publ[[i]] <- gsub("[&]amp;", "&", publ[[i]])

        if (synonyms) {
          # Extracting possible synonyms
          syns[[i]] <- gsub(".*Has\\s[0-9]\\s+", "",
                            paste0(html[[i]], collapse = ""))
          syns[[i]] <- gsub("+</span></li>(\\s){1,}</ul>(\\s){1,}</div>.+",
                            "", syns[[i]])
          syns[[i]] <- gsub(".*taxon-section__content+", "", syns[[i]])
        }

        dist_nat_bot[[i]] <- dist_nat[[i]]
        tf <- grepl("<", dist_nat_bot[[i]])
        if (any(tf)) {
          dist_nat_bot[[i]][tf] <- "unknown"
        }
        dist_intr_bot[[i]] <- dist_intr[[i]]
        tf <- grepl("<", dist_intr_bot[[i]])
        if (any(tf)) {
          dist_intr_bot[[i]][tf] <- "unknown"
        }
        # Correcting list of countries that come with different regions.
        dist_nat[[i]] <- botdiv_to_countries(dist_nat, i)
        dist_intr[[i]] <- botdiv_to_countries(dist_intr, i)
      }
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

  if (synonyms == FALSE &
      gen_sp_nbr == TRUE) {
    list_data <- list(dist_nat, dist_nat_bot, dist_intr,
                      dist_intr_bot, publ, sp_nbr)
    names(list_data) <- c("dist_nat", "dist_nat_bot", "dist_intr",
                          "dist_intr_bot", "publ", "sp_nbr")
  }
  if (gen_sp_nbr == FALSE &
      synonyms == TRUE) {
    list_data <- list(dist_nat, dist_nat_bot, dist_intr,
                      dist_intr_bot, publ, syns)
    names(list_data) <- c("dist_nat", "dist_nat_bot", "dist_intr",
                          "dist_intr_bot", "publ", "syns")
  }
  if (gen_sp_nbr == FALSE &
      synonyms == FALSE) {
    list_data <- list(dist_nat, dist_nat_bot, dist_intr,
                      dist_intr_bot, publ)
    names(list_data) <- c("dist_nat", "dist_nat_bot", "dist_intr",
                          "dist_intr_bot", "publ")
  }

  return(list_data)
}

#_______________________________________________________________________________
# Secondary function to synonyms and associated data
get_synonyms <- function(df, syns) {
  temp_taxa <- df$kew_id
  tf <- grepl("Synonym", syns)
  for (i in which(tf == TRUE)) {
    temp <- strsplit(syns[[i]], "heading-4")[[1]][-1]
    temp <- strsplit(temp, "p-regular teal-link")
    for (j in seq_along(temp)) {
      temp[[j]][1] <- gsub("s<.+", "", temp[[j]][1])
      temp[[j]][1] <- gsub(".+>", "", temp[[j]][1])
      temp_uri <- temp_opus <- temp_syns <- as.vector("")
      for (k in seq_along(temp[[j]][-1])) {

        # Extracting a string between other two strings
        temp_uri <- gsub(".*[:]names[:](.+)\"><em.*",
                         "\\1", temp[[j]][-1][k])
        temp_opus <- ifelse(grepl("[)]$", temp[[j]][-1][k]),
                            gsub(".*class[=]'p-s'>",
                                 "", temp[[j]][-1][k]),
                            gsub(".*class[=]'p-s'>(.+)</span></li>.*",
                                 "\\1", temp[[j]][-1][k]))
        tf_opus <- grepl("ipni[.]org", temp_opus)
        if(any(tf)) {
          temp_opus[tf_opus] <- gsub(".*class[=]'p-s'>(.+),\\s.*",
                                     "\\1", temp_opus[tf_opus])
        }
        tf_opus <- grepl("unknown\\spublication$|ipni[.]org", temp_opus)
        if(any(tf)) {
          temp_opus[tf_opus] <- "unknown publication"
        }
        temp_syns <- gsub(".*><em\\slang[=]\'la\'>(.+)</a>\\sin\\s<span.*",
                          "\\1", temp[[j]][-1][k])
        temp_syns <- gsub("</em>|<em lang='la'>", "",  temp_syns)

        pos <- which(df$kew_id %in% temp_taxa[i] == TRUE)
        df <- tibble::add_row(df, .after = pos)

        df$family[pos+1] <- df$family[pos]
        df$scientific_name[pos+1] <- temp_syns
        df$genus[pos+1] <- gsub("\\s.*", "", temp_syns)
        df$taxon_name[pos+1] <- flora::remove.authors(temp_syns)
        df$accepted_name[pos+1] <- df$scientific_name[pos]
        df$status[pos+1] <-temp[[j]][1]
        df$publication[pos+1] <- temp_opus
        df$kew_id[pos+1] <- temp_uri
        df$powo_uri[pos+1] <- paste0("http://www.plantsoftheworldonline.org/",
                                     "taxon/urn:lsid:ipni.org:names:",
                                     temp_uri)
      }
    }
  }
  return(df)
}

#_______________________________________________________________________________
# Secondary function to find related country for each botanical subdivision
botdiv_to_countries <- function(x, i) {

  # Load botanical subdivisions
  utils::data("botregions", package = "expowo")

  temp <- strsplit(x[[i]], ", ")[[1]]

  for (n in seq_along(temp)) {
    tf <- botregions$botanical_division %in% temp[n]
    if (length(botregions$country[tf]) == 0) {
      # Use strtrim to limit the length of each character/botanical region
      # because POWO has limited chars.
      bot_temp <- strtrim(botregions$botanical_division, 20)
      tf <- bot_temp %in% temp[n]
      if (length(botregions$country[tf]) > 0) {
        temp[n] <- botregions$country[tf]
      }
    } else {
      if (any(!botregions$country[tf] %in% temp[n])) {
        if (length(botregions$country[tf]) > 1) {
          temp[n] <- paste(botregions$country[tf], collapse = ", ")
        } else {
          temp[n] <- botregions$country[tf]
        }
      }
    }
  }
  temp <- sort(unique(temp))
  x[[i]] <- paste(temp, collapse = ", ")
  g <- grepl("<", x[[i]])
  if (any(g)) {
    x[[i]][g] <- "unknown"
  }

  return(x[[i]])
}

#_______________________________________________________________________________
# Secondary function to filter taxa in specific countries
country_filter <- function(df,
                           country = country,
                           synonyms = synonyms,
                           verbose = verbose) {
  temp <- vector()
  for (i in seq_along(df$native_to_country)) {
    tt <- gsub("^\\s", "",
               strsplit(df$native_to_country[i], ",")[[1]]) %in% country
    if (any(tt)) {
      temp[i] <- TRUE
    } else {
      temp[i] <- FALSE
    }

  }

  # The following conditions is just to show/print how the df will be
  # subsetted according to the provided country vector.
  if (verbose) {
    if (any(temp)) {
      tl <- list()
      for (i in seq_along(country)) {
        tv <- vector()
        for (l in seq_along(df$native_to_country)) {
          tv[l] <-
            country[i] %in% gsub("^\\s", "",
                                 strsplit(df$native_to_country[l], ",")[[1]])
        }
        if (length(which(tv == TRUE)) == 0) {
          tl[[i]] <- FALSE
        }
        if (length(which(tv == TRUE)) != 0 &
            length(which(tv == TRUE)) < length(tv)) {
          tl[[i]] <- TRUE
        }
        if (length(which(tv == TRUE)) == length(tv)) {
          tl[[i]] <- TRUE
        }
      }
      cv <- country[unlist(tl)]

      if (length(country[country %in% cv]) != length(country)) {
        cat(paste("Your search returned taxa with distribution only in the
                    following countries:\n", "\n",

                  paste(country[country %in% cv], collapse = ", "), "\n",
                  "\n",

                  "There is no taxa occurring in the countries below:\n",
                  "\n",

                  paste(country[!country %in% cv], collapse = ", "), "\n",
                  "\n",

                  "Check whether any taxon does not occur in the countries
                    above either because:\n",
                  "1. The taxon indeed does not occur in the provided
                    country vector;\n",
                  "2. The country name is written with any typo;\n",
                  "3. Any country name in the country vector is not written
                    in English language.\n", "\n"))
      }
    } else {
      cat(paste("Your search returned an empty data frame either because:\n",
                "1. No taxon occurs in the provided country vector;\n",
                "2. The country vector has any typo;\n",
                "3. Any country name in the country vector is not written in
                  English language."))
    }
  }

  # Subset the searched genera according to the country vector.
  if (verbose) {
    if(length(df$genus[temp]) != length(temp)) {
      cat(paste("Taxa listed below were removed from the original search
                  because they are not native to any of the given country
                  vector:\n", "\n",
                df$genus[!temp]))
    }
  }

  if (synonyms) {
    tf <- sub("^(\\S*\\s+\\S+).*",
              "\\1", df$accepted_name) %in% df$taxon_name[temp]
    temp[which(tf == TRUE)] <- TRUE
    df <- df[temp, ]
  } else {
    df <- df[temp, ]
  }

  return(df)
}

#_______________________________________________________________________________
# Secondary function to fill with NA some lists that returned NULL results
.fill_NA <- function(x) {
  temp <- lapply(x, is.null)
  x[unlist(temp)] <- NA
  temp <- lapply(x, function(x) length(x) == 0)
  x[unlist(temp)] <- NA
  return(x)
}
