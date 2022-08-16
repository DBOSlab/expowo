# Auxiliary function to get genus URI

getGenURI <- function(powo_codes,
                      genus = NULL,
                      verbose = verbose) {

  powo_fams_uri <- list()
  list_fams <- list()
  for (i in seq_along(powo_codes$uri)) {
    # Adding a pause of 300 seconds at every 500th search,
    # because POWO website cannot permit constant search.
    if (i%%500 == 0) {
      Sys.sleep(300)
    }
    # Adding a counter to identify each running search.
    if (verbose) {
      print(paste0("Searching... ",
                   powo_codes$family[i], " ", i, "/",
                   length(powo_codes$family)))
    }

    # Visiting the POWO source page for each entry family.
    powo_fams_uri[[i]] <- readLines(powo_codes$uri[i], encoding = "UTF-8",
                                    warn = F)

    # Find whether the family exists by searching a pattern for constituent
    # accepted genera.
    tt <- grepl("\\s{2,}This is a synonym of", powo_fams_uri[[i]])
    temp <- powo_fams_uri[[i]][tt]

    if (length(temp) != 0) {

      tt <- grepl("<a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]",
                  powo_fams_uri[[i]])

      powo_fams_uri[[i]][tt] <- gsub(".*org[:]names[:]", "",
                                     powo_fams_uri[[i]][tt])
      new_powo_fam_uri <- gsub("\".*", "", powo_fams_uri[[i]][tt])
      new_fam_name <- gsub(".*[=]", "", powo_fams_uri[[i]][tt])
      new_fam_name <- gsub("<.*", "", new_fam_name)
      new_fam_name <- gsub(".*>", "", new_fam_name)
      print(paste(powo_codes$family[i], "is under synonym of",  new_fam_name))

      if (any(!new_fam_name %in% powo_codes$family)) {
        print(paste("Searching genera now under", paste0(new_fam_name, "...")))
        new_powo_fam_uri <-
          paste("http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:",
                new_powo_fam_uri, sep = "")

        # Visiting POWO source page again for the accepted family name of an
        # originally entered family that is under synonym.
        powo_fams_uri[[i]] <- readLines(new_powo_fam_uri, encoding = "UTF-8",
                                        warn = F)

        tt <- grepl("\\s{2,}This is a synonym of", powo_fams_uri[[i]])
        temp <- powo_fams_uri[[i]][tt]

        powo_codes$family[i] <- new_fam_name

      } else {
        powo_codes <- powo_codes[!powo_codes$family %in% powo_codes$family[i], ]
      }

    }

    if (length(temp) == 0) {
      # The temp vector get the URI for each genus within the family.
      temp <-
        grepl("<li><a href[=]\"[/]taxon[/]urn[:]lsid[:]ipni[.]org[:]names[:]",
              powo_fams_uri[[i]])
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
                                   kew_id = NA,
                                   powo_uri = NA)

      # Filling retrieved information in each column.
      list_fams[[i]][["temp_genus_uri"]] <-
        gsub(".*<li><a href[=]\"", "", list_fams[[i]][["temp_genus_uri"]])
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

      # Select specific columns of interest.
      list_fams[[i]] <- list_fams[[i]] %>% select("family", "genus", "authors",
                                                  "scientific_name", "kew_id",
                                                  "powo_uri")
    }

    # Ending the main for loop.
  }
  names(list_fams) <- powo_codes$family

  # Combining all dataframes from the list of each family search.
  # Each dataframe includes the retrieved genus URI for each searched family.
  if (length(list_fams) == 1) {
    df <- list_fams[[1]]
  } else {
    df <- list_fams[[1]]
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
