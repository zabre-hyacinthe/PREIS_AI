# ============================================================
# PREIS-AI — STEP 12
# Final AI tracker
# Consolidation / prioritization / QA
# HARDENED FULL VERSION
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(lubridate)
})

build_ai_tracker_final <- function(project_dir = "D:/PREIS_AI") {
  
  out_file_enriched  <- file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final.csv")
  out_file_dashboard <- file.path(project_dir, "dashboard", "data", "dashboard", "ai_tracker.csv")
  qa_file            <- file.path(project_dir, "data", "outputs", "ai", "ai_tracker_final_qc.csv")
  
  dir.create(dirname(out_file_enriched), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_file_dashboard), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qa_file), recursive = TRUE, showWarnings = FALSE)
  
  clean_dir    <- file.path(project_dir, "data", "clean", "ai")
  enriched_dir <- file.path(project_dir, "data", "enriched", "ai")
  outputs_dir  <- file.path(project_dir, "data", "outputs", "ai")
  
  message("[PREIS-AI] STEP 12 — build_ai_tracker_final started...")
  
  normalize_blank_to_na <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)
    x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN")] <- NA_character_
    x
  }
  
  normalize_geo_to_na <- function(x) {
    x <- normalize_blank_to_na(x)
    x[x %in% c("no_geo", "No_geo", "NO_GEO", "Unspecified", "Unknown", "Africa", "All")] <- NA_character_
    x
  }
  
  get_first_existing <- function(df, candidates, default = NA_character_) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    
    vals <- lapply(hit, function(nm) normalize_blank_to_na(df[[nm]]))
    out <- vals[[1]]
    
    if (length(vals) > 1) {
      for (i in 2:length(vals)) {
        out <- dplyr::coalesce(out, vals[[i]])
      }
    }
    
    if (!is.na(default)) out[is.na(out)] <- default
    out
  }
  
  get_first_existing_numeric <- function(df, candidates, default = 0) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    
    vals <- lapply(hit, function(nm) suppressWarnings(as.numeric(as.character(df[[nm]]))))
    out <- vals[[1]]
    
    if (length(vals) > 1) {
      for (i in 2:length(vals)) {
        out <- dplyr::coalesce(out, vals[[i]])
      }
    }
    
    out[is.na(out)] <- default
    out
  }
  
  get_first_existing_int <- function(df, candidates, default = NA_integer_) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    
    vals <- lapply(hit, function(nm) suppressWarnings(as.integer(as.character(df[[nm]]))))
    out <- vals[[1]]
    
    if (length(vals) > 1) {
      for (i in 2:length(vals)) {
        out <- dplyr::coalesce(out, vals[[i]])
      }
    }
    
    out
  }
  
  parse_publication_date <- function(x) {
    x <- normalize_blank_to_na(x)
    
    if (length(x) == 0) return(character(0))
    
    x <- gsub("T", " ", x, fixed = TRUE)
    x <- sub("Z$", "", x)
    x <- sub("\\+00:00$", "", x)
    x <- sub("\\+0000$", "", x)
    x <- sub(" UTC$", "", x)
    x <- gsub("([0-9])\\.([0-9]+)$", "\\1", x)
    x <- gsub("([0-9])\\.([0-9]+) ", "\\1 ", x)
    
    out <- rep(NA_character_, length(x))
    
    try_formats <- c(
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%Y%m%d",
      "%Y-%m-%d %H:%M:%S",
      "%Y/%m/%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M",
      "%d/%m/%Y",
      "%m/%d/%Y",
      "%d-%m-%Y",
      "%m-%d-%Y",
      "%d %b %Y",
      "%d %B %Y",
      "%b %d %Y",
      "%B %d %Y"
    )
    
    for (fmt in try_formats) {
      idx <- which(is.na(out) & !is.na(x))
      if (length(idx) == 0) break
      
      parsed <- suppressWarnings(as.Date(x[idx], format = fmt))
      ok <- !is.na(parsed)
      
      if (any(ok)) {
        out[idx[ok]] <- as.character(parsed[ok])
      }
    }
    
    # year-month fallback
    idx_ym <- which(is.na(out) & !is.na(x) & grepl("^\\d{4}-\\d{2}$", x))
    if (length(idx_ym) > 0) {
      out[idx_ym] <- paste0(x[idx_ym], "-01")
    }
    
    # year fallback
    idx_y <- which(is.na(out) & !is.na(x) & grepl("^\\d{4}$", x))
    if (length(idx_y) > 0) {
      out[idx_y] <- paste0(x[idx_y], "-01-01")
    }
    
    out
  }
  
  parse_fetch_date <- function(x, default_date = Sys.Date()) {
    out <- parse_publication_date(x)
    out[is.na(out)] <- as.character(default_date)
    out
  }
  
  # --------------------------------------------------------
  # INPUT FILE
  # --------------------------------------------------------
  candidate_inputs <- c(
    file.path(clean_dir, "ai_multisource_deduplicated.csv"),
    file.path(clean_dir, "ai_multisource_merged.csv"),
    file.path(enriched_dir, "ai_tracker_final_geo.csv"),
    file.path(outputs_dir, "ai_tracker_final_geo.csv"),
    file.path(enriched_dir, "ai_novelty_stage1.csv")
  )
  
  candidate_inputs <- unique(candidate_inputs[file.exists(candidate_inputs)])
  
  if (length(candidate_inputs) == 0) {
    stop("No valid tracker input file found.")
  }
  
  in_file <- candidate_inputs[1]
  message("[PREIS-AI] Input file used: ", in_file)
  
  df <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  
  if (nrow(df) == 0) {
    stop("Input file is empty: ", in_file)
  }
  
  # Ensure record_id exists before joins
  if (!"record_id" %in% names(df)) {
    if ("rec_id_provisional" %in% names(df)) {
      df$record_id <- as.character(df$rec_id_provisional)
    } else {
      df$record_id <- paste0("rec_", seq_len(nrow(df)))
    }
  } else {
    df$record_id <- as.character(df$record_id)
  }
  
  # --------------------------------------------------------
  # LOAD SCORING FILES SAFELY
  # --------------------------------------------------------
  # [FIX P0 2026-08-05] Le scoreur (10_score_ai_africa_relevance.R) écrit
  # ai_scored_stage1.csv, PAS ai_africa_relevance_scored.csv. L'ancien chemin
  # n'existait jamais -> africa_df = NULL -> africa_relevance_level vide.
  africa_file  <- file.path(project_dir, "data", "enriched", "ai", "ai_scored_stage1.csv")
  novelty_file <- file.path(project_dir, "data", "enriched", "ai", "ai_novelty_stage1.csv")
  
  africa_df <- if (file.exists(africa_file)) read.csv(africa_file, stringsAsFactors = FALSE, check.names = FALSE) else NULL
  novelty_df <- if (file.exists(novelty_file)) read.csv(novelty_file, stringsAsFactors = FALSE, check.names = FALSE) else NULL
  
  if (!is.null(africa_df)) {
    # [FIX P0] Clé de jointure = rec_id_provisional (identique à la base df).
    # La colonne record_id, quand elle existe, peut diverger et casse la jointure.
    if ("rec_id_provisional" %in% names(africa_df)) {
      africa_df$record_id <- as.character(africa_df$rec_id_provisional)
    }

    keep_africa <- intersect(
      c("record_id", "africa_relevance_score", "africa_relevance_level", "africa_relevance_reason",
        "operational_value_score"),   # [FIX P1] faire remonter l'axe utilité
      names(africa_df)
    )
    
    africa_df <- africa_df[, keep_africa, drop = FALSE]
    
    rename_map <- c(
      africa_relevance_score = "africa_relevance_score_scored",
      africa_relevance_level = "africa_relevance_level_scored",
      africa_relevance_reason = "africa_relevance_reason_scored"
    )
    
    for (nm in names(rename_map)) {
      if (nm %in% names(africa_df)) names(africa_df)[names(africa_df) == nm] <- rename_map[[nm]]
    }
    
    df <- dplyr::left_join(df, africa_df, by = "record_id")
  }
  
  if (!is.null(novelty_df)) {
    # [FIX P0] Clé de jointure = rec_id_provisional. Le fichier novelty possède
    # une colonne record_id dont les valeurs divergent de la base -> la jointure
    # par record_id renvoyait NA -> novelty_level vide. On force la clé commune.
    if ("rec_id_provisional" %in% names(novelty_df)) {
      novelty_df$record_id <- as.character(novelty_df$rec_id_provisional)
    }

    keep_novelty <- intersect(
      c("record_id", "novelty_score", "novelty_level", "novelty_reason"),
      names(novelty_df)
    )
    
    novelty_df <- novelty_df[, keep_novelty, drop = FALSE]
    
    rename_map <- c(
      novelty_score = "novelty_score_scored",
      novelty_level = "novelty_level_scored",
      novelty_reason = "novelty_reason_scored"
    )
    
    for (nm in names(rename_map)) {
      if (nm %in% names(novelty_df)) names(novelty_df)[names(novelty_df) == nm] <- rename_map[[nm]]
    }
    
    df <- dplyr::left_join(df, novelty_df, by = "record_id")
  }
  
  # [FIX P1] Garantir présence + propreté de operational_value_score (axe utilité)
  if (!"operational_value_score" %in% names(df)) df$operational_value_score <- 0L
  df$operational_value_score <- suppressWarnings(as.integer(df$operational_value_score))
  df$operational_value_score[is.na(df$operational_value_score)] <- 0L

  # --------------------------------------------------------
  # BUILD FINAL TRACKER
  # --------------------------------------------------------
  df_out <- df %>%
    mutate(
      record_id = as.character(record_id),
      
      pub_year = get_first_existing_int(
        .,
        c("pub_year", "year", "publication_year", "publicationYear", "pubYear")
      ),
      
      publication_date_raw = get_first_existing(
        .,
        c(
          "publication_date_raw",
          "publication_date",
          "article_date_raw",
          "pub_date_raw",
          "medline_date",
          "pub_date",
          "first_publication_date",
          "firstPublicationDate",
          "epmc_first_publication_date",
          "epmc_firstPublicationDate",
          "publication_date_epmc",
          "electronic_publication_date",
          "first_index_date",
          "date",
          "article_date",
          "articleDate",
          "published",
          "published_date",
          "updated_date",
          "online_date",
          "onlineDate",
          "epubdate",
          "sortpubdate"
        )
      ),
      
      publication_date = parse_publication_date(publication_date_raw),
      
      publication_date = dplyr::coalesce(
        publication_date,
        ifelse(!is.na(pub_year), paste0(pub_year, "-01-01"), NA_character_)
      ),
      
      publication_date_present = !is.na(publication_date) & publication_date != "",
      
      title       = get_first_existing(., c("title")),
      doi         = get_first_existing(., c("doi")),
      pmid        = get_first_existing(., c("pmid")),
      pmcid       = get_first_existing(., c("pmcid")),
      journal     = get_first_existing(., c("journal")),
      authors     = get_first_existing(., c("authors")),
      abstract    = get_first_existing(., c("abstract")),
      affiliation = get_first_existing(., c("affiliation", "author_affiliations")),
      
      country = dplyr::coalesce(
        normalize_geo_to_na(get_first_existing(., c("country"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("country_txt"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("country_norm"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("country_detected"), NA_character_))
      ),
      
      iso3 = dplyr::coalesce(
        normalize_geo_to_na(get_first_existing(., c("iso3"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("iso3_txt"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("iso3_norm"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("iso3_detected"), NA_character_))
      ),
      
      rcc = dplyr::coalesce(
        normalize_geo_to_na(get_first_existing(., c("rcc"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("rcc_txt"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("rcc_norm"), NA_character_)),
        normalize_geo_to_na(get_first_existing(., c("rcc_detected"), NA_character_))
      ),
      
      region_level = get_first_existing(., c("region_level")),
      
      source = normalize_blank_to_na(
        get_first_existing(., c("source", "source_db", "database"), NA_character_)
      ),
      
      source_name = dplyr::case_when(
        source == "PubMed" ~ "PubMed",
        source == "pubmed" ~ "PubMed",
        source == "PMC" ~ "EuropePMC",
        source == "EuropePMC" ~ "EuropePMC",
        source == "AGR" ~ "arXiv",
        source == "arXiv" ~ "arXiv",
        source == "MED" ~ "medRxiv",
        source == "medRxiv" ~ "medRxiv",
        source == "PPR" ~ "bioRxiv",
        source == "bioRxiv" ~ "bioRxiv",
        source == "ETH" ~ "ETH archive",
        !is.na(source) & source != "" ~ source,
        TRUE ~ normalize_blank_to_na(get_first_existing(., c("source_name"), NA_character_))
      ),
      
      record_origin = normalize_blank_to_na(get_first_existing(., c("record_origin"), NA_character_)),
      server        = normalize_blank_to_na(get_first_existing(., c("server"), NA_character_)),
      source_query  = get_first_existing(., c("source_query", "query_scope")),
      query_id      = get_first_existing(., c("query_id", "query_string")),
      url           = normalize_blank_to_na(get_first_existing(., c("url"), NA_character_)),
      
      date_fetched = parse_fetch_date(
        get_first_existing(
          .,
          c(
            "date_fetched",
            "fetched_date",
            "fetch_date",
            "retrieved_date",
            "date_downloaded",
            "created_at",
            "updated_at"
          ),
          NA_character_
        ),
        default_date = Sys.Date()
      ),
      
      has_ai_signal = get_first_existing(., c("has_ai_signal")),
      has_nlp_surveillance = get_first_existing(., c("has_nlp_surveillance")),
      primary_ai_method = get_first_existing(., c("primary_ai_method", "method_family_primary")),
      primary_surveillance_function = get_first_existing(., c("primary_surveillance_function", "application_type_primary")),
      primary_ai_usecase = get_first_existing(., c("primary_ai_usecase")),
      primary_data_type = get_first_existing(., c("primary_data_type")),
      ai_surveillance_relevance = get_first_existing(., c("ai_surveillance_relevance")),
      ai_all_matches = get_first_existing(., c("ai_all_matches")),
      
      method_family_primary = get_first_existing(., c("method_family_primary", "primary_ai_method")),
      application_type_primary = get_first_existing(., c("application_type_primary", "primary_surveillance_function")),
      ai_evidence_type = get_first_existing(., c("ai_evidence_type")),
      
      # IMPORTANT: take scored columns first if present
      africa_relevance_level = get_first_existing(
        .,
        c("africa_relevance_level_scored", "africa_relevance_level"),
        NA_character_
      ),
      
      novelty_level = get_first_existing(
        .,
        c("novelty_level_scored", "novelty_level"),
        NA_character_
      ),
      
      africa_relevance_score = get_first_existing_numeric(
        .,
        c("africa_relevance_score_scored", "africa_relevance_score"),
        default = 0
      ),
      
      novelty_score = get_first_existing_numeric(
        .,
        c("novelty_score_scored", "novelty_score"),
        default = 0
      ),
      
      novelty_reason = get_first_existing(
        .,
        c("novelty_reason_scored", "novelty_reason"),
        NA_character_
      ),
      
      africa_relevance_reason = get_first_existing(
        .,
        c("africa_relevance_reason_scored", "africa_relevance_reason"),
        NA_character_
      ),
      
      method_present = !is.na(normalize_blank_to_na(method_family_primary)),
      application_present = !is.na(normalize_blank_to_na(application_type_primary)),
      
      ai_profile_flag = case_when(
        method_present & application_present ~ "Well-classified AI record",
        method_present | application_present ~ "Partially classified AI record",
        TRUE ~ "Weakly classified AI record"
      ),
      
      africa_weight = case_when(
        africa_relevance_level == "High" ~ 30,
        africa_relevance_level %in% c("Moderate", "Medium") ~ 20,
        africa_relevance_level == "Low" ~ 10,
        TRUE ~ 0
      ),
      
      novelty_weight = case_when(
        novelty_level == "High" ~ 30,
        novelty_level %in% c("Moderate", "Medium") ~ 20,
        novelty_level == "Low" ~ 10,
        TRUE ~ 0
      ),
      
      classification_weight = case_when(
        ai_profile_flag == "Well-classified AI record" ~ 20,
        ai_profile_flag == "Partially classified AI record" ~ 10,
        TRUE ~ 0
      ),
      
      evidence_weight = case_when(
        ai_evidence_type == "APPLICATION" ~ 15,
        ai_evidence_type == "VALIDATION" ~ 10,
        ai_evidence_type == "THEORY" ~ 5,
        TRUE ~ 0
      ),
      
      # [FIX P1] Axe utilité opérationnelle (0–6) -> 0–30, pour que les méthodes
      # transférables fortes remontent même sans ancrage africain.
      operational_weight = pmin(30L, operational_value_score * 5L),

      priority_score_raw =
        africa_weight +
        novelty_weight +
        classification_weight +
        evidence_weight +
        operational_weight +
        pmin(10, round(africa_relevance_score / 10)) +
        pmin(10, round(novelty_score / 10)),
      
      priority_score = pmin(100L, as.integer(priority_score_raw)),
      
      priority_level = case_when(
        priority_score >= 70 ~ "High",
        priority_score >= 45 ~ "Moderate",
        TRUE ~ "Low"
      ),
      
      tracker_recommendation = case_when(
        priority_level == "High" ~ "Prioritize for dashboard and expert review",
        priority_level == "Moderate" ~ "Retain for monitoring and thematic review",
        TRUE ~ "Archive for background tracking"
      ),
      
      source_present = (!is.na(source) & source != "") | (!is.na(source_name) & source_name != ""),
      geo_present = (!is.na(country)) | (!is.na(iso3)) | (!is.na(rcc))
    ) %>%
    select(
      any_of(c(
        "record_id", "title", "abstract", "pub_year",
        "publication_date", "publication_date_raw", "publication_date_present",
        "doi", "pmid", "pmcid", "journal", "authors",
        "affiliation", "country", "iso3", "rcc", "region_level",
        "source", "source_name", "record_origin", "server", "source_query", "query_id",
        "url", "date_fetched",
        "has_ai_signal", "has_nlp_surveillance", "primary_ai_method",
        "primary_surveillance_function", "primary_ai_usecase", "primary_data_type",
        "ai_surveillance_relevance", "ai_all_matches",
        "method_family_primary", "application_type_primary", "ai_evidence_type",
        "africa_relevance_score", "africa_relevance_level", "africa_relevance_reason",
        "operational_value_score", "operational_weight",
        "novelty_score", "novelty_level", "novelty_reason",
        "ai_profile_flag", "priority_score", "priority_level", "tracker_recommendation",
        "source_present", "geo_present"
      )),
      everything()
    )
  
  qa <- tibble(
    metric = c(
      "input_file_used",
      "n_records",
      "n_high_priority",
      "n_moderate_priority",
      "n_low_priority",
      "n_well_classified",
      "n_partially_classified",
      "n_weakly_classified",
      "n_high_africa",
      "n_high_novelty",
      "n_with_africa_score_gt0",
      "n_with_novelty_score_gt0",
      "n_with_source",
      "n_with_source_name",
      "n_unique_sources",
      "n_unique_source_names",
      "n_with_geo",
      "n_unique_rcc",
      "n_unique_iso3",
      "n_unique_country",
      "n_with_publication_date",
      "n_with_date_fetched"
    ),
    value = c(
      basename(in_file),
      as.character(nrow(df_out)),
      as.character(sum(df_out$priority_level == "High", na.rm = TRUE)),
      as.character(sum(df_out$priority_level == "Moderate", na.rm = TRUE)),
      as.character(sum(df_out$priority_level == "Low", na.rm = TRUE)),
      as.character(sum(df_out$ai_profile_flag == "Well-classified AI record", na.rm = TRUE)),
      as.character(sum(df_out$ai_profile_flag == "Partially classified AI record", na.rm = TRUE)),
      as.character(sum(df_out$ai_profile_flag == "Weakly classified AI record", na.rm = TRUE)),
      as.character(sum(df_out$africa_relevance_level == "High", na.rm = TRUE)),
      as.character(sum(df_out$novelty_level == "High", na.rm = TRUE)),
      as.character(sum(df_out$africa_relevance_score > 0, na.rm = TRUE)),
      as.character(sum(df_out$novelty_score > 0, na.rm = TRUE)),
      as.character(sum(!is.na(df_out$source) & df_out$source != "", na.rm = TRUE)),
      as.character(sum(!is.na(df_out$source_name) & df_out$source_name != "", na.rm = TRUE)),
      as.character(dplyr::n_distinct(df_out$source[!is.na(df_out$source) & df_out$source != ""])),
      as.character(dplyr::n_distinct(df_out$source_name[!is.na(df_out$source_name) & df_out$source_name != ""])),
      as.character(sum(df_out$geo_present, na.rm = TRUE)),
      as.character(dplyr::n_distinct(df_out$rcc[!is.na(df_out$rcc)])),
      as.character(dplyr::n_distinct(df_out$iso3[!is.na(df_out$iso3)])),
      as.character(dplyr::n_distinct(df_out$country[!is.na(df_out$country)])),
      as.character(sum(df_out$publication_date_present, na.rm = TRUE)),
      as.character(sum(!is.na(df_out$date_fetched) & df_out$date_fetched != "", na.rm = TRUE))
    )
  )
  
  stopifnot(nrow(df_out) == nrow(df))
  stopifnot(all(!is.na(df_out$priority_level)))
  stopifnot(all(df_out$priority_score >= 0 & df_out$priority_score <= 100))
  
  message("[PREIS-AI] CHECK SCORES")
  message("  africa_relevance_score > 0: ", sum(df_out$africa_relevance_score > 0, na.rm = TRUE))
  message("  novelty_score > 0        : ", sum(df_out$novelty_score > 0, na.rm = TRUE))
  message("  high priority            : ", sum(df_out$priority_level == "High", na.rm = TRUE))
  message("  moderate priority        : ", sum(df_out$priority_level == "Moderate", na.rm = TRUE))
  message("  low priority             : ", sum(df_out$priority_level == "Low", na.rm = TRUE))
  
  readr::write_csv(df_out, out_file_enriched, na = "")
  readr::write_csv(df_out, out_file_dashboard, na = "")
  readr::write_csv(qa, qa_file, na = "")
  
  invisible(list(
    data = df_out,
    qa = qa,
    input_file = in_file,
    output_file_enriched = out_file_enriched,
    output_file_dashboard = out_file_dashboard,
    qa_file = qa_file
  ))
}

if (sys.nframe() == 0) {
  build_ai_tracker_final()
}