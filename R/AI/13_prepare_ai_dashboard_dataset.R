# ============================================================
# PREIS-AI — STEP 13
# Prepare AI dashboard dataset
# Corrected full version
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(lubridate)
})

prepare_ai_dashboard_dataset <- function(project_dir = "D:/PREIS_AI") {
  
  in_file <- file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final.csv")
  
  out_tracker_dashboard <- file.path(project_dir, "dashboard", "data", "dashboard", "ai_tracker.csv")
  out_articles_ai       <- file.path(project_dir, "dashboard", "data", "dashboard", "articles_ai.csv")
  out_articles_tagged   <- file.path(project_dir, "dashboard", "data", "dashboard", "articles_ai_tagged.csv")
  out_qc                <- file.path(project_dir, "data", "outputs", "ai", "ai_dashboard_qc.csv")
  
  if (!file.exists(in_file)) {
    stop("Required tracker file not found: ", in_file)
  }
  
  dir.create(dirname(out_tracker_dashboard), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_qc), recursive = TRUE, showWarnings = FALSE)
  
  df <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  
  if (nrow(df) == 0) {
    stop("Tracker input file is empty: ", in_file)
  }
  
  normalize_blank_to_na <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)
    x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN")] <- NA_character_
    x
  }
  
  normalize_geo_to_na <- function(x) {
    x <- normalize_blank_to_na(x)
    x[x %in% c("Unspecified", "Unknown", "Africa", "All", "UNK", "ALL", "no_geo", "NO_GEO")] <- NA_character_
    x
  }
  
  get_first_existing <- function(df, candidates, default = "") {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    out <- as.character(df[[hit[1]]])
    out[is.na(out)] <- default
    out
  }
  
  coalesce_existing_clean <- function(df, candidates, default = NA_character_) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    
    vals <- lapply(hit, function(nm) normalize_blank_to_na(df[[nm]]))
    out <- vals[[1]]
    
    if (length(vals) > 1) {
      for (i in 2:length(vals)) {
        out <- dplyr::coalesce(out, vals[[i]])
      }
    }
    
    out[is.na(out)] <- default
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
  
  parse_date_safe <- function(x) {
    x <- normalize_blank_to_na(x)
    
    if (length(x) == 0) {
      return(character(0))
    }
    
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
      
      parsed <- as.Date(x[idx], format = fmt)
      ok <- !is.na(parsed)
      
      if (any(ok)) {
        out[idx[ok]] <- as.character(parsed[ok])
      }
    }
    
    out
  }
  
  dash <- df %>%
    mutate(
      record_id   = get_first_existing(., c("record_id")),
      title       = get_first_existing(., c("title")),
      
      pub_year    = get_first_existing_int(., c("pub_year", "year", "publication_year", "publicationYear")),
      
      publication_date_raw = coalesce_existing_clean(., c("publication_date_raw")),
      publication_date = parse_date_safe(coalesce_existing_clean(., c("publication_date", "publication_date_raw"))),
      
      publication_date_present = if ("publication_date_present" %in% names(.)) {
        as.logical(publication_date_present)
      } else {
        !is.na(publication_date) & publication_date != ""
      },
      
      doi         = get_first_existing(., c("doi")),
      pmid        = get_first_existing(., c("pmid")),
      pmcid       = get_first_existing(., c("pmcid")),
      journal     = get_first_existing(., c("journal")),
      authors     = get_first_existing(., c("authors")),
      affiliation = get_first_existing(., c("affiliation")),
      
      country = normalize_geo_to_na(coalesce_existing_clean(., c("country"))),
      iso3    = normalize_geo_to_na(coalesce_existing_clean(., c("iso3"))),
      rcc     = normalize_geo_to_na(coalesce_existing_clean(., c("rcc"))),
      region_level = get_first_existing(., c("region_level")),
      
      source = normalize_blank_to_na(coalesce_existing_clean(., c("source"))),
      source_name_raw = normalize_blank_to_na(coalesce_existing_clean(., c("source_name"))),
      
      source_name = dplyr::case_when(
        !is.na(source_name_raw) & source_name_raw != "" ~ source_name_raw,
        source %in% c("PubMed", "pubmed") ~ "PubMed",
        source %in% c("PMC", "EuropePMC") ~ "EuropePMC",
        source %in% c("AGR", "arxiv", "arXiv", "ARXIV") ~ "arXiv",
        source %in% c("MED", "medrxiv", "medRxiv", "MEDRXIV") ~ "medRxiv",
        source %in% c("PPR", "biorxiv", "bioRxiv", "BIORXIV") ~ "bioRxiv",
        source %in% c("ETH", "ETH archive") ~ "ETH archive",
        !is.na(source) & source != "" ~ source,
        TRUE ~ NA_character_
      ),
      
      record_origin = normalize_blank_to_na(coalesce_existing_clean(., c("record_origin"))),
      server        = normalize_blank_to_na(coalesce_existing_clean(., c("server"))),
      source_query  = get_first_existing(., c("source_query", "query_scope")),
      query_id      = get_first_existing(., c("query_id", "query_string")),
      url           = normalize_blank_to_na(coalesce_existing_clean(., c("url"))),
      date_fetched  = parse_date_safe(coalesce_existing_clean(., c("date_fetched"))),
      
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
      
      africa_relevance_score = get_first_existing_numeric(., c("africa_relevance_score")),
      africa_relevance_level = get_first_existing(., c("africa_relevance_level")),
      africa_relevance_reason = get_first_existing(., c("africa_relevance_reason")),
      
      novelty_score = get_first_existing_numeric(., c("novelty_score")),
      novelty_level = get_first_existing(., c("novelty_level")),
      novelty_reason = get_first_existing(., c("novelty_reason")),
      
      ai_profile_flag = get_first_existing(., c("ai_profile_flag")),
      priority_score = get_first_existing_numeric(., c("priority_score")),
      priority_level = get_first_existing(., c("priority_level")),
      tracker_recommendation = get_first_existing(., c("tracker_recommendation")),
      
      source_present = if ("source_present" %in% names(.)) {
        as.logical(source_present)
      } else {
        ((!is.na(source) & source != "") | (!is.na(source_name) & source_name != ""))
      },
      
      geo_present = if ("geo_present" %in% names(.)) {
        as.logical(geo_present)
      } else {
        ((!is.na(country) & country != "") |
           (!is.na(iso3) & iso3 != "") |
           (!is.na(rcc) & rcc != ""))
      }
    ) %>%
    select(
      any_of(c(
        "record_id",
        "title",
        "pub_year",
        "publication_date",
        "publication_date_raw",
        "publication_date_present",
        "doi",
        "pmid",
        "pmcid",
        "journal",
        "authors",
        "affiliation",
        "country",
        "iso3",
        "rcc",
        "region_level",
        "source",
        "source_name",
        "record_origin",
        "server",
        "source_query",
        "query_id",
        "url",
        "date_fetched",
        "has_ai_signal",
        "has_nlp_surveillance",
        "primary_ai_method",
        "primary_surveillance_function",
        "primary_ai_usecase",
        "primary_data_type",
        "ai_surveillance_relevance",
        "ai_all_matches",
        "method_family_primary",
        "application_type_primary",
        "africa_relevance_score",
        "africa_relevance_level",
        "africa_relevance_reason",
        "novelty_score",
        "novelty_level",
        "novelty_reason",
        "ai_profile_flag",
        "priority_score",
        "priority_level",
        "tracker_recommendation",
        "source_present",
        "geo_present"
      )),
      everything()
    )
  
  articles_ai <- dash %>%
    transmute(
      record_id,
      title,
      pub_year,
      publication_date,
      journal,
      doi,
      pmid,
      pmcid,
      authors,
      country,
      iso3,
      rcc,
      source,
      source_name,
      record_origin,
      primary_ai_method,
      primary_surveillance_function,
      primary_ai_usecase,
      priority_score,
      priority_level,
      africa_relevance_level,
      novelty_level,
      url
    )
  
  articles_ai_tagged <- dash %>%
    transmute(
      record_id,
      title,
      pub_year,
      publication_date,
      publication_date_raw,
      publication_date_present,
      doi,
      pmid,
      pmcid,
      journal,
      authors,
      affiliation,
      country,
      iso3,
      rcc,
      region_level,
      source,
      source_name,
      record_origin,
      server,
      source_query,
      query_id,
      url,
      date_fetched,
      has_ai_signal,
      has_nlp_surveillance,
      primary_ai_method,
      primary_surveillance_function,
      primary_ai_usecase,
      primary_data_type,
      ai_surveillance_relevance,
      ai_all_matches,
      method_family_primary,
      application_type_primary,
      africa_relevance_score,
      africa_relevance_level,
      africa_relevance_reason,
      novelty_score,
      novelty_level,
      novelty_reason,
      ai_profile_flag,
      priority_score,
      priority_level,
      tracker_recommendation,
      source_present,
      geo_present
    )
  
  qc <- tibble(
    metric = c(
      "n_records",
      "n_high_priority",
      "n_moderate_priority",
      "n_low_priority",
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
      nrow(dash),
      sum(dash$priority_level == "High", na.rm = TRUE),
      sum(dash$priority_level == "Moderate", na.rm = TRUE),
      sum(dash$priority_level == "Low", na.rm = TRUE),
      sum(!is.na(dash$source) & dash$source != "", na.rm = TRUE),
      sum(!is.na(dash$source_name) & dash$source_name != "", na.rm = TRUE),
      dplyr::n_distinct(dash$source[!is.na(dash$source) & dash$source != ""]),
      dplyr::n_distinct(dash$source_name[!is.na(dash$source_name) & dash$source_name != ""]),
      sum(dash$geo_present, na.rm = TRUE),
      dplyr::n_distinct(dash$rcc[!is.na(dash$rcc) & dash$rcc != ""]),
      dplyr::n_distinct(dash$iso3[!is.na(dash$iso3) & dash$iso3 != ""]),
      dplyr::n_distinct(dash$country[!is.na(dash$country) & dash$country != ""]),
      sum(dash$publication_date_present, na.rm = TRUE),
      sum(!is.na(dash$date_fetched) & dash$date_fetched != "", na.rm = TRUE)
    )
  )
  
  stopifnot(nrow(dash) == nrow(df))
  stopifnot(nrow(articles_ai) == nrow(df))
  stopifnot(nrow(articles_ai_tagged) == nrow(df))
  
  readr::write_csv(dash, out_tracker_dashboard, na = "")
  readr::write_csv(articles_ai, out_articles_ai, na = "")
  readr::write_csv(articles_ai_tagged, out_articles_tagged, na = "")
  readr::write_csv(qc, out_qc, na = "")
  
  invisible(list(
    dashboard_tracker = dash,
    articles_ai = articles_ai,
    articles_ai_tagged = articles_ai_tagged,
    qc = qc,
    out_tracker_dashboard = out_tracker_dashboard,
    out_articles_ai = out_articles_ai,
    out_articles_tagged = out_articles_tagged,
    out_qc = out_qc
  ))
}

if (sys.nframe() == 0) {
  prepare_ai_dashboard_dataset()
}