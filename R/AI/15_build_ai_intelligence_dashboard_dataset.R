## ============================================================
## PREIS-AI — STEP 15
## AI Intelligence Dashboard Dataset
## Clean final dataset for dashboard / briefs / SITREP
## Corrected version: NO invented publication dates
## ============================================================

build_ai_intelligence_dashboard_dataset <- function(project_dir = "D:/PREIS_AI") {
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(tibble)
    library(lubridate)
  })
  
  message("[PREIS-AI] STEP 15 — building AI intelligence dashboard dataset...")
  
  in_file <- file.path(
    project_dir,
    "data", "enriched", "ai", "ai_tracker_final_geo.csv"
  )
  
  out_dir <- file.path(
    project_dir,
    "data", "dashboard"
  )
  
  qa_dir <- file.path(
    project_dir,
    "data", "outputs", "ai"
  )
  
  out_file <- file.path(out_dir, "ai_intelligence_dashboard_dataset.csv")
  qa_file  <- file.path(qa_dir, "ai_intelligence_dashboard_dataset_qc.csv")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(in_file)) {
    stop("Input file not found: ", in_file)
  }
  
  x <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  
  if (nrow(x) == 0) {
    stop("Input dataset is empty: ", in_file)
  }
  
  clean_txt <- function(v, default = "Unspecified") {
    v <- as.character(v)
    v[is.na(v)] <- ""
    v <- stringr::str_squish(v)
    v[v %in% c("", "NA", "NaN", "NULL", "null", "N/A", "n/a")] <- default
    v
  }
  
  clean_num <- function(v, default = NA_real_) {
    out <- suppressWarnings(as.numeric(v))
    out[is.na(out)] <- default
    out
  }
  
  pick_first <- function(df, candidates, default = "Unspecified") {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    clean_txt(df[[hit[1]]], default = default)
  }
  
  pick_first_num <- function(df, candidates, default = NA_real_) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    clean_num(df[[hit[1]]], default = default)
  }
  
  pick_first_raw <- function(df, candidates) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
    
    out <- rep(NA_character_, nrow(df))
    
    for (nm in hit) {
      v <- as.character(df[[nm]])
      v <- stringr::str_squish(v)
      v[v %in% c("", "NA", "NaN", "NULL", "null", "N/A", "n/a")] <- NA_character_
      
      fill_idx <- is.na(out) & !is.na(v)
      out[fill_idx] <- v[fill_idx]
    }
    
    out
  }
  
  parse_real_publication_date <- function(v) {
    v <- as.character(v)
    v <- stringr::str_squish(v)
    v[v %in% c("", "NA", "NaN", "NULL", "null", "N/A", "n/a")] <- NA_character_
    
    out <- rep(NA_character_, length(v))
    
    if (length(v) == 0) {
      return(out)
    }
    
    ## Reject year-only or year-month pseudo dates
    invalid_partial <- !is.na(v) & (
      grepl("^\\d{4}$", v) |
        grepl("^\\d{4}[-/]\\d{1,2}$", v)
    )
    
    vv <- v
    vv[invalid_partial] <- NA_character_
    
    parsed_dt <- suppressWarnings(
      lubridate::parse_date_time(
        vv,
        orders = c(
          "Y-m-d",
          "Y/m/d",
          "Ymd",
          "Y-m-d H:M:S",
          "Y/m/d H:M:S",
          "Y-m-d H:M",
          "Y/m/d H:M",
          "Y-m-dTH:M:S",
          "Y-m-dTH:M:Sz",
          "Y-m-dTH:M:SZ",
          "Y-m-dTH:M:SOS",
          "Y-m-dTH:M:OS",
          "d/m/Y",
          "m/d/Y",
          "d-m-Y",
          "m-d-Y",
          "d b Y",
          "d B Y",
          "b d Y",
          "B d Y"
        ),
        tz = "UTC",
        quiet = TRUE
      )
    )
    
    out <- as.character(as.Date(parsed_dt))
    out[out %in% c("NA", "")] <- NA_character_
    out
  }
  
  ## IMPORTANT:
  ## publication_date is accepted ONLY if it comes from an actual date field.
  ## pub_year is NOT used here to create a fake date.
  publication_date_raw <- pick_first_raw(
    x,
    c(
      "publication_date_raw",
      "first_publication_date",
      "epmc_first_publication_date",
      "publication_date_clean",
      "pub_date",
      "date",
      "article_date",
      "published",
      "online_date",
      "epubdate",
      "sortpubdate",
      "publication_date"
    )
  )
  
  publication_date_clean <- parse_real_publication_date(publication_date_raw)
  
  ## Guard against pseudo-dates inherited from upstream such as YYYY-01-01
  year_ref <- suppressWarnings(as.integer(pick_first(x, c("year", "pub_year", "publication_year", "year_txt"), default = NA_character_)))
  pub_date_year <- suppressWarnings(lubridate::year(as.Date(publication_date_clean)))
  
  pseudo_from_year <- !is.na(publication_date_clean) &
    !is.na(year_ref) &
    !is.na(pub_date_year) &
    pub_date_year == year_ref &
    grepl("-01-01$", publication_date_clean) &
    is.na(pick_first_raw(
      x,
      c(
        "publication_date_raw",
        "first_publication_date",
        "epmc_first_publication_date",
        "publication_date_clean",
        "pub_date",
        "date",
        "article_date",
        "published",
        "online_date",
        "epubdate",
        "sortpubdate"
      )
    ))
  
  publication_date_clean[pseudo_from_year] <- NA_character_
  
  x2 <- x %>%
    mutate(
      record_id = pick_first(., c("record_id"), default = ""),
      record_id = if_else(record_id == "", paste0("rec_", row_number()), record_id),
      
      title   = pick_first(., c("title"), default = "Untitled"),
      year    = pick_first(., c("year", "pub_year", "publication_year", "year_txt")),
      journal = pick_first(., c("journal")),
      doi     = pick_first(., c("doi")),
      pmid    = pick_first(., c("pmid")),
      pmcid   = pick_first(., c("pmcid")),
      url     = pick_first(., c("url", "link")),
      
      source_name  = pick_first(., c("source_name", "source")),
      article_type = pick_first(., c("article_type")),
      authors      = pick_first(., c("authors")),
      
      country = pick_first(., c("country", "country_txt", "country_norm")),
      iso3    = pick_first(., c("iso3", "iso3_txt", "iso3_norm")),
      rcc     = pick_first(., c("rcc", "rcc_txt", "rcc_norm")),
      
      publication_date = publication_date_clean,
      publication_date_present = !is.na(publication_date_clean) & publication_date_clean != "",
      
      method_family_primary    = pick_first(., c("method_family_primary")),
      application_type_primary = pick_first(., c("application_type_primary")),
      
      africa_relevance_level = pick_first(., c("africa_relevance_level")),
      novelty_level          = pick_first(., c("novelty_level")),
      priority_level         = pick_first(., c("priority_level")),
      tracker_recommendation = pick_first(., c("tracker_recommendation")),
      
      africa_relevance_score = pick_first_num(., c("africa_relevance_score"), default = NA_real_),
      novelty_score          = pick_first_num(., c("novelty_score"), default = NA_real_),
      priority_score         = pick_first_num(., c("priority_score"), default = NA_real_),
      
      predictive_signal  = pick_first_num(., c("predictive_signal"), default = 0),
      one_health_signal  = pick_first_num(., c("one_health_signal"), default = 0),
      digital_signal     = pick_first_num(., c("digital_signal"), default = 0),
      operational_signal = pick_first_num(., c("operational_signal"), default = 0),
      
      has_method_signal      = pick_first_num(., c("has_method_signal"), default = 0),
      has_application_signal = pick_first_num(., c("has_application_signal"), default = 0),
      
      cited_by_count = pick_first_num(., c("cited_by_count"), default = NA_real_)
    ) %>%
    mutate(
      year = clean_txt(year, default = "Unspecified"),
      
      method_family_primary    = clean_txt(method_family_primary, default = "Unspecified"),
      application_type_primary = clean_txt(application_type_primary, default = "Unspecified"),
      country                  = clean_txt(country, default = "Unspecified"),
      iso3                     = clean_txt(iso3, default = "Unspecified"),
      rcc                      = clean_txt(rcc, default = "Unspecified"),
      source_name              = clean_txt(source_name, default = "Unspecified"),
      article_type             = clean_txt(article_type, default = "Unspecified"),
      authors                  = clean_txt(authors, default = "Unspecified"),
      journal                  = clean_txt(journal, default = "Unspecified"),
      doi                      = clean_txt(doi, default = "Unspecified"),
      pmid                     = clean_txt(pmid, default = "Unspecified"),
      pmcid                    = clean_txt(pmcid, default = "Unspecified"),
      url                      = clean_txt(url, default = "Unspecified"),
      tracker_recommendation   = clean_txt(tracker_recommendation, default = "Unspecified"),
      
      africa_relevance_level = case_when(
        africa_relevance_level %in% c("High", "high") ~ "High",
        africa_relevance_level %in% c("Moderate", "moderate") ~ "Moderate",
        africa_relevance_level %in% c("Low", "low") ~ "Low",
        TRUE ~ "Unspecified"
      ),
      
      novelty_level = case_when(
        novelty_level %in% c("High", "high") ~ "High",
        novelty_level %in% c("Moderate", "moderate") ~ "Moderate",
        novelty_level %in% c("Low", "low") ~ "Low",
        TRUE ~ "Unspecified"
      ),
      
      priority_level = case_when(
        priority_level %in% c("High", "high") ~ "High",
        priority_level %in% c("Moderate", "moderate") ~ "Moderate",
        priority_level %in% c("Low", "low") ~ "Low",
        TRUE ~ "Unspecified"
      ),
      
      predictive_signal  = if_else(is.na(predictive_signal), 0, predictive_signal),
      one_health_signal  = if_else(is.na(one_health_signal), 0, one_health_signal),
      digital_signal     = if_else(is.na(digital_signal), 0, digital_signal),
      operational_signal = if_else(is.na(operational_signal), 0, operational_signal),
      
      africa_rank = case_when(
        africa_relevance_level == "High" ~ 3L,
        africa_relevance_level == "Moderate" ~ 2L,
        africa_relevance_level == "Low" ~ 1L,
        TRUE ~ 0L
      ),
      
      novelty_rank = case_when(
        novelty_level == "High" ~ 3L,
        novelty_level == "Moderate" ~ 2L,
        novelty_level == "Low" ~ 1L,
        TRUE ~ 0L
      ),
      
      priority_rank = case_when(
        priority_level == "High" ~ 3L,
        priority_level == "Moderate" ~ 2L,
        priority_level == "Low" ~ 1L,
        TRUE ~ 0L
      ),
      
      geography_status = case_when(
        country != "Unspecified" ~ "Country detected",
        rcc != "Unspecified" ~ "RCC detected only",
        TRUE ~ "No African geography detected"
      ),
      
      signal_profile = case_when(
        predictive_signal == 1 & one_health_signal == 1 & digital_signal == 1 ~ "Predictive + One Health + Digital",
        predictive_signal == 1 & one_health_signal == 1 ~ "Predictive + One Health",
        predictive_signal == 1 & digital_signal == 1 ~ "Predictive + Digital",
        one_health_signal == 1 & digital_signal == 1 ~ "One Health + Digital",
        predictive_signal == 1 ~ "Predictive",
        one_health_signal == 1 ~ "One Health",
        digital_signal == 1 ~ "Digital",
        operational_signal == 1 ~ "Operational",
        TRUE ~ "General AI surveillance"
      ),
      
      dashboard_bucket = case_when(
        priority_level == "High" & country != "Unspecified" ~ "High priority Africa-linked",
        priority_level == "High" ~ "High priority global",
        priority_level == "Moderate" & country != "Unspecified" ~ "Moderate priority Africa-linked",
        priority_level == "Moderate" ~ "Moderate priority global",
        TRUE ~ "Background tracking"
      )
    ) %>%
    select(
      record_id,
      title,
      year,
      publication_date,
      publication_date_present,
      journal,
      authors,
      article_type,
      source_name,
      doi,
      pmid,
      pmcid,
      url,
      country,
      iso3,
      rcc,
      geography_status,
      method_family_primary,
      application_type_primary,
      signal_profile,
      predictive_signal,
      one_health_signal,
      digital_signal,
      operational_signal,
      africa_relevance_score,
      africa_relevance_level,
      novelty_score,
      novelty_level,
      priority_score,
      priority_level,
      africa_rank,
      novelty_rank,
      priority_rank,
      dashboard_bucket,
      tracker_recommendation,
      cited_by_count
    ) %>%
    arrange(
      desc(priority_rank),
      desc(priority_score),
      desc(novelty_rank),
      desc(africa_rank),
      desc(novelty_score)
    )
  
  qc <- tibble(
    metric = c(
      "n_records",
      "n_high_priority",
      "n_moderate_priority",
      "n_low_priority",
      "n_country_detected",
      "n_rcc_detected",
      "n_predictive",
      "n_one_health",
      "n_digital",
      "n_operational",
      "n_methods",
      "n_applications",
      "n_with_publication_date"
    ),
    value = c(
      nrow(x2),
      sum(x2$priority_level == "High", na.rm = TRUE),
      sum(x2$priority_level == "Moderate", na.rm = TRUE),
      sum(x2$priority_level == "Low", na.rm = TRUE),
      sum(x2$country != "Unspecified", na.rm = TRUE),
      sum(x2$rcc != "Unspecified", na.rm = TRUE),
      sum(x2$predictive_signal == 1, na.rm = TRUE),
      sum(x2$one_health_signal == 1, na.rm = TRUE),
      sum(x2$digital_signal == 1, na.rm = TRUE),
      sum(x2$operational_signal == 1, na.rm = TRUE),
      dplyr::n_distinct(x2$method_family_primary),
      dplyr::n_distinct(x2$application_type_primary),
      sum(!is.na(x2$publication_date) & x2$publication_date != "", na.rm = TRUE)
    )
  )
  
  stopifnot(nrow(x2) == nrow(x))
  stopifnot(all(!is.na(x2$priority_level)))
  stopifnot(all(!is.na(x2$dashboard_bucket)))
  
  readr::write_csv(x2, out_file, na = "")
  readr::write_csv(qc, qa_file, na = "")
  
  message("[PREIS-AI] STEP 15 completed.")
  print(qc)
  
  invisible(list(
    data = x2,
    qc = qc,
    output_file = out_file,
    qc_file = qa_file
  ))
}