build_ai_alert_engine <- function(project_dir = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Building AI alert engine...")
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(tibble)
    library(lubridate)
  })
  
  ## ----------------------------------------------------------
  ## 1. Detect best input file
  ## ----------------------------------------------------------
  candidate_files <- c(
    file.path(project_dir, "data", "dashboard", "ai_intelligence_dashboard_dataset.csv"),
    file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final_geo.csv"),
    file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final.csv"),
    file.path(project_dir, "dashboard", "data", "dashboard", "articles_ai_tagged.csv"),
    file.path(project_dir, "dashboard", "data", "dashboard", "ai_tracker.csv")
  )
  
  in_file <- candidate_files[file.exists(candidate_files)][1]
  
  if (is.na(in_file) || length(in_file) == 0) {
    stop("No suitable AI dataset found.")
  }
  
  message("[PREIS-AI] Using dataset: ", in_file)
  
  ## ----------------------------------------------------------
  ## 2. Output paths
  ## ----------------------------------------------------------
  out_dir <- file.path(project_dir, "data", "outputs", "ai", "alerts")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  alerts_file <- file.path(out_dir, "ai_alerts.csv")
  qa_file     <- file.path(out_dir, "ai_alerts_qc.csv")
  summary_file <- file.path(out_dir, paste0("AI_ALERT_SUMMARY_", Sys.Date(), ".txt"))
  
  ## ----------------------------------------------------------
  ## 3. Read data
  ## ----------------------------------------------------------
  df <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  
  if (nrow(df) == 0) stop("Input dataset is empty: ", in_file)
  
  ## ----------------------------------------------------------
  ## 4. Helpers
  ## ----------------------------------------------------------
  clean_txt <- function(x, default = "Unspecified") {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringr::str_squish(x)
    x[x == ""] <- default
    x
  }
  
  pick_first <- function(df, candidates, default = "Unspecified") {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    clean_txt(df[[hit[1]]], default = default)
  }
  
  pick_first_num <- function(df, candidates, default = 0) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    out <- suppressWarnings(as.numeric(df[[hit[1]]]))
    out[is.na(out)] <- default
    out
  }
  
  pick_first_date <- function(df, candidates) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(as.Date(rep(NA, nrow(df))))
    out <- suppressWarnings(as.Date(df[[hit[1]]]))
    out
  }
  
  ## ----------------------------------------------------------
  ## 5. Standardize fields
  ## ----------------------------------------------------------
  x <- df %>%
    mutate(
      record_id = pick_first(., c("record_id"), default = ""),
      record_id = if_else(record_id == "", paste0("rec_", row_number()), record_id),
      
      title = pick_first(., c("title"), default = "Untitled"),
      year = pick_first_num(., c("year", "pub_year", "publication_year"), default = NA_real_),
      country = pick_first(., c("country"), default = "Unspecified"),
      iso3 = toupper(pick_first(., c("iso3"), default = "Unspecified")),
      rcc = pick_first(., c("rcc"), default = "Unspecified"),
      
      method_family_primary = pick_first(., c("method_family_primary"), default = "Unspecified"),
      application_type_primary = pick_first(., c("application_type_primary"), default = "Unspecified"),
      
      africa_relevance_level = pick_first(., c("africa_relevance_level"), default = "Unspecified"),
      novelty_level = pick_first(., c("novelty_level"), default = "Unspecified"),
      priority_level = pick_first(., c("priority_level"), default = "Unspecified"),
      
      africa_relevance_score = pick_first_num(., c("africa_relevance_score"), 0),
      novelty_score = pick_first_num(., c("novelty_score"), 0),
      priority_score = pick_first_num(., c("priority_score"), 0),
      
      predictive_signal = pick_first_num(., c("predictive_signal"), 0),
      one_health_signal = pick_first_num(., c("one_health_signal"), 0),
      digital_signal = pick_first_num(., c("digital_signal"), 0),
      operational_signal = pick_first_num(., c("operational_signal"), 0),
      
      journal = pick_first(., c("journal"), default = "Unspecified"),
      doi = pick_first(., c("doi"), default = "Unspecified"),
      pmid = pick_first(., c("pmid"), default = "Unspecified"),
      url = pick_first(., c("url", "link"), default = "Unspecified"),
      
      pub_date = pick_first_date(., c("publication_date", "pub_date", "date", "first_publication_date", "epmc_first_publication_date"))
    ) %>%
    mutate(
      year = if_else(is.na(year), NA_real_, as.numeric(year))
    )
  
  ## ----------------------------------------------------------
  ## 6. Recency logic
  ## ----------------------------------------------------------
  today <- Sys.Date()
  current_year <- year(today)
  month_start <- floor_date(today, "month")
  
  x <- x %>%
    mutate(
      is_new_this_month = !is.na(pub_date) & pub_date >= month_start,
      is_new_this_year = !is.na(year) & year == current_year
    )
  
  ## fallback if no real dates
  has_month_dates <- any(x$is_new_this_month, na.rm = TRUE)
  
  x <- x %>%
    mutate(
      recency_flag = case_when(
        has_month_dates & is_new_this_month ~ "New this month",
        !has_month_dates & is_new_this_year ~ "New this year",
        TRUE ~ "Not recent"
      )
    )
  
  ## ----------------------------------------------------------
  ## 7. Build alert rules
  ## ----------------------------------------------------------
  alerts <- x %>%
    mutate(
      rule_high_priority = priority_level == "High",
      rule_africa_priority = priority_level %in% c("High", "Moderate") & country != "Unspecified",
      rule_predictive_high = predictive_signal == 1 & priority_level == "High",
      rule_digital_high = digital_signal == 1 & priority_level == "High",
      rule_one_health_high = one_health_signal == 1 & priority_level == "High",
      rule_novel_high = novelty_level == "High",
      rule_new_record = recency_flag %in% c("New this month", "New this year"),
      rule_operational_high = operational_signal == 1 & priority_level == "High"
    ) %>%
    mutate(
      alert_type = case_when(
        rule_high_priority & rule_africa_priority & rule_predictive_high ~ "High priority Africa predictive AI",
        rule_high_priority & rule_africa_priority & rule_digital_high ~ "High priority Africa digital AI",
        rule_high_priority & rule_africa_priority & rule_one_health_high ~ "High priority Africa One Health AI",
        rule_high_priority & rule_africa_priority ~ "High priority Africa-linked AI",
        rule_predictive_high ~ "High priority predictive AI",
        rule_digital_high ~ "High priority digital surveillance AI",
        rule_one_health_high ~ "High priority One Health AI",
        rule_operational_high ~ "High priority operational AI",
        rule_novel_high ~ "Novel AI record",
        rule_new_record & priority_level == "High" ~ "New high priority AI record",
        rule_new_record & country != "Unspecified" ~ "New Africa-linked AI record",
        rule_new_record ~ "New AI record",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(alert_type)) %>%
    mutate(
      alert_date = as.character(today),
      alert_id = paste0(
        "AIALERT_",
        row_number(), "_",
        gsub("[^A-Za-z0-9]+", "_", substr(title, 1, 40))
      ),
      alert_priority = case_when(
        str_detect(alert_type, "High priority Africa") ~ "Critical",
        str_detect(alert_type, "High priority") ~ "High",
        str_detect(alert_type, "Novel") ~ "Moderate",
        str_detect(alert_type, "New") ~ "Moderate",
        TRUE ~ "Low"
      ),
      alert_summary = paste0(
        alert_type, " | ",
        "Method: ", method_family_primary, " | ",
        "Application: ", application_type_primary, " | ",
        "Country: ", country, " | ",
        "RCC: ", rcc, " | ",
        "Priority: ", priority_level, " | ",
        "Novelty: ", novelty_level
      )
    ) %>%
    select(
      alert_id,
      alert_date,
      alert_priority,
      alert_type,
      title,
      year,
      country,
      iso3,
      rcc,
      method_family_primary,
      application_type_primary,
      priority_level,
      priority_score,
      novelty_level,
      novelty_score,
      africa_relevance_level,
      africa_relevance_score,
      predictive_signal,
      one_health_signal,
      digital_signal,
      operational_signal,
      recency_flag,
      alert_summary,
      journal,
      doi,
      pmid,
      url
    ) %>%
    distinct()
  
  ## ----------------------------------------------------------
  ## 8. QA
  ## ----------------------------------------------------------
  qa <- tibble(
    metric = c(
      "n_input_records",
      "n_alerts_total",
      "n_critical_alerts",
      "n_high_alerts",
      "n_moderate_alerts",
      "n_africa_alerts",
      "n_predictive_alerts",
      "n_digital_alerts",
      "n_one_health_alerts",
      "n_novel_alerts",
      "n_new_alerts"
    ),
    value = c(
      nrow(x),
      nrow(alerts),
      sum(alerts$alert_priority == "Critical", na.rm = TRUE),
      sum(alerts$alert_priority == "High", na.rm = TRUE),
      sum(alerts$alert_priority == "Moderate", na.rm = TRUE),
      sum(str_detect(alerts$alert_type, "Africa"), na.rm = TRUE),
      sum(str_detect(alerts$alert_type, "predictive"), na.rm = TRUE),
      sum(str_detect(alerts$alert_type, "digital"), na.rm = TRUE),
      sum(str_detect(alerts$alert_type, "One Health"), na.rm = TRUE),
      sum(str_detect(alerts$alert_type, "Novel"), na.rm = TRUE),
      sum(str_detect(alerts$alert_type, "New"), na.rm = TRUE)
    )
  )
  
  ## ----------------------------------------------------------
  ## 9. Summary text
  ## ----------------------------------------------------------
  top_alerts <- alerts %>%
    arrange(desc(alert_priority), desc(priority_score), desc(novelty_score)) %>%
    slice_head(n = 10)
  
  summary_lines <- c(
    "PREIS-AI ALERT SUMMARY",
    "======================",
    paste("Generated:", today),
    paste("Input dataset:", in_file),
    "",
    "KEY METRICS",
    paste(qa$metric, qa$value, sep = ": "),
    "",
    "TOP ALERTS",
    if (nrow(top_alerts) == 0) "No alerts generated." else
      paste0(
        seq_len(nrow(top_alerts)), ". ",
        top_alerts$alert_type, " | ",
        top_alerts$title, " | ",
        top_alerts$country, " | ",
        top_alerts$method_family_primary, " | ",
        top_alerts$application_type_primary
      )
  )
  
  ## ----------------------------------------------------------
  ## 10. Write outputs
  ## ----------------------------------------------------------
  readr::write_csv(alerts, alerts_file, na = "")
  readr::write_csv(qa, qa_file, na = "")
  writeLines(summary_lines, summary_file)
  
  message("[PREIS-AI] Alert engine completed.")
  message("[PREIS-AI] Alerts file: ", alerts_file)
  
  invisible(list(
    alerts = alerts,
    qa = qa,
    alerts_file = alerts_file,
    qa_file = qa_file,
    summary_file = summary_file
  ))
}