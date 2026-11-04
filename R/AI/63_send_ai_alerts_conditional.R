# ============================================================
# PREIS-AI — CONDITIONAL ALERT SENDER
# File: D:/PREIS_AI/R/AI/63_send_ai_alerts_conditional.R
# Fully corrected version
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
})


email_file <- file.path(dirname(normalizePath(sys.frame(1)$ofile)), "60_email.R")
if (file.exists(email_file)) source(email_file)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------
# Basic helpers
# ------------------------------------------------------------
safe_read_csv <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(tibble())
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message("Failed to read file: ", path, " | ", conditionMessage(e))
      tibble()
    }
  )
}

safe_nrow <- function(x) {
  if (is.null(x) || !is.data.frame(x)) return(0L)
  nrow(x)
}

normalize_text <- function(x, default = "") {
  x <- as.character(x)
  x[is.na(x)] <- default
  trimws(x)
}

normalize_blank_na <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  x
}

normalize_scope <- function(x) {
  x <- normalize_text(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- "All"
  x
}

normalize_email <- function(x) {
  tolower(normalize_text(x))
}

collapse_ws <- function(x) {
  x <- normalize_text(x)
  gsub("\\s+", " ", x)
}

make_title_key <- function(x) {
  x <- tolower(collapse_ws(x))
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

safe_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  
  out <- suppressWarnings(as.Date(x))
  
  idx <- is.na(out) & !is.na(x)
  if (any(idx)) out[idx] <- suppressWarnings(as.Date(substr(x[idx], 1, 10)))
  
  idx <- is.na(out) & !is.na(x)
  if (any(idx)) out[idx] <- suppressWarnings(as.Date(x[idx], format = "%Y/%m/%d"))
  
  idx <- is.na(out) & !is.na(x)
  if (any(idx)) out[idx] <- suppressWarnings(as.Date(x[idx], format = "%d/%m/%Y"))
  
  out
}

coalesce_chr <- function(...) {
  vals <- list(...)
  vals <- lapply(vals, as.character)
  out <- vals[[1]]
  if (length(vals) > 1) {
    for (i in 2:length(vals)) {
      out <- dplyr::coalesce(out, vals[[i]])
    }
  }
  out
}

safe_select_first <- function(df, candidates, default = NA_character_) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(default, nrow(df)))
  as.character(df[[hit[1]]])
}

file_date_safe <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(as.Date(NA))
  as.Date(file.info(path)$mtime)
}

first_existing_file <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  for (p in paths) {
    if (file.exists(p)) return(p)
  }
  NULL
}

latest_existing_file <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(NULL)
  info <- file.info(paths)
  paths[which.max(info$mtime)]
}

# ------------------------------------------------------------
# Sent log
# ------------------------------------------------------------
empty_sent_log_tbl <- function() {
  tibble(
    date_sent = character(),
    datetime_sent = character(),
    recipient_name = character(),
    recipient_email = character(),
    recipient_rcc = character(),
    scope = character(),
    subject = character(),
    n_alerts = numeric(),
    status = character(),
    alert_id = character()
  )
}

ensure_sent_log <- function(sent_log_file) {
  dir.create(dirname(sent_log_file), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(sent_log_file)) {
    readr::write_csv(empty_sent_log_tbl(), sent_log_file)
  }
  invisible(sent_log_file)
}

read_sent_log_clean <- function(sent_log_file) {
  ensure_sent_log(sent_log_file)
  
  old <- safe_read_csv(sent_log_file)
  needed <- names(empty_sent_log_tbl())
  
  if (nrow(old) == 0) return(empty_sent_log_tbl())
  
  for (nm in setdiff(needed, names(old))) {
    old[[nm]] <- NA
  }
  
  old %>%
    mutate(
      date_sent = as.character(date_sent),
      datetime_sent = as.character(datetime_sent),
      recipient_name = as.character(recipient_name),
      recipient_email = normalize_email(recipient_email),
      recipient_rcc = as.character(recipient_rcc),
      scope = normalize_scope(scope),
      subject = as.character(subject),
      n_alerts = suppressWarnings(as.numeric(n_alerts)),
      status = as.character(status),
      alert_id = normalize_text(alert_id)
    ) %>%
    select(all_of(needed))
}

append_sent_log <- function(sent_log_file, rows_df) {
  old <- read_sent_log_clean(sent_log_file)
  
  if (is.null(rows_df) || nrow(rows_df) == 0) {
    return(invisible(old))
  }
  
  needed <- names(empty_sent_log_tbl())
  for (nm in setdiff(needed, names(rows_df))) {
    rows_df[[nm]] <- NA
  }
  
  rows_df <- rows_df %>%
    mutate(
      date_sent = as.character(date_sent),
      datetime_sent = as.character(datetime_sent),
      recipient_name = as.character(recipient_name),
      recipient_email = normalize_email(recipient_email),
      recipient_rcc = as.character(recipient_rcc),
      scope = normalize_scope(scope),
      subject = as.character(subject),
      n_alerts = suppressWarnings(as.numeric(n_alerts)),
      status = as.character(status),
      alert_id = normalize_text(alert_id)
    ) %>%
    select(all_of(needed))
  
  out <- bind_rows(old, rows_df)
  readr::write_csv(out, sent_log_file)
  invisible(out)
}

get_last_successful_send_date <- function(sent_log) {
  if (safe_nrow(sent_log) == 0) return(as.Date("1900-01-01"))
  
  out <- sent_log %>%
    mutate(
      status_l = tolower(normalize_text(status)),
      date_sent_d = safe_parse_date(date_sent)
    ) %>%
    filter(!is.na(date_sent_d)) %>%
    filter(status_l == "" | str_detect(status_l, "^sent$"))
  
  if (nrow(out) == 0) return(as.Date("1900-01-01"))
  max(out$date_sent_d, na.rm = TRUE)
}

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
get_preis_ai_paths <- function(root = "D:/PREIS_AI") {
  list(
    root = root,
    dashboard_dir = file.path(root, "dashboard", "data", "dashboard"),
    
    # Alerts: prioritize active dashboard files first
    alerts_file = first_existing_file(c(
      file.path(root, "dashboard", "data", "dashboard", "alerts_ai.csv"),
      file.path(root, "dashboard", "data", "dashboard", "alerts_ai_surveillance.csv"),
      file.path(root, "dashboard", "data", "dashboard", "alerts.csv"),
      file.path(root, "data", "outputs", "ai", "alerts_ai.csv"),
      file.path(root, "data", "outputs", "ai", "alerts.csv"),
      file.path(root, "data", "outputs", "ai", "alerts", "ai_alerts.csv")
    )),
    
    # Articles
    articles_file = first_existing_file(c(
      file.path(root, "dashboard", "data", "dashboard", "articles_ai.csv"),
      file.path(root, "dashboard", "data", "dashboard", "articles_ai_tagged.csv"),
      file.path(root, "data", "outputs", "ai", "articles_ai.csv"),
      file.path(root, "data", "outputs", "ai", "articles_ai_tagged.csv"),
      file.path(root, "data", "enriched", "ai", "articles_ai_tagged.csv")
    )),
    
    # Recipients
    recipients_file = first_existing_file(c(
      file.path(root, "config", "alert_recipients_ai.csv"),
      file.path(root, "dashboard", "data", "dashboard", "alert_recipients_ai.csv"),
      file.path(root, "dashboard", "data", "dashboard", "alert_recipients.csv"),
      file.path(root, "config", "alert_recipients.csv")
    )),
    
    sent_log_file = file.path(root, "dashboard", "data", "dashboard", "sent_log_ai.csv"),
    
    preview_alerts_file   = file.path(root, "dashboard", "data", "dashboard", "alerts_ai_to_send_1.csv"),
    preview_articles_file = file.path(root, "dashboard", "data", "dashboard", "articles_ai_to_send_1.csv"),
    qc_file               = file.path(root, "dashboard", "data", "dashboard", "ai_email_qc.csv")
  )
}

# ------------------------------------------------------------
# Harmonization
# ------------------------------------------------------------
harmonize_alerts <- function(alerts) {
  if (safe_nrow(alerts) == 0) return(tibble())
  
  out <- alerts %>%
    mutate(
      title = safe_select_first(., c("title")),
      country = coalesce_chr(
        normalize_blank_na(safe_select_first(., c("country", "country_name", "member_state"))),
        rep("Unspecified", nrow(.))
      ),
      iso3 = normalize_blank_na(safe_select_first(., c("iso3", "iso_code"))),
      rcc = normalize_scope(safe_select_first(., c("rcc", "scope", "recipient_rcc"), "All")),
      pub_year = suppressWarnings(as.numeric(safe_select_first(., c("pub_year", "year")))),
      doi = normalize_blank_na(safe_select_first(., c("doi"))),
      pmid = normalize_blank_na(safe_select_first(., c("pmid"))),
      url = safe_select_first(., c("url", "link")),
      primary_ai_method = safe_select_first(., c("primary_ai_method", "method_family_primary")),
      primary_surveillance_function = safe_select_first(., c("primary_surveillance_function", "application_type_primary")),
      primary_ai_usecase = safe_select_first(., c("primary_ai_usecase", "disease_primary")),
      ai_surveillance_relevance = safe_select_first(., c("priority_level", "alert_level", "ai_surveillance_relevance")),
      alert_rule = safe_select_first(., c("alert_rule", "tracker_recommendation")),
      date_detected = safe_parse_date(
        safe_select_first(
          .,
          c(
            "date_detected",
            "detection_date",
            "generated_date",
            "date_published",
            "publication_date",
            "first_publication_date"
          )
        )
      ),
      title_key = coalesce_chr(
        normalize_blank_na(safe_select_first(., c("title_key"))),
        make_title_key(title)
      )
    )
  
  alert_id_col <- intersect(c("alert_id", "id", "record_id"), names(out))
  
  if (length(alert_id_col) > 0) {
    out$alert_id <- normalize_blank_na(as.character(out[[alert_id_col[1]]]))
  } else {
    out$alert_id <- NA_character_
  }
  
  out %>%
    mutate(
      alert_id = coalesce_chr(alert_id, doi, pmid, title_key, make_title_key(title)),
      date_detected = dplyr::coalesce(date_detected, as.Date(generated_date %||% Sys.Date())),
      country = normalize_text(country, "Unspecified"),
      rcc = normalize_scope(rcc),
      primary_ai_method = normalize_text(primary_ai_method, "Unspecified"),
      primary_surveillance_function = normalize_text(primary_surveillance_function, "Unspecified"),
      primary_ai_usecase = normalize_text(primary_ai_usecase, "Unspecified"),
      ai_surveillance_relevance = normalize_text(ai_surveillance_relevance, "Moderate")
    ) %>%
    select(
      alert_id, title, country, iso3, rcc, pub_year, doi, pmid, url,
      primary_ai_method, primary_surveillance_function, primary_ai_usecase,
      ai_surveillance_relevance, alert_rule, date_detected, title_key
    ) %>%
    filter(!is.na(alert_id), alert_id != "") %>%
    distinct(alert_id, .keep_all = TRUE)
}

harmonize_articles <- function(articles) {
  if (safe_nrow(articles) == 0) return(tibble())
  
  articles %>%
    mutate(
      title = safe_select_first(., c("title")),
      country = coalesce_chr(safe_select_first(., c("country")), rep("Unspecified", nrow(.))),
      iso3 = safe_select_first(., c("iso3")),
      rcc = normalize_scope(safe_select_first(., c("rcc", "scope"), "All")),
      doi = normalize_blank_na(safe_select_first(., c("doi"))),
      pmid = normalize_blank_na(safe_select_first(., c("pmid"))),
      url = safe_select_first(., c("url", "link")),
      pub_year = suppressWarnings(as.numeric(safe_select_first(., c("pub_year", "year")))),
      first_publication_date = safe_parse_date(safe_select_first(., c("first_publication_date", "publication_date"))),
      primary_ai_method = safe_select_first(., c("primary_ai_method", "method_family_primary")),
      primary_surveillance_function = safe_select_first(., c("primary_surveillance_function", "application_type_primary")),
      primary_ai_usecase = safe_select_first(., c("primary_ai_usecase")),
      ai_surveillance_relevance = safe_select_first(., c("ai_surveillance_relevance", "priority_level")),
      title_key = coalesce_chr(
        normalize_blank_na(safe_select_first(., c("title_key"))),
        make_title_key(title)
      )
    ) %>%
    select(
      title, country, iso3, rcc, doi, pmid, url, pub_year,
      first_publication_date, primary_ai_method,
      primary_surveillance_function, primary_ai_usecase,
      ai_surveillance_relevance, title_key
    ) %>%
    distinct()
}

harmonize_alerts <- function(alerts) {
  if (safe_nrow(alerts) == 0) return(tibble())
  
  out <- alerts %>%
    mutate(
      title = safe_select_first(., c("title")),
      country = coalesce_chr(
        normalize_blank_na(safe_select_first(., c("country"))),
        rep("Unspecified", nrow(.))
      ),
      iso3 = normalize_blank_na(safe_select_first(., c("iso3"))),
      rcc = normalize_scope(safe_select_first(., c("rcc", "scope"), "All")),
      pub_year = suppressWarnings(as.numeric(safe_select_first(., c("pub_year", "year")))),
      doi = normalize_blank_na(safe_select_first(., c("doi"))),
      pmid = normalize_blank_na(safe_select_first(., c("pmid"))),
      url = safe_select_first(., c("url", "link")),
      primary_ai_method = safe_select_first(., c("primary_ai_method", "method_family_primary")),
      primary_surveillance_function = safe_select_first(., c("primary_surveillance_function", "application_type_primary")),
      primary_ai_usecase = safe_select_first(., c("primary_ai_usecase")),
      ai_surveillance_relevance = safe_select_first(., c("ai_surveillance_relevance", "priority_level")),
      alert_rule = safe_select_first(., c("alert_rule")),
      date_detected = safe_parse_date(safe_select_first(., c("date_detected", "detection_date", "date_created", "date_sent"))),
      title_key = coalesce_chr(
        normalize_blank_na(safe_select_first(., c("title_key"))),
        make_title_key(title)
      )
    )
  
  alert_id_col <- intersect(c("alert_id", "id"), names(out))
  if (length(alert_id_col) > 0) {
    out$alert_id <- normalize_blank_na(as.character(out[[alert_id_col[1]]]))
  } else {
    out$alert_id <- NA_character_
  }
  
  out %>%
    mutate(
      alert_id = coalesce_chr(
        alert_id,
        doi,
        pmid,
        title_key,
        make_title_key(title)
      ),
      country = normalize_text(country, "Unspecified"),
      primary_ai_method = normalize_text(primary_ai_method, "Unspecified"),
      primary_surveillance_function = normalize_text(primary_surveillance_function, "Unspecified"),
      primary_ai_usecase = normalize_text(primary_ai_usecase, "Unspecified"),
      ai_surveillance_relevance = normalize_text(ai_surveillance_relevance, "Moderate")
    ) %>%
    select(
      alert_id, title, country, iso3, rcc, pub_year, doi, pmid, url,
      primary_ai_method, primary_surveillance_function, primary_ai_usecase,
      ai_surveillance_relevance, alert_rule, date_detected, title_key
    ) %>%
    filter(!is.na(alert_id), alert_id != "") %>%
    filter(!is.na(date_detected)) %>%
    distinct(alert_id, .keep_all = TRUE)
}

join_alerts_articles <- function(alerts, articles) {
  if (safe_nrow(alerts) == 0) return(alerts)
  if (safe_nrow(articles) == 0) return(alerts)
  
  art_doi <- articles %>% filter(!is.na(doi), doi != "") %>% distinct(doi, .keep_all = TRUE)
  art_pmid <- articles %>% filter(!is.na(pmid), pmid != "") %>% distinct(pmid, .keep_all = TRUE)
  art_title <- articles %>% filter(!is.na(title_key), title_key != "") %>% distinct(title_key, .keep_all = TRUE)
  
  alerts %>%
    left_join(art_doi %>% rename_with(~ paste0(.x, "_art_doi"), -doi), by = "doi") %>%
    left_join(art_pmid %>% rename_with(~ paste0(.x, "_art_pmid"), -pmid), by = "pmid") %>%
    left_join(art_title %>% rename_with(~ paste0(.x, "_art_title"), -title_key), by = "title_key") %>%
    mutate(
      title = coalesce_chr(title, title_art_doi, title_art_pmid, title_art_title),
      country = coalesce_chr(country, country_art_doi, country_art_pmid, country_art_title),
      iso3 = coalesce_chr(iso3, iso3_art_doi, iso3_art_pmid, iso3_art_title),
      rcc = normalize_scope(coalesce_chr(rcc, rcc_art_doi, rcc_art_pmid, rcc_art_title)),
      url = coalesce_chr(url, url_art_doi, url_art_pmid, url_art_title),
      pub_year = suppressWarnings(as.numeric(coalesce_chr(
        as.character(pub_year),
        as.character(pub_year_art_doi),
        as.character(pub_year_art_pmid),
        as.character(pub_year_art_title)
      ))),
      primary_ai_method = coalesce_chr(
        primary_ai_method, primary_ai_method_art_doi, primary_ai_method_art_pmid, primary_ai_method_art_title
      ),
      primary_surveillance_function = coalesce_chr(
        primary_surveillance_function,
        primary_surveillance_function_art_doi,
        primary_surveillance_function_art_pmid,
        primary_surveillance_function_art_title
      ),
      primary_ai_usecase = coalesce_chr(
        primary_ai_usecase, primary_ai_usecase_art_doi, primary_ai_usecase_art_pmid, primary_ai_usecase_art_title
      ),
      ai_surveillance_relevance = coalesce_chr(
        ai_surveillance_relevance,
        ai_surveillance_relevance_art_doi,
        ai_surveillance_relevance_art_pmid,
        ai_surveillance_relevance_art_title
      )
    ) %>%
    select(
      alert_id, title, country, iso3, rcc, pub_year, doi, pmid, url,
      primary_ai_method, primary_surveillance_function, primary_ai_usecase,
      ai_surveillance_relevance, alert_rule, date_detected, title_key
    )
}

classify_priority <- function(x) {
  y <- tolower(normalize_text(x))
  case_when(
    str_detect(y, "critical") ~ "Critical",
    str_detect(y, "high") ~ "High",
    str_detect(y, "moderate") ~ "Moderate",
    str_detect(y, "low") ~ "Low",
    TRUE ~ "Moderate"
  )
}

# ------------------------------------------------------------
# Load all
# ------------------------------------------------------------
load_ai_email_sender_data <- function(root = "D:/PREIS_AI") {
  p <- get_preis_ai_paths(root)
  
  alerts_raw <- safe_read_csv(p$alerts_file)
  articles_raw <- safe_read_csv(p$articles_file)
  recipients_raw <- safe_read_csv(p$recipients_file)
  sent_log <- read_sent_log_clean(p$sent_log_file)
  
  alerts <- harmonize_alerts(alerts_raw)
  articles <- harmonize_articles(articles_raw)
  alerts <- join_alerts_articles(alerts, articles)
  recipients <- harmonize_recipients(recipients_raw)
  
  list(
    paths = p,
    alerts_raw = alerts_raw,
    articles_raw = articles_raw,
    alerts = alerts,
    articles = articles,
    recipients = recipients,
    sent_log = sent_log
  )
}

# ------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------
print_ai_email_sender_diagnostics <- function(root = "D:/PREIS_AI") {
  x <- load_ai_email_sender_data(root)
  last_sent <- get_last_successful_send_date(x$sent_log)
  
  cat("\n=====================================\n")
  cat("PREIS-AI EMAIL SENDER DIAGNOSTICS\n")
  cat("=====================================\n\n")
  
  cat("Resolved files:\n")
  cat("- alerts_file      :", x$paths$alerts_file %||% "NOT FOUND", "\n")
  cat("- articles_file    :", x$paths$articles_file %||% "NOT FOUND", "\n")
  cat("- recipients_file  :", x$paths$recipients_file %||% "NOT FOUND", "\n")
  cat("- sent_log_file    :", x$paths$sent_log_file %||% "NOT FOUND", "\n\n")
  
  cat("Resolved file dates:\n")
  cat("- alerts_date      :", as.character(file_date_safe(x$paths$alerts_file)), "\n")
  cat("- articles_date    :", as.character(file_date_safe(x$paths$articles_file)), "\n")
  cat("- recipients_date  :", as.character(file_date_safe(x$paths$recipients_file)), "\n")
  cat("- last_sent_date   :", as.character(last_sent), "\n\n")
  
  cat("Detected volumes:\n")
  cat("- Alerts raw       :", safe_nrow(x$alerts_raw), "\n")
  cat("- Alerts harmonized:", safe_nrow(x$alerts), "\n")
  cat("- Articles raw     :", safe_nrow(x$articles_raw), "\n")
  cat("- Articles         :", safe_nrow(x$articles), "\n")
  cat("- Recipients       :", safe_nrow(x$recipients), "\n")
  cat("- AI sent log rows :", safe_nrow(x$sent_log), "\n\n")
  
  invisible(x)
}

# ------------------------------------------------------------
# New alerts since last send
# ------------------------------------------------------------
get_new_alerts_since_last_send <- function(alerts, sent_log) {
  if (safe_nrow(alerts) == 0) return(tibble())
  
  last_sent_date <- get_last_successful_send_date(sent_log)
  message("Last successful AI send date: ", as.character(last_sent_date))
  
  alerts_new <- alerts %>%
    mutate(
      date_detected = safe_parse_date(date_detected),
      priority_group = classify_priority(ai_surveillance_relevance)
    ) %>%
    filter(priority_group %in% c("Critical", "High"))
  
  # Normal rule: only alerts newer than last successful send
  alerts_by_date <- alerts_new %>%
    filter(!is.na(date_detected)) %>%
    filter(date_detected > last_sent_date)
  
  # Fallback: if date logic blocks everything, keep today’s CRIT/HIGH digest
  if (safe_nrow(alerts_by_date) > 0) {
    alerts_new <- alerts_by_date
  } else {
    message("Fallback: no alerts newer than last sent date; using current CRIT/HIGH digest.")
  }
  
  if (safe_nrow(sent_log) > 0 && "alert_id" %in% names(sent_log)) {
    sent_ids <- sent_log %>%
      mutate(
        status_l = tolower(normalize_text(status)),
        alert_id = normalize_text(alert_id)
      ) %>%
      filter(alert_id != "") %>%
      filter(status_l == "" | str_detect(status_l, "^sent$")) %>%
      pull(alert_id) %>%
      unique()
    
    if (length(sent_ids) > 0) {
      alerts_new <- alerts_new %>%
        filter(!(alert_id %in% sent_ids))
    }
  }
  
  alerts_new
}

# ------------------------------------------------------------
# Email body / subject
# ------------------------------------------------------------
make_email_subject <- function(scope, n_alerts, ref_date = Sys.Date()) {
  paste0(
    "PREIS-AI INTELLIGENCE EMAIL | ",
    scope, " | ",
    format(ref_date, "%Y-%m-%d"),
    " | New alerts: ",
    n_alerts
  )
}

build_scope_alert_body <- function(alerts_scope, scope_label, ref_date = Sys.Date()) {
  if (safe_nrow(alerts_scope) == 0) {
    return(
      paste0(
        "PREIS-AI INTELLIGENCE EMAIL\n",
        "Generated: ", format(ref_date, "%Y-%m-%d"), "\n\n",
        "Scope: ", scope_label, "\n\n",
        "ALERT METRICS New alerts: 0 Critical alerts: 0 High alerts: 0 Moderate alerts: 0\n\n",
        "No new AI surveillance articles were detected since the last successful email run for this scope.\n"
      )
    )
  }
  
  alerts_scope <- alerts_scope %>%
    mutate(priority_group = classify_priority(ai_surveillance_relevance))
  
  crit_n <- sum(alerts_scope$priority_group == "Critical", na.rm = TRUE)
  high_n <- sum(alerts_scope$priority_group == "High", na.rm = TRUE)
  mod_n  <- sum(alerts_scope$priority_group == "Moderate", na.rm = TRUE)
  
  top_tbl <- alerts_scope %>%
    arrange(desc(date_detected), desc(priority_group == "Critical"), desc(priority_group == "High")) %>%
    slice_head(n = 8)
  
  top_lines <- purrr::pmap_chr(top_tbl, function(...) {
    r <- list(...)
    paste0(
      normalize_text(r$ai_surveillance_relevance, "AI alert"),
      " | ",
      normalize_text(r$title, "Untitled"),
      " | Country: ", normalize_text(r$country, "Unspecified"),
      " | RCC: ", normalize_scope(r$rcc),
      " | Method: ", normalize_text(r$primary_ai_method, "Unspecified"),
      " | Application: ", normalize_text(r$primary_surveillance_function, "Unspecified")
    )
  })
  
  paste0(
    "PREIS-AI INTELLIGENCE EMAIL\n",
    "Generated: ", format(ref_date, "%Y-%m-%d"), "\n\n",
    "Scope: ", scope_label, "\n\n",
    "ALERT METRICS New alerts: ", nrow(alerts_scope),
    " Critical alerts: ", crit_n,
    " High alerts: ", high_n,
    " Moderate alerts: ", mod_n, "\n\n",
    "TOP NEW ALERTS\n\n",
    paste(top_lines, collapse = "\n"),
    "\n"
  )
}

# ------------------------------------------------------------
# Preview exports
# ------------------------------------------------------------
write_preview_outputs <- function(alerts_new, paths) {
  dir.create(dirname(paths$preview_alerts_file), recursive = TRUE, showWarnings = FALSE)
  
  tryCatch(readr::write_csv(alerts_new, paths$preview_alerts_file), error = function(e) {
    message("Could not write preview alerts: ", conditionMessage(e))
  })
  
  tryCatch(readr::write_csv(alerts_new, paths$preview_articles_file), error = function(e) {
    message("Could not write preview articles: ", conditionMessage(e))
  })
  
  tryCatch({
    qc <- tibble(
      metric = c("generated_date", "n_new_alerts", "n_scopes_with_alerts"),
      value = c(
        as.character(Sys.Date()),
        as.character(nrow(alerts_new)),
        as.character(if (nrow(alerts_new) == 0) 0 else dplyr::n_distinct(alerts_new$rcc))
      )
    )
    readr::write_csv(qc, paths$qc_file)
  }, error = function(e) {
    message("Could not write QC file: ", conditionMessage(e))
  })
  
  invisible(TRUE)
}

# ------------------------------------------------------------
# Prepare dispatch
# ------------------------------------------------------------
prepare_ai_email_dispatch <- function(root = "D:/PREIS_AI",
                                      keep_zero_all = TRUE) {
  x <- load_ai_email_sender_data(root)
  
  if (safe_nrow(x$recipients) == 0) {
    stop("No recipients found. Check alert_recipients_ai.csv / alert_recipients.csv", call. = FALSE)
  }
  
  if (safe_nrow(x$alerts) == 0) {
    stop(
      paste0(
        "No usable alerts found after harmonization.\n",
        "alerts_file used: ", x$paths$alerts_file %||% "NOT FOUND"
      ),
      call. = FALSE
    )
  }
  
  alerts_new <- get_new_alerts_since_last_send(
    alerts = x$alerts,
    sent_log = x$sent_log
  )
  
  write_preview_outputs(alerts_new, x$paths)
  
  out <- list()
  
  for (i in seq_len(nrow(x$recipients))) {
    rec_name  <- x$recipients$name[i]
    rec_email <- x$recipients$email[i]
    rec_rcc   <- x$recipients$rcc[i]
    
    desired_scope <- ifelse(toupper(rec_rcc) == "ALL", "All", rec_rcc)
    
    alerts_scope <- if (toupper(desired_scope) == "ALL") {
      alerts_new
    } else {
      alerts_new %>% filter(normalize_scope(rcc) == normalize_scope(desired_scope))
    }
    
    sent_for_recipient <- x$sent_log %>%
      mutate(
        recipient_email = normalize_email(recipient_email),
        alert_id = normalize_text(alert_id),
        status_l = tolower(normalize_text(status))
      ) %>%
      filter(recipient_email == normalize_email(rec_email)) %>%
      filter(alert_id != "") %>%
      filter(status_l == "" | str_detect(status_l, "^sent$")) %>%
      pull(alert_id) %>%
      unique()
    
    if (length(sent_for_recipient) > 0 && safe_nrow(alerts_scope) > 0) {
      alerts_scope <- alerts_scope %>%
        filter(!(alert_id %in% sent_for_recipient))
    }
    
    n_alerts_val <- safe_nrow(alerts_scope)
    
    if (desired_scope != "All" && n_alerts_val <= 0) next
    if (desired_scope == "All" && !keep_zero_all && n_alerts_val <= 0) next
    
    out[[length(out) + 1]] <- list(
      recipient_name = rec_name,
      recipient_email = rec_email,
      recipient_rcc = rec_rcc,
      scope = desired_scope,
      subject = make_email_subject(desired_scope, n_alerts_val, Sys.Date()),
      n_alerts = n_alerts_val,
      body = build_scope_alert_body(alerts_scope, desired_scope, Sys.Date()),
      alerts_scope = alerts_scope
    )
  }
  
  out
}

# ------------------------------------------------------------
# Main sender
# ------------------------------------------------------------
send_ai_alerts_conditional <- function(send_now = FALSE,
                                       root = "D:/PREIS_AI",
                                       preview_chars = 1200,
                                       use_sent_log = TRUE,
                                       keep_zero_all = TRUE) {
  x <- load_ai_email_sender_data(root = root)
  
  dispatch <- prepare_ai_email_dispatch(
    root = root,
    keep_zero_all = keep_zero_all
  )
  
  cat("\n=====================================\n")
  cat("PREIS-AI CONDITIONAL EMAIL DISPATCH\n")
  cat("=====================================\n\n")
  cat("Packages prepared:", length(dispatch), "\n\n")
  
  if (send_now && !ai_email_env_ok()) {
    stop("SMTP variables missing. Use Sys.setenv() or .Renviron first.", call. = FALSE)
  }
  
  ensure_sent_log(x$paths$sent_log_file)
  sent_rows <- list()
  
  for (i in seq_along(dispatch)) {
    item <- dispatch[[i]]
    
    cat("---- PACKAGE", i, "----\n")
    cat("Recipient:", item$recipient_name, "\n")
    cat("Email    :", item$recipient_email, "\n")
    cat("RCC      :", item$recipient_rcc, "\n")
    cat("Scope    :", item$scope, "\n")
    cat("Alerts   :", item$n_alerts, "\n")
    cat("Subject  :", item$subject, "\n\n")
    cat(substr(item$body, 1, preview_chars), "\n\n")
    
    if (!identical(item$scope, "All") && (is.na(item$n_alerts) || item$n_alerts <= 0)) {
      cat("Skipped: no alerts for scope ", item$scope, "\n\n", sep = "")
      next
    }
    
    if (identical(item$scope, "All") && !keep_zero_all && (is.na(item$n_alerts) || item$n_alerts <= 0)) {
      cat("Skipped: no alerts for scope All\n\n")
      next
    }
    
    if (!send_now) next
    
    email_obj <- ai_compose_email(item$body)
    
    tryCatch({
      ai_smtp_send(
        email_obj = email_obj,
        to = item$recipient_email,
        subject = item$subject,
        verbose = FALSE
      )
      
      cat("Sent -> ", item$recipient_email, "\n\n", sep = "")
      
      if (use_sent_log) {
        alerts_sent <- item$alerts_scope
        
        if (safe_nrow(alerts_sent) == 0) {
          sent_rows[[length(sent_rows) + 1]] <- tibble(
            date_sent = as.character(Sys.Date()),
            datetime_sent = as.character(Sys.time()),
            recipient_name = as.character(item$recipient_name),
            recipient_email = normalize_email(item$recipient_email),
            recipient_rcc = as.character(item$recipient_rcc),
            scope = normalize_scope(item$scope),
            subject = as.character(item$subject),
            n_alerts = as.numeric(item$n_alerts),
            status = "sent",
            alert_id = NA_character_
          )
        } else {
          sent_rows[[length(sent_rows) + 1]] <- alerts_sent %>%
            transmute(
              date_sent = as.character(Sys.Date()),
              datetime_sent = as.character(Sys.time()),
              recipient_name = as.character(item$recipient_name),
              recipient_email = normalize_email(item$recipient_email),
              recipient_rcc = as.character(item$recipient_rcc),
              scope = normalize_scope(item$scope),
              subject = as.character(item$subject),
              n_alerts = as.numeric(item$n_alerts),
              status = "sent",
              alert_id = as.character(alert_id)
            )
        }
      }
      
    }, error = function(e) {
      cat("Send failed -> ", item$recipient_email, "\n", sep = "")
      cat("Reason: ", conditionMessage(e), "\n\n", sep = "")
      
      if (use_sent_log) {
        sent_rows[[length(sent_rows) + 1]] <- tibble(
          date_sent = as.character(Sys.Date()),
          datetime_sent = as.character(Sys.time()),
          recipient_name = as.character(item$recipient_name),
          recipient_email = normalize_email(item$recipient_email),
          recipient_rcc = as.character(item$recipient_rcc),
          scope = normalize_scope(item$scope),
          subject = as.character(item$subject),
          n_alerts = as.numeric(item$n_alerts),
          status = paste0("failed: ", conditionMessage(e)),
          alert_id = NA_character_
        )
      }
    })
  }
  
  if (send_now && length(sent_rows) > 0 && use_sent_log) {
    append_sent_log(x$paths$sent_log_file, bind_rows(sent_rows))
  }
  
  if (!send_now) {
    message("Preview mode only. No emails sent.")
  }
  
  invisible(dispatch)
}