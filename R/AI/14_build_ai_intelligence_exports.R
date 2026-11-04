# =========================================================
# PREIS-AI - BUILD INTELLIGENCE EXPORTS (HARDENED V2)
# File: 14_build_ai_intelligence_exports.R
# Purpose:
#   - Rebuild dashboard and output exports from fresh tracker
#   - Force overwrite of alerts_ai.csv and ai_alerts.csv
#   - Force overwrite of articles_ai.csv
#   - Avoid ambiguous date parsing / charToDate crashes
# =========================================================

options(stringsAsFactors = FALSE, warn = 1)

# -----------------------------
# 0) CONFIG
# -----------------------------
PROJECT_DIR <- "D:/PREIS_AI_CLEAN"

TRACKER_FILE <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")

DASH_DIR         <- file.path(PROJECT_DIR, "dashboard", "data", "dashboard")
OUTPUT_ALERT_DIR <- file.path(PROJECT_DIR, "data", "outputs", "ai", "alerts")

OUT_ALERTS_DASH <- file.path(DASH_DIR, "alerts_ai.csv")
OUT_ALERTS_OUT  <- file.path(OUTPUT_ALERT_DIR, "ai_alerts.csv")
OUT_ARTICLES    <- file.path(DASH_DIR, "articles_ai.csv")

dir.create(DASH_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_ALERT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) LOGGING
# -----------------------------
log14 <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", paste0(..., collapse = ""), "\n")
}

banner14 <- function(txt) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
  cat(txt, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
}

# -----------------------------
# 2) SAFE HELPERS
# -----------------------------
safe_read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Unable to read CSV: ", path, " | ", conditionMessage(e))
  )
  if (!nrow(df)) stop("CSV is empty: ", path)
  df
}

clean_txt <- function(x, default = NA_character_) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  if (length(default) == 1 && !is.na(default)) x[is.na(x)] <- default
  x
}

safe_num <- function(x, default = 0) {
  y <- suppressWarnings(as.numeric(x))
  y[is.na(y)] <- default
  y
}

safe_date_one <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) return(as.Date(NA))
  x <- as.character(x[1])
  x <- trimws(x)
  
  if (!nzchar(x) || x %in% c("NA", "N/A", "NULL", "null")) {
    return(as.Date(NA))
  }
  
  x <- sub("T.*$", "", x)
  x <- sub(" +.*$", "", x)
  x <- gsub("/", "-", x)
  
  # exact day
  for (fmt in c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y")) {
    d <- suppressWarnings(as.Date(x, format = fmt))
    if (!is.na(d)) return(d)
  }
  
  # YYYY-MM
  if (grepl("^\\d{4}-\\d{2}$", x)) {
    d <- suppressWarnings(as.Date(paste0(x, "-01")))
    if (!is.na(d)) return(d)
  }
  
  # YYYY
  if (grepl("^\\d{4}$", x)) {
    d <- suppressWarnings(as.Date(paste0(x, "-01-01")))
    if (!is.na(d)) return(d)
  }
  
  # fallback: do not trust raw as.Date(x) if ambiguous
  as.Date(NA)
}

safe_date_vec <- function(x) {
  as.Date(vapply(seq_along(x), function(i) {
    d <- safe_date_one(x[i])
    if (is.na(d)) return(NA_real_)
    as.numeric(d)
  }, numeric(1)), origin = "1970-01-01")
}

first_present <- function(df, candidates, default = NA_character_) {
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(default, nrow(df)))
  clean_txt(df[[hits[1]]], default = default)
}

coalesce_first_present <- function(df, candidates, default = NA_character_) {
  out <- rep(NA_character_, nrow(df))
  for (nm in candidates) {
    if (!nm %in% names(df)) next
    vals <- clean_txt(df[[nm]], default = NA_character_)
    idx <- is.na(out) & !is.na(vals) & nzchar(vals)
    out[idx] <- vals[idx]
  }
  if (length(default) == 1 && !is.na(default)) out[is.na(out)] <- default
  out
}

write_csv_checked <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  tmp <- file.path(
    dirname(path),
    paste0(tools::file_path_sans_ext(basename(path)), "_tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  
  utils::write.csv(df, tmp, row.names = FALSE, na = "")
  
  if (!file.exists(tmp)) stop("Temporary file was not written: ", tmp)
  info_tmp <- file.info(tmp)
  if (is.na(info_tmp$size) || info_tmp$size <= 0) stop("Temporary file is empty: ", tmp)
  
  final_path <- path
  
  if (file.exists(path)) {
    suppressWarnings(try(file.remove(path), silent = TRUE))
  }
  
  if (file.exists(path)) {
    final_path <- file.path(
      dirname(path),
      paste0(tools::file_path_sans_ext(basename(path)), "_NEW_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    )
    ok <- file.copy(tmp, final_path, overwrite = TRUE)
    if (!ok || !file.exists(final_path)) {
      stop("Target locked and fallback copy failed: ", path)
    }
  } else {
    ok <- file.rename(tmp, path)
    if (!ok || !file.exists(path)) {
      ok2 <- file.copy(tmp, path, overwrite = TRUE)
      if (!ok2 || !file.exists(path)) {
        final_path <- file.path(
          dirname(path),
          paste0(tools::file_path_sans_ext(basename(path)), "_NEW_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        )
        ok3 <- file.copy(tmp, final_path, overwrite = TRUE)
        if (!ok3 || !file.exists(final_path)) {
          stop("Could not finalize write for: ", path)
        }
      } else {
        final_path <- path
      }
    }
  }
  
  back <- utils::read.csv(final_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(back)) stop("Written file has zero rows: ", final_path)
  
  final_path
}

safe_mtime_date <- function(path) {
  if (!file.exists(path)) return(as.Date(NA))
  mt <- file.info(path)$mtime
  if (is.na(mt)) return(as.Date(NA))
  as.Date(format(mt, "%Y-%m-%d"))  # [20261028] date LOCALE (as.Date(POSIXct) = UTC : faux echec entre 00h et 03h a UTC+3)
}

# -----------------------------
# 3) LOAD TRACKER
# -----------------------------
banner14("PREIS-AI EXPORTS BUILD START")

log14("TRACKER_FILE: ", TRACKER_FILE)
trk <- safe_read_csv(TRACKER_FILE)

log14("Tracker rows: ", nrow(trk))
log14("Tracker cols: ", ncol(trk))

# -----------------------------
# 4) COMMON FIELDS
# -----------------------------
pub_date_chr <- coalesce_first_present(
  trk,
  c("publication_date", "publication_date_fixed", "first_publication_date", "pub_date", "publication_date_raw"),
  default = NA_character_
)

pub_date_vec <- safe_date_vec(pub_date_chr)
# [20261010] Dates futures (annee > annee courante) = erreur de metadonnees -> NA (valeur brute conservee dans le tracker)
fut_date <- !is.na(pub_date_vec) & suppressWarnings(as.integer(format(as.Date(pub_date_vec), "%Y"))) > as.integer(format(Sys.Date(), "%Y"))
pub_date_vec[which(fut_date)] <- NA
rm(fut_date)

pub_year_chr <- coalesce_first_present(
  trk,
  c("year", "pub_year"),
  default = NA_character_
)

record_id_chr <- coalesce_first_present(trk, c("record_id", "id"), default = NA_character_)
title_chr     <- first_present(trk, c("title"), default = NA_character_)

# -----------------------------
# 5) BUILD ARTICLES EXPORT
# -----------------------------
articles <- data.frame(
  record_id                 = record_id_chr,
  title                     = title_chr,
  year                      = pub_year_chr,
  publication_date          = ifelse(is.na(pub_date_vec), NA_character_, as.character(pub_date_vec)),
  journal                   = coalesce_first_present(trk, c("journal", "source_name"), default = NA_character_),
  authors                   = first_present(trk, c("authors"), default = NA_character_),
  article_type              = first_present(trk, c("article_type"), default = NA_character_),
  record_origin = coalesce_first_present(trk, c("record_origin"), default = NA_character_),
  country = coalesce_first_present(trk, c("country"), default = NA_character_),
  iso3 = coalesce_first_present(trk, c("iso3"), default = NA_character_),
  rcc = coalesce_first_present(trk, c("rcc"), default = NA_character_),
  source_name               = coalesce_first_present(trk, c("source_name", "source"), default = NA_character_),
  doi                       = first_present(trk, c("doi"), default = NA_character_),
  pmid                      = first_present(trk, c("pmid"), default = NA_character_),
  pmcid                     = first_present(trk, c("pmcid"), default = NA_character_),
  url                       = coalesce_first_present(trk, c("url", "source_url", "link"), default = NA_character_),
  abstract                  = first_present(trk, c("abstract"), default = NA_character_),
  method_family_primary     = first_present(trk, c("method_family_primary"), default = NA_character_),
  application_type_primary  = first_present(trk, c("application_type_primary"), default = NA_character_),
  disease_primary           = coalesce_first_present(trk, c("disease_primary", "disease"), default = NA_character_),
  africa_relevance_score    = safe_num(first_present(trk, c("africa_relevance_score"), default = NA_character_), 0),
  africa_relevance_level    = first_present(trk, c("africa_relevance_level"), default = NA_character_),
  novelty_score             = safe_num(first_present(trk, c("novelty_score"), default = NA_character_), 0),
  novelty_level             = first_present(trk, c("novelty_level"), default = NA_character_),
  priority_score            = safe_num(first_present(trk, c("priority_score"), default = NA_character_), 0),
  priority_level            = first_present(trk, c("priority_level"), default = NA_character_),
  dashboard_bucket          = first_present(trk, c("dashboard_bucket"), default = NA_character_),
  tracker_recommendation    = first_present(trk, c("tracker_recommendation"), default = NA_character_),
  ph_scope_tier            = first_present(trk, c("ph_scope_tier"), default = NA_character_),
  ph_in_scope              = first_present(trk, c("ph_in_scope"), default = NA_character_),
  ai_method_evidence       = first_present(trk, c("ai_method_evidence"), default = NA_character_),
  disease_confidence       = first_present(trk, c("disease_confidence"), default = NA_character_),
  disease_mentioned        = first_present(trk, c("disease_mentioned"), default = NA_character_),
  disease_evidence         = first_present(trk, c("disease_evidence"), default = NA_character_),
  ebola_mentioned          = first_present(trk, c("ebola_mentioned"), default = NA_character_),
  geo_evidence             = first_present(trk, c("geo_evidence"), default = NA_character_),
  countries_mentioned      = first_present(trk, c("countries_mentioned"), default = NA_character_),
  application_score_top    = first_present(trk, c("application_score_top"), default = NA_character_),
  stringsAsFactors = FALSE
)

articles <- articles[!is.na(articles$title) & trimws(articles$title) != "", , drop = FALSE]

if (nrow(articles)) {
  key_articles <- ifelse(
    !is.na(articles$record_id) & nzchar(articles$record_id),
    paste0("RID::", articles$record_id),
    paste0(
      "TTL::", clean_txt(tolower(articles$title), ""),
      "::DOI::", clean_txt(tolower(articles$doi), ""),
      "::PMID::", clean_txt(articles$pmid, "")
    )
  )
  articles <- articles[!duplicated(key_articles), , drop = FALSE]
}

final_articles_path <- write_csv_checked(articles, OUT_ARTICLES)
log14("articles_ai.csv rebuilt: ", final_articles_path)
log14("articles rows written: ", nrow(articles))

# -----------------------------
# 6) BUILD ALERTS EXPORT
# -----------------------------
africa_score   <- safe_num(first_present(trk, c("africa_relevance_score"), default = NA_character_), 0)
novelty_score  <- safe_num(first_present(trk, c("novelty_score"), default = NA_character_), 0)
priority_score <- safe_num(first_present(trk, c("priority_score"), default = NA_character_), 0)

priority_level <- first_present(trk, c("priority_level"), default = "Moderate")
priority_level[is.na(priority_level) | trimws(priority_level) == ""] <- "Moderate"

alert_id <- record_id_chr
idx_missing_id <- is.na(alert_id) | trimws(alert_id) == ""
if (any(idx_missing_id)) {
  alert_id[idx_missing_id] <- paste0("AI_", seq_len(sum(idx_missing_id)))
}

alerts <- data.frame(
  alert_id                 = alert_id,
  record_id                = alert_id,
  title                    = title_chr,
  publication_date         = ifelse(is.na(pub_date_vec), NA_character_, as.character(pub_date_vec)),
  year                     = pub_year_chr,
  journal                  = coalesce_first_present(trk, c("journal", "source_name"), default = NA_character_),
  authors                  = first_present(trk, c("authors"), default = NA_character_),
  doi                      = first_present(trk, c("doi"), default = NA_character_),
  pmid                     = first_present(trk, c("pmid"), default = NA_character_),
  pmcid                    = first_present(trk, c("pmcid"), default = NA_character_),
  url                      = coalesce_first_present(trk, c("url", "source_url", "link"), default = NA_character_),
  article_type             = first_present(trk, c("article_type"), default = NA_character_),
  record_origin = coalesce_first_present(trk, c("record_origin"), default = NA_character_),
  country = coalesce_first_present(trk, c("country"), default = NA_character_),
  iso3 = coalesce_first_present(trk, c("iso3"), default = NA_character_),
  rcc = coalesce_first_present(trk, c("rcc"), default = NA_character_),
  source_name              = coalesce_first_present(trk, c("source_name", "source"), default = NA_character_),
  method_family_primary    = first_present(trk, c("method_family_primary"), default = NA_character_),
  application_type_primary = first_present(trk, c("application_type_primary"), default = NA_character_),
  disease_primary          = coalesce_first_present(trk, c("disease_primary", "disease"), default = NA_character_),
  signal_profile           = first_present(trk, c("signal_profile"), default = NA_character_),
  predictive_signal        = first_present(trk, c("predictive_signal"), default = NA_character_),
  operational_signal       = first_present(trk, c("operational_signal"), default = NA_character_),
  digital_signal           = first_present(trk, c("digital_signal"), default = NA_character_),
  one_health_signal        = first_present(trk, c("one_health_signal"), default = NA_character_),
  africa_relevance_score   = africa_score,
  africa_relevance_level   = first_present(trk, c("africa_relevance_level"), default = NA_character_),
  novelty_score            = novelty_score,
  novelty_level            = first_present(trk, c("novelty_level"), default = NA_character_),
  priority_score           = priority_score,
  priority_level           = priority_level,
  dashboard_bucket         = first_present(trk, c("dashboard_bucket"), default = NA_character_),
  tracker_recommendation   = first_present(trk, c("tracker_recommendation"), default = NA_character_),
  ph_scope_tier            = first_present(trk, c("ph_scope_tier"), default = NA_character_),
  ph_in_scope              = first_present(trk, c("ph_in_scope"), default = NA_character_),
  ai_method_evidence       = first_present(trk, c("ai_method_evidence"), default = NA_character_),
  disease_confidence       = first_present(trk, c("disease_confidence"), default = NA_character_),
  disease_mentioned        = first_present(trk, c("disease_mentioned"), default = NA_character_),
  disease_evidence         = first_present(trk, c("disease_evidence"), default = NA_character_),
  ebola_mentioned          = first_present(trk, c("ebola_mentioned"), default = NA_character_),
  geo_evidence             = first_present(trk, c("geo_evidence"), default = NA_character_),
  countries_mentioned      = first_present(trk, c("countries_mentioned"), default = NA_character_),
  application_score_top    = first_present(trk, c("application_score_top"), default = NA_character_),
  generated_date           = as.character(Sys.Date()),
  stringsAsFactors = FALSE
)

alerts <- alerts[!is.na(alerts$title) & trimws(alerts$title) != "", , drop = FALSE]

# [Finding #4 - 2026-10-05] Les articles hors perimetre sante publique/epidemies ne generent pas d'alerte (restent dans articles_ai.csv).
if ("ph_in_scope" %in% names(trk)) {
  n_before_ph <- nrow(alerts)
  alerts <- alerts[is.na(alerts$ph_in_scope) | toupper(trimws(alerts$ph_in_scope)) != "FALSE", , drop = FALSE]
  log14("ph_scope filter: alerts ", n_before_ph, " -> ", nrow(alerts))
}

# [2026-10-06] Aucune preuve d'IA ni de modelisation (ai_method_evidence == "none") : pas d'alerte (reste dans articles_ai.csv).
if ("ai_method_evidence" %in% names(alerts)) {
  n_before_ai <- nrow(alerts)
  alerts <- alerts[is.na(alerts$ai_method_evidence) | alerts$ai_method_evidence != "none", , drop = FALSE]
  log14("ai_method_evidence filter: alerts ", n_before_ai, " -> ", nrow(alerts))
}

if (nrow(alerts)) {
  key_alerts <- ifelse(
    !is.na(alerts$record_id) & nzchar(alerts$record_id),
    paste0("RID::", alerts$record_id),
    paste0(
      "TTL::", clean_txt(tolower(alerts$title), ""),
      "::DOI::", clean_txt(tolower(alerts$doi), ""),
      "::PMID::", clean_txt(alerts$pmid, "")
    )
  )
  alerts <- alerts[!duplicated(key_alerts), , drop = FALSE]
}

if (nrow(alerts)) {
  # use numeric ordinals directly; no as.Date() call here
  pub_ord <- suppressWarnings(as.numeric(safe_date_vec(alerts$publication_date)))
  ord <- order(
    -safe_num(alerts$priority_score, 0),
    -safe_num(alerts$novelty_score, 0),
    -safe_num(alerts$africa_relevance_score, 0),
    pub_ord,
    na.last = TRUE
  )
  alerts <- alerts[ord, , drop = FALSE]
}

final_alerts_dash <- write_csv_checked(alerts, OUT_ALERTS_DASH)
final_alerts_out  <- write_csv_checked(alerts, OUT_ALERTS_OUT)

log14("alerts dashboard written to: ", final_alerts_dash)
log14("alerts output written to   : ", final_alerts_out)
log14("alerts rows written: ", nrow(alerts))

# -----------------------------
# 7) POST-WRITE VALIDATION
# -----------------------------
info_alert_dash <- file.info(final_alerts_dash)
info_alert_out  <- file.info(final_alerts_out)
info_articles   <- file.info(final_articles_path)

log14("alerts dashboard mtime: ", as.character(info_alert_dash$mtime))
log14("alerts output mtime   : ", as.character(info_alert_out$mtime))
log14("articles mtime        : ", as.character(info_articles$mtime))

if (safe_mtime_date(final_alerts_dash) < Sys.Date()) {
  stop("alerts_ai.csv dashboard not refreshed today")
}
if (safe_mtime_date(final_alerts_out) < Sys.Date()) {
  stop("ai_alerts.csv output not refreshed today")
}
if (safe_mtime_date(final_articles_path) < Sys.Date()) {
  stop("articles_ai.csv not refreshed today")
}

alerts_dash_back <- safe_read_csv(final_alerts_dash)
alerts_out_back  <- safe_read_csv(final_alerts_out)
articles_back    <- safe_read_csv(final_articles_path)

log14("alerts dashboard rows re-read: ", nrow(alerts_dash_back))
log14("alerts output rows re-read   : ", nrow(alerts_out_back))
log14("articles rows re-read        : ", nrow(articles_back))

if (nrow(alerts_dash_back) == 0) stop("Dashboard alerts file re-read with zero rows")
if (nrow(alerts_out_back) == 0) stop("Output alerts file re-read with zero rows")
if (nrow(articles_back) == 0) stop("Articles file re-read with zero rows")

banner14("PREIS-AI EXPORTS BUILD END")
log14("14_build_ai_intelligence_exports.R completed successfully")
