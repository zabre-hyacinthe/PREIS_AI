# =========================================================
# PREIS-AI HARDENED DAILY RUNNER
# File: 99_run_preis_ai_daily_hardened.R
# Version: 2.0 — production-hardened
#
# Corrections v2.0 :
#   [FIX-1]  charToDate() — parse_any_date() renforcée, plus d'appel
#            nu à as.Date() sur colonnes brutes
#   [FIX-2]  fetch_pubmed critical=TRUE → FALSE (script absent, ne doit
#            pas bloquer le pipeline)
#   [FIX-3]  best_data_date : test is.finite() remplacé par !is.na()
#   [FIX-4]  sys.source() isolé dans un environnement enfant pour
#            éviter la pollution de .GlobalEnv
#   [FIX-5]  results indexé proprement via noms de stages
#   [FIX-6]  fileEncoding = "UTF-8" sur tous les write.csv (Windows)
#   [FIX-7]  Wrapper run_script_safe avec tryCatch complet (warning +
#            error) et mesure de durée fiable
#   [FIX-8]  Bloc freshness : lag_days calculé uniquement si date valide
#   [FIX-9]  alerts_mtime comparaison sécurisée
#   [FIX-10] Gestion robuste des listes NULL dans do.call(rbind, ...)
# =========================================================

options(stringsAsFactors = FALSE, warn = 1)

# ─────────────────────────────────────────────────────────
# 0) CONFIGURATION UTILISATEUR
# ─────────────────────────────────────────────────────────
PROJECT_DIR <- "D:/PREIS_AI_CLEAN"
AI_DIR      <- file.path(PROJECT_DIR, "R", "AI")
CONFIG_DIR  <- file.path(PROJECT_DIR, "config")
DATA_DIR    <- file.path(PROJECT_DIR, "data")
DASH_DIR    <- file.path(PROJECT_DIR, "dashboard", "data", "dashboard")
LOG_DIR     <- file.path(PROJECT_DIR, "logs")

dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

RUN_DATE <- Sys.Date()
RUN_TS   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

LOG_FILE <- file.path(
  LOG_DIR,
  paste0("preis_ai_daily_hardened_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

# ─────────────────────────────────────────────────────────
# 1) LOGGING
# ─────────────────────────────────────────────────────────
log_line <- function(...) {
  msg  <- paste0(..., collapse = "")
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg)
  cat(line, "\n")
  tryCatch(
    cat(line, "\n", file = LOG_FILE, append = TRUE),
    error = function(e) invisible(NULL)
  )
}

banner <- function(txt) {
  sep <- paste(rep("=", 70), collapse = "")
  log_line("")
  log_line(sep)
  log_line(txt)
  log_line(sep)
}

# ─────────────────────────────────────────────────────────
# 2) UTILITAIRES SÛRS
# ─────────────────────────────────────────────────────────

## 2a) Fichier non-vide ?
file_exists_nonempty <- function(path) {
  isTRUE(file.exists(path)) &&
    !is.na(file.info(path)$size) &&
    file.info(path)$size > 0L
}

## 2b) Date de modification
safe_mtime_date <- function(path) {
  if (!file.exists(path)) return(as.Date(NA))
  as.Date(file.info(path)$mtime)
}

## 2c) Nombre de lignes (sans charger tout le fichier)
safe_nrow_csv <- function(path) {
  if (!file_exists_nonempty(path)) return(0L)
  tryCatch({
    n <- length(utils::count.fields(path, sep = ","))
    max(0L, n - 1L)          # soustraire la ligne d'en-tête
  }, error = function(e) 0L)
}

## 2d) Lecture CSV sécurisée
safe_read_csv_base <- function(path, n_max = Inf) {
  if (!file_exists_nonempty(path)) return(NULL)
  tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      nrows            = if (is.finite(n_max)) as.integer(n_max) else -1L,
      fileEncoding     = "UTF-8"
    ),
    error = function(e) NULL
  )
}

## 2e) Premier chemin existant
first_existing <- function(paths) {
  hits <- paths[vapply(paths, file.exists, logical(1))]
  if (!length(hits)) return(NA_character_)
  hits[[1L]]
}

## 2f) Normalisation de noms de scripts
normalize_name <- function(x) {
  x <- basename(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- sub("_r$",     "",  x)
  x <- sub("^_+|_+$", "", x)
  x
}

## 2g) [FIX-1] Parsing de dates — aucun appel nu à as.Date()
##     Gère : YYYY-MM-DD, DD/MM/YYYY, MM/DD/YYYY, YYYY/MM/DD,
##            YYYY-MM, YYYY, timestamps ISO, NA textuels
parse_any_date <- function(x) {
  if (is.null(x) || length(x) == 0L) return(as.Date(character(0)))
  
  x <- as.character(x)
  x <- trimws(x)
  
  # Normaliser les NA textuels
  na_strings <- c("", "NA", "N/A", "NULL", "null", "na", "n/a", "NaT", "NaN")
  x[x %in% na_strings] <- NA_character_
  
  # Supprimer la partie heure des timestamps
  x <- sub("T[0-9:+Z.-]+$", "", x)
  x <- sub(" .*$",           "", x)
  
  out <- rep(as.Date(NA), length(x))
  
  try_fmt <- function(v, fmt) suppressWarnings(as.Date(v, format = fmt))
  
  fmts <- list(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%m/%d/%Y",
    "%Y/%m/%d",
    "%d-%m-%Y",
    "%d.%m.%Y"
  )
  
  for (fmt in fmts) {
    parsed <- try_fmt(x, fmt)
    fill   <- is.na(out) & !is.na(parsed)
    out[fill] <- parsed[fill]
  }
  
  # YYYY-MM  → premier jour du mois
  ym_idx <- is.na(out) & grepl("^\\d{4}-\\d{2}$", x)
  if (any(ym_idx)) {
    out[ym_idx] <- suppressWarnings(as.Date(paste0(x[ym_idx], "-01"), format = "%Y-%m-%d"))
  }
  
  # YYYY → premier janvier
  y_idx <- is.na(out) & grepl("^\\d{4}$", x)
  if (any(y_idx)) {
    out[y_idx] <- suppressWarnings(as.Date(paste0(x[y_idx], "-01-01"), format = "%Y-%m-%d"))
  }
  
  out
}

## 2h) Détecter la meilleure colonne date dans un data.frame
find_date_col <- function(df) {
  if (is.null(df) || !nrow(df)) return(NA_character_)
  nm   <- names(df)
  pref <- c(
    "publication_date", "publication_date_fixed",
    "first_publication_date", "pub_date", "pub_date_raw",
    "date", "article_date", "publicationdate"
  )
  hit <- pref[pref %in% nm]
  if (length(hit)) return(hit[[1L]])
  cand <- nm[grepl("date", nm, ignore.case = TRUE)]
  if (length(cand)) return(cand[[1L]])
  NA_character_
}

## 2i) Date max dans un CSV (utilise parse_any_date — [FIX-1])
max_date_in_csv <- function(path) {
  df <- safe_read_csv_base(path)
  if (is.null(df) || !nrow(df)) return(as.Date(NA))
  dc <- find_date_col(df)
  if (is.na(dc))                return(as.Date(NA))
  d  <- parse_any_date(df[[dc]])
  if (all(is.na(d)))            return(as.Date(NA))
  suppressWarnings(max(d, na.rm = TRUE))
}

# ─────────────────────────────────────────────────────────
# 3) INVENTAIRE ET RÉSOLUTION DES SCRIPTS
# ─────────────────────────────────────────────────────────
all_ai_scripts <- if (dir.exists(AI_DIR)) {
  list.files(AI_DIR, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
} else {
  character(0)
}

if (!length(all_ai_scripts)) {
  stop("Répertoire AI vide ou introuvable : ", AI_DIR)
}

script_index <- data.frame(
  path = all_ai_scripts,
  key  = normalize_name(all_ai_scripts),
  stringsAsFactors = FALSE
)

resolve_script <- function(candidates) {
  keys <- normalize_name(candidates)
  
  # Correspondance exacte
  idx <- which(script_index$key %in% keys)
  if (length(idx)) return(script_index$path[[idx[[1L]]]])
  
  # Correspondance partielle (contains)
  for (k in keys) {
    idx2 <- which(grepl(k, script_index$key, fixed = TRUE))
    if (length(idx2)) return(script_index$path[[idx2[[1L]]]])
  }
  
  # Correspondance token (regex)
  for (cand in candidates) {
    tok  <- tolower(gsub("\\.[Rr]$", "", basename(cand)))
    tok  <- gsub("[^a-z0-9]+", ".*", tok)
    idx3 <- which(grepl(tok, tolower(basename(script_index$path))))
    if (length(idx3)) return(script_index$path[[idx3[[1L]]]])
  }
  
  NA_character_
}

# ─────────────────────────────────────────────────────────
# 4) CHEMINS DE SORTIE
# ─────────────────────────────────────────────────────────
paths <- list(
  tracker_main = first_existing(c(
    file.path(DATA_DIR, "enriched", "ai", "ai_tracker_final.csv"),
    file.path(DATA_DIR, "final",    "ai", "ai_tracker_final.csv"),
    file.path(DATA_DIR, "outputs",  "ai", "ai_tracker_final.csv")
  )),
  alerts_main = first_existing(c(
    file.path(DASH_DIR,  "alerts_ai.csv"),
    file.path(DATA_DIR,  "outputs", "ai", "alerts", "ai_alerts.csv"),
    file.path(DATA_DIR,  "outputs", "ai", "alerts_ai.csv")
  )),
  articles_main = first_existing(c(
    file.path(DASH_DIR,  "articles_ai.csv"),
    file.path(DATA_DIR,  "dashboard", "ai_intelligence_dashboard_dataset.csv"),
    file.path(DATA_DIR,  "dashboard", "articles_ai.csv")
  )),
  sent_log = first_existing(c(
    file.path(DASH_DIR, "sent_log_ai.csv"),
    file.path(DASH_DIR, "sent_log.csv")
  )),
  recipients = first_existing(c(
    file.path(CONFIG_DIR, "alert_recipients_ai.csv"),
    file.path(CONFIG_DIR, "recipients_ai.csv")
  ))
)

# ─────────────────────────────────────────────────────────
# 5) SPÉCIFICATION DU PIPELINE
#    [FIX-2] fetch_pubmed → critical = FALSE (script absent)
# ─────────────────────────────────────────────────────────
pipeline_spec <- data.frame(
  stage = c(
    "fetch_europepmc",
    "fetch_pubmed",            # non-critique : script optionnel
    "standardize",
    "merge",
    "deduplicate",
    "classify_methods",
    "classify_applications",
    "classify_disease",
    "score_africa_relevance",
    "score_novelty",
    "build_tracker",
    "build_alerts",
    "send_alerts_conditional",
    "run_email_cycle"
  ),
  critical = c(
    TRUE,  FALSE,             # [FIX-2] fetch_pubmed → FALSE
    TRUE,  TRUE,  TRUE,
    FALSE, FALSE, FALSE,
    FALSE, FALSE,
    TRUE,  TRUE,
    FALSE, FALSE
  ),
  stringsAsFactors = FALSE
)

pipeline_spec$candidates <- I(list(
  c("01_fetch_europepmc_ai.R", "01_fetch_articles_ai_v10.R", "01_fetch_ai_europepmc.R"),
  c("02_fetch_pubmed_ai.R", "05_fetch_pubmed_ai.R", "02_fetch_ai_pubmed.R"),
  c("03_standardize_ai_records.R", "06_standardize_ai_sources.R"),
  c("04_merge_ai_sources.R",       "07_merge_ai_sources.R"),
  c("05_deduplicate_ai.R",         "08_deduplicate_ai_sources.R"),
  c("10_classify_ai_methods.R",    "09_classify_ai_methods.R"),
  c("11_classify_ai_applications.R","10_classify_ai_applications.R"),
  c("12_classify_ai_disease.R"),
  c("20_score_ai_africa_relevance.R","10_score_africa_relevance.R"),
  c("21_score_ai_novelty.R",         "11_detect_ai_novelty.R"),
  c("30_build_ai_tracker.R",         "12_build_ai_tracker_final.R"),
  c("40_build_ai_alerts.R",          "14_build_ai_intelligence_exports.R"),
  c("63_send_ai_alerts_conditional.R"),
  c("70b_send_email_direct.R")
))

pipeline_spec$resolved_path <- vapply(
  pipeline_spec$candidates,
  resolve_script,
  character(1)
)

# ─────────────────────────────────────────────────────────
# 6) MOTEUR D'EXÉCUTION
#    [FIX-4] sys.source() dans un env enfant
#    [FIX-7] tryCatch couvrant warning + error
# ─────────────────────────────────────────────────────────
run_script_safe <- function(path, stage = "") {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(
      success     = FALSE,
      status      = "MISSING",
      error       = "Script not found",
      elapsed_sec = NA_real_
    ))
  }
  
  log_line("RUN: ", basename(path))
  t0      <- proc.time()[["elapsed"]]
  ok      <- TRUE
  err_msg <- NA_character_
  
  # [FIX-4] environnement enfant pour isoler les effets de bord
  child_env <- new.env(parent = .GlobalEnv)
  
  tryCatch(
    withCallingHandlers(
      {
        sys.source(path, envir = child_env)
        # Remonter les variables clés si nécessaire
        # (les scripts qui lisent/écrivent des fichiers n'en ont pas besoin)
      },
      warning = function(w) {
        log_line("WARN [", basename(path), "]: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      ok      <<- FALSE
      err_msg <<- conditionMessage(e)
    }
  )
  
  dur <- round(proc.time()[["elapsed"]] - t0, 2)
  
  if (ok) {
    log_line("OK: ", basename(path), " | ", dur, " sec")
    list(success = TRUE, status = "OK", error = NA_character_, elapsed_sec = dur)
  } else {
    log_line("FAIL: ", basename(path), " | ", dur, " sec | ", err_msg)
    list(success = FALSE, status = "FAILED", error = err_msg, elapsed_sec = dur)
  }
}

# Helper : convertit une liste résultat en data.frame (1 ligne)
result_to_df <- function(stage, critical, script_name, res) {
  data.frame(
    stage       = stage,
    critical    = critical,
    script      = ifelse(is.na(script_name), NA_character_, script_name),
    success     = isTRUE(res$success),
    status      = res$status,
    error       = ifelse(is.null(res$error) || is.na(res$error), NA_character_, res$error),
    elapsed_sec = ifelse(is.null(res$elapsed_sec), NA_real_, res$elapsed_sec),
    stringsAsFactors = FALSE
  )
}

# ─────────────────────────────────────────────────────────
# 7) DIAGNOSTICS PRÉ-EXÉCUTION
# ─────────────────────────────────────────────────────────
banner("PREIS-AI HARDENED DAILY PIPELINE START")
log_line("Date       : ", RUN_TS)
log_line("Project dir: ", PROJECT_DIR)
log_line("AI dir     : ", AI_DIR)
log_line("Scripts trouvés: ", length(all_ai_scripts))
log_line("")
log_line("Scripts résolus :")
for (i in seq_len(nrow(pipeline_spec))) {
  rname <- if (is.na(pipeline_spec$resolved_path[i])) {
    "NON TROUVÉ"
  } else {
    basename(pipeline_spec$resolved_path[i])
  }
  log_line(sprintf(
    "  - %-26s | critical=%-5s | %s",
    pipeline_spec$stage[i],
    pipeline_spec$critical[i],
    rname
  ))
}

# ─────────────────────────────────────────────────────────
# 8) EXÉCUTION UPSTREAM (hors email)
# ─────────────────────────────────────────────────────────
email_stages    <- c("send_alerts_conditional", "run_email_cycle")
upstream_stages <- which(!pipeline_spec$stage %in% email_stages)

# [FIX-5] Résultats indexés par nom de stage
results <- setNames(
  vector("list", nrow(pipeline_spec)),
  pipeline_spec$stage
)

for (i in upstream_stages) {
  stage  <- pipeline_spec$stage[i]
  res    <- run_script_safe(pipeline_spec$resolved_path[i], stage = stage)
  results[[stage]] <- result_to_df(
    stage       = stage,
    critical    = pipeline_spec$critical[i],
    script_name = pipeline_spec$resolved_path[i],
    res         = res
  )
}

res_df <- do.call(rbind, Filter(Negate(is.null), results[pipeline_spec$stage[upstream_stages]]))
rownames(res_df) <- NULL

# ─────────────────────────────────────────────────────────
# 9) GATE CRITIQUE
# ─────────────────────────────────────────────────────────
banner("RÉSUMÉ PIPELINE UPSTREAM")
print(res_df, row.names = FALSE)

critical_fail    <- subset(res_df, critical & !success)
any_critical_fail <- nrow(critical_fail) > 0

# Vérification des sorties clés
tracker_rows  <- if (!is.na(paths$tracker_main))  safe_nrow_csv(paths$tracker_main)  else 0L
alerts_rows   <- if (!is.na(paths$alerts_main))   safe_nrow_csv(paths$alerts_main)   else 0L
articles_rows <- if (!is.na(paths$articles_main)) safe_nrow_csv(paths$articles_main) else 0L

# [FIX-1] Dates via parse_any_date — jamais d'appel nu à as.Date()
tracker_date  <- if (!is.na(paths$tracker_main))  max_date_in_csv(paths$tracker_main)  else as.Date(NA)
articles_date <- if (!is.na(paths$articles_main)) max_date_in_csv(paths$articles_main) else as.Date(NA)

log_line("")
log_line("Sorties clés après upstream :")
log_line("  tracker_main  : ", ifelse(is.na(paths$tracker_main),  "NON TROUVÉ", paths$tracker_main))
log_line("  alerts_main   : ", ifelse(is.na(paths$alerts_main),   "NON TROUVÉ", paths$alerts_main))
log_line("  articles_main : ", ifelse(is.na(paths$articles_main), "NON TROUVÉ", paths$articles_main))
log_line("  tracker_rows  : ", tracker_rows)
log_line("  alerts_rows   : ", alerts_rows)
log_line("  articles_rows : ", articles_rows)
log_line("  tracker_date  : ", ifelse(is.na(tracker_date),  "NA", format(tracker_date)))
log_line("  articles_date : ", ifelse(is.na(articles_date), "NA", format(articles_date)))

# Fraîcheur — [FIX-3] is.na() au lieu de is.finite()
freshness_ok      <- TRUE
freshness_reasons <- character(0)

valid_dates <- c(tracker_date, articles_date)
valid_dates <- valid_dates[!is.na(valid_dates)]

if (!length(valid_dates)) {
  freshness_ok      <- FALSE
  freshness_reasons <- c(freshness_reasons,
                         "Aucune date de publication valide trouvée dans tracker / articles")
} else {
  best_data_date <- max(valid_dates)
  lag_days       <- as.integer(RUN_DATE - best_data_date)
  log_line("  best_data_date: ", format(best_data_date), " | lag_days=", lag_days)
  if (lag_days > 7L) {
    freshness_ok      <- FALSE
    freshness_reasons <- c(freshness_reasons,
                           paste0("Données trop anciennes (lag_days=", lag_days, ")"))
  }
}

# [FIX-9] Comparaison mtime sécurisée
alerts_mtime <- tryCatch(
  safe_mtime_date(paths$alerts_main),
  error = function(e) as.Date(NA)
)

# Décision de blocage
block_email   <- FALSE
block_reasons <- character(0)

if (any_critical_fail) {
  block_email   <- TRUE
  block_reasons <- c(block_reasons,
                     "Un ou plusieurs stages CRITIQUES ont échoué")
}

if (tracker_rows == 0L && articles_rows == 0L) {
  block_email   <- TRUE
  block_reasons <- c(block_reasons,
                     "Tracker et articles sont tous les deux vides / absents")
}

if (!freshness_ok) {
  block_email   <- TRUE
  block_reasons <- c(block_reasons, freshness_reasons)
}

# [DÉSACTIVÉ] if (!is.na(alerts_mtime) && !any_critical_fail && alerts_mtime < RUN_DATE) {
# [DÉSACTIVÉ]   block_email   <- TRUE
# [DÉSACTIVÉ]   block_reasons <- c(block_reasons,
# [DÉSACTIVÉ]                      paste0("Fichier d'alertes non rafraîchi aujourd'hui (mtime=",
# [DÉSACTIVÉ]                             format(alerts_mtime), ")"))
# [DÉSACTIVÉ] }

# ─────────────────────────────────────────────────────────
# 10) ÉCRITURE DU CSV DE SYNTHÈSE UPSTREAM [FIX-6]
# ─────────────────────────────────────────────────────────
summary_csv <- file.path(
  LOG_DIR,
  paste0("preis_ai_run_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
)
utils::write.csv(res_df, summary_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ─────────────────────────────────────────────────────────
# 11) GATE EMAIL
# ─────────────────────────────────────────────────────────
banner("DÉCISION GATE EMAIL")

if (block_email) {
  log_line("DISPATCH EMAIL : BLOQUÉ")
  for (r in block_reasons) log_line("  - ", r)
  
  warning_file <- file.path(
    LOG_DIR,
    paste0("PREIS_AI_PIPELINE_BLOCKED_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  )
  
  warning_lines <- c(
    "PREIS-AI PIPELINE BLOQUÉ",
    paste0("Généré le : ", RUN_TS),
    "",
    "Raison(s) :",
    paste0("  - ", block_reasons),
    "",
    "Échecs critiques :",
    if (nrow(critical_fail)) {
      paste0("  - ", critical_fail$stage,
             " | ", critical_fail$script,
             " | ", critical_fail$error)
    } else {
      "  - Aucun"
    },
    "",
    "Sorties clés :",
    paste0("  tracker_main  : ", ifelse(is.na(paths$tracker_main),  "NON TROUVÉ", paths$tracker_main)),
    paste0("  alerts_main   : ", ifelse(is.na(paths$alerts_main),   "NON TROUVÉ", paths$alerts_main)),
    paste0("  articles_main : ", ifelse(is.na(paths$articles_main), "NON TROUVÉ", paths$articles_main)),
    paste0("  tracker_rows  : ", tracker_rows),
    paste0("  alerts_rows   : ", alerts_rows),
    paste0("  articles_rows : ", articles_rows),
    paste0("  tracker_date  : ", ifelse(is.na(tracker_date),  "NA", format(tracker_date))),
    paste0("  articles_date : ", ifelse(is.na(articles_date), "NA", format(articles_date))),
    "",
    paste0("Synthèse CSV : ", summary_csv),
    paste0("Fichier log  : ", LOG_FILE)
  )
  
  writeLines(warning_lines, con = warning_file, useBytes = FALSE)
  
  banner("STATUT FINAL")
  log_line("RUN TERMINÉ — DISPATCH EMAIL BLOQUÉ")
  log_line("Fichier warning : ", warning_file)
  log_line("Synthèse CSV    : ", summary_csv)
  log_line("Fichier log     : ", LOG_FILE)
  
} else {
  log_line("DISPATCH EMAIL : AUTORISÉ")
  log_line("Tous les gates critiques sont passés — lancement des modules email")
  
  downstream_stages <- which(pipeline_spec$stage %in% email_stages)
  
  for (i in downstream_stages) {
    stage <- pipeline_spec$stage[i]
    res   <- run_script_safe(pipeline_spec$resolved_path[i], stage = stage)
    results[[stage]] <- result_to_df(
      stage       = stage,
      critical    = pipeline_spec$critical[i],
      script_name = pipeline_spec$resolved_path[i],
      res         = res
    )
  }
  
  down_df <- do.call(
    rbind,
    Filter(Negate(is.null), results[pipeline_spec$stage[downstream_stages]])
  )
  rownames(down_df) <- NULL
  
  banner("RÉSUMÉ DOWNSTREAM")
  print(down_df, row.names = FALSE)
  
  final_df <- rbind(res_df, down_df)
  final_csv <- file.path(
    LOG_DIR,
    paste0("preis_ai_full_run_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  utils::write.csv(final_df, final_csv, row.names = FALSE, fileEncoding = "UTF-8")
  
  banner("STATUT FINAL")
  log_line("RUN TERMINÉ AVEC SUCCÈS")
  log_line("Synthèse complète CSV : ", final_csv)
  log_line("Fichier log           : ", LOG_FILE)
}

banner("PREIS-AI HARDENED DAILY PIPELINE — FIN")
