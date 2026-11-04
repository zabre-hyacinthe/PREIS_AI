# ============================================================
# PREIS-AI — RUN EMAIL CYCLE
# File: D:/PREIS_AI/R/AI/70_run_ai_email_cycle.R
# Safe orchestration layer aligned with 63_send_ai_alerts_conditional.R
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
safe_dir_create <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_write_lines <- function(text, path) {
  safe_dir_create(dirname(path))
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
  invisible(path)
}

append_log_line <- function(log_file, ...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  safe_dir_create(dirname(log_file))
  cat(msg, "\n", file = log_file, append = TRUE)
}

safe_nrow <- function(x) {
  if (is.null(x) || !is.data.frame(x)) return(0L)
  nrow(x)
}

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
get_ai_email_cycle_paths <- function(root = "D:/PREIS_AI_CLEAN") {
  list(
    root = root,
    r_ai_dir = file.path(root, "R", "AI"),
    log_dir = file.path(root, "logs"),
    cycle_log_file = file.path(root, "logs", "ai_email_cycle.log"),
    cycle_qc_file = file.path(root, "dashboard", "data", "dashboard", "ai_email_cycle_qc.csv"),
    dispatch_rds_file = file.path(root, "dashboard", "data", "dashboard", "ai_email_dispatch_preview.rds")
  )
}

# ------------------------------------------------------------
# Main cycle
# ------------------------------------------------------------
run_ai_email_cycle <- function(send_now = TRUE,
                               root = "D:/PREIS_AI_CLEAN",
                               keep_zero_all = TRUE,
                               preview_chars = 1000,
                               save_dispatch_preview = TRUE) {
  p <- get_ai_email_cycle_paths(root)
  
  safe_dir_create(p$log_dir)
  
  append_log_line(p$cycle_log_file, "START run_ai_email_cycle")
  append_log_line(p$cycle_log_file, "Root:", root)
  append_log_line(p$cycle_log_file, "send_now:", send_now)
  
  source(file.path(p$r_ai_dir, "60_email.R"))
  source(file.path(p$r_ai_dir, "63_send_ai_alerts_conditional.R"))
  
  append_log_line(p$cycle_log_file, "Sourced 60_email.R and 63_send_ai_alerts_conditional.R")
  
  diag_obj <- NULL
  dispatch <- NULL
  qc_tbl <- NULL
  
  tryCatch({
    diag_obj <- print_ai_email_sender_diagnostics(root = root)
    append_log_line(
      p$cycle_log_file,
      "Diagnostics loaded.",
      "alerts_raw:", safe_nrow(diag_obj$alerts_raw),
      "alerts:", safe_nrow(diag_obj$alerts),
      "articles:", safe_nrow(diag_obj$articles),
      "recipients:", safe_nrow(diag_obj$recipients),
      "sent_log:", safe_nrow(diag_obj$sent_log)
    )
    
    dispatch <- send_ai_alerts_conditional(
      send_now = send_now,
      root = root,
      preview_chars = preview_chars,
      use_sent_log = TRUE,
      keep_zero_all = keep_zero_all
    )
    
    n_packages <- length(dispatch)
    
    n_alerts_total <- if (n_packages == 0) {
      0
    } else {
      sum(vapply(dispatch, function(x) as.numeric(x$n_alerts %||% 0), numeric(1)), na.rm = TRUE)
    }
    
    n_nonzero_packages <- if (n_packages == 0) {
      0
    } else {
      sum(vapply(dispatch, function(x) as.numeric(x$n_alerts %||% 0), numeric(1)) > 0, na.rm = TRUE)
    }
    
    qc_tbl <- tibble(
      run_date = as.character(Sys.Date()),
      run_time = as.character(Sys.time()),
      send_now = as.character(send_now),
      alerts_file = diag_obj$paths$alerts_file %||% NA_character_,
      articles_file = diag_obj$paths$articles_file %||% NA_character_,
      recipients_file = diag_obj$paths$recipients_file %||% NA_character_,
      sent_log_file = diag_obj$paths$sent_log_file %||% NA_character_,
      n_alerts_raw = safe_nrow(diag_obj$alerts_raw),
      n_alerts_harmonized = safe_nrow(diag_obj$alerts),
      n_articles = safe_nrow(diag_obj$articles),
      n_recipients = safe_nrow(diag_obj$recipients),
      n_sent_log_rows = safe_nrow(diag_obj$sent_log),
      n_dispatch_packages = n_packages,
      n_dispatch_packages_nonzero = n_nonzero_packages,
      n_alerts_total_in_dispatch = n_alerts_total,
      status = "ok"
    )
    
    readr::write_csv(qc_tbl, p$cycle_qc_file)
    
    if (save_dispatch_preview) {
      saveRDS(dispatch, p$dispatch_rds_file)
    }
    
    append_log_line(
      p$cycle_log_file,
      "SUCCESS.",
      "packages:", n_packages,
      "nonzero_packages:", n_nonzero_packages,
      "alerts_total:", n_alerts_total
    )
    
  }, error = function(e) {
    append_log_line(p$cycle_log_file, "ERROR:", conditionMessage(e))
    
    qc_tbl <<- tibble(
      run_date = as.character(Sys.Date()),
      run_time = as.character(Sys.time()),
      send_now = as.character(send_now),
      alerts_file = if (!is.null(diag_obj)) diag_obj$paths$alerts_file %||% NA_character_ else NA_character_,
      articles_file = if (!is.null(diag_obj)) diag_obj$paths$articles_file %||% NA_character_ else NA_character_,
      recipients_file = if (!is.null(diag_obj)) diag_obj$paths$recipients_file %||% NA_character_ else NA_character_,
      sent_log_file = if (!is.null(diag_obj)) diag_obj$paths$sent_log_file %||% NA_character_ else NA_character_,
      n_alerts_raw = if (!is.null(diag_obj)) safe_nrow(diag_obj$alerts_raw) else NA_real_,
      n_alerts_harmonized = if (!is.null(diag_obj)) safe_nrow(diag_obj$alerts) else NA_real_,
      n_articles = if (!is.null(diag_obj)) safe_nrow(diag_obj$articles) else NA_real_,
      n_recipients = if (!is.null(diag_obj)) safe_nrow(diag_obj$recipients) else NA_real_,
      n_sent_log_rows = if (!is.null(diag_obj)) safe_nrow(diag_obj$sent_log) else NA_real_,
      n_dispatch_packages = NA_real_,
      n_dispatch_packages_nonzero = NA_real_,
      n_alerts_total_in_dispatch = NA_real_,
      status = paste0("error: ", conditionMessage(e))
    )
    
    readr::write_csv(qc_tbl, p$cycle_qc_file)
    stop(e)
  })
  
  invisible(list(
    diagnostics = diag_obj,
    dispatch = dispatch,
    qc = qc_tbl,
    paths = p
  ))
}

# ------------------------------------------------------------
# Convenience wrappers
# ------------------------------------------------------------
preview_ai_email_cycle <- function(root = "D:/PREIS_AI_CLEAN",
                                   keep_zero_all = TRUE,
                                   preview_chars = 1000) {
  run_ai_email_cycle(
    send_now = FALSE,
    root = root,
    keep_zero_all = keep_zero_all,
    preview_chars = preview_chars,
    save_dispatch_preview = TRUE
  )
}

send_ai_email_cycle <- function(root = "D:/PREIS_AI_CLEAN",
                                keep_zero_all = TRUE,
                                preview_chars = 1000) {
  run_ai_email_cycle(
    send_now = TRUE,
    root = root,
    keep_zero_all = keep_zero_all,
    preview_chars = preview_chars,
    save_dispatch_preview = TRUE
  )
}

# ── Exécution automatique ──────────────────────────────
run_ai_email_cycle(
  send_now = TRUE,
  root     = "D:/PREIS_AI_CLEAN"
)
