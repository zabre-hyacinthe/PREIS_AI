# ============================================================
# PREIS-AI — CONDITIONAL EMAIL SENDER
# POLIO-ENGINE STYLE / PREIS-compatible
# File: D:/PREIS_AI/R/AI/64_send_ai_alerts_production.R
# Corrected to align with PREIS-AI email_intelligence + sent_log_ai
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(blastula)
})
source("D:/PREIS_AI/R/AI/60_email.R")
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------
# 1) HELPERS
# ------------------------------------------------------------
safe_read_csv <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(tibble::tibble())
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) tibble::tibble()
  )
}

safe_nrow <- function(x) {
  if (is.null(x) || !is.data.frame(x)) return(0L)
  nrow(x)
}

first_existing_file <- function(paths) {
  for (p in paths) {
    if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
  }
  NULL
}

normalize_text <- function(x, default = "") {
  x <- as.character(x)
  x[is.na(x)] <- default
  trimws(x)
}

normalize_scope <- function(x) {
  x <- normalize_text(x)
  x[x == ""] <- "All"
  x
}

normalize_email <- function(x) {
  tolower(normalize_text(x))
}

read_text_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

empty_sent_log_tbl <- function() {
  tibble::tibble(
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
  
  if (nrow(old) == 0) {
    return(empty_sent_log_tbl())
  }
  
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

safe_write_sent_log <- function(sent_log_file, sent_df) {
  dir.create(dirname(sent_log_file), recursive = TRUE, showWarnings = FALSE)
  
  old <- read_sent_log_clean(sent_log_file)
  
  if (is.null(sent_df) || nrow(sent_df) == 0) {
    return(invisible(old))
  }
  
  needed <- names(empty_sent_log_tbl())
  for (nm in setdiff(needed, names(sent_df))) {
    sent_df[[nm]] <- NA
  }
  
  sent_df <- sent_df %>%
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
  
  out <- bind_rows(old, sent_df)
  readr::write_csv(out, sent_log_file)
  invisible(out)
}

# ------------------------------------------------------------
# 2) PATHS
# ------------------------------------------------------------
get_preis_ai_paths <- function(root = "D:/PREIS_AI") {
  list(
    root = root,
    ai_email_qc_file = first_existing_file(c(
      file.path(root, "data", "outputs", "ai", "email_intelligence", "ai_email_qc.csv"),
      file.path(root, "dashboard", "data", "dashboard", "ai_email_qc.csv")
    )),
    ai_email_packages_file = first_existing_file(c(
      file.path(root, "data", "outputs", "ai", "email_intelligence", "ai_email_packages.csv"),
      file.path(root, "dashboard", "data", "dashboard", "ai_email_packages.csv")
    )),
    recipients_file = first_existing_file(c(
      file.path(root, "config", "alert_recipients_ai.csv"),
      file.path(root, "dashboard", "data", "dashboard", "alert_recipients.csv"),
      file.path(root, "config", "alert_recipients.csv"),
      file.path(root, "data", "outputs", "ai", "email_intelligence", "alert_recipients.csv")
    )),
    articles_file = first_existing_file(c(
      file.path(root, "dashboard", "data", "dashboard", "articles_ai_tagged.csv"),
      file.path(root, "data", "outputs", "ai", "articles_ai_tagged.csv"),
      file.path(root, "data", "enriched", "ai", "articles_ai_tagged.csv")
    )),
    tracker_file = first_existing_file(c(
      file.path(root, "data", "enriched", "ai", "ai_tracker_final.csv"),
      file.path(root, "dashboard", "data", "dashboard", "ai_tracker_final.csv"),
      file.path(root, "dashboard", "data", "dashboard", "ai_tracker.csv"),
      file.path(root, "data", "outputs", "ai", "ai_tracker.csv"),
      file.path(root, "data", "outputs", "ai", "email_intelligence", "ai_tracker.csv")
    )),
    alerts_file = first_existing_file(c(
      file.path(root, "data", "outputs", "ai", "alerts", "ai_alerts.csv"),
      file.path(root, "dashboard", "data", "dashboard", "alerts.csv"),
      file.path(root, "data", "outputs", "ai", "alerts.csv"),
      file.path(root, "data", "outputs", "ai", "email_intelligence", "alerts.csv")
    )),
    sent_log_file = file.path(root, "dashboard", "data", "dashboard", "sent_log_ai.csv")
  )
}

# ------------------------------------------------------------
# 3) LOAD DATA
# ------------------------------------------------------------
load_ai_email_sender_data <- function(root = "D:/PREIS_AI") {
  p <- get_preis_ai_paths(root)
  
  ai_email_qc <- safe_read_csv(p$ai_email_qc_file)
  ai_email_packages <- safe_read_csv(p$ai_email_packages_file)
  recipients <- safe_read_csv(p$recipients_file)
  articles <- safe_read_csv(p$articles_file)
  tracker <- safe_read_csv(p$tracker_file)
  alerts <- safe_read_csv(p$alerts_file)
  sent_log <- read_sent_log_clean(p$sent_log_file)
  
  if (safe_nrow(ai_email_packages) > 0) {
    ai_email_packages <- ai_email_packages %>%
      mutate(
        scope = normalize_scope(scope),
        subject = as.character(subject),
        recipients = as.character(recipients),
        n_alerts = suppressWarnings(as.numeric(n_alerts)),
        body_file = as.character(body_file)
      ) %>%
      distinct(scope, subject, recipients, n_alerts, body_file, .keep_all = TRUE)
  }
  
  if (safe_nrow(recipients) > 0) {
    nm_col <- if ("name" %in% names(recipients)) "name" else names(recipients)[1]
    em_col <- if ("email" %in% names(recipients)) "email" else NA_character_
    rc_col <- if ("rcc" %in% names(recipients)) "rcc" else NA_character_
    
    recipients <- recipients %>%
      mutate(
        name = as.character(if (!is.na(nm_col)) .data[[nm_col]] else "Recipient"),
        email = as.character(if (!is.na(em_col)) .data[[em_col]] else NA_character_),
        rcc = as.character(if (!is.na(rc_col)) .data[[rc_col]] else "All")
      ) %>%
      select(name, email, rcc) %>%
      mutate(
        rcc = normalize_scope(rcc),
        email = normalize_email(email)
      ) %>%
      filter(!is.na(email), email != "") %>%
      distinct(name, email, rcc, .keep_all = TRUE)
  }
  
  list(
    paths = p,
    ai_email_qc = ai_email_qc,
    ai_email_packages = ai_email_packages,
    recipients = recipients,
    articles = articles,
    tracker = tracker,
    alerts = alerts,
    sent_log = sent_log
  )
}

# ------------------------------------------------------------
# 4) BODY FILE RESOLUTION
# ------------------------------------------------------------
resolve_body_file <- function(body_file, root = "D:/PREIS_AI") {
  if (is.na(body_file) || !nzchar(body_file)) return(NULL)
  
  candidates <- c(
    body_file,
    file.path(root, body_file),
    file.path(root, "dashboard", "data", "dashboard", body_file),
    file.path(root, "data", "outputs", "ai", "email_intelligence", basename(body_file))
  )
  
  first_existing_file(candidates)
}

get_package_body <- function(body_file, root = "D:/PREIS_AI") {
  read_text_file(resolve_body_file(body_file, root))
}

# ------------------------------------------------------------
# 5) FALLBACK BASELINE BODY
# ------------------------------------------------------------
build_ai_baseline_summary <- function(scope_label,
                                      recipient_name,
                                      recipient_email,
                                      data_list) {
  paste0(
    "PREIS_AI — Initial Baseline Summary\n\n",
    "Recipient: ", recipient_name, "\n",
    "Email: ", recipient_email, "\n",
    "Scope: ", scope_label, "\n",
    "Mode: SUMMARY\n\n",
    "This is the initial PREIS_AI baseline message. It summarizes the current stock already available in the AI surveillance intelligence system. ",
    "The full stock is not reproduced in the body of this email in order to keep the message concise and operational.\n\n",
    "Current baseline volumes in scope:\n\n",
    "Articles: ", safe_nrow(data_list$articles), "\n",
    "AI tracker records: ", safe_nrow(data_list$tracker), "\n",
    "Alert-linked AI records: ", safe_nrow(data_list$alerts), "\n\n",
    "Download / access:\n\n",
    "Use the PREIS_AI dashboard or the shared project folder to retrieve the AI baseline files.\n",
    "Next emails will contain only newly detected AI surveillance publications relevant to your scope.\n"
  )
}

# ------------------------------------------------------------
# 6) PREPARE DISPATCH
# ------------------------------------------------------------
prepare_ai_email_dispatch <- function(root = "D:/PREIS_AI") {
  x <- load_ai_email_sender_data(root)
  recipients <- x$recipients
  packages <- x$ai_email_packages
  
  if (safe_nrow(recipients) == 0) {
    stop("No recipients found. Check alert_recipients_ai.csv / alert_recipients.csv", call. = FALSE)
  }
  if (safe_nrow(packages) == 0) {
    stop("No email packages found. Check ai_email_packages.csv", call. = FALSE)
  }
  
  out <- list()
  
  for (i in seq_len(nrow(recipients))) {
    rec_name  <- recipients$name[i]
    rec_email <- recipients$email[i]
    rec_rcc   <- recipients$rcc[i]
    
    desired_scope <- ifelse(toupper(rec_rcc) == "ALL", "All", rec_rcc)
    
    pkg <- packages %>% filter(scope == desired_scope)
    
    if (nrow(pkg) == 0 && toupper(rec_rcc) != "ALL") {
      pkg <- packages %>% filter(scope == "All")
    }
    
    if (nrow(pkg) == 0) next
    
    pkg1 <- pkg[1, , drop = FALSE]
    body_txt <- get_package_body(pkg1$body_file[1], root = root)
    
    if (is.null(body_txt) || !nzchar(body_txt)) {
      body_txt <- build_ai_baseline_summary(
        scope_label = pkg1$scope[1],
        recipient_name = rec_name,
        recipient_email = rec_email,
        data_list = x
      )
    }
    
    out[[length(out) + 1]] <- list(
      recipient_name = rec_name,
      recipient_email = rec_email,
      recipient_rcc = rec_rcc,
      scope = pkg1$scope[1],
      subject = pkg1$subject[1],
      n_alerts = pkg1$n_alerts[1] %||% 0,
      body = body_txt
    )
  }
  
  out
}

# ------------------------------------------------------------
# 7) DIAGNOSTICS
# ------------------------------------------------------------
print_ai_email_sender_diagnostics <- function(root = "D:/PREIS_AI") {
  x <- load_ai_email_sender_data(root)
  
  cat("\n=====================================\n")
  cat("PREIS-AI EMAIL SENDER DIAGNOSTICS\n")
  cat("=====================================\n\n")
  
  cat("Resolved files:\n")
  cat("- ai_email_qc_file      :", x$paths$ai_email_qc_file %||% "NOT FOUND", "\n")
  cat("- ai_email_packages_file:", x$paths$ai_email_packages_file %||% "NOT FOUND", "\n")
  cat("- recipients_file       :", x$paths$recipients_file %||% "NOT FOUND", "\n")
  cat("- articles_file         :", x$paths$articles_file %||% "NOT FOUND", "\n")
  cat("- tracker_file          :", x$paths$tracker_file %||% "NOT FOUND", "\n")
  cat("- alerts_file           :", x$paths$alerts_file %||% "NOT FOUND", "\n")
  cat("- sent_log_file         :", x$paths$sent_log_file %||% "NOT FOUND", "\n\n")
  
  cat("Detected volumes:\n")
  cat("- Articles              :", safe_nrow(x$articles), "\n")
  cat("- Tracker records       :", safe_nrow(x$tracker), "\n")
  cat("- Alert-linked records  :", safe_nrow(x$alerts), "\n")
  cat("- Recipients            :", safe_nrow(x$recipients), "\n")
  cat("- Email packages        :", safe_nrow(x$ai_email_packages), "\n")
  cat("- AI sent log rows      :", safe_nrow(x$sent_log), "\n\n")
  
  invisible(x)
}

# ------------------------------------------------------------
# 8) SEND FUNCTION — POLIO STYLE
# ------------------------------------------------------------
send_ai_alerts_production <- function(send_now = FALSE,
                                      root = "D:/PREIS_AI",
                                      preview_chars = 1200,
                                      use_sent_log = TRUE,
                                      force_send = FALSE) {
  dispatch <- prepare_ai_email_dispatch(root = root)
  x <- load_ai_email_sender_data(root = root)
  
  smtp_user <- Sys.getenv("SMTP_USER")
  smtp_pass <- Sys.getenv("SMTP_PASS")
  alert_from <- Sys.getenv("ALERT_FROM")
  smtp_host <- Sys.getenv("SMTP_HOST", unset = "smtp.gmail.com")
  smtp_port <- Sys.getenv("SMTP_PORT", unset = "465")
  
  cat("\n=====================================\n")
  cat("PREIS-AI PRODUCTION EMAIL DISPATCH\n")
  cat("=====================================\n\n")
  cat("Packages prepared:", length(dispatch), "\n\n")
  
  if (send_now) {
    if (!nzchar(smtp_user) || !nzchar(smtp_pass) || !nzchar(alert_from)) {
      stop("Missing SMTP_USER, SMTP_PASS, or ALERT_FROM environment variables.", call. = FALSE)
    }
  }
  
  already_sent_keys <- character(0)
  
  if (use_sent_log && safe_nrow(x$sent_log) > 0) {
    if (all(c("date_sent", "recipient_email", "scope", "subject") %in% names(x$sent_log))) {
      already_sent_keys <- paste(
        as.character(x$sent_log$date_sent),
        normalize_email(x$sent_log$recipient_email),
        normalize_scope(x$sent_log$scope),
        as.character(x$sent_log$subject),
        sep = "||"
      )
    }
  }
  
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
    
    send_key <- paste(
      as.character(Sys.Date()),
      normalize_email(item$recipient_email),
      normalize_scope(item$scope),
      as.character(item$subject),
      sep = "||"
    )
    
    if (!identical(item$scope, "All") && (is.na(item$n_alerts) || item$n_alerts <= 0)) {
      cat("Skipped: no alerts for scope ", item$scope, "\n\n", sep = "")
      next
    }
    
    if (use_sent_log && !force_send && send_key %in% already_sent_keys) {
      cat("Skipped: already sent today -> ", item$recipient_email, "\n\n", sep = "")
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
      
      sent_rows[[length(sent_rows) + 1]] <- tibble::tibble(
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
      
    }, error = function(e) {
      cat("Send failed -> ", item$recipient_email, "\n", sep = "")
      cat("Reason: ", conditionMessage(e), "\n\n", sep = "")
      
      sent_rows[[length(sent_rows) + 1]] <- tibble::tibble(
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
    })
  }
  
  if (!send_now) {
    message("Preview mode only. No emails sent.")
    return(invisible(dispatch))
  }
  
  if (length(sent_rows) > 0 && use_sent_log) {
    sent_df <- bind_rows(sent_rows)
    safe_write_sent_log(x$paths$sent_log_file, sent_df)
  }
  
  invisible(dispatch)
}