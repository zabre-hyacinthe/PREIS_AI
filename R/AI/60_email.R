# ============================================================
# PREIS-AI — CORE EMAIL ENGINE
# File: D:/PREIS_AI/R/AI/60_email.R
# ============================================================

suppressPackageStartupMessages({
  library(blastula)
  library(readr)
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

ai_email_env_ok <- function() {
  vars <- c("SMTP_USER", "SMTP_PASS", "ALERT_FROM")
  vals <- Sys.getenv(vars, unset = "")
  all(nzchar(vals))
}

ai_email_env_report <- function() {
  tibble::tibble(
    variable = c("SMTP_USER", "SMTP_PASS", "ALERT_FROM", "SMTP_HOST", "SMTP_PORT"),
    value = c(
      Sys.getenv("SMTP_USER"),
      paste0("<len:", nchar(Sys.getenv("SMTP_PASS")), ">"),
      Sys.getenv("ALERT_FROM"),
      Sys.getenv("SMTP_HOST", unset = "smtp.gmail.com"),
      Sys.getenv("SMTP_PORT", unset = "465")
    )
  )
}

ai_compose_email <- function(body_text) {
  blastula::compose_email(
    body = blastula::md(body_text)
  )
}

ai_smtp_send <- function(email_obj, to, subject, verbose = TRUE) {
  if (!ai_email_env_ok()) {
    stop("Missing SMTP_USER, SMTP_PASS, or ALERT_FROM environment variables.", call. = FALSE)
  }
  
  smtp_host <- Sys.getenv("SMTP_HOST", unset = "smtp.gmail.com")
  smtp_port <- as.numeric(Sys.getenv("SMTP_PORT", unset = "465"))
  from_addr <- Sys.getenv("ALERT_FROM")
  
  if (verbose) {
    message("Sending email to: ", to)
  }
  
  blastula::smtp_send(
    email = email_obj,
    to = to,
    from = from_addr,
    subject = subject,
    credentials = blastula::creds_envvar(
      user = Sys.getenv("SMTP_USER"),
      pass_envvar = "SMTP_PASS",
      provider = "gmail",
      host = smtp_host,
      port = smtp_port,
      use_ssl = TRUE
    )
  )
  
  invisible(TRUE)
}

ai_test_email <- function(to = Sys.getenv("SMTP_USER")) {
  email_obj <- ai_compose_email(
    paste0(
      "PREIS-AI SMTP test\n\n",
      "Date: ", Sys.Date(), "\n",
      "If you received this email, SMTP is working."
    )
  )
  
  ai_smtp_send(
    email_obj = email_obj,
    to = to,
    subject = paste0("PREIS-AI SMTP test - ", Sys.Date())
  )
}