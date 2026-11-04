# ============================================================
# PREIS-AI — ADD DISEASE COMPONENT TO DASHBOARD DATASET
# File: D:/PREIS_AI/R/AI/13_add_disease_component_to_dashboard_dataset.R
# ============================================================

source("D:/PREIS_AI/R/AI/11_create_ai_disease_dictionary.R")
source("D:/PREIS_AI/R/AI/12_classify_ai_disease.R")

add_disease_component_to_dashboard_dataset <- function(
    project_dir = "D:/PREIS_AI",
    overwrite_main_dataset = TRUE
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(tibble)
  })
  
  # ------------------------------------------------------------
  # Helpers
  # ------------------------------------------------------------
  safe_read_csv <- function(path) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      return(tibble::tibble())
    }
    tryCatch(
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
      error = function(e) tibble::tibble()
    )
  }
  
  normalize_text <- function(x, default = "") {
    x <- as.character(x)
    x[is.na(x)] <- default
    trimws(x)
  }
  
  get_col_or_default <- function(df, candidates, default = NA_character_) {
    hit <- candidates[candidates %in% names(df)][1]
    if (length(hit) == 0 || is.na(hit)) {
      return(rep(default, nrow(df)))
    }
    df[[hit]]
  }
  
  # ------------------------------------------------------------
  # Paths
  # ------------------------------------------------------------
  dashboard_file <- file.path(
    project_dir, "data", "dashboard", "ai_intelligence_dashboard_dataset.csv"
  )
  
  backup_dir <- file.path(project_dir, "data", "dashboard", "backups")
  
  qc_file <- file.path(project_dir, "data", "dashboard", "ai_disease_component_qc.csv")
  disease_summary_file <- file.path(project_dir, "data", "dashboard", "ai_disease_summary.csv")
  disease_method_file <- file.path(project_dir, "data", "dashboard", "ai_disease_method_summary.csv")
  disease_application_file <- file.path(project_dir, "data", "dashboard", "ai_disease_application_summary.csv")
  disease_rcc_file <- file.path(project_dir, "data", "dashboard", "ai_disease_rcc_summary.csv")
  disease_year_file <- file.path(project_dir, "data", "dashboard", "ai_disease_year_summary.csv")
  
  dict_file <- file.path(project_dir, "config", "ai_disease_dictionary.csv")
  
  dir.create(dirname(dashboard_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(dict_file), recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(dashboard_file)) {
    stop("Dashboard dataset not found: ", dashboard_file, call. = FALSE)
  }
  
  if (!file.exists(dict_file)) {
    create_ai_disease_dictionary(out_file = dict_file)
  }
  
  # ------------------------------------------------------------
  # Read dashboard dataset
  # ------------------------------------------------------------
  dsb <- safe_read_csv(dashboard_file)
  
  if (nrow(dsb) == 0) {
    stop("Dashboard dataset is empty: ", dashboard_file, call. = FALSE)
  }
  
  n_before <- nrow(dsb)
  
  # Remove accidental unnamed junk columns like ...5, ...6, etc.
  dsb <- dsb[, !grepl("^\\.\\.\\.[0-9]+$", names(dsb)), drop = FALSE]
  
  # Backup before overwrite
  backup_file <- file.path(
    backup_dir,
    paste0(
      "ai_intelligence_dashboard_dataset_backup_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
  )
  readr::write_csv(dsb, backup_file, na = "")
  
  # ------------------------------------------------------------
  # Add disease component
  # ------------------------------------------------------------
  dsb2 <- classify_ai_disease(dsb, dict_file = dict_file)
  
  # Standardize some columns for downstream summaries
  dsb2 <- dsb2 %>%
    mutate(
      method_family_primary = normalize_text(
        get_col_or_default(., c("method_family_primary", "method_family"), "Unspecified"),
        "Unspecified"
      ),
      application_type_primary = normalize_text(
        get_col_or_default(., c("application_type_primary", "application_type"), "Unspecified"),
        "Unspecified"
      ),
      country = normalize_text(
        get_col_or_default(., c("country", "country_name", "country_detected"), "Unspecified"),
        "Unspecified"
      ),
      rcc = normalize_text(
        get_col_or_default(., c("rcc"), "Unspecified"),
        "Unspecified"
      ),
      year = suppressWarnings(as.integer(
        get_col_or_default(., c("year"), NA_integer_)
      ))
    )
  
  # ------------------------------------------------------------
  # Overwrite main dashboard dataset if requested
  # ------------------------------------------------------------
  if (isTRUE(overwrite_main_dataset)) {
    readr::write_csv(dsb2, dashboard_file, na = "")
  }
  
  # ------------------------------------------------------------
  # QC
  # ------------------------------------------------------------
  qc <- tibble::tibble(
    metric = c(
      "n_rows_before",
      "n_rows_after",
      "n_disease_detected",
      "n_disease_unspecified",
      "n_unique_disease_primary",
      "n_unique_disease_group",
      "n_unique_pathogen_class"
    ),
    value = c(
      n_before,
      nrow(dsb2),
      sum(dsb2$disease_detected, na.rm = TRUE),
      sum(dsb2$disease_primary == "Unspecified", na.rm = TRUE),
      dplyr::n_distinct(dsb2$disease_primary),
      dplyr::n_distinct(dsb2$disease_group),
      dplyr::n_distinct(dsb2$pathogen_class)
    )
  )
  
  readr::write_csv(qc, qc_file, na = "")
  
  # ------------------------------------------------------------
  # Summaries for dashboard components
  # ------------------------------------------------------------
  disease_summary <- dsb2 %>%
    count(disease_primary, disease_group, pathogen_class, sort = TRUE)
  
  disease_method_summary <- dsb2 %>%
    count(disease_primary, method_family_primary, sort = TRUE)
  
  disease_application_summary <- dsb2 %>%
    count(disease_primary, application_type_primary, sort = TRUE)
  
  disease_rcc_summary <- dsb2 %>%
    count(disease_primary, rcc, sort = TRUE)
  
  disease_year_summary <- dsb2 %>%
    filter(!is.na(year)) %>%
    count(disease_primary, year, sort = TRUE)
  
  readr::write_csv(disease_summary, disease_summary_file, na = "")
  readr::write_csv(disease_method_summary, disease_method_file, na = "")
  readr::write_csv(disease_application_summary, disease_application_file, na = "")
  readr::write_csv(disease_rcc_summary, disease_rcc_file, na = "")
  readr::write_csv(disease_year_summary, disease_year_file, na = "")
  
  message("Disease component added successfully.")
  message("Backup file: ", backup_file)
  message("Updated dashboard dataset: ", dashboard_file)
  
  invisible(list(
    dataset = dsb2,
    qc = qc,
    backup_file = backup_file,
    dashboard_file = dashboard_file,
    disease_summary_file = disease_summary_file,
    disease_method_file = disease_method_file,
    disease_application_file = disease_application_file,
    disease_rcc_file = disease_rcc_file,
    disease_year_file = disease_year_file
  ))
}