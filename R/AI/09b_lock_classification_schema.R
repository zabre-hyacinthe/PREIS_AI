############################################################
# PREIS-AI OBSERVATORY
# 09b_lock_classification_schema.R
# Step 8-bis — Lock schema for classified stage 1
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

ai_classified_col_types <- readr::cols(
  record_origin = readr::col_character(),
  source_name = readr::col_character(),
  query_scope = readr::col_character(),
  title = readr::col_character(),
  abstract = readr::col_character(),
  text_all = readr::col_character(),
  year = readr::col_integer(),
  journal = readr::col_character(),
  doi = readr::col_character(),
  pmid = readr::col_character(),
  pmcid = readr::col_character(),
  authors = readr::col_character(),
  article_type = readr::col_character(),
  source = readr::col_character(),
  is_open_access = readr::col_character(),
  cited_by_count = readr::col_integer(),
  url = readr::col_character(),
  rec_id_provisional = readr::col_character(),
  merge_batch = readr::col_character(),
  text_for_classification = readr::col_character(),
  method_hits = readr::col_character(),
  application_hits = readr::col_character(),
  method_categories = readr::col_character(),
  application_categories = readr::col_character(),
  method_family_primary = readr::col_character(),
  application_type_primary = readr::col_character(),
  n_method_hits = readr::col_integer(),
  n_application_hits = readr::col_integer(),
  has_method_signal = readr::col_logical(),
  has_application_signal = readr::col_logical()
)

lock_classification_schema <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Locking classification schema...")
  
  in_file <- file.path(project_root, "data", "enriched", "ai", "ai_classified_stage1.csv")
  qc_file <- file.path(project_root, "data", "outputs", "ai", "ai_classification_stage1_schema_qc.csv")
  
  x <- read_csv(in_file, show_col_types = FALSE, col_types = ai_classified_col_types) %>%
    mutate(across(where(is.character), ~coalesce(., "")))
  
  write_csv(x, in_file, na = "")
  
  qc <- tibble(
    dataset = "ai_classified_stage1",
    n_rows = nrow(x),
    method_family_primary_character = is.character(x$method_family_primary),
    application_type_primary_character = is.character(x$application_type_primary),
    year_integer = is.integer(x$year),
    pmid_character = is.character(x$pmid)
  )
  
  write_csv(qc, qc_file)
  
  message("[PREIS-AI] Classification schema locked.")
  print(qc)
  
  invisible(list(data = x, qc = qc))
}

if (sys.nframe() == 0) {
  lock_classification_schema("D:/PREIS_AI")
}