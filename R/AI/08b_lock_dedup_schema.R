############################################################
# PREIS-AI OBSERVATORY
# 08b_lock_dedup_schema.R
# Step 7-bis — Lock schema after deduplication
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

ai_dedup_col_types <- readr::cols(
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
  merge_batch = readr::col_character()
)

ai_removed_col_types <- readr::cols(
  dedup_stage = readr::col_character(),
  record_origin = readr::col_character(),
  source_name = readr::col_character(),
  query_scope = readr::col_character(),
  title = readr::col_character(),
  year = readr::col_integer(),
  doi = readr::col_character(),
  pmid = readr::col_character(),
  pmcid = readr::col_character(),
  rec_id_provisional = readr::col_character(),
  merge_batch = readr::col_character()
)

lock_dedup_schema <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Locking deduplicated schema...")
  
  dedup_file   <- file.path(project_root, "data", "clean", "ai", "ai_multisource_deduplicated.csv")
  removed_file <- file.path(project_root, "data", "outputs", "ai", "ai_dedup_removed_records.csv")
  qc_file      <- file.path(project_root, "data", "outputs", "ai", "ai_dedup_schema_qc.csv")
  
  dedup <- read_csv(dedup_file, show_col_types = FALSE, col_types = ai_dedup_col_types) %>%
    mutate(across(where(is.character), ~coalesce(., "")))
  
  removed <- read_csv(removed_file, show_col_types = FALSE, col_types = ai_removed_col_types) %>%
    mutate(across(where(is.character), ~coalesce(., "")))
  
  write_csv(dedup, dedup_file, na = "")
  write_csv(removed, removed_file, na = "")
  
  qc <- tibble(
    dataset = c("deduplicated", "removed"),
    n_rows = c(nrow(dedup), nrow(removed)),
    pmid_character = c(is.character(dedup$pmid), is.character(removed$pmid)),
    year_integer = c(is.integer(dedup$year), is.integer(removed$year))
  )
  
  write_csv(qc, qc_file)
  
  message("[PREIS-AI] Dedup schema locked.")
  print(qc)
  
  invisible(list(
    deduplicated = dedup,
    removed = removed,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  lock_dedup_schema("D:/PREIS_AI")
}