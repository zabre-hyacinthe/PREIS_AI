############################################################
# PREIS-AI OBSERVATORY
# 06b_lock_ai_schema.R
# Step 6A-bis — Lock schema for standardized AI files
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
})

ai_col_types <- cols(
  record_origin = col_character(),
  source_name = col_character(),
  query_scope = col_character(),
  title = col_character(),
  abstract = col_character(),
  text_all = col_character(),
  year = col_integer(),
  journal = col_character(),
  doi = col_character(),
  pmid = col_character(),
  pmcid = col_character(),
  authors = col_character(),
  article_type = col_character(),
  source = col_character(),
  is_open_access = col_character(),
  cited_by_count = col_integer(),
  url = col_character(),
  rec_id_provisional = col_character()
)

read_ai_standardized <- function(path) {
  read_csv(path, show_col_types = FALSE, col_types = ai_col_types)
}

lock_ai_schema <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Locking standardized schema...")
  
  clean_dir <- file.path(project_root, "data", "clean", "ai")
  out_qc    <- file.path(project_root, "data", "outputs", "ai", "ai_schema_lock_qc.csv")
  
  epmc_file <- file.path(clean_dir, "europepmc_ai_standardized.csv")
  pubm_file <- file.path(clean_dir, "pubmed_ai_standardized.csv")
  
  epmc <- read_ai_standardized(epmc_file) %>%
    mutate(across(where(is.character), ~coalesce(., "")))
  
  pubm <- read_ai_standardized(pubm_file) %>%
    mutate(across(where(is.character), ~coalesce(., "")))
  
  # Rewrite locked versions with same names
  write_csv(epmc, epmc_file, na = "")
  write_csv(pubm, pubm_file, na = "")
  
  qc <- tibble(
    dataset = c("EuropePMC", "PubMed"),
    n_rows = c(nrow(epmc), nrow(pubm)),
    pmid_character = c(is.character(epmc$pmid), is.character(pubm$pmid)),
    journal_character = c(is.character(epmc$journal), is.character(pubm$journal)),
    article_type_character = c(is.character(epmc$article_type), is.character(pubm$article_type)),
    cited_by_count_integer = c(is.integer(epmc$cited_by_count), is.integer(pubm$cited_by_count))
  )
  
  write_csv(qc, out_qc)
  
  message("[PREIS-AI] Schema locked.")
  print(qc)
  
  invisible(list(
    europepmc = epmc,
    pubmed = pubm,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  lock_ai_schema("D:/PREIS_AI")
}