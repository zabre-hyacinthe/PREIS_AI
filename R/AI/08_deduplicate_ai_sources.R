############################################################
# PREIS-AI OBSERVATORY
# 08_deduplicate_ai_sources.R
# Step 7 — Scientific controlled deduplication
# CORRECTED VERSION WITH DATE PRESERVATION
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

ai_col_types_merged <- readr::cols(
  record_origin = readr::col_character(),
  source_name = readr::col_character(),
  query_scope = readr::col_character(),
  query_string = readr::col_character(),
  title = readr::col_character(),
  abstract = readr::col_character(),
  text_all = readr::col_character(),
  pub_year = readr::col_integer(),
  year = readr::col_integer(),
  publication_date_raw = readr::col_character(),
  publication_date_source = readr::col_character(),
  publication_date = readr::col_character(),
  date_precision = readr::col_character(),
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
  pdf_url = readr::col_character(),
  rec_id_provisional = readr::col_character(),
  merge_batch = readr::col_character(),
  source_file = readr::col_character()
)

normalize_text_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("<[^>]+>", " ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

normalize_blank <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN")] <- NA_character_
  x
}

date_precision_rank <- function(x) {
  case_when(
    x == "day" ~ 4L,
    x == "month" ~ 3L,
    x == "year" ~ 2L,
    x == "unknown" ~ 1L,
    TRUE ~ 0L
  )
}

deduplicate_ai_sources <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Deduplicating merged AI records...")
  
  in_file      <- file.path(project_root, "data", "clean", "ai", "ai_multisource_merged.csv")
  out_file     <- file.path(project_root, "data", "clean", "ai", "ai_multisource_deduplicated.csv")
  qc_file      <- file.path(project_root, "data", "outputs", "ai", "ai_dedup_qc.csv")
  removed_file <- file.path(project_root, "data", "outputs", "ai", "ai_dedup_removed_records.csv")
  
  dat <- read_csv(in_file, show_col_types = FALSE, col_types = ai_col_types_merged) %>%
    mutate(
      doi = dplyr::coalesce(doi, ""),
      pmid = dplyr::coalesce(pmid, ""),
      pmcid = dplyr::coalesce(pmcid, ""),
      title = dplyr::coalesce(title, ""),
      abstract = dplyr::coalesce(abstract, ""),
      journal = dplyr::coalesce(journal, ""),
      authors = dplyr::coalesce(authors, ""),
      publication_date_raw = normalize_blank(publication_date_raw),
      publication_date = normalize_blank(publication_date),
      date_precision = dplyr::coalesce(normalize_blank(date_precision), "unknown"),
      
      doi_key = ifelse(doi == "", NA_character_, str_to_lower(str_squish(doi))),
      pmid_key = ifelse(pmid == "", NA_character_, str_squish(pmid)),
      pmcid_key = ifelse(pmcid == "", NA_character_, str_to_upper(str_squish(pmcid))),
      title_key = ifelse(title == "", NA_character_, normalize_text_key(title)),
      text_key = ifelse(text_all == "", NA_character_, normalize_text_key(text_all)),
      
      source_priority = case_when(
        source_name == "PubMed" ~ 1L,
        source_name == "EuropePMC" ~ 2L,
        source_name == "arXiv" ~ 3L,
        source_name == "medRxiv" ~ 4L,
        source_name == "bioRxiv" ~ 5L,
        TRUE ~ 9L
      ),
      
      completeness_score =
        as.integer(title != "") +
        as.integer(abstract != "") +
        as.integer(doi != "") +
        as.integer(pmid != "") +
        as.integer(pmcid != "") +
        as.integer(journal != "") +
        as.integer(authors != "") +
        as.integer(!is.na(publication_date_raw)) +
        as.integer(!is.na(publication_date)),
      
      precision_score = date_precision_rank(date_precision)
    )
  
    dat_ranked <- dat %>%
    arrange(
      desc(precision_score),
      desc(completeness_score),
      source_priority,
      desc(pub_year),
      title
    ) %>%
    mutate(row_id_internal = row_number())
  
  # Stage 1: DOI
  stage1 <- dat_ranked %>%
    mutate(dedup_key_stage1 = ifelse(!is.na(doi_key), paste0("doi:", doi_key), NA_character_))
  
  keep1 <- stage1 %>%
    group_by(dedup_key_stage1) %>%
    mutate(rank_in_group = row_number()) %>%
    ungroup()
  
  kept_after_doi <- keep1 %>%
    filter(is.na(dedup_key_stage1) | rank_in_group == 1)
  
  removed_doi <- keep1 %>%
    filter(!is.na(dedup_key_stage1) & rank_in_group > 1) %>%
    mutate(dedup_stage = "DOI")
  
  # Stage 2: PMID
  stage2 <- kept_after_doi %>%
    mutate(dedup_key_stage2 = ifelse(!is.na(pmid_key), paste0("pmid:", pmid_key), NA_character_))
  
  keep2 <- stage2 %>%
    group_by(dedup_key_stage2) %>%
    mutate(rank_in_group2 = row_number()) %>%
    ungroup()
  
  kept_after_pmid <- keep2 %>%
    filter(is.na(dedup_key_stage2) | rank_in_group2 == 1)
  
  removed_pmid <- keep2 %>%
    filter(!is.na(dedup_key_stage2) & rank_in_group2 > 1) %>%
    mutate(dedup_stage = "PMID")
  
  # Stage 3: PMCID
  stage3 <- kept_after_pmid %>%
    mutate(dedup_key_stage3 = ifelse(!is.na(pmcid_key), paste0("pmcid:", pmcid_key), NA_character_))
  
  keep3 <- stage3 %>%
    group_by(dedup_key_stage3) %>%
    mutate(rank_in_group3 = row_number()) %>%
    ungroup()
  
  kept_after_pmcid <- keep3 %>%
    filter(is.na(dedup_key_stage3) | rank_in_group3 == 1)
  
  removed_pmcid <- keep3 %>%
    filter(!is.na(dedup_key_stage3) & rank_in_group3 > 1) %>%
    mutate(dedup_stage = "PMCID")
  
  # Stage 4: title fallback
  stage4 <- kept_after_pmcid %>%
    mutate(
      dedup_key_stage4 = case_when(
        !is.na(title_key) & nchar(title_key) >= 25 ~ paste0("title:", title_key),
        TRUE ~ NA_character_
      )
    )
  
  keep4 <- stage4 %>%
    group_by(dedup_key_stage4) %>%
    mutate(rank_in_group4 = row_number()) %>%
    ungroup()
  
  deduplicated <- keep4 %>%
    filter(is.na(dedup_key_stage4) | rank_in_group4 == 1) %>%
    select(
      record_origin, source_name, query_scope, query_string,
      title, abstract, text_all,
      pub_year, year,
      publication_date_raw, publication_date_source, publication_date, date_precision,
      journal, doi, pmid, pmcid,
      authors, article_type, source,
      is_open_access, cited_by_count, url, pdf_url,
      rec_id_provisional, merge_batch, source_file
    )
  
  removed_title <- keep4 %>%
    filter(!is.na(dedup_key_stage4) & rank_in_group4 > 1) %>%
    mutate(dedup_stage = "TITLE_FALLBACK")
  
  removed_all <- bind_rows(
    removed_doi,
    removed_pmid,
    removed_pmcid,
    removed_title
  ) %>%
    select(
      dedup_stage,
      record_origin, source_name, query_scope,
      title, pub_year, year,
      publication_date_raw, publication_date, date_precision,
      doi, pmid, pmcid,
      rec_id_provisional, merge_batch
    )
  
  write_csv(deduplicated, out_file, na = "")
  write_csv(removed_all, removed_file, na = "")
  
  qc <- tibble(
    metric = c(
      "n_input_merged",
      "n_output_deduplicated",
      "n_removed_total",
      "n_removed_doi",
      "n_removed_pmid",
      "n_removed_pmcid",
      "n_removed_title_fallback",
      "n_unique_doi_output",
      "n_unique_pmid_output",
      "n_unique_pmcid_output",
      "n_with_publication_date_raw_output",
      "n_with_publication_date_output",
      "n_day_precision_output"
    ),
    value = c(
      nrow(dat),
      nrow(deduplicated),
      nrow(removed_all),
      nrow(removed_doi),
      nrow(removed_pmid),
      nrow(removed_pmcid),
      nrow(removed_title),
      n_distinct(deduplicated$doi[deduplicated$doi != ""]),
      n_distinct(deduplicated$pmid[deduplicated$pmid != ""]),
      n_distinct(deduplicated$pmcid[deduplicated$pmcid != ""]),
      sum(!is.na(deduplicated$publication_date_raw) & deduplicated$publication_date_raw != "", na.rm = TRUE),
      sum(!is.na(deduplicated$publication_date) & deduplicated$publication_date != "", na.rm = TRUE),
      sum(deduplicated$date_precision == "day", na.rm = TRUE)
    )
  )
  
  write_csv(qc, qc_file)
  
  message("[PREIS-AI] Deduplication completed.")
  print(qc)
  
  invisible(list(
    deduplicated = deduplicated,
    removed = removed_all,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  deduplicate_ai_sources("D:/PREIS_AI")
}