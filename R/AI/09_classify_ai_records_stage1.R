############################################################
# PREIS-AI OBSERVATORY
# 09_classify_ai_records_stage1.R
# Step 8 — Stage 1 dictionary-based AI classification
# FAST VECTORIZED VERSION
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
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

make_pattern <- function(keywords) {
  keywords <- unique(keywords)
  keywords <- keywords[!is.na(keywords) & keywords != ""]
  keywords <- stringr::str_replace_all(keywords, "([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1")
  paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")
}

extract_hits_from_text <- function(text_vec, keywords) {
  pattern <- make_pattern(keywords)
  m <- stringr::str_extract_all(text_vec, regex(pattern, ignore_case = TRUE))
  
  vapply(m, function(x) {
    x <- unique(tolower(x))
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) "" else paste(x, collapse = "; ")
  }, FUN.VALUE = character(1))
}

detect_categories_from_hits <- function(hit_string, dict_df) {
  if (is.na(hit_string) || hit_string == "") return("")
  hits <- str_split(hit_string, ";\\s*")[[1]]
  cats <- dict_df %>%
    filter(keyword %in% hits) %>%
    pull(category) %>%
    unique()
  if (length(cats) == 0) "" else paste(cats, collapse = "; ")
}

pick_primary_category_from_hits <- function(hit_string, dict_df) {
  if (is.na(hit_string) || hit_string == "") return("")
  hits <- str_split(hit_string, ";\\s*")[[1]]
  
  tab <- dict_df %>%
    filter(keyword %in% hits) %>%
    group_by(category) %>%
    summarise(
      n_hits = n(),
      longest_kw = max(nchar(keyword), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n_hits), desc(longest_kw), category)
  
  if (nrow(tab) == 0) "" else tab$category[[1]]
}

classify_ai_records_stage1 <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Classifying AI records (stage 1, fast)...")
  
  in_file   <- file.path(project_root, "data", "clean", "ai", "ai_multisource_deduplicated.csv")
  dict_file <- file.path(project_root, "config", "ai_dictionary_master.csv")
  out_file  <- file.path(project_root, "data", "enriched", "ai", "ai_classified_stage1.csv")
  qc_file   <- file.path(project_root, "data", "outputs", "ai", "ai_classification_stage1_qc.csv")
  
  dat <- read_csv(in_file, show_col_types = FALSE, col_types = ai_dedup_col_types) %>%
    mutate(
      across(where(is.character), ~coalesce(., "")),
      text_for_classification = str_squish(str_to_lower(paste(title, abstract)))
    )
  
  dic <- read_csv(dict_file, show_col_types = FALSE) %>%
    mutate(
      dict_type = str_squish(dict_type),
      category = str_squish(category),
      keyword = str_to_lower(str_squish(keyword))
    )
  
  dic_method <- dic %>% filter(dict_type == "METHOD")
  dic_app    <- dic %>% filter(dict_type == "APPLICATION")
  
  method_keywords <- unique(dic_method$keyword)
  app_keywords    <- unique(dic_app$keyword)
  
  method_hits_vec <- extract_hits_from_text(dat$text_for_classification, method_keywords)
  app_hits_vec    <- extract_hits_from_text(dat$text_for_classification, app_keywords)
  
  method_categories_vec <- vapply(
    method_hits_vec,
    detect_categories_from_hits,
    FUN.VALUE = character(1),
    dict_df = dic_method
  )
  
  app_categories_vec <- vapply(
    app_hits_vec,
    detect_categories_from_hits,
    FUN.VALUE = character(1),
    dict_df = dic_app
  )
  
  method_primary_vec <- vapply(
    method_hits_vec,
    pick_primary_category_from_hits,
    FUN.VALUE = character(1),
    dict_df = dic_method
  )
  
  app_primary_vec <- vapply(
    app_hits_vec,
    pick_primary_category_from_hits,
    FUN.VALUE = character(1),
    dict_df = dic_app
  )
  
  out <- dat %>%
    mutate(
      method_hits = method_hits_vec,
      application_hits = app_hits_vec,
      method_categories = method_categories_vec,
      application_categories = app_categories_vec,
      method_family_primary = method_primary_vec,
      application_type_primary = app_primary_vec,
      n_method_hits = ifelse(method_hits == "", 0L, str_count(method_hits, ";") + 1L),
      n_application_hits = ifelse(application_hits == "", 0L, str_count(application_hits, ";") + 1L),
      has_method_signal = n_method_hits > 0,
      has_application_signal = n_application_hits > 0
    )
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  write_csv(out, out_file, na = "")
  
  qc <- tibble(
    metric = c(
      "n_input_records",
      "n_output_records",
      "n_with_method_signal",
      "n_with_application_signal",
      "n_with_both_method_and_application",
      "n_without_any_signal",
      "n_primary_method_assigned",
      "n_primary_application_assigned"
    ),
    value = c(
      nrow(dat),
      nrow(out),
      sum(out$has_method_signal, na.rm = TRUE),
      sum(out$has_application_signal, na.rm = TRUE),
      sum(out$has_method_signal & out$has_application_signal, na.rm = TRUE),
      sum(!out$has_method_signal & !out$has_application_signal, na.rm = TRUE),
      sum(out$method_family_primary != "", na.rm = TRUE),
      sum(out$application_type_primary != "", na.rm = TRUE)
    )
  )
  
  write_csv(qc, qc_file)
  
  message("[PREIS-AI] Stage 1 classification completed.")
  print(qc)
  
  invisible(list(
    classified = out,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  classify_ai_records_stage1("D:/PREIS_AI")
}