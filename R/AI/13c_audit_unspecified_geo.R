suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

audit_unspecified_geo <- function(project_dir = "D:/PREIS_AI", top_n = 200) {
  
  in_file <- file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final_geo.csv")
  out_dir <- file.path(project_dir, "data", "outputs", "ai")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(in_file)) stop("Input file not found: ", in_file)
  
  df <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  
  norm_txt <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x |>
      str_to_lower() |>
      str_replace_all("[[:punct:]]+", " ") |>
      str_replace_all("\\s+", " ") |>
      str_replace_all("\\s+", " ") |>
      str_trim()
  }
  
  txt <- df %>%
    mutate(
      country = ifelse(is.na(country), "Unspecified", country),
      iso3    = ifelse(is.na(iso3), "Unspecified", iso3),
      rcc     = ifelse(is.na(rcc), "Unspecified", rcc),
      full_text = paste(
        if ("title" %in% names(.)) title else "",
        if ("abstract" %in% names(.)) abstract else "",
        if ("authors" %in% names(.)) authors else "",
        if ("journal" %in% names(.)) journal else "",
        if ("text_all" %in% names(.)) text_all else "",
        if ("country_txt" %in% names(.)) country_txt else "",
        if ("iso3_txt" %in% names(.)) iso3_txt else "",
        if ("rcc_txt" %in% names(.)) rcc_txt else ""
      ),
      full_text_norm = norm_txt(full_text)
    ) %>%
    filter(country == "Unspecified")
  
  # top repeated non-trivial tokens / bigrams from unspecified records
  stop_words <- c(
    "the","and","for","with","from","using","among","into","during","after","before",
    "this","that","these","those","study","studies","analysis","model","models","data",
    "public","health","surveillance","disease","diseases","outbreak","epidemic",
    "machine","learning","deep","intelligence","artificial","digital","risk","prediction",
    "forecasting","approach","system","review","scoping","systematic","global","africa","african"
  )
  
  words_tbl <- txt %>%
    transmute(word = str_split(full_text_norm, "\\s+")) %>%
    tidyr::unnest(word) %>%
    filter(
      nchar(word) >= 4,
      !word %in% stop_words,
      !str_detect(word, "^\\d+$")
    ) %>%
    count(word, sort = TRUE)
  
  bigrams_tbl <- txt %>%
    transmute(
      bigram = str_extract_all(full_text_norm, "\\b[[:alpha:]][[:alpha:]-]+\\s+[[:alpha:]][[:alpha:]-]+\\b")
    ) %>%
    tidyr::unnest(bigram) %>%
    filter(!is.na(bigram), bigram != "") %>%
    count(bigram, sort = TRUE)
  
  sample_unspecified <- txt %>%
    transmute(
      record_id = if ("record_id" %in% names(.)) record_id else row_number(),
      title = if ("title" %in% names(.)) title else "",
      journal = if ("journal" %in% names(.)) journal else "",
      authors = if ("authors" %in% names(.)) authors else "",
      method_family_primary = if ("method_family_primary" %in% names(.)) method_family_primary else "",
      application_type_primary = if ("application_type_primary" %in% names(.)) application_type_primary else "",
      priority_level = if ("priority_level" %in% names(.)) priority_level else "",
      africa_relevance_level = if ("africa_relevance_level" %in% names(.)) africa_relevance_level else "",
      text_snippet = str_sub(full_text, 1, 500)
    ) %>%
    slice_head(n = top_n)
  
  qa <- tibble(
    metric = c(
      "n_records_total",
      "n_unspecified_country",
      "pct_unspecified_country"
    ),
    value = c(
      nrow(df),
      nrow(txt),
      round(100 * nrow(txt) / nrow(df), 2)
    )
  )
  
  readr::write_csv(qa, file.path(out_dir, "ai_unspecified_geo_qc.csv"), na = "")
  readr::write_csv(words_tbl, file.path(out_dir, "ai_unspecified_top_words.csv"), na = "")
  readr::write_csv(bigrams_tbl, file.path(out_dir, "ai_unspecified_top_bigrams.csv"), na = "")
  readr::write_csv(sample_unspecified, file.path(out_dir, "ai_unspecified_sample.csv"), na = "")
  
  invisible(list(
    qa = qa,
    words = words_tbl,
    bigrams = bigrams_tbl,
    sample = sample_unspecified
  ))
}