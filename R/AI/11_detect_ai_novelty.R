# ============================================================
# PREIS-AI — STEP 10
# Novelty detection v2
# More granular / more useful / still conservative
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

detect_ai_novelty <- function(project_dir = "D:/PREIS_AI") {
  
  message("[PREIS-AI] STEP 10 — novelty detection v2 started...")
  
  in_file  <- file.path(project_dir, "data", "enriched", "ai", "ai_scored_stage1.csv")
  out_file <- file.path(project_dir, "data", "enriched", "ai", "ai_novelty_stage1.csv")
  qa_file  <- file.path(project_dir, "data", "outputs", "ai", "ai_novelty_qc.csv")
  
  if (!file.exists(in_file)) {
    stop("Required Step 9 file not found: ", in_file)
  }
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qa_file), recursive = TRUE, showWarnings = FALSE)
  
  df <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  
  if (nrow(df) == 0) {
    stop("Input file is empty: ", in_file)
  }
  
  na2empty <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }
  
  norm_txt <- function(x) {
    x |>
      na2empty() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[[:punct:]]+", " ") |>
      stringr::str_replace_all("\\s+", " ") |>
      stringr::str_trim()
  }
  
  get_first_existing <- function(df, candidates, default = "") {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(default, nrow(df)))
    out <- as.character(df[[hit[1]]])
    out[is.na(out)] <- default
    out
  }
  
  # ----------------------------------------
  # Build normalized fields
  # ----------------------------------------
  df2 <- df %>%
    mutate(
      record_id = if ("record_id" %in% names(.)) as.character(record_id) else paste0("rec_", row_number()),
      
      title_txt   = get_first_existing(., c("title")),
      country_txt = get_first_existing(., c("country", "country_name", "country_primary")),
      iso3_txt    = get_first_existing(., c("iso3", "country_iso3")),
      rcc_txt     = get_first_existing(., c("rcc", "rcc_primary")),
      
      method_txt  = get_first_existing(., c("method_family_primary")),
      app_txt     = get_first_existing(., c("application_type_primary")),
      africa_txt  = get_first_existing(., c("africa_relevance_level")),
      
      year_txt    = get_first_existing(., c("year", "pub_year", "publication_year")),
      pred_txt    = get_first_existing(., c("predictive_signal", "is_predictive_signal")),
      oh_txt      = get_first_existing(., c("one_health_signal", "is_one_health_signal")),
      dig_txt     = get_first_existing(., c("digital_signal", "is_digital_signal")),
      op_txt      = get_first_existing(., c("operational_signal", "is_operational_signal")),
      
      title_norm  = norm_txt(title_txt),
      country_norm = norm_txt(country_txt),
      iso3_norm    = norm_txt(iso3_txt),
      rcc_norm     = norm_txt(rcc_txt),
      method_norm  = norm_txt(method_txt),
      app_norm     = norm_txt(app_txt),
      africa_norm  = norm_txt(africa_txt),
      year_norm    = norm_txt(year_txt),
      pred_norm    = norm_txt(pred_txt),
      oh_norm      = norm_txt(oh_txt),
      dig_norm     = norm_txt(dig_txt),
      op_norm      = norm_txt(op_txt)
    ) %>%
    mutate(
      geo_unit = case_when(
        iso3_norm != "" ~ iso3_norm,
        country_norm != "" ~ country_norm,
        rcc_norm != "" ~ rcc_norm,
        TRUE ~ "no_geo"
      ),
      method_unit = if_else(method_norm == "", "unspecified_method", method_norm),
      app_unit    = if_else(app_norm == "", "unspecified_application", app_norm),
      africa_unit = if_else(africa_norm == "", "unspecified_africa", africa_norm),
      year_unit   = if_else(year_norm == "", "unspecified_year", year_norm),
      
      pred_unit   = if_else(pred_norm == "", "na", pred_norm),
      oh_unit     = if_else(oh_norm == "", "na", oh_norm),
      dig_unit    = if_else(dig_norm == "", "na", dig_norm),
      op_unit     = if_else(op_norm == "", "na", op_norm)
    ) %>%
    mutate(
      novelty_key_main = paste(
        method_unit, app_unit, africa_unit, year_unit,
        pred_unit, oh_unit, dig_unit, op_unit,
        sep = " | "
      ),
      novelty_key_geo = paste(
        geo_unit, method_unit, app_unit, year_unit,
        sep = " | "
      ),
      novelty_key_title = title_norm
    )
  
  # ----------------------------------------
  # Frequencies
  # ----------------------------------------
  freq_main <- df2 %>%
    count(novelty_key_main, name = "freq_main")
  
  freq_geo <- df2 %>%
    count(novelty_key_geo, name = "freq_geo")
  
  freq_title <- df2 %>%
    count(novelty_key_title, name = "freq_title")
  
  df3 <- df2 %>%
    left_join(freq_main, by = "novelty_key_main") %>%
    left_join(freq_geo, by = "novelty_key_geo") %>%
    left_join(freq_title, by = "novelty_key_title") %>%
    mutate(
      freq_main = if_else(is.na(freq_main), 1L, as.integer(freq_main)),
      freq_geo = if_else(is.na(freq_geo), 1L, as.integer(freq_geo)),
      freq_title = if_else(is.na(freq_title), 1L, as.integer(freq_title))
    )
  
  # ----------------------------------------
  # Recalibrated score
  # ----------------------------------------
  df_out <- df3 %>%
    mutate(
      score_main = case_when(
        freq_main == 1L ~ 35L,
        freq_main <= 2L ~ 28L,
        freq_main <= 5L ~ 22L,
        freq_main <= 10L ~ 16L,
        freq_main <= 25L ~ 10L,
        TRUE ~ 4L
      ),
      score_geo = case_when(
        freq_geo == 1L ~ 25L,
        freq_geo <= 2L ~ 20L,
        freq_geo <= 5L ~ 16L,
        freq_geo <= 10L ~ 12L,
        freq_geo <= 25L ~ 8L,
        TRUE ~ 3L
      ),
      score_title = case_when(
        freq_title == 1L ~ 30L,
        freq_title == 2L ~ 20L,
        freq_title <= 5L ~ 12L,
        TRUE ~ 4L
      ),
      score_africa_bonus = case_when(
        africa_unit == "high" ~ 10L,
        africa_unit == "moderate" ~ 6L,
        africa_unit == "low" ~ 2L,
        TRUE ~ 0L
      ),
      novelty_score = pmin(
        100L,
        score_main + score_geo + score_title + score_africa_bonus
      ),
      novelty_level = case_when(
        novelty_score >= 75 ~ "High",
        novelty_score >= 45 ~ "Moderate",
        TRUE ~ "Low"
      ),
      novelty_reason = case_when(
        freq_main == 1L & freq_geo == 1L & freq_title == 1L ~
          "Unique profile by method, application, geography, year and title",
        freq_main <= 2L & freq_geo <= 2L ~
          "Rare analytical and geographic profile in the corpus",
        freq_title == 1L & novelty_score >= 45 ~
          "Unique title with relatively uncommon corpus profile",
        freq_main > 25L & freq_geo > 25L ~
          "Common recurrent profile in the corpus",
        TRUE ~
          "Intermediate rarity profile in the corpus"
      )
    )
  
  qa <- tibble(
    metric = c(
      "n_records",
      "n_high_novelty",
      "n_moderate_novelty",
      "n_low_novelty",
      "n_unique_main_profiles",
      "n_unique_geo_profiles",
      "n_unique_titles",
      "median_freq_main",
      "median_freq_geo",
      "median_freq_title"
    ),
    value = c(
      nrow(df_out),
      sum(df_out$novelty_level == "High", na.rm = TRUE),
      sum(df_out$novelty_level == "Moderate", na.rm = TRUE),
      sum(df_out$novelty_level == "Low", na.rm = TRUE),
      sum(df_out$freq_main == 1L, na.rm = TRUE),
      sum(df_out$freq_geo == 1L, na.rm = TRUE),
      sum(df_out$freq_title == 1L, na.rm = TRUE),
      stats::median(df_out$freq_main, na.rm = TRUE),
      stats::median(df_out$freq_geo, na.rm = TRUE),
      stats::median(df_out$freq_title, na.rm = TRUE)
    )
  )
  
  stopifnot(nrow(df_out) == nrow(df))
  stopifnot(all(!is.na(df_out$novelty_level)))
  stopifnot(all(df_out$novelty_score >= 0 & df_out$novelty_score <= 100))
  
  readr::write_csv(df_out, out_file, na = "")
  readr::write_csv(qa, qa_file, na = "")
  
  if (!file.exists(out_file)) {
    stop("Novelty output file was not created: ", out_file)
  }
  
  message("[PREIS-AI] STEP 10 completed.")
  message("[PREIS-AI] Output saved to: ", out_file)
  print(qa)
  
  invisible(list(
    data = df_out,
    qa = qa,
    output_file = out_file,
    qa_file = qa_file
  ))
}