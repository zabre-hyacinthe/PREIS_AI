library(readr)

root <- "D:/PREIS_AI"
setwd(root)

dir.create("logs", showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(root, "logs", "pipeline_log.txt")

log_message <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), msg, "\n",
      file = log_file, append = TRUE)
}

run_step <- function(script_path) {
  step_name <- basename(script_path)
  log_message("START ", step_name)
  
  if (!file.exists(script_path)) {
    log_message("ERROR ", step_name, " : file not found")
    return(FALSE)
  }
  
  ok <- tryCatch({
    source(script_path, local = FALSE)
    TRUE
  }, error = function(e) {
    log_message("ERROR ", step_name, " : ", conditionMessage(e))
    FALSE
  })
  
  if (ok) log_message("DONE ", step_name)
  ok
}

log_message("========== PREIS AI PRODUCTION START ==========")

pipeline_scripts <- c(
  "R/AI/01_fetch_articles_ai_v10.R",
  "R/AI/06_standardize_ai_sources.R",
  "R/AI/07_merge_ai_sources.R",
  "R/AI/08_deduplicate_ai_sources.R",
  "R/AI/09_classify_ai_records_stage1.R",
  "R/AI/10_score_ai_africa_relevance.R",
  "R/AI/11_detect_ai_novelty.R",
  "R/AI/12_build_ai_tracker_final.R",
  "R/AI/13b_rebuild_ai_geo_from_text.R",
  "R/AI/13_prepare_ai_dashboard_dataset.R",
  "R/AI/14_build_ai_intelligence_exports.R",
  "R/AI/15_build_ai_intelligence_dashboard_dataset.R",
  "R/AI/16_build_ai_intelligence_indicators.R"
)

results <- lapply(pipeline_scripts, run_step)
names(results) <- basename(pipeline_scripts)

# Schema check
schema_fp  <- file.path(root, "config", "ai_schema_lock.csv")
tracker_fp <- file.path(root, "data/enriched/ai", "ai_tracker_final.csv")

if (file.exists(schema_fp) && file.exists(tracker_fp)) {
  expected <- read_csv(schema_fp, show_col_types = FALSE)
  trk <- read_csv(tracker_fp, show_col_types = FALSE)
  
  if (!"column" %in% names(expected)) {
    log_message("ERROR schema: ai_schema_lock.csv must contain a column named 'column'")
  } else {
    missing <- setdiff(expected$column, names(trk))
    extra   <- setdiff(names(trk), expected$column)
    
    if (length(missing) > 0) {
      log_message("SCHEMA BROKEN - missing: ", paste(missing, collapse = ", "))
    } else {
      log_message("Schema OK")
    }
    
    if (length(extra) > 0) {
      log_message("Schema extra columns: ", paste(extra, collapse = ", "))
    }
  }
} else {
  log_message("Schema check skipped")
}

log_message("========== PREIS AI PRODUCTION END ==========")