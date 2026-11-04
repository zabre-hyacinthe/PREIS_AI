# =========================================================
# PREIS-AI - FILTER INTELLIGENCE ALERTS (HARDENED V4)
# =========================================================

options(stringsAsFactors = FALSE, warn = 1)

PROJECT_DIR <- "D:/PREIS_AI"

INPUT_FILE <- file.path(PROJECT_DIR, "dashboard", "data", "dashboard", "alerts_ai.csv")

OUT_DASH <- file.path(PROJECT_DIR, "dashboard", "data", "dashboard")
OUT_OUT  <- file.path(PROJECT_DIR, "data", "outputs", "ai", "alerts")

dir.create(OUT_DASH, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_OUT,  recursive = TRUE, showWarnings = FALSE)

log15 <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", paste0(..., collapse = ""), "\n")
}

safe_read <- function(path) {
  if (!file.exists(path)) stop("Missing: ", path)
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(x)) stop("Empty file: ", path)
  x
}

clean_txt <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  x
}

safe_num <- function(x, default = 0) {
  y <- suppressWarnings(as.numeric(as.character(x)))
  y[is.na(y)] <- default
  y
}

find_col <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(NA_character_)
  hits[1]
}

level_to_score <- function(x, type = c("priority", "africa", "novelty")) {
  type <- match.arg(type)
  x <- tolower(clean_txt(x))
  out <- rep(0, length(x))
  
  # more permissive matching
  high_pat <- "critical|very high|high|strong|major|important"
  mod_pat  <- "moderate|medium|intermediate"
  low_pat  <- "low|minor|limited|weak"
  
  if (type == "priority") {
    out[grepl("critical|very high", x)] <- 100
    out[grepl(high_pat, x) & out == 0] <- 75
    out[grepl(mod_pat, x)  & out == 0] <- 50
    out[grepl(low_pat, x)  & out == 0] <- 25
  }
  
  if (type %in% c("africa", "novelty")) {
    out[grepl("very high", x)] <- 90
    out[grepl(high_pat, x) & out == 0] <- 70
    out[grepl(mod_pat, x)  & out == 0] <- 45
    out[grepl(low_pat, x)  & out == 0] <- 20
  }
  
  out
}

percent_rank <- function(x) {
  x <- safe_num(x, 0)
  if (length(unique(x)) <= 1) return(rep(0.5, length(x)))
  r <- rank(x, na.last = "keep", ties.method = "average")
  (r - min(r, na.rm = TRUE)) / (max(r, na.rm = TRUE) - min(r, na.rm = TRUE))
}

alerts <- safe_read(INPUT_FILE)

log15("Input alerts: ", nrow(alerts))
log15("Input columns: ", paste(names(alerts), collapse = " | "))

title_col <- find_col(alerts, c("title"))
priority_score_col <- find_col(alerts, c("priority_score"))
africa_score_col   <- find_col(alerts, c("africa_relevance_score"))
novelty_score_col  <- find_col(alerts, c("novelty_score"))
priority_level_col <- find_col(alerts, c("priority_level"))
africa_level_col   <- find_col(alerts, c("africa_relevance_level"))
novelty_level_col  <- find_col(alerts, c("novelty_level"))

if (is.na(title_col)) stop("Missing title column in alerts_ai.csv")

alerts$title <- clean_txt(alerts[[title_col]])

priority_score_num <- if (!is.na(priority_score_col)) safe_num(alerts[[priority_score_col]], 0) else rep(0, nrow(alerts))
africa_score_num   <- if (!is.na(africa_score_col))   safe_num(alerts[[africa_score_col]], 0)   else rep(0, nrow(alerts))
novelty_score_num  <- if (!is.na(novelty_score_col))  safe_num(alerts[[novelty_score_col]], 0)  else rep(0, nrow(alerts))

priority_level_txt <- if (!is.na(priority_level_col)) clean_txt(alerts[[priority_level_col]]) else rep(NA_character_, nrow(alerts))
africa_level_txt   <- if (!is.na(africa_level_col))   clean_txt(alerts[[africa_level_col]])   else rep(NA_character_, nrow(alerts))
novelty_level_txt  <- if (!is.na(novelty_level_col))  clean_txt(alerts[[novelty_level_col]])  else rep(NA_character_, nrow(alerts))

alerts$priority_score <- ifelse(priority_score_num > 0, priority_score_num, level_to_score(priority_level_txt, "priority"))
alerts$africa_relevance_score <- ifelse(africa_score_num > 0, africa_score_num, level_to_score(africa_level_txt, "africa"))
alerts$novelty_score <- ifelse(novelty_score_num > 0, novelty_score_num, level_to_score(novelty_level_txt, "novelty"))

log15("Unique priority levels: ", paste(head(sort(unique(na.omit(priority_level_txt))), 20), collapse = " | "))
log15("Unique africa levels: ", paste(head(sort(unique(na.omit(africa_level_txt))), 20), collapse = " | "))
log15("Unique novelty levels: ", paste(head(sort(unique(na.omit(novelty_level_txt))), 20), collapse = " | "))

log15("Score summaries after fallback:")
log15("  priority_score > 0: ", sum(alerts$priority_score > 0, na.rm = TRUE))
log15("  africa_score > 0  : ", sum(alerts$africa_relevance_score > 0, na.rm = TRUE))
log15("  novelty_score > 0 : ", sum(alerts$novelty_score > 0, na.rm = TRUE))

# Composite score: standardized + relative ranking
p_rank <- percent_rank(alerts$priority_score)
a_rank <- percent_rank(alerts$africa_relevance_score)
n_rank <- percent_rank(alerts$novelty_score)

alerts$composite_score <- round(
  100 * (0.45 * p_rank + 0.35 * a_rank + 0.20 * n_rank),
  1
)

log15("  composite_score > 0: ", sum(alerts$composite_score > 0, na.rm = TRUE))
log15("  composite_score >= 50: ", sum(alerts$composite_score >= 50, na.rm = TRUE))

# Relative filters
q80 <- as.numeric(stats::quantile(alerts$composite_score, probs = 0.80, na.rm = TRUE, type = 7))
q90 <- as.numeric(stats::quantile(alerts$composite_score, probs = 0.90, na.rm = TRUE, type = 7))
q95 <- as.numeric(stats::quantile(alerts$composite_score, probs = 0.95, na.rm = TRUE, type = 7))

log15("Composite quantiles: q80=", round(q80,1), " | q90=", round(q90,1), " | q95=", round(q95,1))

filtered <- alerts[
  !is.na(alerts$title) & alerts$composite_score >= q80,
  ,
  drop = FALSE
]

decision <- alerts[
  !is.na(alerts$title) & alerts$composite_score >= q90,
  ,
  drop = FALSE
]

email_ready <- alerts[
  !is.na(alerts$title) & alerts$composite_score >= q95,
  ,
  drop = FALSE
]

# Ensure non-empty sensible outputs
ord_all <- order(-alerts$composite_score, -alerts$priority_score, na.last = TRUE)

if (nrow(filtered) == 0) filtered <- alerts[ord_all, , drop = FALSE]
if (nrow(filtered) > 500) filtered <- filtered[seq_len(500), , drop = FALSE]

if (nrow(decision) == 0) decision <- alerts[ord_all, , drop = FALSE]
if (nrow(decision) > 200) decision <- decision[seq_len(200), , drop = FALSE]

if (nrow(email_ready) == 0) email_ready <- alerts[ord_all, , drop = FALSE]
if (nrow(email_ready) > 50) email_ready <- email_ready[seq_len(50), , drop = FALSE]

decision <- decision[order(-decision$composite_score, -decision$priority_score, na.last = TRUE), , drop = FALSE]
top50 <- decision
if (nrow(top50) > 50) top50 <- top50[seq_len(50), , drop = FALSE]

write.csv(filtered, file.path(OUT_DASH, "alerts_ai_filtered.csv"), row.names = FALSE, na = "")
write.csv(top50, file.path(OUT_DASH, "alerts_ai_top50.csv"), row.names = FALSE, na = "")
write.csv(filtered, file.path(OUT_OUT, "ai_alerts_filtered.csv"), row.names = FALSE, na = "")
write.csv(email_ready, file.path(OUT_OUT, "ai_alerts_email_ready.csv"), row.names = FALSE, na = "")

qc <- data.frame(
  metric = c(
    "input_rows",
    "priority_score_gt_0",
    "africa_score_gt_0",
    "novelty_score_gt_0",
    "filtered_rows",
    "decision_rows",
    "top50_rows",
    "email_ready_rows"
  ),
  value = c(
    nrow(alerts),
    sum(alerts$priority_score > 0, na.rm = TRUE),
    sum(alerts$africa_relevance_score > 0, na.rm = TRUE),
    sum(alerts$novelty_score > 0, na.rm = TRUE),
    nrow(filtered),
    nrow(decision),
    nrow(top50),
    nrow(email_ready)
  ),
  stringsAsFactors = FALSE
)

write.csv(qc, file.path(OUT_OUT, "ai_alerts_filter_qc.csv"), row.names = FALSE)

log15("Filtered: ", nrow(filtered))
log15("Decision: ", nrow(decision))
log15("Top50: ", nrow(top50))
log15("Email ready: ", nrow(email_ready))
log15("Filtering DONE")