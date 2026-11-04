# =========================================================
# PREIS-AI - AI × DISEASE MATRIX (THEORY vs APPLICATION)
# =========================================================

options(stringsAsFactors = FALSE)

PROJECT_DIR <- "D:/PREIS_AI"

INPUT_FILE <- file.path(PROJECT_DIR, "dashboard", "data", "dashboard", "alerts_ai_filtered.csv")

OUT_DIR <- file.path(PROJECT_DIR, "data", "outputs", "ai", "analytics")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log16 <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", paste0(..., collapse = ""), "\n")
}

safe_read <- function(f) {
  if (!file.exists(f)) stop("Missing: ", f)
  read.csv(f, stringsAsFactors = FALSE)
}

clean <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA
  x
}

# =========================================================
# 1. LOAD DATA
# =========================================================

alerts <- safe_read(INPUT_FILE)

alerts$disease <- clean(alerts$disease_primary)
alerts$method  <- clean(alerts$method_family_primary)
alerts$app     <- clean(alerts$application_type_primary)
alerts$type    <- clean(alerts$article_type)

# =========================================================
# 2. CLASSIFY THEORY vs APPLICATION
# =========================================================

alerts$ai_evidence_type <- ifelse(
  grepl("review|meta", alerts$type),
  "REVIEW",
  ifelse(
    grepl("trial|evaluation|validation|real-world|dataset", paste(alerts$title, alerts$abstract), ignore.case = TRUE),
    "APPLICATION",
    ifelse(
      grepl("model|framework|propose|approach", paste(alerts$title, alerts$abstract), ignore.case = TRUE),
      "THEORY",
      "UNKNOWN"
    )
  )
)

# =========================================================
# 3. FILTER VALID RECORDS
# =========================================================

alerts <- alerts[
  !is.na(alerts$disease) &
    !is.na(alerts$method),
]

# =========================================================
# 4. GLOBAL MATRIX
# =========================================================

mat_global <- as.data.frame(table(
  alerts$disease,
  alerts$method,
  alerts$app,
  alerts$ai_evidence_type
))

names(mat_global) <- c("disease", "method", "application", "evidence_type", "n")
mat_global <- mat_global[mat_global$n > 0, ]

write.csv(mat_global,
          file.path(OUT_DIR, "ai_disease_method_matrix_full.csv"),
          row.names = FALSE)

# =========================================================
# 5. AFRICA FOCUS
# =========================================================

africa <- alerts[
  alerts$africa_relevance_score >= 60,
]

mat_africa <- as.data.frame(table(
  africa$disease,
  africa$method,
  africa$app,
  africa$ai_evidence_type
))

names(mat_africa) <- c("disease", "method", "application", "evidence_type", "n")
mat_africa <- mat_africa[mat_africa$n > 0, ]

write.csv(mat_africa,
          file.path(OUT_DIR, "ai_disease_africa_matrix.csv"),
          row.names = FALSE)

# =========================================================
# 6. THEORY vs APPLICATION SUMMARY
# =========================================================

summary <- aggregate(
  n ~ disease + method + evidence_type,
  data = mat_africa,
  sum
)

write.csv(summary,
          file.path(OUT_DIR, "ai_theory_vs_application_summary.csv"),
          row.names = FALSE)

# =========================================================
# 7. TOP INSIGHTS (STRATEGIC)
# =========================================================

top <- mat_africa[order(-mat_africa$n), ]
top <- head(top, 50)

write.csv(top,
          file.path(OUT_DIR, "ai_disease_top_insights.csv"),
          row.names = FALSE)

log16("Matrix + theory/application built successfully")