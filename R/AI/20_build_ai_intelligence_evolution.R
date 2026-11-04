# ============================================================
# PREIS AI — INTELLIGENCE EVOLUTION ENGINE
# STEP 20
# Scientific intelligence layer
# ============================================================

cat("[PREIS-AI] STEP 20 — Intelligence evolution engine\n")

library(dplyr)
library(readr)
library(stringr)

PROJECT_ROOT <- "D:/PREIS_AI"

tracker_file <- file.path(
  
  PROJECT_ROOT,
  "data/enriched/ai/ai_tracker_final.csv"
  
)

out_dir <- file.path(
  
  PROJECT_ROOT,
  "data/outputs/ai/intelligence_evolution"
  
)

dir.create(out_dir,
           recursive=TRUE,
           showWarnings=FALSE)

trk <- read_csv(
  tracker_file,
  show_col_types=FALSE
)

# ============================================================
# METHOD TRENDS
# ============================================================

method_trends <- trk %>%
  
  count(
    
    year,
    method_family_primary
    
  ) %>%
  
  arrange(
    
    year,
    desc(n)
    
  )

write_csv(
  
  method_trends,
  
  file.path(
    out_dir,
    "ai_method_trends.csv"
  )
  
)

# ============================================================
# APPLICATION TRENDS
# ============================================================

app_trends <- trk %>%
  
  count(
    
    year,
    application_type_primary
    
  )

write_csv(
  
  app_trends,
  
  file.path(
    out_dir,
    "ai_application_trends.csv"
  )
  
)

# ============================================================
# AFRICA SIGNAL EVOLUTION
# ============================================================

africa_trend <- trk %>%
  
  count(
    
    year,
    africa_relevance_level
    
  )

write_csv(
  
  africa_trend,
  
  file.path(
    out_dir,
    "ai_africa_trends.csv"
  )
  
)

# ============================================================
# NOVELTY EVOLUTION
# ============================================================

novelty_trend <- trk %>%
  
  count(
    
    year,
    novelty_level
    
  )

write_csv(
  
  novelty_trend,
  
  file.path(
    out_dir,
    "ai_novelty_trends.csv"
  )
  
)

# ============================================================
# EMERGING METHODS DETECTION
# ============================================================

emerging_methods <- trk %>%
  
  filter(
    
    novelty_level=="High"
    
  ) %>%
  
  count(
    
    method_family_primary,
    sort=TRUE
    
  )

write_csv(
  
  emerging_methods,
  
  file.path(
    out_dir,
    "ai_emerging_methods.csv"
  )
  
)

# ============================================================
# WEAK SIGNAL DETECTION
# ============================================================

weak_signals <- trk %>%
  
  filter(
    
    africa_relevance_level=="High" &
      novelty_level=="High"
    
  )

write_csv(
  
  weak_signals,
  
  file.path(
    out_dir,
    "ai_weak_signals.csv"
  )
  
)

# ============================================================
# INTELLIGENCE INDICATORS
# ============================================================

indicators <- tibble(
  
  metric=c(
    
    "n_records",
    "n_high_novelty",
    "n_high_africa",
    "n_weak_signals"
    
  ),
  
  value=c(
    
    nrow(trk),
    
    sum(trk$novelty_level=="High"),
    
    sum(trk$africa_relevance_level=="High"),
    
    nrow(weak_signals)
    
  )
  
)

write_csv(
  
  indicators,
  
  file.path(
    out_dir,
    "ai_intelligence_indicators.csv"
  )
  
)

cat("[PREIS-AI] STEP 20 completed\n")