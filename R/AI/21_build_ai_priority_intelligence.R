# ============================================================
# PREIS AI — PRIORITY INTELLIGENCE ENGINE
# STEP 21 (FINAL)
# World-class surveillance intelligence layer
# ============================================================

cat("[PREIS-AI] STEP 21 — Priority intelligence engine\n")

library(dplyr)
library(readr)

PROJECT_ROOT <- "D:/PREIS_AI"

tracker_file <- file.path(
  
  PROJECT_ROOT,
  "data/enriched/ai/ai_tracker_final.csv"
  
)

out_dir <- file.path(
  
  PROJECT_ROOT,
  "data/outputs/ai/priority_intelligence"
  
)

dir.create(
  
  out_dir,
  recursive=TRUE,
  showWarnings=FALSE
  
)

trk <- read_csv(
  
  tracker_file,
  show_col_types=FALSE
  
)

# ============================================================
# NUMERIC SCORES
# ============================================================

trk <- trk %>%
  
  mutate(
    
    novelty_score_num = case_when(
      
      novelty_level=="High"~3,
      novelty_level=="Moderate"~2,
      TRUE~1
      
    ),
    
    africa_score_num = case_when(
      
      africa_relevance_level=="High"~3,
      africa_relevance_level=="Moderate"~2,
      TRUE~1
      
    ),
    
    priority_score_num = case_when(
      
      priority_level=="High"~3,
      priority_level=="Moderate"~2,
      TRUE~1
      
    ),
    
    operational_score = case_when(
      
      application_type_primary %in%
        
        c(
          
          "Epidemic Intelligence",
          "Early Detection",
          "Forecasting",
          "Decision Support"
          
        )~3,
      
      TRUE~1
      
    )
    
  )

# ============================================================
# PREIS INTELLIGENCE SCORE
# ============================================================

trk <- trk %>%
  
  mutate(
    
    PREIS_INTELLIGENCE_SCORE =
      
      novelty_score_num +
      africa_score_num +
      priority_score_num +
      operational_score
    
  )

# ============================================================
# TOP INTELLIGENCE RECORDS
# ============================================================

top_intelligence <- trk %>%
  
  arrange(
    
    desc(PREIS_INTELLIGENCE_SCORE)
    
  ) %>%
  
  slice(1:100)

write_csv(
  
  top_intelligence,
  
  file.path(
    
    out_dir,
    "ai_top_intelligence.csv"
    
  )
  
)

# ============================================================
# STRATEGIC MONITORING LIST
# ============================================================

strategic <- trk %>%
  
  filter(
    
    PREIS_INTELLIGENCE_SCORE>=10
    
  )

write_csv(
  
  strategic,
  
  file.path(
    
    out_dir,
    "ai_strategic_monitoring.csv"
    
  )
  
)

# ============================================================
# RCC INTELLIGENCE
# ============================================================

rcc_priority <- trk %>%
  
  filter(
    
    PREIS_INTELLIGENCE_SCORE>=9
    
  ) %>%
  
  count(
    
    rcc,
    sort=TRUE
    
  )

write_csv(
  
  rcc_priority,
  
  file.path(
    
    out_dir,
    "ai_rcc_priority.csv"
    
  )
  
)

# ============================================================
# FINAL INDICATORS
# ============================================================

indicators <- tibble(
  
  metric=c(
    
    "n_top_intelligence",
    "n_strategic",
    "max_score",
    "mean_score"
    
  ),
  
  value=c(
    
    nrow(top_intelligence),
    
    nrow(strategic),
    
    max(trk$PREIS_INTELLIGENCE_SCORE),
    
    mean(trk$PREIS_INTELLIGENCE_SCORE)
    
  )
  
)

write_csv(
  
  indicators,
  
  file.path(
    
    out_dir,
    "ai_priority_indicators.csv"
    
  )
  
)

cat("[PREIS-AI] STEP 21 completed\n")