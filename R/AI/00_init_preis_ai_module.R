############################################################
# PREIS-AI OBSERVATORY FOR AFRICA
# 00_init_preis_ai_module.R
# Step 1 - Initialize module structure and methodology configs
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
})

init_preis_ai_module <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Initializing module structure in: ", project_root)
  
  # ------------------------------------------------------------------
  # 1. Define directories
  # ------------------------------------------------------------------
  dirs <- c(
    file.path(project_root, "R", "AI"),
    file.path(project_root, "config"),
    file.path(project_root, "data", "raw", "ai"),
    file.path(project_root, "data", "clean", "ai"),
    file.path(project_root, "data", "enriched", "ai"),
    file.path(project_root, "data", "outputs", "ai")
  )
  
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 2. Create config files from validated methodology
  # ------------------------------------------------------------------
  
  ai_search_terms <- tribble(
    ~block, ~term,
    "AI", "artificial intelligence",
    "AI", "machine learning",
    "AI", "deep learning",
    "AI", "natural language processing",
    "AI", "large language model",
    "AI", "LLM",
    "AI", "neural network",
    "AI", "transformer",
    "AI", "predictive analytics",
    "AI", "anomaly detection",
    "AI", "data mining",
    "AI", "multimodal AI",
    "AI", "computer vision",
    
    "SURVEILLANCE", "public health surveillance",
    "SURVEILLANCE", "epidemic intelligence",
    "SURVEILLANCE", "public health intelligence",
    "SURVEILLANCE", "early warning",
    "SURVEILLANCE", "outbreak detection",
    "SURVEILLANCE", "event-based surveillance",
    "SURVEILLANCE", "event based surveillance",
    "SURVEILLANCE", "digital surveillance",
    "SURVEILLANCE", "disease monitoring",
    "SURVEILLANCE", "signal detection",
    "SURVEILLANCE", "signal triage",
    "SURVEILLANCE", "forecasting",
    "SURVEILLANCE", "nowcasting",
    "SURVEILLANCE", "risk assessment",
    "SURVEILLANCE", "decision support",
    
    "HEALTH", "infectious disease",
    "HEALTH", "epidemic",
    "HEALTH", "pandemic",
    "HEALTH", "zoonotic",
    "HEALTH", "One Health",
    "HEALTH", "environmental surveillance",
    "HEALTH", "wastewater surveillance",
    "HEALTH", "vector-borne",
    "HEALTH", "public health",
    
    "GEO", "Africa",
    "GEO", "Africa CDC",
    "GEO", "African Union"
  )
  
  ai_taxonomy <- tribble(
    ~method_family, ~keyword,
    "Machine Learning", "machine learning",
    "Machine Learning", "random forest",
    "Machine Learning", "xgboost",
    "Machine Learning", "support vector machine",
    "Machine Learning", "svm",
    "Machine Learning", "gradient boosting",
    
    "Deep Learning", "deep learning",
    "Deep Learning", "cnn",
    "Deep Learning", "convolutional neural network",
    "Deep Learning", "lstm",
    "Deep Learning", "recurrent neural network",
    "Deep Learning", "rnn",
    "Deep Learning", "autoencoder",
    
    "NLP", "natural language processing",
    "NLP", "text mining",
    "NLP", "named entity recognition",
    "NLP", "topic modeling",
    "NLP", "topic modelling",
    
    "LLM", "large language model",
    "LLM", "gpt",
    "LLM", "bert",
    "LLM", "transformer",
    
    "Statistical", "bayesian",
    "Statistical", "time series",
    "Statistical", "arima",
    
    "Geospatial", "spatial model",
    "Geospatial", "spatiotemporal",
    "Geospatial", "spatio-temporal",
    
    "Hybrid", "hybrid model",
    "Hybrid", "rule-based",
    "Hybrid", "rules-based",
    
    "Multimodal", "multimodal",
    "Anomaly Detection", "anomaly detection",
    "Anomaly Detection", "outlier detection"
  )
  
  ai_applications <- tribble(
    ~application_type, ~keyword,
    "Early Detection", "early warning",
    "Early Detection", "outbreak detection",
    "Early Detection", "signal detection",
    "Early Detection", "event detection",
    
    "Epidemic Intelligence", "epidemic intelligence",
    "Epidemic Intelligence", "public health intelligence",
    
    "Monitoring", "disease monitoring",
    "Monitoring", "situational awareness",
    
    "Forecasting", "forecasting",
    "Forecasting", "predictive model",
    "Forecasting", "prediction",
    "Forecasting", "nowcasting",
    
    "Triage", "signal triage",
    "Triage", "prioritization",
    "Triage", "classification",
    
    "Risk Assessment", "risk assessment",
    "Risk Assessment", "risk prediction",
    
    "Decision Support", "decision support",
    "Decision Support", "resource allocation",
    
    "One Health", "one health",
    "One Health", "zoonotic surveillance",
    
    "Environmental", "environmental surveillance",
    "Environmental", "wastewater surveillance",
    
    "Digital Surveillance", "digital surveillance",
    "Digital Surveillance", "online surveillance",
    "Digital Surveillance", "internet-based surveillance"
  )
  
  ai_inclusion_rules <- tribble(
    ~rule_id, ~description,
    1, "Contains at least one AI term",
    2, "Contains at least one surveillance or application term",
    3, "Has title",
    4, "Has abstract or summary text",
    5, "Publication year >= 2010"
  )
  
  ai_exclusion_rules <- tribble(
    ~rule_id, ~description,
    1, "Purely clinical diagnosis without surveillance relevance",
    2, "Treatment or therapeutics only",
    3, "No methodological content",
    4, "No surveillance or epidemic intelligence relevance",
    5, "Non-health application"
  )
  
  ai_africa_scoring <- tribble(
    ~criterion, ~weight,
    "African priority relevance", 3,
    "Low data requirement", 2,
    "Low infrastructure requirement", 2,
    "Transferability to RCC/MS", 3,
    "Operational maturity", 2,
    "Validation reported", 2,
    "Explainability", 1,
    "One Health relevance", 2,
    "Digital surveillance relevance", 2,
    "Predictive utility", 2
  )
  
  # ------------------------------------------------------------------
  # 3. Write files
  # ------------------------------------------------------------------
  write_csv(ai_search_terms,
            file.path(project_root, "config", "ai_search_terms.csv"))
  write_csv(ai_taxonomy,
            file.path(project_root, "config", "ai_taxonomy.csv"))
  write_csv(ai_applications,
            file.path(project_root, "config", "ai_applications.csv"))
  write_csv(ai_inclusion_rules,
            file.path(project_root, "config", "ai_inclusion_rules.csv"))
  write_csv(ai_exclusion_rules,
            file.path(project_root, "config", "ai_exclusion_rules.csv"))
  write_csv(ai_africa_scoring,
            file.path(project_root, "config", "ai_africa_scoring.csv"))
  
  # ------------------------------------------------------------------
  # 4. Validation summary
  # ------------------------------------------------------------------
  summary_tbl <- tibble(
    file = c(
      "ai_search_terms.csv",
      "ai_taxonomy.csv",
      "ai_applications.csv",
      "ai_inclusion_rules.csv",
      "ai_exclusion_rules.csv",
      "ai_africa_scoring.csv"
    ),
    rows = c(
      nrow(ai_search_terms),
      nrow(ai_taxonomy),
      nrow(ai_applications),
      nrow(ai_inclusion_rules),
      nrow(ai_exclusion_rules),
      nrow(ai_africa_scoring)
    )
  )
  
  write_csv(summary_tbl,
            file.path(project_root, "data", "outputs", "ai", "ai_init_summary.csv"))
  
  message("[PREIS-AI] Initialization complete.")
  print(summary_tbl)
  
  invisible(summary_tbl)
}

if (sys.nframe() == 0) {
  init_preis_ai_module("D:/PREIS_AI")
}