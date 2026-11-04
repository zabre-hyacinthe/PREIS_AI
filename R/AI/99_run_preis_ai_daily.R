# ============================================================
# PREIS-AI DAILY ORCHESTRATOR
# File: 99_run_preis_ai_daily.R
# Master runner (safe orchestration)
# ============================================================

cat("\n=====================================\n")
cat("PREIS-AI DAILY PIPELINE START\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("=====================================\n\n")

root <- "D:/PREIS_AI"
r_ai <- file.path(root,"R","AI")

safe_source <- function(script){
  
  f <- file.path(r_ai,script)
  
  if(!file.exists(f)){
    cat("SKIP (missing):",script,"\n")
    return(FALSE)
  }
  
  cat("RUN:",script,"\n")
  
  tryCatch({
    
    source(f)
    
    cat("OK:",script,"\n\n")
    
    TRUE
    
  },error=function(e){
    
    cat("ERROR:",script,"\n")
    cat(conditionMessage(e),"\n\n")
    
    FALSE
    
  })
  
}

pipeline_steps <- c(
  
  "01_fetch_europepmc_ai.R",
  "02_fetch_pubmed_ai.R",
  
  "03_standardize_ai_records.R",
  
  "04_merge_ai_sources.R",
  
  "05_deduplicate_ai.R",
  
  "10_classify_ai_methods.R",
  
  "11_classify_ai_applications.R",
  
  "12_classify_ai_disease.R",
  
  "20_score_ai_africa_relevance.R",
  
  "21_score_ai_novelty.R",
  
  "30_build_ai_tracker.R",
  
  "40_build_ai_alerts.R",
  
  "63_send_ai_alerts_conditional.R",
  
  "70_run_ai_email_cycle.R"
  
)

results <- sapply(pipeline_steps,safe_source)

cat("\n=====================================\n")
cat("PIPELINE SUMMARY\n")
cat("=====================================\n")

summary <- data.frame(
  
  script=pipeline_steps,
  success=results
  
)

print(summary)

cat("\nSuccessful:",
    sum(results),"/",length(results),"\n")

cat("\nFAILED:",
    sum(!results),"\n")

if(sum(!results)>0){
  
  cat("\nFailed scripts:\n")
  
  print(summary[!summary$success,])
  
}

cat("\n=====================================\n")
cat("RUN EMAIL CYCLE\n")
cat("=====================================\n")

tryCatch({
  
  source(file.path(r_ai,"70_run_ai_email_cycle.R"))
  
  send_ai_email_cycle()
  
  cat("EMAIL CYCLE OK\n")
  
},error=function(e){
  
  cat("EMAIL ERROR:\n")
  
  cat(conditionMessage(e),"\n")
  
})

cat("\n=====================================\n")
cat("PREIS-AI DAILY PIPELINE FINISHED\n")
cat("=====================================\n")