# Script de DEPLOIEMENT ponctuel (Finding #3) : vide application_type_primary/
# application_type_all/ai_evidence_type pour TOUTES les lignes afin que le cache
# de 11_classify_ai_applications.R ne masque pas le nouvel ordre de priorite.
TRACKER <- "D:/PREIS_AI_CLEAN/data/enriched/ai/ai_tracker_final.csv"
trk <- read.csv(TRACKER, stringsAsFactors = FALSE, check.names = FALSE)
cat("Lignes :", nrow(trk), "\n")
cat("--- AVANT ---\n"); print(sort(table(trk$application_type_primary, useNA = "ifany"), decreasing = TRUE))
trk$application_type_primary <- ""
trk$application_type_all <- ""
trk$ai_evidence_type <- ""
write.csv(trk, TRACKER, row.names = FALSE, na = "")
cat("CLEAR_ALL_OK\n")
