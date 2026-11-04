# Script de test TEMPORAIRE -- vide application_type_primary/all/ai_evidence_type
# uniquement pour les 11 lignes identifiees dans la revue de validation humaine
# (desaccords Surveillance/Prediction), pour forcer leur reclassification par
# le script 11_classify_ai_applications.R corrige (le cache du script ignore
# toute ligne deja classee). A supprimer apres le test.
TRACKER <- "D:/PREIS_AI_CLEAN/data/enriched/ai/ai_tracker_final.csv"
trk <- read.csv(TRACKER, stringsAsFactors = FALSE, check.names = FALSE)

target_ids <- c(
  "doi:10.7189/jogh.15.04114",
  "doi:10.1186/s12879-018-3124-7",
  "2506.14144v1",
  "doi:10.1145/2808719.2808735",
  "doi:10.1371/journal.pntd.0004676",
  "doi:10.1016/j.compbiomed.2026.111736",
  "doi:10.1186/s12916-026-04857-1",
  "doi:10.1007/s11538-015-0100-x",
  "doi:10.1155/2022/8990907",
  "2601.11907v1",
  "doi:10.1038/s41598-021-85987-9"
)

idx <- trk$record_id %in% target_ids
cat("Lignes trouvees :", sum(idx), "/", length(target_ids), "attendues\n")
cat("--- AVANT (application_type_primary actuel) ---\n")
print(trk[idx, c("record_id", "application_type_primary")])

trk$application_type_primary[idx] <- ""
trk$application_type_all[idx] <- ""
trk$ai_evidence_type[idx] <- ""

write.csv(trk, TRACKER, row.names = FALSE, na = "")
cat("CLEAR_OK -- ", sum(idx), " lignes videes et reecrites\n")
