## 66_export_alert_candidates.R — Exporte un fichier COMPACT pour le routeur cloud (GitHub Actions).
## A lancer sur le PC apres le pipeline. Sortie : data/alerting/alert_candidates.csv (sans resume : metadonnees bibliographiques seulement).
## Filtre : evidence IA explicite + perimetre Core + publies depuis N jours (defaut 120). Le routeur applique ensuite ses propres regles.
root <- Sys.getenv("PREIS_ROOT", unset = getwd()); N <- as.numeric(Sys.getenv("PREIS_CAND_DAYS", "120"))
x <- read.csv(file.path(root, "data/enriched/ai/ai_tracker_final.csv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = "", encoding = "UTF-8")
names(x) <- sub("^(﻿|ï»¿)", "", names(x)); x[is.na(x)] <- ""
d <- suppressWarnings(as.Date(substr(x$publication_date, 1, 10)))
keep <- x$ai_method_evidence == "explicit" & x$ph_scope_tier == "Core" & !is.na(d) & d >= Sys.Date() - N & d <= Sys.Date() + 1
drop <- intersect(c("abstract","abstract_clean","text_for_classification","full_text","text_all"), names(x))
big <- names(x)[vapply(x, function(v) max(nchar(v, allowNA = TRUE, keepNA = FALSE), 0L, na.rm = TRUE) > 1500, logical(1))]
drop <- union(drop, big)
y <- x[keep, setdiff(names(x), drop), drop = FALSE]
dir.create(file.path(root, "data/alerting"), recursive = TRUE, showWarnings = FALSE)
write.csv(y, file.path(root, "data/alerting/alert_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("alert_candidates.csv :", nrow(y), "lignes,", ncol(y), "colonnes,", round(file.size(file.path(root, "data/alerting/alert_candidates.csv"))/1e6, 2), "Mo\n")
