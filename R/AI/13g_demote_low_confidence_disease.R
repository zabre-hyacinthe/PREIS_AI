## ============================================================
## 13g_demote_low_confidence_disease.R - Retrograde les maladies a confiance FAIBLE (mention isolee)
## Constat (audit 2026-10-13, journal_corrections C013) : 286 articles etiquetes COVID-19 sur la seule mention
## isolee du mot "covid" ; sur le test aveugle, 1/6 etiquettes Low concordaient avec l'humain.
## Regle : disease_confidence == "Low" ET disease_evidence == "single_mention" ET maladie specifique
##         -> disease_primary = "Unspecified" ; l'ancienne etiquette est CONSERVEE dans disease_mentioned.
## Exception : filovirus (Ebola, Marburg) JAMAIS retrogrades automatiquement -> flag needs_human_review + fichier de revue.
## Sorties : tracker mis a jour (copie de sauvegarde datee), data/outputs/ai/disease_demotion/{summary,filovirus_low_review}.csv
## A lancer juste apres 13_classify_disease_into_tracker.R. Idempotent. Aucune donnee supprimee.
## Usage : Rscript R/AI/13g_demote_low_confidence_disease.R      (PREIS_ROOT, PREIS_TRACKER_PATH optionnels ; PREIS_DEMOTE_DRY=true -> n'ecrit pas le tracker)
## ============================================================
## [20261026] racine ancree (le planificateur lance R depuis C:/Windows/System32 -> getwd() cassait l'etape, journal_corrections C016)
root <- Sys.getenv("PREIS_ROOT", unset = Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN"))
if (!file.exists(file.path(root, "data/enriched/ai/ai_tracker_final.csv")) && file.exists(file.path(getwd(), "data/enriched/ai/ai_tracker_final.csv"))) root <- getwd()
trk_path <- Sys.getenv("PREIS_TRACKER_PATH", file.path(root, "data/enriched/ai/ai_tracker_final.csv"))
DRY <- tolower(Sys.getenv("PREIS_DEMOTE_DRY", "false")) == "true"
FILO <- c("Ebola", "Marburg")
log13g <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "[13g]", ..., "\n")
x <- read.csv(trk_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0), check.names = FALSE, encoding = "UTF-8")
names(x) <- sub("^(﻿|ï»¿)", "", names(x))
need <- c("disease_primary", "disease_confidence", "disease_evidence")
if (!all(need %in% names(x))) stop("Colonnes manquantes : ", paste(setdiff(need, names(x)), collapse = ", "))
n0 <- nrow(x)
if (!"disease_mentioned" %in% names(x)) x$disease_mentioned <- ""
if (!"needs_human_review" %in% names(x)) x$needs_human_review <- ""
if (!"disease_demoted" %in% names(x)) x$disease_demoted <- ""
low <- x$disease_confidence == "Low" & x$disease_evidence == "single_mention"
generic <- x$disease_primary %in% c("Unspecified", "Multiple / generic", "")
## a) maladies specifiques a mention isolee -> Unspecified (ancienne etiquette conservee)
dm <- low & !generic & !(x$disease_primary %in% FILO)
x$disease_mentioned[dm & !nzchar(x$disease_mentioned)] <- x$disease_primary[dm & !nzchar(x$disease_mentioned)]
before <- table(x$disease_primary[dm])
x$disease_primary[dm] <- "Unspecified"
if ("disease_group" %in% names(x)) x$disease_group[dm] <- "Unspecified"
if ("pathogen_class" %in% names(x)) x$pathogen_class[dm] <- "Unspecified"
x$disease_demoted[dm] <- "low_confidence_single_mention"
## b) deja 'Multiple / generic' : on garde la liste des maladies citees
mg <- low & x$disease_primary == "Multiple / generic" & !nzchar(x$disease_mentioned) & "disease_mentions" %in% names(x)
if (any(mg)) x$disease_mentioned[mg] <- x$disease_mentions[mg]
## c) filovirus a faible confiance : revue humaine
fl <- low & x$disease_primary %in% FILO
x$needs_human_review[fl & !nzchar(x$needs_human_review)] <- "filovirus_low_confidence"
if (nrow(x) != n0) stop("Nombre de lignes modifie : abandon")
log13g("Records:", n0, "| retrogrades:", sum(dm), "| filovirus a revoir:", sum(fl), "| 'Multiple / generic' documentes:", sum(mg))
if (length(before)) { cat("Detail des retrogradations (ancienne etiquette -> Unspecified) :\n"); print(sort(before, decreasing = TRUE)) }
out_dir <- file.path(root, "data/outputs/ai/disease_demotion"); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
sm <- data.frame(date = format(Sys.Date()), n_records = n0, n_demoted = sum(dm), n_filovirus_review = sum(fl), n_multiple_generic_documented = sum(mg), stringsAsFactors = FALSE)
write.csv(sm, file.path(out_dir, paste0("summary_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
keep <- intersect(c("record_id", "title", "publication_date", "disease_primary", "disease_confidence", "disease_keywords_matched", "disease_mentions", "country", "countries_mentioned", "geo_evidence", "doi", "url"), names(x))
rv <- x[fl, keep, drop = FALSE]; rv$human_verdict <- ""; rv$human_correct_disease <- ""; rv$human_correct_country <- ""; rv$comment <- ""
write.csv(rv, file.path(out_dir, "filovirus_low_review.csv"), row.names = FALSE, fileEncoding = "UTF-8")
log13g("Fichier de revue humaine :", file.path(out_dir, "filovirus_low_review.csv"), "(", nrow(rv), "lignes )")
if (DRY) { log13g("PREIS_DEMOTE_DRY=true : tracker NON modifie"); quit(save = "no") }
if (sum(dm) > 0 || any(c("disease_mentioned", "needs_human_review", "disease_demoted") %in% names(x))) {
  bk <- file.path(dirname(trk_path), paste0(basename(trk_path), ".bak_before_13g_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (!file.exists(bk)) file.copy(trk_path, bk)
  write.csv(x, trk_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  log13g("Tracker ecrit :", trk_path, "| sauvegarde :", basename(bk))
}
