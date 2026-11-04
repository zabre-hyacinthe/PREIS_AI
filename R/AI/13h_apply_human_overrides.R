## ============================================================
## 13h_apply_human_overrides.R - Applique les corrections HUMAINES persistantes (priorite sur la classification automatique)
## Entree : config/human_overrides.csv (record_id, disease_primary, reviewer, review_date, source, note) ; ajouter des lignes, ne jamais en supprimer.
## Effet : disease_primary impose si fourni ; human_validated="yes" ; needs_human_review vide ; ancienne etiquette conservee dans disease_mentioned.
## A lancer APRES 13g. Idempotent. Cree 28/10/2026 (journal_corrections C018). Usage : Rscript R/AI/13h_apply_human_overrides.R (PREIS_ROOT optionnel)
## ============================================================
root <- Sys.getenv("PREIS_ROOT", unset = Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN"))
if (!file.exists(file.path(root, "data/enriched/ai/ai_tracker_final.csv")) && file.exists(file.path(getwd(), "data/enriched/ai/ai_tracker_final.csv"))) root <- getwd()
trk_path <- Sys.getenv("PREIS_TRACKER_PATH", file.path(root, "data/enriched/ai/ai_tracker_final.csv"))
ov_path  <- file.path(root, "config/human_overrides.csv")
log13h <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "[13h]", ..., "\n")
if (!file.exists(ov_path)) { log13h("Pas de fichier", ov_path, "- rien a faire"); quit(save = "no") }
x  <- read.csv(trk_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0), check.names = FALSE, encoding = "UTF-8")
names(x) <- sub("^(\ufeff|\u00ef\u00bb\u00bf)", "", names(x))
ov <- read.csv(ov_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0), check.names = FALSE, encoding = "UTF-8")
names(ov) <- sub("^(\ufeff|\u00ef\u00bb\u00bf)", "", names(ov))
ov <- ov[!duplicated(ov$record_id, fromLast = TRUE), , drop = FALSE]   # derniere ligne = decision retenue
n0 <- nrow(x)
for (cc in c("human_validated", "disease_mentioned", "needs_human_review", "disease_demoted")) if (!cc %in% names(x)) x[[cc]] <- ""
i <- match(ov$record_id, x$record_id); found <- !is.na(i)
changed <- 0L
for (k in which(found)) {
  r <- i[k]; new <- ov$disease_primary[k]
  if (nzchar(new) && x$disease_primary[r] != new) {
    if (!nzchar(x$disease_mentioned[r])) x$disease_mentioned[r] <- x$disease_primary[r]
    x$disease_primary[r] <- new
    if ("disease_group" %in% names(x)) x$disease_group[r] <- if (new == "Unspecified") "Unspecified" else x$disease_group[r]
    if ("pathogen_class" %in% names(x)) x$pathogen_class[r] <- if (new == "Unspecified") "Unspecified" else x$pathogen_class[r]
    x$disease_demoted[r] <- "human_override"; changed <- changed + 1L
  }
  x$human_validated[r] <- "yes"; x$needs_human_review[r] <- ""
}
if (nrow(x) != n0) stop("Nombre de lignes modifie : abandon")
log13h("Corrections lues:", nrow(ov), "| retrouvees dans le tracker:", sum(found), "| etiquette maladie modifiee:", changed)
if (any(!found)) log13h("Non retrouvees (record_id absents du tracker courant):", paste(ov$record_id[!found], collapse = "; "))
if (tolower(Sys.getenv("PREIS_DEMOTE_DRY", "false")) == "true") { log13h("DRY: tracker NON modifie"); quit(save = "no") }
write.csv(x, trk_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
log13h("Tracker ecrit :", trk_path)
