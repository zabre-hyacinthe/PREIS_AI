# =============================================================
# PREIS-AI — CLASSIFICATION MALADIE DANS LE TRACKER (P1)
# Fichier : R/AI/13_classify_disease_into_tracker.R
# Auteur  : Claude — 2026-08-05
#
# POURQUOI
#   Le tracker n'avait AUCUNE colonne maladie -> Ebola non filtrable.
#   La fonction classify_ai_disease() (12_classify_ai_disease.R) existait
#   mais était câblée comme une "étape" no-op (jamais appelée).
#
# CE QUE FAIT CE SCRIPT (top-level, comme 10_/11_)
#   - lit le tracker enrichi
#   - applique le dictionnaire maladie (config/ai_disease_dictionary.csv)
#     sur titre + résumé + mots-clés
#   - ajoute disease_primary / disease_group / pathogen_class /
#     disease_detected / disease_keywords_matched
#   - réécrit le tracker + un fichier QC (distribution des maladies)
#
#   À insérer APRÈS build_tracker/classify_applications et AVANT
#   build_exports, pour que la maladie se propage au dashboard.
# =============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tibble)
})

PROJECT_DIR <- Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN")
TRACKER <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")
DICT    <- file.path(PROJECT_DIR, "config", "ai_disease_dictionary.csv")
QC      <- file.path(PROJECT_DIR, "data", "outputs", "ai", "ai_disease_qc.csv")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

log13 <- function(...) cat(sprintf("[%s] %s\n",
  format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = "")))

# --- 1) Charger la fonction de classification maladie --------
fn_file <- file.path(PROJECT_DIR, "R", "AI", "12_classify_ai_disease.R")
if (!file.exists(fn_file)) stop("Introuvable : ", fn_file)
source(fn_file)  # définit classify_ai_disease(df, dict_file)

if (!exists("classify_ai_disease"))
  stop("La fonction classify_ai_disease() n'a pas été chargée.")

# --- 2) Vérifs d'entrée --------------------------------------
if (!file.exists(TRACKER)) stop("Tracker introuvable : ", TRACKER)
if (!file.exists(DICT))    stop("Dictionnaire maladie introuvable : ", DICT)

log13("Lecture du tracker : ", TRACKER)
trk <- readr::read_csv(TRACKER, show_col_types = FALSE, progress = FALSE)
n0  <- nrow(trk)
log13("Lignes : ", n0)

# --- 3) Classification maladie ------------------------------
log13("Application du dictionnaire maladie...")
trk2 <- classify_ai_disease(trk, dict_file = DICT)

# Sécurité : ne jamais perdre de lignes
if (nrow(trk2) != n0)
  stop(sprintf("Nombre de lignes modifié (%d -> %d) — abandon.", n0, nrow(trk2)))

# --- 3b) Confiance et demotion des mentions isolees multi-maladies ----
# Audit 2026-10-06 : parmi les articles etiquetes Ebola sur une SEULE mention,
# la majorite cite plusieurs maladies en passant (revues generales, outils
# generiques) et ne porte pas sur Ebola. Regle : evidence = single_mention
# ET >= 2 maladies mentionnees  ->  disease_primary = "Multiple / generic".
# Le detail reste dans disease_mentions / ebola_mentioned (rien n'est perdu).
# disease_confidence : High (titre) / Medium (mentions multiples dans le
# texte) / Low (mention isolee) / None (aucune maladie).
n_dis <- ifelse(is.na(trk2$disease_mentions) | trk2$disease_mentions == "", 0L,
                lengths(strsplit(as.character(trk2$disease_mentions), ";\\s*")))
trk2$disease_confidence <- dplyr::case_when(
  trk2$disease_evidence == "title" ~ "High",
  trk2$disease_evidence == "text_multiple" ~ "Medium",
  trk2$disease_evidence == "single_mention" ~ "Low",
  TRUE ~ "None")
demote <- !is.na(trk2$disease_evidence) & trk2$disease_evidence == "single_mention" & n_dis >= 2L
log13("Demotion mentions isolees multi-maladies -> 'Multiple / generic' : ", sum(demote))
trk2$disease_primary_before_demotion <- trk2$disease_primary
trk2$disease_primary[demote] <- "Multiple / generic"
trk2$disease_group[demote]   <- "Multiple diseases (generic mention)"
trk2$pathogen_class[demote]  <- "Mixed"
trk2$disease_confidence[demote] <- "Low"

# --- 4) Réécriture + QC -------------------------------------
readr::write_csv(trk2, TRACKER, na = "")
log13("Tracker réécrit avec colonnes maladie.")

qc <- trk2 %>%
  count(disease_primary, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))
dir.create(dirname(QC), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(qc, QC, na = "")

# --- 5) Résumé console --------------------------------------
n_detected <- sum(trk2$disease_detected %in% c(TRUE, "TRUE"), na.rm = TRUE)
n_ebola    <- sum(trk2$disease_primary == "Ebola", na.rm = TRUE)
n_vhf      <- sum(trk2$disease_group %in%
  c("Viral hemorrhagic fever"), na.rm = TRUE)

log13("Maladie détectée (non 'Unspecified') : ", n_detected,
      " / ", n0, " (", round(100 * n_detected / n0, 1), "%)")
log13("Ebola : ", n_ebola, " | Fièvres hémorragiques virales (groupe) : ", n_vhf)
log13("Top maladies :")
print(utils::head(as.data.frame(qc), 15))
log13("QC écrit : ", QC)
