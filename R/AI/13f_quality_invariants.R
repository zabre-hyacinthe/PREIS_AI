# =============================================================
# PREIS-AI - CONTRE-VERIFICATION AUTOMATIQUE (invariants + confiance)
# Fichier : R/AI/13f_quality_invariants.R   (2026-10-06)
# A executer APRES geo (13d) et AVANT les exports (14).
#  1) Controles d'invariants entre dimensions (coherence interne).
#  2) Verification croisee Ebola par une regex INDEPENDANTE du dictionnaire.
#  3) classification_confidence (High / Medium / Low) par article : nombre de
#     dimensions a preuve faible (methode, maladie, pays, application).
#  Ecrit : data/outputs/ai/qc_invariants.csv et ajoute au tracker les colonnes
#  classification_flags / classification_confidence. Ne supprime rien.
# =============================================================
suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr) })
PROJECT_DIR <- Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN")
TRACKER <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")
QC      <- file.path(PROJECT_DIR, "data", "outputs", "ai", "qc_invariants.csv")
log13f <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = "")))

trk <- readr::read_csv(TRACKER, show_col_types = FALSE, progress = FALSE)
n <- nrow(trk)

# --- 0) Normalisation (2026-10-06) -----------------------------------
# a) Noms de source : une seule graphie canonique (valeur d'origine gardee dans source_name_raw).
if ("source_name" %in% names(trk)) {
  canon <- c(arxiv = "arXiv", medrxiv = "medRxiv", biorxiv = "bioRxiv", pubmed = "PubMed",
             europepmc = "EuropePMC")
  raw_src <- as.character(trk$source_name)
  if (!"source_name_raw" %in% names(trk)) trk$source_name_raw <- raw_src
  k <- tolower(trimws(raw_src))
  hit <- !is.na(k) & k %in% names(canon)
  trk$source_name[hit] <- unname(canon[k[hit]])
}
# b) Dates futures : annee > annee courante = erreur de metadonnees. La valeur d'origine est
#    conservee (pub_year_raw), pub_year passe a NA et date_flag = "future_date".
if ("pub_year" %in% names(trk)) {
  if (!"pub_year_raw" %in% names(trk)) trk$pub_year_raw <- trk$pub_year
  yr <- suppressWarnings(as.integer(trk$pub_year_raw))
  fut <- !is.na(yr) & yr > as.integer(format(Sys.Date(), "%Y"))
  trk$date_flag <- ifelse(fut, "future_date", "")
  trk$pub_year[fut] <- NA
  if ("year" %in% names(trk)) { trk$year <- suppressWarnings(as.integer(trk$year)); trk$year[fut] <- NA }
  if (!is.integer(trk$pub_year) && !is.numeric(trk$pub_year)) trk$pub_year <- suppressWarnings(as.integer(trk$pub_year))
}
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
col <- function(nm) if (nm %in% names(trk)) chr(trk[[nm]]) else rep("", n)
txt <- tolower(paste(col("title"), col("abstract")))

checks <- list()
add <- function(name, bad, note = "") {
  checks[[length(checks) + 1]] <<- data.frame(check = name, n_violations = sum(bad, na.rm = TRUE),
                                              n_checked = n, note = note, stringsAsFactors = FALSE)
}

# --- 1) Invariants --------------------------------------------------
add("record_id unique", duplicated(col("record_id")))
add("country renseigne mais iso3 manquant", col("country") != "Unspecified" & col("country") != "" & (col("iso3") %in% c("", "Unspecified")))
add("country renseigne mais absent de countries_mentioned",
    col("country") != "Unspecified" & col("country") != "" &
      !mapply(function(c, m) grepl(c, m, fixed = TRUE), col("country"), col("countries_mentioned")))
add("methode renseignee mais ai_method_evidence = none", col("method_family_primary") != "" & col("ai_method_evidence") == "none")
add("ai_method_evidence != none mais methode vide", col("method_family_primary") == "" & !col("ai_method_evidence") %in% c("none", ""))
add("Ebola direct mais ebola_mentioned FALSE", col("disease_primary") == "Ebola" & toupper(col("ebola_mentioned")) != "TRUE")
add("Annee de publication future", suppressWarnings(as.integer(col("pub_year_raw"))) > as.integer(format(Sys.Date(), "%Y")),
    "dates a verifier (preprints a venir / erreurs de saisie)")
add("Source avec variantes de casse", length(unique(col("source_name"))) != length(unique(tolower(col("source_name")))))

# --- 2) Verification croisee Ebola (regex independante du dictionnaire) --
ebola_rx <- "\\bebola\\b|\\bebolavirus\\b|\\borthoebolavirus\\b|\\bebov\\b|\\bbundibugyo\\b|\\bsudan virus\\b"
txt_noeos <- gsub("ebola optimi[sz]ation( search)?( algorithm)?", " ", txt)
indep_ebola <- grepl(ebola_rx, txt_noeos, perl = TRUE)
add("Ebola direct (dictionnaire) sans aucun terme Ebola dans le texte", col("disease_primary") == "Ebola" & !indep_ebola)
add("Terme Ebola dans le TITRE mais disease_primary != Ebola",
    grepl(ebola_rx, gsub("ebola optimi[sz]ation( search)?( algorithm)?", " ", tolower(col("title"))), perl = TRUE) & col("disease_primary") != "Ebola",
    "a examiner : titre Ebola classe autre maladie")
# [20261026] Controle de la correction maladie 13g (journal C016) : alerte si elle n'a pas tourne
add("correction 13g non appliquee (colonne disease_demoted absente)", if ("disease_demoted" %in% names(trk)) logical(n) else rep(TRUE, n),
    "ECHEC si > 0 : l'etape demote_low_conf_disease n'a pas ecrit le tracker (voir journal du pipeline)")
add("maladie a confiance faible (mention unique) non retrogradee, hors Ebola/Marburg",
    col("disease_confidence") == "Low" & col("disease_evidence") == "single_mention" &
      !col("disease_primary") %in% c("Unspecified", "Multiple / generic", "", "Ebola", "Marburg"),
    "doit etre 0 apres 13g")
if (!"disease_demoted" %in% names(trk)) log13f("!!! ALERTE : la colonne disease_demoted est absente - la correction 13g n'a pas ete appliquee dans ce run !!!")
qc <- dplyr::bind_rows(checks)

# --- 3) Confiance de classification ---------------------------------
f_method <- col("ai_method_evidence") %in% c("mention_only", "none")
f_dis    <- col("disease_confidence") == "Low"
f_geo    <- col("geo_evidence") == "ambiguous"
app_top  <- suppressWarnings(as.integer(col("application_score_top")))
f_app    <- is.na(app_top) | app_top <= 1L
nflag <- as.integer(f_method) + as.integer(f_dis) + as.integer(f_geo) + as.integer(f_app)
trk$classification_flags <- paste0(ifelse(f_method, "method;", ""), ifelse(f_dis, "disease;", ""),
                                   ifelse(f_geo, "country;", ""), ifelse(f_app, "application;", ""))
trk$classification_confidence <- ifelse(nflag == 0L, "High", ifelse(nflag == 1L, "Medium", "Low"))

dir.create(dirname(QC), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(qc, QC, na = "")
readr::write_csv(trk, TRACKER, na = "")
print(as.data.frame(qc))
log13f("Confiance de classification : ")
print(table(trk$classification_confidence, useNA = "ifany"))
log13f("QC invariants ecrit : ", QC)
