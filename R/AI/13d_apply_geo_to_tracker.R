# =============================================================
# PREIS-AI — GÉOGRAPHIE DANS LE TRACKER (P1) — version autonome
# Fichier : R/AI/13d_apply_geo_to_tracker.R
# Auteur  : Claude — 2026-08-06
#
# Détecte country / iso3 / rcc à partir du texte (titre + résumé +
# text_all) via une table des 54 pays de l'Union africaine, et écrit
# le résultat DIRECTEMENT dans ai_tracker_final.csv.
#
# Autonome : ne dépend pas de rebuild_ai_geo_from_text() (qui utilisait
# cur_data(), déprécié et défaillant sous dplyr 1.1 / R 4.6).
# =============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tibble)
})

PROJECT_DIR <- Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN")
TRACKER <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")
QC      <- file.path(PROJECT_DIR, "data", "outputs", "ai", "ai_geo_qc.csv")

log_ <- function(...) cat(sprintf("[%s] %s\n",
  format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = "")))

# --- Geographie par gazetteer v2 (2026-10-06) ------------------
# Ancienne methode : "premier pays de la liste alphabetique qui matche" ->
# erreurs (Sudan virus -> Soudan, Zaire ebolavirus -> RDC, Papua New Guinea ->
# Guinee, 1 pays cite en passant = pays de l'etude). Nouvelle methode :
# lib_geo_gazetteer.R (alias/gentiles/villes, homonymes neutralises, pays du
# titre puis pays dominant). RCC issu de config/rcc_country_fv.csv.
source(file.path(PROJECT_DIR, "R", "AI", "lib_geo_gazetteer.R"))
G <- geo_load(PROJECT_DIR)

trk <- readr::read_csv(TRACKER, show_col_types = FALSE, progress = FALSE)

na2 <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
log_("Detection geographique (gazetteer v2, ", nrow(G$gaz), " alias)...")
n <- nrow(trk)
res <- geo_resolve(na2(trk$title),
                   if ("abstract" %in% names(trk)) na2(trk$abstract) else rep("", n), G)
trk$country <- res$country
trk$iso3    <- res$iso3
trk$rcc     <- res$rcc
trk$countries_mentioned <- res$countries_mentioned
trk$geo_evidence        <- res$geo_evidence

# --- Écriture + QC -------------------------------------------
readr::write_csv(trk, TRACKER, na = "")

n_country <- sum(trk$country != "Unspecified")
log_("Pays détecté : ", n_country, " / ", n,
     " (", round(100 * n_country / n, 1), "%)")

qc <- trk %>% count(country, sort = TRUE)
dir.create(dirname(QC), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(qc, QC, na = "")

log_("Top pays :")
print(utils::head(as.data.frame(
  sort(table(trk$country[trk$country != "Unspecified"]), decreasing = TRUE)), 12))

# Focus Ebola : RDC / Ouganda
if ("disease_group" %in% names(trk)) {
  vhf <- trk[trk$disease_group == "Viral hemorrhagic fever" &
               !is.na(trk$disease_group), ]
  log_("VHF (", nrow(vhf), ") — RDC : ",
       sum(vhf$country == "Democratic Republic of the Congo"),
       " | Ouganda : ", sum(vhf$country == "Uganda"),
       " | Guinée : ", sum(vhf$country == "Guinea"),
       " | Nigeria : ", sum(vhf$country == "Nigeria"))
}
log_("QC écrit : ", QC)
