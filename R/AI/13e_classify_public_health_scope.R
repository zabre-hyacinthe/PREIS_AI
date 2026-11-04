# =========================================================
# PREIS-AI - PUBLIC HEALTH SCOPE FILTER (Finding #4)
# File: 13e_classify_public_health_scope.R
# Cree le 2026-10-05.
# Purpose: etiqueter (sans jamais supprimer) la pertinence thematique
#   "maladies infectieuses / epidemies" de chaque article du tracker.
# Colonnes ajoutees : ph_scope_tier (Core / Adjacent / Off-topic),
#   ph_in_scope (TRUE/FALSE), ph_scope_reason.
# Regle (v1, mots-cles, auditee sur 120 articles lus a l'aveugle) :
#   Core     = maladie du dictionnaire dans le titre, OU maladie + >=1 terme
#              epidemiologique, OU terme epidemiologique dans le titre,
#              OU >=3 termes epidemiologiques.
#   Adjacent = au moins une maladie du dictionnaire OU un terme epi.
#   Off-topic = aucun des deux.
# Garde-fou : Ebola/Marburg/Lassa/CCHF (dictionnaire) => jamais Off-topic.
# v2 (2026-10-28) : voir ci-dessous bio_terms ; effet : 276 notices Core -> Adjacent sur 8 645 ; pilote n=50 : precision Core 0,81 -> 0,87, rappel 0,92 inchange (regle ajustee sur ces 50 : non independant).
# Limite : etiquette automatique de premier niveau, non une validation humaine.
# =========================================================
options(stringsAsFactors = FALSE, warn = 1)
## [20261028 v2] racine ancree (PREIS_ROOT) + regle v2 (journal_corrections C017)
PROJECT_DIR <- Sys.getenv("PREIS_ROOT", unset = Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN"))
TRACKER <- Sys.getenv("PREIS_TRACKER_PATH", file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv"))
DICT    <- file.path(PROJECT_DIR, "config", "ai_disease_dictionary.csv")
log13e <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", paste0(..., collapse = ""), "\n")

trk  <- utils::read.csv(TRACKER, stringsAsFactors = FALSE, check.names = FALSE)
dict <- utils::read.csv(DICT, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(trk)) stop("Tracker vide")
kw <- trimws(unlist(strsplit(dict$keywords, ";")))
kw <- unique(kw[nzchar(kw)])
esc <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
dis_rx <- paste0("\\b(?:", paste(esc(kw), collapse = "|"), ")\\b")
strong <- paste0("outbreaks?|epidemics?|pandemics?|epidemiolog\\w*|infectious diseases?|communicable|",
  "pathogens?|zoonotic|zoonosis|contact tracing|case fatality|disease surveillance|",
  "public health surveillance|epidemic intelligence|event-based surveillance|syndromic|",
  "emerging diseases?|vaccin\\w+|infection rates?|transmission dynamics|reproduction number|",
  "virus(?:es)?|viral|bacteri\\w+|antimicrobial|antibiotic")
epi_rx <- paste0("\\b(?:", strong, ")\\b")
## v2 : termes purement biologiques (virus, bacteries, pathogenes, antimicrobien, antibiotique) ne suffisent plus
## pour "Core" (test pilote n=50 : 3 faux positifs Core sur des sujets de biologie/therapie). Ils restent
## comptes pour "Adjacent". Les termes epidemiques (outbreak, epidemic, vaccin*, surveillance...) gardent la regle v1.
strong_epi <- paste0("outbreaks?|epidemics?|pandemics?|epidemiolog\\w*|infectious diseases?|communicable|",
  "zoonotic|zoonosis|contact tracing|case fatality|disease surveillance|",
  "public health surveillance|epidemic intelligence|event-based surveillance|syndromic|",
  "emerging diseases?|vaccin\\w+|infection rates?|transmission dynamics|reproduction number")
epi_only_rx <- paste0("\\b(?:", strong_epi, ")\\b")

nhits <- function(rx, x) vapply(gregexpr(rx, x, perl = TRUE, ignore.case = TRUE),
                                function(m) if (m[1] == -1) 0L else length(m), integer(1))
ttl <- if ("title" %in% names(trk)) trk$title else rep("", nrow(trk))
abs <- if ("abstract" %in% names(trk)) trk$abstract else rep("", nrow(trk))
ttl[is.na(ttl)] <- ""; abs[is.na(abs)] <- ""
txt <- paste0(ttl, ". ", abs)

d_all <- nhits(dis_rx, txt); d_ttl <- nhits(dis_rx, ttl) > 0
b_all <- nhits(epi_rx, txt)                      # tous termes (epidemiques + biologiques) -> Adjacent
s_all <- nhits(epi_only_rx, txt); s_ttl <- nhits(epi_only_rx, ttl) > 0   # termes epidemiques seuls -> Core

tier <- ifelse(d_ttl | (d_all > 0 & s_all > 0) | s_ttl | s_all >= 3, "Core",
        ifelse(d_all > 0 | b_all > 0, "Adjacent", "Off-topic"))
reason <- ifelse(d_ttl, "disease_in_title",
          ifelse(d_all > 0 & s_all > 0, "disease_plus_epi_term",
          ifelse(s_ttl, "epi_term_in_title",
          ifelse(s_all >= 3, "three_or_more_epi_terms",
          ifelse(d_all > 0, "disease_only",
          ifelse(b_all > 0, "epi_term_only", "no_signal"))))))
trk$ph_scope_tier   <- tier
trk$ph_in_scope     <- tier != "Off-topic"
trk$ph_scope_reason <- reason

utils::write.csv(trk, TRACKER, row.names = FALSE, na = "")
log13e("Public-health scope done. Rows: ", nrow(trk))
print(table(trk$ph_scope_tier, useNA = "ifany"))
