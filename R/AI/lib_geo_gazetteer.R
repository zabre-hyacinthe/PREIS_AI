# =============================================================
# PREIS-AI - Bibliotheque geographique partagee (gazetteer v2)
# Fichier : R/AI/lib_geo_gazetteer.R   (cree le 2026-10-06)
#
# Methode (geoparsing a base de gazetteer) :
#  1. Referentiel unique : config/africa_gazetteer_v2.csv
#     (55 Etats membres UA ; noms officiels, variantes EN/FR/PT, anciens noms,
#      gentiles, grandes villes et regions d'epidemie ; ISO3 ; RCC issu de
#      config/rcc_country_fv.csv).
#  2. Neutralisation des homonymes non geographiques AVANT detection :
#     config/africa_nongeo_patterns.csv (Sudan virus, Zaire ebolavirus,
#     Papua New Guinea, guinea pig, Aspergillus niger, Crimean-Congo, ...).
#  3. Remplacement des alias du plus long au plus court par un jeton ISO3
#     ("south sudan" avant "sudan", "guinea-bissau" avant "guinea").
#  4. Pays principal : pays du TITRE (le plus frequent), sinon pays dominant
#     du resume (>= 2 mentions et >= 2x le deuxieme). Sinon "Unspecified"
#     (les pays cites restent dans countries_mentioned).
# Meme logique pour 13d_apply_geo_to_tracker.R et 10_score_ai_africa_relevance.R.
# =============================================================
suppressPackageStartupMessages({ library(stringi); library(readr) })

geo_load <- function(project_root) {
  gaz <- readr::read_csv(file.path(project_root, "config", "africa_gazetteer_v2.csv"),
                         show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  ng  <- readr::read_csv(file.path(project_root, "config", "africa_nongeo_patterns.csv"),
                         show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  gaz$pat <- ifelse(gaz$alias_type == "regex", gaz$alias,
              paste0("(?<![a-z0-9])", stringi::stri_replace_all_regex(
                       geo_fold(gaz$alias), "([\\\\.\\[\\]\\(\\)\\{\\}\\^\\$\\|\\?\\*\\+\\-])", "\\\\$1"),
                     "(?![a-z0-9])"))
  gaz$len <- nchar(gaz$alias)
  gaz <- gaz[order(-gaz$len), ]
  ctry <- gaz[!duplicated(gaz$iso3), c("iso3", "country", "rcc")]
  list(gaz = gaz, countries = ctry,
       ng_rx = paste0("\\b(?:", paste(ng$pattern, collapse = "|"), ")\\b"),
       any_rx = paste(gaz$pat, collapse = "|"))
}

geo_fold <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringi::stri_replace_all_regex(x, "[‘’`]", "'")
  stringi::stri_trans_tolower(x)
}

# Retourne une matrice (n x K) du nombre de mentions par pays (colonnes = ISO3)
geo_counts <- function(text, G) {
  t <- stringi::stri_replace_all_regex(geo_fold(text), G$ng_rx, " ")
  K <- nrow(G$countries)
  out <- matrix(0L, nrow = length(t), ncol = K, dimnames = list(NULL, G$countries$iso3))
  cand <- which(stringi::stri_detect_regex(t, G$any_rx))
  if (!length(cand)) return(out)
  tc <- t[cand]
  for (i in seq_len(nrow(G$gaz))) {
    tc <- stringi::stri_replace_all_regex(tc, G$gaz$pat[i],
            paste0(" zz", tolower(G$gaz$iso3[i]), "zz "))
  }
  for (k in seq_len(K)) {
    out[cand, k] <- stringi::stri_count_fixed(tc, paste0("zz", tolower(G$countries$iso3[k]), "zz"))
  }
  out
}

# Analyse complete titre + resume : pays principal, preuve, pays cites
geo_resolve <- function(title, abstract, G) {
  title <- as.character(title); title[is.na(title)] <- ""
  abstract <- as.character(abstract); abstract[is.na(abstract)] <- ""
  ct <- geo_counts(title, G)
  ca <- geo_counts(paste0(title, ". ", abstract), G)
  n <- nrow(ca); iso <- G$countries$iso3
  best <- rep(NA_integer_, n); evid <- rep("none", n); ment <- rep("", n)
  for (r in seq_len(n)) {
    if (sum(ca[r, ]) == 0) next
    ord <- order(-ca[r, ], seq_along(iso))
    ment[r] <- paste(G$countries$country[ord[ca[r, ord] > 0]], collapse = "; ")
    in_t <- which(ct[r, ] > 0)
    if (length(in_t)) {
      pool <- ct[r, ]; m <- max(pool); cands <- which(pool == m)
      if (length(cands) > 1) { m2 <- max(ca[r, cands]); cands <- cands[ca[r, cands] == m2] }
      best[r] <- cands[1]; evid[r] <- "title"
    } else {
      v <- sort(ca[r, ], decreasing = TRUE); top <- v[1]; second <- v[2]
      if (top >= 2 && top >= 2 * max(second, 1)) { best[r] <- which.max(ca[r, ]); evid[r] <- "dominant" }
      else evid[r] <- "ambiguous"
    }
  }
  data.frame(
    country = ifelse(is.na(best), "Unspecified", G$countries$country[best]),
    iso3    = ifelse(is.na(best), "Unspecified", iso[best]),
    rcc     = ifelse(is.na(best), "Unspecified", G$countries$rcc[best]),
    countries_mentioned = ment, geo_evidence = evid,
    n_countries_mentioned = rowSums(ca > 0),
    africa_country_in_title = rowSums(ct) > 0,
    africa_primary_mentions = apply(ca, 1, max),
    stringsAsFactors = FALSE)
}
