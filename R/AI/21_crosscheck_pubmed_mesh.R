# =============================================================
# PREIS-AI - CONTRE-VERIFICATION INDEPENDANTE PAR PUBMED (MeSH + affiliations)
# Fichier : R/AI/21_crosscheck_pubmed_mesh.R   (2026-10-06)
#
# Source INDEPENDANTE des regles du tracker : les descripteurs MeSH (indexation
# manuelle/semi-automatique de la NLM) et les affiliations d'auteurs des articles
# PubMed. Aucune relecture humaine requise.
#  - Maladie : maladie du tracker vs maladies deduites des MeSH.
#  - IA      : preuve d'IA du tracker vs MeSH "Artificial Intelligence / Machine
#              Learning / Deep Learning / Neural Networks / NLP ...".
#  - Pays    : pays du tracker vs pays des affiliations d'auteurs (gazetteer v2).
# Limites : seuls les articles PubMed (PMID) ET deja indexes MeSH sont comparables
# (les articles recents ne sont souvent pas encore indexes). C'est un accord entre
# deux sources, pas une verite terrain absolue (MeSH = indexation de la NLM).
# Reseau : eutils.ncbi.nlm.nih.gov (comme 05_fetch_pubmed_ai.R). Cache local :
# les PMID deja telecharges ne sont pas re-demandes.
# Sorties : data/outputs/ai/validation/crosscheck_pubmed_summary.csv
#           data/outputs/ai/validation/crosscheck_pubmed_detail.csv
# =============================================================
suppressPackageStartupMessages({ library(httr); library(xml2); library(readr); library(dplyr); library(stringr) })
PROJECT_DIR <- Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN")
TRACKER <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")
OUT_DIR <- file.path(PROJECT_DIR, "data", "outputs", "ai", "validation")
CACHE   <- file.path(OUT_DIR, "pubmed_mesh_cache.csv")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
log21 <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = "")))
source(file.path(PROJECT_DIR, "R", "AI", "lib_geo_gazetteer.R"))

# ---------- 1) Telechargement PubMed (avec cache) ----------
parse_pubmed_xml <- function(doc) {
  arts <- xml2::xml_find_all(doc, ".//PubmedArticle")
  do.call(rbind, lapply(arts, function(a) {
    pm <- xml2::xml_text(xml2::xml_find_first(a, ".//MedlineCitation/PMID"))
    mesh <- xml2::xml_text(xml2::xml_find_all(a, ".//MeshHeadingList/MeshHeading/DescriptorName"))
    kw   <- xml2::xml_text(xml2::xml_find_all(a, ".//KeywordList/Keyword"))
    aff  <- xml2::xml_text(xml2::xml_find_all(a, ".//AuthorList/Author/AffiliationInfo/Affiliation"))
    data.frame(pmid = pm, mesh = paste(mesh, collapse = " | "), keywords = paste(kw, collapse = " | "),
               affiliations = paste(aff, collapse = " || "), n_mesh = length(mesh), stringsAsFactors = FALSE)
  }))
}
fetch_batch <- function(ids) {
  r <- httr::POST("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
                  body = list(db = "pubmed", id = paste(ids, collapse = ","), retmode = "xml"),
                  encode = "form", httr::timeout(90))
  if (httr::status_code(r) != 200) stop("HTTP ", httr::status_code(r))
  parse_pubmed_xml(xml2::read_xml(httr::content(r, as = "text", encoding = "UTF-8")))
}

trk <- readr::read_csv(TRACKER, show_col_types = FALSE, progress = FALSE)
trk$pmid <- as.character(trk$pmid)
pm <- unique(trk$pmid[!is.na(trk$pmid) & grepl("^[0-9]+$", trk$pmid)])
cache <- if (file.exists(CACHE)) readr::read_csv(CACHE, show_col_types = FALSE, col_types = readr::cols(.default = "c")) else NULL
todo <- setdiff(pm, if (is.null(cache)) character(0) else cache$pmid)
log21("PMID dans le tracker : ", length(pm), " | deja en cache : ", length(pm) - length(todo), " | a telecharger : ", length(todo))
if (length(todo)) {
  batches <- split(todo, ceiling(seq_along(todo) / 100))
  for (i in seq_along(batches)) {
    res <- tryCatch(fetch_batch(batches[[i]]), error = function(e) { log21("  lot ", i, " echec : ", conditionMessage(e)); NULL })
    if (!is.null(res)) {
      res[] <- lapply(res, as.character)
      cache <- dplyr::bind_rows(cache, res)
      readr::write_csv(cache, CACHE, na = "")
    }
    log21("  lot ", i, "/", length(batches), " ok (", if (is.null(res)) 0 else nrow(res), " notices)")
    Sys.sleep(0.6)
  }
}
if (is.null(cache) || !nrow(cache)) stop("Aucune notice PubMed disponible (reseau ?).")

# ---------- 2) Correspondances MeSH -> maladies / IA ----------
mesh_dis <- c(
  "Ebola" = "ebolavirus|hemorrhagic fever, ebola|\\bebola", "Marburg" = "marburg", "Lassa fever" = "lassa",
  "COVID-19" = "covid-19|sars-cov-2|coronavirus infections", "Influenza" = "influenza",
  "HIV" = "\\bhiv\\b|acquired immunodeficiency", "Malaria" = "malaria|plasmodium", "Tuberculosis" = "tuberculosis",
  "Dengue" = "dengue", "Zika" = "zika", "Chikungunya" = "chikungunya", "Mpox" = "mpox|monkeypox",
  "Cholera" = "cholera", "Measles" = "measles", "Hepatitis" = "hepatitis", "Rabies" = "rabies",
  "Polio" = "poliomyelitis|poliovirus", "Yellow fever" = "yellow fever", "West Nile fever" = "west nile",
  "Rift Valley fever" = "rift valley", "Crimean-Congo hemorrhagic fever" = "crimean", "Typhoid fever" = "typhoid",
  "Meningitis" = "meningitis|meningococcal", "Leishmaniasis" = "leishmania", "Anthrax" = "anthrax",
  "Plague" = "plague|yersinia pestis", "Nipah" = "nipah", "Respiratory syncytial virus" = "respiratory syncytial",
  "Antimicrobial resistance" = "drug resistance, (microbial|bacterial|multiple)|anti-bacterial agents",
  "Schistosomiasis" = "schistosom", "Diphtheria" = "diphtheria")
mesh_ai <- "artificial intelligence|machine learning|deep learning|neural networks, computer|natural language processing|support vector machine|large language models|data mining|decision support systems, clinical"
mesh_to_dis <- function(m) { l <- tolower(m); names(mesh_dis)[vapply(mesh_dis, function(p) grepl(p, l, perl = TRUE), logical(1))] }

# ---------- 3) Comparaison ----------
d <- dplyr::inner_join(trk, cache[, c("pmid", "mesh", "keywords", "affiliations", "n_mesh")], by = "pmid")
d$n_mesh <- suppressWarnings(as.integer(d$n_mesh)); d$n_mesh[is.na(d$n_mesh)] <- 0L
dm <- d[d$n_mesh > 0, ]
log21("Articles PubMed du tracker avec notice : ", nrow(d), " | indexes MeSH : ", nrow(dm))
summ <- list(); S <- function(rubrique, metrique, valeur, n) summ[[length(summ) + 1]] <<- data.frame(rubrique, metrique, valeur = as.character(valeur), n = n, stringsAsFactors = FALSE)
pct <- function(a, b) if (b > 0) round(100 * a / b, 1) else NA
S("Couverture", "articles PubMed avec notice", nrow(d), nrow(d)); S("Couverture", "dont indexes MeSH", nrow(dm), nrow(d))

# 3a) Maladie
dis_mesh <- lapply(dm$mesh, mesh_to_dis)
dm$mesh_diseases <- vapply(dis_mesh, paste, character(1), collapse = "; ")
sys_dis <- dm$disease_primary; sys_has <- !is.na(sys_dis) & !sys_dis %in% c("Unspecified", "Multiple / generic")
in_mesh <- mapply(function(s, ms) s %in% ms, sys_dis, dis_mesh)
dm$disease_supported_by_mesh <- ifelse(sys_has, in_mesh, NA)
S("Maladie", "articles ou le tracker donne une maladie (MeSH-indexes)", sum(sys_has), nrow(dm))
S("Maladie", "% de ces maladies confirmees par un MeSH (precision)", pct(sum(in_mesh & sys_has), sum(sys_has)), sum(sys_has))
has_mesh_dis <- vapply(dis_mesh, length, integer(1)) > 0
S("Maladie", "articles avec une maladie MeSH reconnue", sum(has_mesh_dis), nrow(dm))
S("Maladie", "% de ces articles ou le tracker a une maladie du MeSH (rappel)", pct(sum(has_mesh_dis & in_mesh), sum(has_mesh_dis)), sum(has_mesh_dis))
for (b in c("Ebola", "COVID-19", "Influenza", "HIV", "Malaria", "Tuberculosis")) {
  mb <- vapply(dis_mesh, function(x) b %in% x, logical(1)); sb <- !is.na(sys_dis) & sys_dis == b
  S(paste0("Maladie - ", b), "MeSH oui & tracker oui | MeSH oui | tracker oui", paste(sum(mb & sb), sum(mb), sum(sb), sep = " | "), nrow(dm))
}
# 3b) IA
mesh_is_ai <- grepl(mesh_ai, tolower(dm$mesh), perl = TRUE)
sys_ai <- dm$ai_method_evidence %in% c("explicit", "mention_only")
dm$mesh_ai <- mesh_is_ai; dm$system_ai <- sys_ai
S("IA", "MeSH IA oui & tracker IA oui", sum(mesh_is_ai & sys_ai), nrow(dm))
S("IA", "MeSH IA oui & tracker IA non", sum(mesh_is_ai & !sys_ai), nrow(dm))
S("IA", "MeSH IA non & tracker IA oui", sum(!mesh_is_ai & sys_ai), nrow(dm))
S("IA", "MeSH IA non & tracker IA non", sum(!mesh_is_ai & !sys_ai), nrow(dm))
S("IA", "% des articles MeSH-IA reperes par le tracker (rappel)", pct(sum(mesh_is_ai & sys_ai), sum(mesh_is_ai)), sum(mesh_is_ai))
# 3c) Pays (affiliations) - sur tous les articles PubMed avec affiliations
da <- d[!is.na(d$affiliations) & d$affiliations != "", ]
G <- geo_load(PROJECT_DIR)
ca <- geo_counts(da$affiliations, G)
aff_ctry <- apply(ca, 1, function(r) paste(G$countries$country[r > 0], collapse = "; "))
da$affiliation_africa_countries <- aff_ctry
sys_c <- da$country; has_c <- !is.na(sys_c) & sys_c != "Unspecified"
sup <- mapply(function(c, a) grepl(c, a, fixed = TRUE), sys_c, aff_ctry)
S("Pays", "articles PubMed avec affiliations", nrow(da), nrow(d))
S("Pays", "articles avec pays tracker", sum(has_c), nrow(da))
S("Pays", "% pays tracker retrouve dans les affiliations (precision)", pct(sum(sup & has_c), sum(has_c)), sum(has_c))
S("Pays", "articles avec affiliation africaine", sum(aff_ctry != ""), nrow(da))
S("Pays", "% affiliation africaine & pays tracker renseigne", pct(sum(aff_ctry != "" & has_c), sum(aff_ctry != "")), sum(aff_ctry != ""))
summary_df <- dplyr::bind_rows(summ)
readr::write_csv(summary_df, file.path(OUT_DIR, "crosscheck_pubmed_summary.csv"), na = "")
det <- dplyr::bind_rows(
  dm %>% transmute(record_id, pmid, title, rubrique = "maladie/IA", disease_primary, mesh_diseases, disease_supported_by_mesh, mesh_ai, system_ai, ai_method_evidence),
  da %>% transmute(record_id, pmid, title, rubrique = "pays", country_tracker = country, affiliation_africa_countries))
readr::write_csv(det, file.path(OUT_DIR, "crosscheck_pubmed_detail.csv"), na = "")
print(as.data.frame(summary_df), right = FALSE)
log21("Ecrit : crosscheck_pubmed_summary.csv / crosscheck_pubmed_detail.csv")
