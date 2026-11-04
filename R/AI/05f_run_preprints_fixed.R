# =============================================================
# PREIS-AI — COLLECTE PREPRINTS (arXiv / medRxiv / bioRxiv) — P1
# Fichier : R/AI/05f_run_preprints_fixed.R  (top-level)
# Auteur  : Claude — 2026-08-06
#
# Remplace 05e_run_new_preprint_fetchers.R qui utilisait la mauvaise racine
# (setwd("D:/PREIS_AI") + project_root="D:/PREIS_AI") — donc écrivait hors de
# _CLEAN. Ici : racine _CLEAN, et surtout FUSION avec l'existant + déduplication
# pour ENRICHIR sans écraser la couverture déjà acquise.
#
# Sorties : data/raw/ai/{arxiv,medrxiv,biorxiv}_standardized.csv
# (lues ensuite par 07_merge_ai_sources.R)
# =============================================================

suppressPackageStartupMessages({ library(readr); library(dplyr) })

PROJECT_DIR <- Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN")
raw_dir <- file.path(PROJECT_DIR, "data", "raw", "ai")
src_dir <- file.path(PROJECT_DIR, "R", "AI")

log_ <- function(...) cat(sprintf("[%s] %s\n",
  format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = "")))

source(file.path(src_dir, "05b_fetch_arxiv_ai.R"))
source(file.path(src_dir, "05c_fetch_medrxiv_ai.R"))
source(file.path(src_dir, "05d_fetch_biorxiv_ai.R"))

read_old <- function(f) {
  if (file.exists(f)) tryCatch(readr::read_csv(f, show_col_types = FALSE, progress = FALSE),
                               error = function(e) NULL) else NULL
}

# Après un refetch (qui écrase std_file), fusionne avec l'ancien + déduplique
# [FIX 2026-09-24] Retourne desormais le delta (nb de lignes VRAIMENT nouvelles)
# au lieu de rien retourner, pour permettre une alerte de staleness ci-dessous.
merge_keep <- function(std_file, before) {
  if (!file.exists(std_file)) return(NA_integer_)
  new <- read_old(std_file)
  n_before <- if (is.null(before)) 0L else nrow(before)
  comb <- dplyr::bind_rows(before, new)
  if (is.null(comb) || !nrow(comb)) return(NA_integer_)
  doi <- if ("doi" %in% names(comb)) tolower(trimws(as.character(comb$doi))) else rep("", nrow(comb))
  ttl <- if ("title" %in% names(comb)) tolower(trimws(as.character(comb$title))) else rep("", nrow(comb))
  key <- ifelse(is.na(doi) | doi == "", paste0("t:", ttl), paste0("d:", doi))
  comb <- comb[!duplicated(key), , drop = FALSE]
  readr::write_csv(comb, std_file, na = "")
  n_new <- nrow(comb) - n_before
  log_(basename(std_file), " : ", nrow(comb), " lignes (apres fusion + dedup) -- ", n_new, " nouvelle(s)")
  n_new
}

## [FIX 2026-09-24] Execute le fetch et le merge dans des try() SEPARES :
## une erreur de fetch reste visible et distincte, et n'empeche plus le
## diagnostic. Alerte explicite si 0 ligne nouvelle -- staleness detectee
## et loguee au lieu de passer inapercue.
run_source <- function(label, std_file, old_data, fetch_call) {
  fetch_ok <- TRUE
  fetch_err <- tryCatch({ fetch_call(); TRUE }, error = function(e) {
    log_("ERREUR FETCH [", label, "] : ", conditionMessage(e))
    FALSE
  })
  if (!fetch_err) {
    log_("ALERTE STALENESS [", label, "] : fetch en erreur, fichier NON mis a jour (reste date du run precedent).")
    return(invisible(NULL))
  }
  n_new <- tryCatch(merge_keep(std_file, old_data), error = function(e) {
    log_("ERREUR MERGE [", label, "] : ", conditionMessage(e)); NA_integer_
  })
  if (!is.na(n_new) && n_new <= 0L) {
    log_("ALERTE STALENESS [", label, "] : fetch execute sans erreur mais 0 ligne nouvelle -- verifier la source/l'API.")
  }
  invisible(n_new)
}

ax <- file.path(raw_dir, "arxiv_standardized.csv");   ax_old <- read_old(ax)
mx <- file.path(raw_dir, "medrxiv_standardized.csv"); mx_old <- read_old(mx)
bx <- file.path(raw_dir, "biorxiv_standardized.csv"); bx_old <- read_old(bx)

log_("Collecte arXiv...")
run_source("arXiv", ax, ax_old, function()
  run_fetch_arxiv_ai(project_root = PROJECT_DIR, max_pages = 20, page_size = 100))

log_("Collecte medRxiv (30 derniers jours)...")
run_source("medRxiv", mx, mx_old, function()
  run_fetch_medrxiv_ai(project_root = PROJECT_DIR,
    from_date = Sys.Date() - 30, to_date = Sys.Date(), max_pages = 10))

log_("Collecte bioRxiv (30 derniers jours)...")
run_source("bioRxiv", bx, bx_old, function()
  run_fetch_biorxiv_ai(project_root = PROJECT_DIR,
    from_date = Sys.Date() - 30, to_date = Sys.Date(), max_pages = 10))

log_("Collecte preprints terminée.")
