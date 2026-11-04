# PREIS-AI - Re-enrichissement seul (score_africa -> exports), sans collecte reseau.
# Derive de 00_run_pipeline_fixed.R (2026-10-06). Aucune donnee brute modifiee.
# Fichier : R/AI/00_run_pipeline_fixed.R
# Auteur  : Claude (co-développeur) — 2026-08-05
#
# POURQUOI CE FICHIER
# -------------------
# L'ancien orchestrateur (99_run_preis_ai_daily_hardened.R) charge les
# scripts d'étape via sys.source(). Or ces scripts encapsulent tout leur
# travail dans une fonction protégée par `if (sys.nframe() == 0) { ... }`.
# Sous sys.source(), sys.nframe() n'est JAMAIS 0 → la fonction est définie
# mais jamais appelée → l'étape s'exécute en 0,01 s sans rien produire.
# Conséquence : la chaîne d'enrichissement est figée depuis 2026-04-21.
#
# CE QUE FAIT CE RUNNER
# ---------------------
#   1. Fixe une racine projet UNIQUE (paramétrable, défaut D:/PREIS_AI_CLEAN).
#   2. Source chaque étape et APPELLE EXPLICITEMENT sa fonction d'entrée
#      (ou exécute les scripts « top-level » qui n'ont pas de fonction).
#   3. Rétablit l'ORDRE correct du DAG, en incluant l'étape 09 (manquante).
#   4. Après chaque étape : vérifie le fichier de sortie, journalise le
#      nombre de lignes et la date, s'arrête si une sortie critique est vide.
#
# CE FICHIER EST ADDITIF : il ne modifie aucun script existant.
# Il ne teste PAS l'email (géré séparément par 63/70b + gate de 99).
#
# LANCEMENT (Windows, dans CMD/PowerShell) :
#   "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" --vanilla ^
#     "D:\PREIS_AI_CLEAN\R\AI\00_run_pipeline_fixed.R"
# =============================================================

options(stringsAsFactors = FALSE, warn = 1)

# ---- 0) Racine projet (paramétrable) ------------------------
PROJECT_DIR <- Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN")
AI_DIR      <- file.path(PROJECT_DIR, "R", "AI")
LOG_DIR     <- file.path(PROJECT_DIR, "logs")
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(
  LOG_DIR,
  paste0("enrichment_only_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

log_line <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  cat(line, "\n")
  tryCatch(cat(line, "\n", file = LOG_FILE, append = TRUE),
           error = function(e) invisible(NULL))
}
banner <- function(txt) {
  sep <- paste(rep("=", 66), collapse = "")
  log_line(""); log_line(sep); log_line(txt); log_line(sep)
}

# ---- 1) Utilitaires de contrôle des sorties -----------------
rows_in_csv <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size == 0) return(0L)
  tryCatch(max(0L, length(utils::count.fields(path, sep = ",")) - 1L),
           error = function(e) 0L)
}
check_output <- function(label, path, critical = TRUE) {
  n <- rows_in_csv(path)
  d <- if (file.exists(path)) format(file.info(path)$mtime, "%Y-%m-%d %H:%M") else "ABSENT"
  log_line(sprintf("   -> %-34s : %7d lignes | %s", label, n, d))
  if (critical && n == 0L) {
    stop(sprintf("SORTIE CRITIQUE VIDE : %s (%s)", label, path), call. = FALSE)
  }
  invisible(n)
}

# ---- 2) Exécuteur d'étape -----------------------------------
# mode = "function" : source le script puis appelle fn(PROJECT_DIR)
# mode = "toplevel" : source le script (il s'exécute lui-même)
run_stage <- function(stage, script, mode, fn = NULL,
                      output = NULL, critical = TRUE) {
  path <- file.path(AI_DIR, script)
  banner(sprintf("ÉTAPE : %s  (%s)", stage, script))
  if (!file.exists(path)) {
    msg <- sprintf("Script introuvable : %s", path)
    if (critical) stop(msg, call. = FALSE) else { log_line("SKIP — ", msg); return(invisible(FALSE)) }
  }
  t0 <- proc.time()[["elapsed"]]
  env <- new.env(parent = globalenv())
  ok <- TRUE; err <- NA_character_
  # [FIX 2026-09-27] tryCatch(warning=...) est NON-REPRENANT : dès le premier
  # warning émis pendant sys.source(), tryCatch abandonne tout le reste du
  # bloc protégé (le reste du script source + l'appel fn()) sans lever
  # d'erreur -> l'étape se termine en OK en quelques centièmes de seconde
  # sans avoir réellement rien fait. Confirmé sur fetch_preprints : le
  # warning 'httr2 built under R x.y.z' tronquait SILENCIEUSEMENT toute la
  # collecte arXiv/medRxiv/bioRxiv depuis la création de ce runner (2026-08-06).
  # withCallingHandlers + muffleWarning journalise le warning SANS interrompre
  # l'exécution, qui reprend juste après le point où il a été émis.
  tryCatch({
    withCallingHandlers({
      sys.source(path, envir = env)
      if (identical(mode, "function")) {
        if (!exists(fn, envir = env, inherits = FALSE))
          stop(sprintf("Fonction d'entrée '%s' absente de %s", fn, script))
        # appel POSITIONNEL : premier argument = racine projet
        do.call(get(fn, envir = env), list(PROJECT_DIR))
      }
    }, warning = function(w) {
      log_line("WARN [", script, "] : ", conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  },
  error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) })
  dur <- round(proc.time()[["elapsed"]] - t0, 2)
  if (!ok) {
    log_line(sprintf("ÉCHEC %s | %s sec | %s", stage, dur, err))
    if (critical) stop(sprintf("Étape critique en échec : %s — %s", stage, err), call. = FALSE)
  } else {
    log_line(sprintf("OK %s | %s sec", stage, dur))
  }
  if (!is.null(output)) check_output(stage, file.path(PROJECT_DIR, output), critical = critical && ok)
  invisible(ok)
}

banner("PREIS-AI — RE-ENRICHISSEMENT SEUL (sans collecte) — DEBUT")
log_line("Racine projet : ", PROJECT_DIR)
log_line("Journal       : ", LOG_FILE)

# Scoring Afrique → Novelty
run_stage("score_africa", "10_score_ai_africa_relevance.R", "function",
          fn = "score_ai_africa_relevance",
          output = "data/enriched/ai/ai_scored_stage1.csv", critical = TRUE)
run_stage("novelty", "11_detect_ai_novelty.R", "function",
          fn = "detect_ai_novelty",
          output = "data/enriched/ai/ai_novelty_stage1.csv", critical = TRUE)

# Construction du tracker (DOIT précéder methods/applications)
run_stage("build_tracker", "12_build_ai_tracker_final.R", "function",
          fn = "build_ai_tracker_final",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = TRUE)

# Enrichissement méthodes / applications (top-level, lisent+réécrivent le tracker)
run_stage("classify_methods", "10_classify_ai_methods.R", "toplevel",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = FALSE)
run_stage("classify_applications", "11_classify_ai_applications.R", "toplevel",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = FALSE)

# Classification maladie -> ajoute disease_primary au tracker (P1)
run_stage("classify_disease", "13_classify_disease_into_tracker.R", "toplevel",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = FALSE)

# Perimetre sante publique / epidemies (Finding #4) -> ph_scope_tier, ph_in_scope
run_stage("ph_scope", "13e_classify_public_health_scope.R", "toplevel",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = FALSE)

# Géographie -> peuple country / iso3 / rcc dans le tracker (P1)
run_stage("geo", "13d_apply_geo_to_tracker.R", "toplevel",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = FALSE)

# Contre-verification (invariants + confiance de classification) - 2026-10-06
run_stage("quality_checks", "13f_quality_invariants.R", "toplevel",
          output = "data/enriched/ai/ai_tracker_final.csv", critical = FALSE)

# Exports dashboard + alertes
run_stage("build_exports", "14_build_ai_intelligence_exports.R", "toplevel",
          output = "dashboard/data/dashboard/alerts_ai.csv", critical = TRUE)

# ---- 4) Bilan de fraîcheur ----------------------------------
banner("BILAN DE FRAÎCHEUR")
check_output("tracker_final", file.path(PROJECT_DIR, "data/enriched/ai/ai_tracker_final.csv"), critical = FALSE)
check_output("alerts_dashboard", file.path(PROJECT_DIR, "dashboard/data/dashboard/alerts_ai.csv"), critical = FALSE)
check_output("articles_dashboard", file.path(PROJECT_DIR, "dashboard/data/dashboard/articles_ai.csv"), critical = FALSE)

banner("PREIS-AI — PIPELINE CORRIGÉ — FIN")
log_line("Terminé. Journal complet : ", LOG_FILE)
