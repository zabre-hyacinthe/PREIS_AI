# 06b_deploy_retry.R - redeploy du dashboard avec nouvelles tentatives et diagnostic (20261010)
# Ne modifie pas 06_deploy_posit_connect.R. Suppose que le dossier deploy/ est deja prepare par 06.
suppressPackageStartupMessages(library(rsconnect))
# [20261104] GitHub Actions : rsconnect peut laisser l'URL du depot vide, et shinyapps.io echoue alors
# avec "Unsupported url scheme: CRAN/src/contrib/...". On impose ici l'URL de CRAN. Sans effet sur le PC.
if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) local({
  depot_cran <- "https://cloud.r-project.org"
  origine <- get("computePackageDependencies", envir = asNamespace("rsconnect"))
  corrigee <- function(...) {
    deps <- origine(...)
    if (nrow(deps) == 0) return(deps)
    avant <- paste(deps$Source, deps$Repository, sep = " | ")
    cat("\n--- Sources des paquets avant correction ---\n"); print(table(avant, useNA = "ifany"))
    vcs <- tolower(deps$Source) %in% c("github", "gitlab", "bitbucket")
    a_fixer <- !is.na(deps$Source) & !vcs
    deps$Source[a_fixer] <- "CRAN"
    deps$Repository[a_fixer] <- depot_cran
    cat("Paquets rattaches a", depot_cran, ":", sum(a_fixer), "sur", nrow(deps), "\n")
    if (any(!a_fixer)) print(deps[!a_fixer, c("Package", "Version", "Source", "Repository")])
    deps
  }
  utils::assignInNamespace("computePackageDependencies", corrigee, ns = "rsconnect")
})
DEPLOY_DIR <- "D:/PREIS_AI_CLEAN/deploy"
ACCT <- "zrhyacinthepreis26"
SRV  <- "shinyapps.io"
ROOT <- "D:/PREIS_AI_CLEAN"
# Preparation du dossier deploy (meme logique que 06): app + donnees a jour
app <- readLines(file.path(ROOT, "dashboard", "app_ai_v3.R"), warn = FALSE)
app <- gsub('project_dir <- "D:/PREIS_AI_CLEAN"', 'project_dir <- getwd()  # Posit Connect: chemin relatif', app, fixed = TRUE)
writeLines(app, file.path(DEPLOY_DIR, "app.R"))
for (f in c("dashboard/data/dashboard/alerts_ai.csv", "data/dashboard/ai_intelligence_dashboard_dataset.csv",
            "maps/africa_countries_rcc.geojson", "maps/country_coords.csv")) {   # [20261031] config/alert_recipients_ai.csv (adresses) retire du paquet public : non utilise par app.R
  dir.create(dirname(file.path(DEPLOY_DIR, f)), recursive = TRUE, showWarnings = FALSE)
  cat("copie", f, file.copy(file.path(ROOT, f), file.path(DEPLOY_DIR, f), overwrite = TRUE), "\n")
}
# [20261114] PREIS AI Atlas: module + registry data (the app hides gracefully if they are absent)
if (file.exists(file.path(ROOT, "dashboard", "registry_module.R"))) cat("copie registry_module.R", file.copy(file.path(ROOT, "dashboard", "registry_module.R"), file.path(DEPLOY_DIR, "registry_module.R"), overwrite = TRUE), "\n")
dir.create(file.path(DEPLOY_DIR, "registry_data"), recursive = TRUE, showWarnings = FALSE)
for (f in c("candidates_latest.csv", "registry_platforms.csv", "disease_gap_table.csv", "registry_leads.csv", "registry_by_country.csv", "registry_summary.csv")) {
  src <- Filter(file.exists, c(file.path(ROOT, "registry_data", f), file.path(ROOT, "PREIS_REGISTRY", "registry_data", f)))
  if (length(src)) cat("copie registry_data/", f, file.copy(src[1], file.path(DEPLOY_DIR, "registry_data", f), overwrite = TRUE), "\n") else cat("ABSENT registry_data/", f, "\n")
}
if (file.exists(file.path(DEPLOY_DIR, "config/alert_recipients_ai.csv"))) cat("ATTENTION : ancienne copie deploy/config/alert_recipients_ai.csv presente (a retirer du dossier deploy)\n")
cat("Dossier:", DEPLOY_DIR, "\n")
cat("app.R modifie:", format(file.mtime(file.path(DEPLOY_DIR, "app.R"))), "\n")
cat("show_names present dans app.R:", any(grepl("show_names", readLines(file.path(DEPLOY_DIR, "app.R"), warn = FALSE))), "\n")
ok <- FALSE
for (i in 1:4) {
  cat(sprintf("\n[tentative %d/4] %s\n", i, format(Sys.time())))
  if (i >= 3) options(rsconnect.http = "curl")
  res <- tryCatch({
    rsconnect::deployApp(appDir = DEPLOY_DIR, appName = "PREIS_AI_Dashboard",
      appTitle = "PREIS AI Tracker Dashboard", account = ACCT, server = SRV,
      forceUpdate = TRUE, launch.browser = FALSE)
    TRUE
  }, error = function(e) { cat("ERREUR:", conditionMessage(e), "\n"); FALSE })
  if (isTRUE(res)) { ok <- TRUE; break }
  if (i < 4) { cat("Nouvelle tentative dans 90 s...\n"); Sys.sleep(90) }
}
cat("\n--- Diagnostic serveur ---\n")
tryCatch({
  apps <- rsconnect::applications(account = ACCT, server = SRV)
  a <- apps[apps$name == "PREIS_AI_Dashboard", ]
  print(a[, intersect(c("name", "status", "url", "updated_time", "deployment"), names(a))])
}, error = function(e) cat("applications():", conditionMessage(e), "\n"))
tryCatch({
  lg <- capture.output(rsconnect::showLogs(appName = "PREIS_AI_Dashboard", account = ACCT, server = SRV, entries = 30, streaming = FALSE))
  cat(tail(lg, 30), sep = "\n")
}, error = function(e) cat("showLogs():", conditionMessage(e), "\n"))
cat(if (ok) "\nDEPLOY_OK\n" else "\nDEPLOY_FAILED\n")
