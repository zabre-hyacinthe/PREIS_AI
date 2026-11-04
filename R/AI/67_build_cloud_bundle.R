## 67_build_cloud_bundle.R — Construit le dossier PUBLIC-SAFE pour GitHub (D:/PREIS_AI_CLOUD par defaut).
## Copie uniquement : scripts d'alerte, config sans email, candidats compacts, workflow. Aucune donnee brute, aucun email.
root <- Sys.getenv("PREIS_ROOT", unset = getwd())
out  <- Sys.getenv("PREIS_CLOUD_DIR", unset = paste0(sub("/+$", "", dirname(normalizePath(root, winslash = "/", mustWork = FALSE))), "/PREIS_AI_CLOUD"))
out  <- sub("/+$", "", gsub("//+", "/", out))
mk <- function(...) dir.create(file.path(out, ...), recursive = TRUE, showWarnings = FALSE)
mk("R/AI"); mk("config"); mk("data/alerting"); mk(".github/workflows")
if (!dir.exists(file.path(out, "R/AI"))) stop("Impossible de creer ", out)
cp <- function(from, to) { stopifnot(file.exists(from)); file.copy(from, file.path(out, to), overwrite = TRUE) }
for (f in c("60_email.R", "64_route_alerts.R", "68_send_from_manifest.R", "71_qa_alert_checks.R")) cp(file.path(root, "R/AI", f), file.path("R/AI", f))
for (f in c("alert_email_templates_i18n.csv", "alert_i18n_strings.csv", "country_default_lang.csv", "outbreak_mode.csv")) cp(file.path(root, "config", f), file.path("config", f))
cp(file.path(root, "data/alerting/alert_candidates.csv"), "data/alerting/alert_candidates.csv")
cp(file.path(root, "cloud/preis_ai_alerts.yml"), ".github/workflows/preis_ai_alerts.yml")
# routage public-safe : emails vides, tout inactif
r <- read.csv(file.path(root, "config/alert_recipients_routing_TEST.csv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = "", encoding = "UTF-8")
names(r) <- sub("^(\ufeff|\u00ef\u00bb\u00bf)", "", names(r))
r$test_email <- ""; r$real_email <- ""; r$active_real <- "FALSE"
write.csv(r, file.path(out, "config/alert_recipients_routing_TEST.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
# garde-fou : aucune adresse email dans le bundle (hors workflow/README)
chk <- list.files(out, recursive = TRUE, full.names = TRUE, pattern = "\\.(csv)$")
bad <- Filter(function(f) any(grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", readLines(f, warn = FALSE, encoding = "UTF-8"))), chk)
if (length(bad)) stop("EMAIL TROUVE dans : ", paste(bad, collapse = ", "), " -> bundle non publiable")
cat("Bundle OK :", normalizePath(out, winslash = "/"), "\n")
