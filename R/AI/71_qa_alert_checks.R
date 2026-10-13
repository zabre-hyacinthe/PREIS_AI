## ============================================================
## 71_qa_alert_checks.R - CONTROLE QUALITE automatique avant envoi des alertes
## Lit : data/outputs/ai/alerting/preview_<date>/manifest.csv + html, alert_log.csv, candidats.
## Ecrit : data/outputs/ai/alerting/qa_report_<date>.csv (aucune adresse email dedans).
## Gravite : CRITICAL = bloque l'envoi reel (code de sortie 1) ; WARN = signale ; INFO = statistique.
## Usage : Rscript R/AI/71_qa_alert_checks.R [dossier_preview]     (PREIS_DRY_RUN=false => mode bloquant)
## Chaque controle correspond a un incident reel documente dans docs/journal_corrections.csv (colonne qa_check).
## ============================================================
args <- commandArgs(trailingOnly = TRUE)
root <- Sys.getenv("PREIS_ROOT", unset = getwd())
LIVE <- tolower(Sys.getenv("PREIS_DRY_RUN", "true")) == "false"
base <- file.path(root, "data/outputs/ai/alerting")
dirs <- sort(list.dirs(base, recursive = FALSE), decreasing = TRUE); dirs <- dirs[grepl("preview_[0-9]{8}$", dirs)]
if (length(args) >= 1) dirs <- dirs[grepl(args[1], dirs)]
Q <- list(); add <- function(id, sev, msg, n = NA) Q[[length(Q) + 1]] <<- data.frame(check = id, severity = sev, detail = msg, n = n, stringsAsFactors = FALSE)
rd <- function(f) read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = "")

if (!length(dirs) || !file.exists(file.path(dirs[1], "manifest.csv"))) {
  add("QA00", "INFO", "Aucun manifeste : rien a controler (aucun message prepare).")
} else {
  d <- dirs[1]; M <- rd(file.path(d, "manifest.csv")); M[is.na(M)] <- ""
  vis <- function(f) { h <- paste(readLines(file.path(d, f), warn = FALSE, encoding = "UTF-8"), collapse = " ")
    h <- gsub("(?s)<!--BANNER-->.*?<!--/BANNER-->", " ", h, perl = TRUE); h <- gsub("<[^>]+>", " ", h)
    h <- gsub("&middot;|&nbsp;", " ", h); gsub("[[:space:]]+", " ", h) }
  for (i in seq_len(nrow(M))) {
    m <- M[i, ]; tag <- paste0(m$group_key, "/", m$tier); txt <- if (file.exists(file.path(d, m$file))) vis(m$file) else ""
    if (!nzchar(txt)) add("QA01", "CRITICAL", paste0(tag, " : fichier HTML absent ou vide"))
    if (grepl("(?<![A-Za-z0-9])NA(?![A-Za-z0-9])", m$subject, perl = TRUE)) add("QA02", "CRITICAL", paste0(tag, " : 'NA' dans le sujet"))
    if (grepl("(?<![A-Za-z0-9])NA(?![A-Za-z0-9])", txt, perl = TRUE)) add("QA03", "CRITICAL", paste0(tag, " : 'NA' dans le corps du message"))
    if (grepl("\\{[A-Za-z_]+\\}|@@ITEMS@@", paste(m$subject, txt))) add("QA04", "CRITICAL", paste0(tag, " : variable de modele non remplacee ({...})"))
    if (suppressWarnings(as.numeric(m$n_items)) < 1 || is.na(suppressWarnings(as.numeric(m$n_items)))) add("QA05", "CRITICAL", paste0(tag, " : message sans aucun article"))
    if (grepl("(Application|Evidence|Disease|Country) : ( · |$)", txt)) add("QA06", "WARN", paste0(tag, " : champ d'article vide affiche"))
    if (!grepl("zrhyacinthepreis26.shinyapps.io", txt, fixed = TRUE)) add("QA07", "WARN", paste0(tag, " : lien dashboard absent"))
    if (isTRUE(as.logical(m$will_send)) && !grepl("^[^@ ]+@[^@ ]+\\.[^@ ]+$", m$to_real)) add("QA08", "CRITICAL", paste0(tag, " : envoi prevu sans adresse valide"))
    ids <- strsplit(m$record_ids, ";")[[1]]
    if (anyDuplicated(ids)) add("QA09", "CRITICAL", paste0(tag, " : article en double dans le meme message"))
  }
  ## doublons avec le journal des envois reels
  lf <- file.path(base, "alert_log.csv")
  if (file.exists(lf)) { L <- rd(lf); L <- L[L$status %in% "sent", , drop = FALSE]
    if (nrow(L)) { k_sent <- paste(L$record_id, L$group)
      for (i in seq_len(nrow(M))) { ids <- strsplit(M$record_ids[i], ";")[[1]]
        dup <- sum(paste(ids, M$group_key[i]) %in% k_sent)
        if (dup > 0 && isTRUE(as.logical(M$will_send[i]))) add("QA10", "CRITICAL", paste0(M$group_key[i], "/", M$tier[i], " : ", dup, " article(s) deja envoye(s) a ce destinataire"), dup) } } }
  ## limites d'envoi
  ns <- sum(as.logical(M$will_send)); mx <- suppressWarnings(as.numeric(Sys.getenv("PREIS_MAX_EMAILS", "60")))
  if (ns > mx) add("QA11", "CRITICAL", paste0(ns, " messages a envoyer > plafond ", mx), ns)
  add("QA00", "INFO", paste0(nrow(M), " message(s) controle(s), ", ns, " a envoyer"), nrow(M))
}

## --- controles sur les donnees candidates (fraicheur, vide, trou geographique) ---
cp <- Sys.getenv("PREIS_TRACKER_PATH", "")
if (nzchar(cp)) { cf <- if (grepl("^(/|[A-Za-z]:[/\\\\])", cp)) cp else file.path(root, cp)
  if (!file.exists(cf)) add("QA12", "CRITICAL", "fichier de candidats introuvable") else {
    X <- rd(cf); X[is.na(X)] <- ""
    if (!nrow(X)) add("QA13", "CRITICAL", "fichier de candidats vide") else {
      pd <- suppressWarnings(as.Date(substr(X$publication_date, 1, 10))); age <- as.numeric(Sys.Date() - max(pd, na.rm = TRUE))
      if (is.finite(age) && age > 14) add("QA14", "WARN", paste0("article le plus recent publie il y a ", age, " jours : candidats peut-etre perimes"), age)
      fil <- X$disease_primary %in% c("Ebola", "Marburg", "Sudan virus", "Bundibugyo")
      gap <- fil & X$priority_level == "High" & X$country %in% c("", "Unspecified")
      add("QA15", "INFO", paste0("Filovirus priorite haute sans pays detecte : ", sum(gap), " / ", sum(fil & X$priority_level == "High"), " (trou geographique connu, voir journal_corrections)"), sum(gap))
      blank_app <- sum(X$application_type_primary == ""); if (blank_app) add("QA16", "INFO", paste0(blank_app, " article(s) sans type d'application (affiche 'Not classified')"), blank_app)
    } } }

R <- do.call(rbind, Q); R$date <- format(Sys.Date())
dir.create(base, recursive = TRUE, showWarnings = FALSE)
write.csv(R, file.path(base, paste0("qa_report_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
nc <- sum(R$severity == "CRITICAL"); nw <- sum(R$severity == "WARN")
cat("== CONTROLE QUALITE ==  CRITICAL:", nc, "| WARN:", nw, "| INFO:", sum(R$severity == "INFO"), "\n")
if (nrow(R)) for (i in seq_len(nrow(R))) cat(sprintf("[%s] %s - %s\n", R$severity[i], R$check[i], R$detail[i]))
sm <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(sm)) cat(c("### Controle qualite", paste0("CRITICAL: ", nc, " | WARN: ", nw), paste0("- [", R$severity, "] ", R$check, " : ", R$detail)), file = sm, sep = "\n", append = TRUE)
if (nc > 0 && LIVE) { cat("ENVOI REEL BLOQUE : corriger les erreurs CRITICAL ci-dessus.\n"); quit(save = "no", status = 1) }
if (nc > 0) cat("(mode simulation : non bloquant, mais a corriger avant tout envoi reel)\n")
