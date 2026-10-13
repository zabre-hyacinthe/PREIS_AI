## 68_send_from_manifest.R — Envoi des alertes decrites par manifest.csv (produit par 64_route_alerts.R)
## SECURITE : n'envoie QUE si PREIS_DRY_RUN=false ET la ligne du manifeste a will_send=TRUE
##            (destinataire actif dans le fichier ET adresse reelle valide). Sinon : simple affichage (dry-run).
## Plafond : PREIS_MAX_EMAILS (defaut 60) par execution. Journal : alert_log.csv (status "sent"), sans adresse email.
## Usage   : Rscript R/AI/68_send_from_manifest.R [AAAAMMJJ]
args <- commandArgs(trailingOnly = TRUE)
root <- Sys.getenv("PREIS_ROOT", unset = getwd())
LIVE <- tolower(Sys.getenv("PREIS_DRY_RUN", "true")) == "false"
MAXN <- as.numeric(Sys.getenv("PREIS_MAX_EMAILS", "60"))
base <- file.path(root, "data/outputs/ai/alerting")
dirs <- sort(list.dirs(base, recursive = FALSE), decreasing = TRUE); dirs <- dirs[grepl("preview_[0-9]{8}$", dirs)]
if (length(args) >= 1) dirs <- dirs[grepl(args[1], dirs)]
if (!length(dirs) || !file.exists(file.path(dirs[1], "manifest.csv"))) { cat("Aucun manifeste a traiter.\n"); quit(save = "no") }
d <- dirs[1]; M <- read.csv(file.path(d, "manifest.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
cat("Manifeste :", d, "|", nrow(M), "messages |", sum(M$will_send), "a envoyer |", if (LIVE) "MODE LIVE" else "DRY-RUN (rien n'est envoye)", "\n")
todo <- M[M$will_send %in% c(TRUE, "TRUE"), , drop = FALSE]
if (!LIVE || !nrow(todo)) { print(M[, c("group_key","tier","lang","n_items","will_send")]); quit(save = "no") }
if (nrow(todo) > MAXN) stop("Plafond de securite depasse : ", nrow(todo), " messages > ", MAXN)
source(file.path(root, "R/AI/60_email.R"))
logf <- file.path(base, "alert_log.csv"); newlog <- list(); nok <- 0
for (i in seq_len(nrow(todo))) {
  r <- todo[i, ]
  html <- paste(readLines(file.path(d, r$file), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- gsub("(?s)<!--BANNER-->.*?<!--/BANNER-->", "", html, perl = TRUE)
  body <- sub("(?s)^.*?<body[^>]*>", "", html, perl = TRUE); body <- sub("(?s)</body>.*$", "", body, perl = TRUE)
  ok <- tryCatch({ ai_smtp_send(blastula::compose_email(body = htmltools::HTML(body)), to = r$to_real, subject = r$subject, verbose = FALSE); TRUE },
                 error = function(e) { cat("ECHEC", r$group_key, ":", conditionMessage(e), "\n"); FALSE })
  if (ok) {
    nok <- nok + 1
    ids <- strsplit(r$record_ids, ";")[[1]]
    newlog[[length(newlog) + 1]] <- data.frame(record_id = ids, group = r$group_key, recipient = "(not logged)", status = "sent",
                                               tier = r$tier, run_date = r$run_date, reason = paste0("rule:", r$group, "/", r$tier), stringsAsFactors = FALSE)
  }
  Sys.sleep(1)
}
if (length(newlog)) {
  L <- do.call(rbind, newlog)
  if (file.exists(logf)) { old <- read.csv(logf, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8"); L <- rbind(old[, intersect(names(old), names(L))], L[, intersect(names(old), names(L))]) }
  write.csv(L, logf, row.names = FALSE, fileEncoding = "UTF-8")
}
cat("Envoyes :", nok, "/", nrow(todo), "\n")
