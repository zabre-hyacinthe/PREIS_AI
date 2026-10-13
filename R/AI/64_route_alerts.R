## ============================================================
## 64_route_alerts.R — Routage conditionnel des alertes PREIS AI (MODE PREVIEW)
## Entrees : data/enriched/ai/ai_tracker_final.csv
##           config/alert_recipients_routing_TEST.csv, config/alert_email_templates.csv
## Sorties : data/outputs/ai/alerting/preview_<date>/*.html + resume_routage.csv
##           data/outputs/ai/alerting/alert_log.csv (journal, jamais ecrase : ajout seulement)
## Securite : AUCUN email n'est envoye par ce script. Les adresses utilisees sont
##            test_email (jamais real_email). Pour envoyer, brancher 60_email.R apres validation.
## Usage    : Rscript R/AI/64_route_alerts.R [YYYY-MM-DD] [fenetre_jours]   (date de reference, defaut = aujourd'hui)
## ============================================================
args <- commandArgs(trailingOnly = TRUE)
ref_date <- if (length(args) >= 1) as.Date(args[1]) else Sys.Date()
root <- Sys.getenv("PREIS_ROOT", unset = getwd())
p <- function(...) { a <- file.path(...); if (grepl("^(/|[A-Za-z]:[/\\\\])", a)) a else file.path(root, a) }
rd <- function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = "")
  names(d) <- sub("^(\ufeff|\u00ef\u00bb\u00bf|\u00c3\u00af\u00c2\u00bb\u00c2\u00bf)", "", names(d)); d[is.na(d)] <- ""
  d
}

## ---- Parametres editables --------------------------------------------------
PRIORITY_DISEASES <- c("Ebola","Marburg","Lassa fever","Crimean-Congo haemorrhagic fever","Mpox",
                       "Cholera","Measles","Meningitis","Yellow fever","Dengue")
FILOVIRUS         <- c("Ebola","Marburg")
FILO_COUNTRIES    <- c("Democratic Republic of the Congo","Uganda","South Sudan","Rwanda","Burundi",
                       "Tanzania","Kenya","Central African Republic","Zambia","Angola","Congo")
WINDOW_DAYS  <- c(immediate = 7, weekly = 7, monthly = 31)   # fenetre sur la date de publication
## Arg 2 (optionnel) = fenetre unique en jours pour un premier passage de rattrapage (ex. 90)
if (length(args) >= 2) { .w <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", args[2]))); if (!is.na(.w) && .w > 0) WINDOW_DAYS[] <- .w else message("Fenetre ignoree (valeur non numerique : ", args[2], ") -> fenetres par defaut") }
MAX_ITEMS    <- c(SDI = 15, CELLULE_EBOLA = 15, RCC = 10, MS = 5, PARTENAIRE = 10)
MAX_IMMEDIATE_PER_DAY <- 3

x   <- rd(p(Sys.getenv("PREIS_TRACKER_PATH", "data/enriched/ai/ai_tracker_final.csv")))
rec <- rd(p(Sys.getenv("PREIS_RECIPIENTS_PATH", "config/alert_recipients_routing_TEST.csv")))
## [20261012] 6 langues officielles de l'UA : EN, FR, AR, PT, ES, SW (textes AR/PT/ES/SW = brouillons a faire relire par des locuteurs natifs)
rd8 <- rd   # meme lecteur UTF-8 que le reste du script
tpl  <- rd8(p("config/alert_email_templates_i18n.csv"))
I18N <- rd8(p("config/alert_i18n_strings.csv"))
CLANG <- tryCatch(rd8(p("config/country_default_lang.csv")), error = function(e) data.frame(country = character(), lang_suggested = character()))
LANGS <- c("EN","FR","AR","PT","ES","SW")
rec$active_real <- toupper(as.character(rec$active_real)) == "TRUE"
if (nzchar(Sys.getenv("PREIS_FORCE_LANG"))) rec$lang <- toupper(Sys.getenv("PREIS_FORCE_LANG"))   # test : forcer la langue (ex. EN) sans modifier la config
## [20261013] Mode : DRY-RUN par defaut (aucun envoi). LIVE seulement si PREIS_DRY_RUN=false ; l'envoi lui-meme est fait par 68_send_from_manifest.R
LIVE <- tolower(Sys.getenv("PREIS_DRY_RUN", "true")) == "false"
rec$active_cfg <- rec$active_real
if (!LIVE) rec$active_real <- FALSE      # en dry-run, personne n'est actif, quel que soit le fichier
if (!"real_email" %in% names(rec)) rec$real_email <- ""
TIERS <- trimws(strsplit(Sys.getenv("PREIS_TIERS", "immediate,weekly,monthly"), ",")[[1]])

## ---- Socle d'eligibilite S1-S6 ------------------------------------------------
x$pub_date <- suppressWarnings(as.Date(substr(x$publication_date, 1, 10)))
S1 <- x$ai_method_evidence == "explicit"
S2 <- x$ph_scope_tier == "Core"
S3 <- !is.na(x$pub_date) & x$pub_date <= ref_date          # date valide, pas dans le futur
S4_log <- if (file.exists(p("data/outputs/ai/alerting/alert_log.csv"))) rd(p("data/outputs/ai/alerting/alert_log.csv")) else
  data.frame(record_id=character(), group=character(), recipient=character(), status=character(), run_date=character())
sent_before <- paste(S4_log$record_id[S4_log$status == "sent"], S4_log$group[S4_log$status == "sent"])
x$.grp_key <- NULL
S5 <- !(x$disease_primary %in% c("Unspecified")) | TRUE   # S5 s'applique aux maladies ; voir ci-dessous
dis_ok <- x$disease_confidence %in% c("High","Medium") & !(x$disease_primary %in% c("Unspecified","Multiple / generic"))
geo_ok <- x$country != "" & !is.na(x$country) & x$country != "Unspecified" & x$geo_evidence %in% c("title","dominant")
base_ok <- S1 & S2 & S3
cat(sprintf("Socle: %d (explicit %d, Core %d, date valide %d)\n", sum(base_ok), sum(S1), sum(S2), sum(S3)))

## ---- Champs rediges automatiquement -----------------------------------------
DASHBOARD_URL <- "https://zrhyacinthepreis26.shinyapps.io/PREIS_AI_Dashboard"
LANGC <- "EN"
tx <- function(k, ...) {                       # traduction avec repli sur l'anglais
  v <- I18N[[LANGC]][match(k, I18N$key)]
  if (is.na(v) || !nzchar(v)) v <- I18N[["EN"]][match(k, I18N$key)]
  a <- list(...); for (n in names(a)) v <- gsub(paste0("{", n, "}"), a[[n]], v, fixed = TRUE)
  v
}
lim_of <- function(r) {
  r[is.na(r)] <- ""
  l <- character()
  if (r$ai_evidence_type %in% c("THEORY","UNKNOWN")) l <- c(l, tx("lim_theory"))
  if (r$ai_evidence_type == "REVIEW") l <- c(l, tx("lim_review"))
  if (r$record_origin == "preprint") l <- c(l, tx("lim_preprint"))
  if (!(r$geo_evidence %in% c("title","dominant"))) l <- c(l, tx("lim_nocountry"))
  if (r$disease_confidence == "Low") l <- c(l, tx("lim_lowconf"))
  if (!length(l)) l <- tx("lim_unverified")
  paste0(toupper(substr(l[1],1,1)), substr(paste(l, collapse="; "),2,1000))
}
impl_of <- function(r) {
  r[is.na(r)] <- ""
  a <- r$application_type_primary
  base <- if (r$disease_primary %in% FILOVIRUS) tx("impl_filo") else
          if (r$disease_primary %in% PRIORITY_DISEASES) tx("impl_prio", d = r$disease_primary) else tx("impl_generic")
  how <- switch(a, "Prediction / Forecasting"=tx("how_pred"), "Risk Mapping / Spatial Analysis"=tx("how_risk"),
    "Surveillance / Event Detection"=tx("how_surv"), "Laboratory / Genomic Intelligence"=tx("how_lab"),
    "Diagnosis / Clinical Decision Support"=tx("how_diag"), "Policy / Decision Support"=tx("how_policy"), tx("how_tbd"))
  if (r$ai_evidence_type == "REVIEW") return(tx("impl_review"))
  paste0(base, " (", how, "). ", tx("impl_tail"))
}
esc <- function(s) { s <- gsub("&","&amp;",s,fixed=TRUE); s <- gsub("<","&lt;",s,fixed=TRUE); gsub(">","&gt;",s,fixed=TRUE) }
item_html <- function(r, why) {
  r[is.na(r)] <- ""
  link <- if (nzchar(r$doi)) paste0("https://doi.org/", r$doi) else r$url
  au <- strsplit(r$authors, ";")[[1]]; au <- paste0(trimws(au[seq_len(min(3,length(au)))]), collapse=", ")
  if (length(strsplit(r$authors, ";")[[1]]) > 3) au <- paste0(au, " et al.")
  sprintf(paste0("<div style='border-", if (LANGC == "AR") "right" else "left", ":4px solid #7A1D2F;padding:6px 10px;margin:10px 0'>",
    "<b>%s</b><br/>%s &middot; %s%s &middot; %s<br/>",
    tx("disease"), " : %s &middot; ", tx("country"), " : %s &middot; ", tx("method"), " : %s &middot; ", tx("application"), " : %s<br/>",
    tx("priority"), " : %s (", tx("score"), " %s) &middot; ", tx("evidence"), " : %s<br/>",
    "<i>", tx("why"), " : %s</i><br/>",
    "<b>", tx("implication"), " :</b> %s<br/><b>", tx("limit"), " :</b> %s<br/>",
    "<a href='%s'>%s</a></div>"),
    esc(r$title), esc(au), if (nzchar(r$journal)) paste0(esc(r$journal), " &middot; ") else "", esc(substr(r$publication_date,1,10)), esc(r$record_origin),
    esc(paste0(r$disease_primary, if (identical(r$disease_confidence, "Low")) paste0(" (", tx("low_conf"), ")") else "")), esc(if (nzchar(r$country) && r$country != "Unspecified") r$country else paste0(tx("not_determined"), if (!is.null(r$countries_mentioned) && nzchar(r$countries_mentioned)) paste0(" (", tx("mentioned_unvalidated"), " : ", r$countries_mentioned, ")") else "")), esc(r$method_family_primary),
    esc(ifelse(nzchar(r$application_type_primary), r$application_type_primary, tx("not_classified"))), esc(r$priority_level), esc(r$priority_score), esc(ifelse(nzchar(r$ai_evidence_type), r$ai_evidence_type, "UNKNOWN")),
    esc(why), esc(impl_of(r)), esc(lim_of(r)), esc(link), esc(link))
}

## ---- Regles par groupe -----------------------------------------------------
is_prio <- x$disease_primary %in% PRIORITY_DISEASES
is_ebola <- x$disease_primary %in% FILOVIRUS
## [20261010] Regle Ebola resserree : alerte immediate seulement si maladie fiable (High/Medium),
## confiance maladie High (maladie dans le titre), priorite High/Moderate et etude applicative ou validee
## (pas une revue, pas de la theorie, pas Low).
ebola_ok  <- is_ebola & dis_ok & x$disease_confidence == "High" & x$priority_level %in% c("High","Moderate")
ebola_imm <- ebola_ok & x$disease_confidence == "High" & x$ai_evidence_type %in% c("APPLICATION","VALIDATION")
pl <- x$priority_level
rules <- list(
  SDI = function(d) {
    imm <- ((pl == "High") & ((is_prio & x$disease_confidence == "High") | x$africa_relevance_level == "High")) | ebola_imm
    wk  <- pl %in% c("High","Moderate")
    ifelse(imm, "immediate", ifelse(wk, "weekly", NA))
  },
  CELLULE_EBOLA = function(d) ifelse(ebola_imm, "immediate", ifelse(ebola_ok, "weekly",
      ifelse(x$disease_primary %in% c("Lassa fever","Crimean-Congo haemorrhagic fever") |
             (x$application_type_primary %in% c("Prediction / Forecasting","Risk Mapping / Spatial Analysis","Surveillance / Event Detection") & dis_ok & !is_ebola), "weekly", NA))),
  RCC = function(d) ifelse(geo_ok, ifelse(pl == "High" & is_prio, "immediate", "weekly"), NA),
  MS  = function(d) ifelse(geo_ok, ifelse(is_ebola & x$country %in% FILO_COUNTRIES, "immediate", "monthly"), NA),
  PARTENAIRE_OMS = function(d) ifelse(is_prio & dis_ok, "monthly", NA),
  PARTENAIRE_GENOMIQUE = function(d) ifelse(x$application_type_primary == "Laboratory / Genomic Intelligence", "monthly", NA),
  PARTENAIRE_MODELISATION = function(d) ifelse(x$application_type_primary %in% c("Prediction / Forecasting","Risk Mapping / Spatial Analysis"), "monthly", NA)
)
needs_reliable <- c("SDI"=FALSE,"CELLULE_EBOLA"=FALSE,"RCC"=TRUE,"MS"=TRUE)  # S5/S6 (maladie/pays fiables)

out_dir <- p("data/outputs/ai/alerting", paste0("preview_", format(ref_date, "%Y%m%d")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
get_tpl <- function(g, lang) {
  gg <- ifelse(grepl("^PARTENAIRE", g), "PARTENAIRE", g)
  t <- tpl[tpl$Groupe == gg & tpl$lang == lang, ]
  if (!nrow(t)) t <- tpl[tpl$Groupe == gg & tpl$lang == "EN", ]
  t[1, ]
}
resolve_lang <- function(r) {              # langue du destinataire ; AUTO/vide = langue suggeree du pays, sinon EN
  l <- toupper(trimws(r$lang))
  if (l %in% c("", "AUTO")) {
    cl <- CLANG$lang_suggested[match(r$country, CLANG$country)]
    l <- if (is.na(cl)) "EN" else toupper(cl)
  }
  if (!l %in% LANGS) l <- "EN"
  l
}
summ <- list(); log_new <- list(); ELIG <- list(); MANI <- list()

for (i in seq_len(nrow(rec))) {
  r <- rec[i, ]; g <- r$group; lang <- resolve_lang(r); LANGC <- lang
  tier <- rules[[g]](x)
  ok <- base_ok & !is.na(tier)
  if (g == "SDI" || g == "CELLULE_EBOLA") ok <- ok & TRUE
  if (g %in% c("RCC")) ok <- ok & x$rcc == r$rcc
  if (g %in% c("MS")) ok <- ok & x$country == r$country
  if (!is.na(needs_reliable[g]) && needs_reliable[g] && g != "SDI") ok <- ok & dis_ok | (ok & g %in% c("MS","RCC") & is_ebola)
  key <- paste(x$record_id, if (g == "RCC") paste0(g, ":", r$rcc) else if (g == "MS") paste0(g, ":", r$country) else g)
  ok <- ok & !(key %in% sent_before)
  d <- x[which(ok), , drop = FALSE]; d$tier <- tier[which(ok)]
  d <- d[order(-suppressWarnings(as.numeric(d$priority_score)), d$pub_date, decreasing = FALSE), ]
  d <- d[order(-suppressWarnings(as.numeric(d$priority_score))), ]
  for (tr in intersect(c("immediate","weekly","monthly"), TIERS)) {
    dd <- d[d$tier == tr & d$pub_date >= ref_date - WINDOW_DAYS[[tr]] - ifelse(tr == "weekly", 0, 0), , drop = FALSE]
    if (tr == "monthly") dd <- d[d$tier == "monthly" & d$pub_date >= ref_date - WINDOW_DAYS[["monthly"]], , drop = FALSE]
    if (!nrow(dd)) next
    ELIG[[length(ELIG) + 1]] <- data.frame(group = g, rcc = r$rcc, country = r$country, tier = tr, record_id = dd$record_id, disease_primary = dd$disease_primary, application = dd$application_type_primary, priority_level = dd$priority_level, stringsAsFactors = FALSE)
    mx <- if (tr == "immediate") MAX_IMMEDIATE_PER_DAY else MAX_ITEMS[[ifelse(grepl("^PARTENAIRE",g),"PARTENAIRE",g)]]
    ## [20261013] Digest : methodes appliquees/validees d'abord ; revues, theorie, indetermine -> section "Also noted" (max 3, hors immediat)
    main_ok <- dd$ai_evidence_type %in% c("APPLICATION", "VALIDATION")
    if (tr == "immediate") { shown <- head(dd, mx); n_main <- nrow(shown) } else {
      m1 <- head(dd[main_ok, , drop = FALSE], mx); m2 <- head(dd[!main_ok, , drop = FALSE], 3)
      shown <- rbind(m1, m2); n_main <- nrow(m1) }
    T <- get_tpl(g, lang)
    why <- function(rw) tx("why_fmt", g = g, t = tr, p = rw$priority_level)
    its <- vapply(seq_len(nrow(shown)), function(k) item_html(shown[k, ], why(shown[k, ])), "")
    if (n_main < nrow(shown)) its <- append(its, paste0("<h4 style='margin:16px 0 4px;color:#555'>", esc(tx("also_noted")), "</h4>"), after = n_main)
    items <- paste(its, collapse = "\n")
    dash <- sprintf("<div style='margin:14px 0;padding:8px 10px;background:#f4f4f4'><b>%s :</b> <a href='%s'>%s</a><br/><span style='font-size:12px'>%s</span></div>",
      tx("dashboard"), DASHBOARD_URL, DASHBOARD_URL, tx("dash_hint"))
    items <- paste0(items, "\n", dash)
    note <- T$Note; body <- T$Body; subj <- T$Subject
    period <- if (lang == "FR") paste0(format(ref_date - WINDOW_DAYS[[tr]], "%d/%m/%Y"), " - ", format(ref_date, "%d/%m/%Y")) else paste0(format(ref_date - WINDOW_DAYS[[tr]], "%Y-%m-%d"), " – ", format(ref_date, "%Y-%m-%d"))
    theme <- sub("^PARTENAIRE_", "", g)
    rep <- list("{n}" = nrow(shown), "{n_high}" = sum(shown$priority_level == "High"), "{note}" = note, "{items}" = "@@ITEMS@@",
                "{signature}" = T$Signature, "{periode}" = period, "{period}" = period, "{rcc}" = r$rcc,
                "{country}" = r$country, "{pays}" = r$country, "{theme}" = theme)
    for (k in names(rep)) { body <- gsub(k, rep[[k]], body, fixed = TRUE); subj <- gsub(k, rep[[k]], subj, fixed = TRUE) }
    rtl <- lang == "AR"
    html_body <- paste0("<html><meta charset='utf-8'><body style='font-family:Calibri,Arial;font-size:14px;max-width:820px'>",
      "<!--BANNER--><div style='background:#fff3cd;padding:6px' dir='ltr'><b>", tx("pv_not_sent"), "</b> ", tx("pv_test_to"), esc(r$test_email),
      tx("pv_intended"), esc(r$recipient_name), ", tier ", tr, ", lang ", lang, ")</div><!--/BANNER-->",
      "<div ", if (rtl) "dir='rtl' style='text-align:right'" else "dir='ltr'", "><h3>", esc(subj), "</h3>",
      gsub("\n", "<br/>", gsub("@@ITEMS@@", "</p>@@ITEMS@@<p>", esc(body), fixed = TRUE), fixed = TRUE), "</div></body></html>")
    html_body <- gsub("@@ITEMS@@", items, html_body, fixed = TRUE)
    html_body <- gsub("&lt;/p&gt;|&lt;p&gt;", "", html_body)
    fn <- sprintf("%s_%s_%s.html", gsub("[^A-Za-z0-9]+", "_", paste(g, ifelse(g == "RCC", r$rcc, ifelse(g == "MS", r$country, "")))), tr, format(ref_date, "%Y%m%d"))
    writeLines(html_body, file.path(out_dir, fn), useBytes = TRUE)
    gkey <- if (g == "RCC") paste0(g, ":", r$rcc) else if (g == "MS") paste0(g, ":", r$country) else g
    will_send <- LIVE && isTRUE(r$active_cfg) && grepl("^[^@ ]+@[^@ ]+\\.[^@ ]+$", r$real_email)
    MANI[[length(MANI) + 1]] <- data.frame(file = fn, group = g, group_key = gkey, tier = tr, lang = lang, subject = subj,
      to_real = if (will_send) r$real_email else "", will_send = will_send, n_items = nrow(shown),
      record_ids = paste(shown$record_id, collapse = ";"), run_date = format(ref_date), stringsAsFactors = FALSE)
    summ[[length(summ) + 1]] <- data.frame(group = g, recipient = r$recipient_name, tier = tr, n_eligible = nrow(dd), n_in_email = nrow(shown), file = fn)
    log_new[[length(log_new) + 1]] <- data.frame(record_id = shown$record_id, group = if (g == "RCC") paste0(g, ":", r$rcc) else if (g == "MS") paste0(g, ":", r$country) else g,
      recipient = "(not logged)", status = "preview", tier = tr, run_date = format(ref_date), reason = paste0("rule:", g, "/", tr))
  }
}
if (length(MANI)) write.csv(do.call(rbind, MANI), file.path(out_dir, "manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (length(ELIG)) write.csv(do.call(rbind, ELIG), file.path(out_dir, "eligible_items.csv"), row.names = FALSE, fileEncoding = "UTF-8")
S <- if (length(summ)) do.call(rbind, summ) else data.frame()
write.csv(S, file.path(out_dir, "resume_routage.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (length(log_new)) {
  L <- do.call(rbind, log_new); lf <- p("data/outputs/ai/alerting/alert_log.csv")
  if (file.exists(lf)) { old <- rd(lf); for (cn in setdiff(names(L), names(old))) old[[cn]] <- NA; for (cn in setdiff(names(old), names(L))) L[[cn]] <- NA; L <- rbind(old[names(L)], L) }
  write.csv(L, lf, row.names = FALSE, fileEncoding = "UTF-8")
}
cat(sprintf("PREVIEW termine : %d emails simules dans %s (aucun envoye)\n", nrow(S), out_dir))
if (nrow(S)) print(aggregate(n_eligible ~ group + tier, S, sum))
