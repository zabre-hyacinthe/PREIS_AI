# =========================================================
# PREIS-AI - CLASSIFY AI APPLICATIONS (HARDENED)
# File: 11_classify_ai_applications.R
# Purpose:
#   - Detect primary AI application
#   - Add application_type_primary, application_type_all
#   - Add ai_evidence_type = THEORY / APPLICATION / VALIDATION / REVIEW / UNKNOWN
# =========================================================

options(stringsAsFactors = FALSE, warn = 1)

PROJECT_DIR <- "D:/PREIS_AI_CLEAN"

TRACKER_IN  <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")
TRACKER_OUT <- TRACKER_IN

log11 <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", paste0(..., collapse = ""), "\n")
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(x)) stop("Empty file: ", path)
  x
}

clean_txt <- function(x, default = NA_character_) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  if (length(default) == 1 && !is.na(default)) x[is.na(x)] <- default
  x
}

collapse_text <- function(...) {
  vals <- list(...)
  vals <- unlist(vals, recursive = FALSE, use.names = FALSE)
  vals <- clean_txt(vals)
  vals[is.na(vals)] <- ""
  txt <- paste(vals, collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  trimws(txt)
}

# ---------------------------------------------------------
# APPLICATION DICTIONARY v2 (2026-10-06)
# - Termes trop generiques retires (monitoring, tracking, estimation,
#   laboratory, optimization, mapping seul ...) ou rendus plus specifiques.
# - Primaire = categorie au plus fort score (occurrences dans le resume +
#   2 x occurrences dans le titre) ; ex-aequo departages par l'ordre de
#   priorite historique (Prediction > Surveillance > Risk Mapping > ...).
# - application_score_top enregistre le score de la categorie retenue ; un
#   score de 1 (une seule mention) = confiance faible.
# ---------------------------------------------------------
APPLICATION_LOGIC_VERSION <- "v2_20261006"

application_patterns <- list(
  "Prediction / Forecasting" = c(
    "\\bpredict\\w*","\\bforecast\\w*","\\bnowcast\\w*","\\bearly warning\\b",
    "\\bprojections?\\b","\\bprognos\\w+","\\bpredictive\\b"),
  "Surveillance / Event Detection" = c(
    "\\bsurveillance\\b","\\bepidemic intelligence\\b","\\bsignal detection\\b",
    "\\boutbreak detection\\b","\\bdetect\\w* (?:of )?(?:outbreaks?|signals?|events?|anomal\\w+)",
    "\\banomaly detection\\b","\\bsyndromic\\b","\\bevent[- ]based\\b",
    "\\bdisease monitoring\\b","\\boutbreak monitoring\\b","\\bepidemic monitoring\\b",
    "\\bcontact tracing\\b","\\bsocial media\\b.{0,60}(?:monitor|signal|detect|mining)"),
  "Diagnosis / Clinical Decision Support" = c(
    "\\bdiagnos\\w+","\\bclinical decision support\\b","\\btriage\\b","\\bscreening\\b",
    "\\bpatient (?:management|outcome)","\\bclinical (?:management|outcome)",
    "\\bmortality prediction\\b","\\bdrug (?:discovery|repurposing)\\b","\\btreatment (?:response|outcome)"),
  "Risk Mapping / Spatial Analysis" = c(
    "\\brisk mapping\\b","\\bspatial analysis\\b","\\bhotspots?\\b","\\bgeospatial\\b",
    "\\bmaps?\\b","\\bmapping\\b","\\bspatio-?temporal\\b","\\bspatial distribution\\b",
    "\\bcluster detection\\b","\\brisk stratification\\b","\\bsuitability\\b",
    "\\bgeographic\\w*","\\bremote sensing\\b","\\bniche model"),
  "Resource Planning / Response Optimization" = c(
    "\\bresource allocation\\b","\\bresponse planning\\b","\\blogistics\\b",
    "\\boperational planning\\b","\\bresource planning\\b","\\bworkforce planning\\b",
    "\\bsupply chain\\b","\\bvaccine allocation\\b",
    "\\boptimal (?:allocation|deployment|strategy|strategies)\\b","\\bhealth system capacity\\b"),
  "Policy / Decision Support" = c(
    "\\bdecision support\\b","\\bpolic(?:y|ies)\\b","\\bscenario\\w*","\\bplanning tool\\b",
    "\\bstrategic planning\\b","\\bgovernance\\b",
    "\\bintervention (?:strateg|effect|impact)\\w*","\\bcontrol (?:measures|strateg\\w+|policy)",
    "\\bpreparedness\\b"),
  "Laboratory / Genomic Intelligence" = c(
    "\\bgenom\\w+","\\bsequenc\\w+","\\bvariant\\w*","\\bphylogen\\w*","\\bmolecular\\b",
    "\\bmetagenom\\w+","\\bprotein\\b","\\bantibod\\w+","\\bmutations?\\b"),
  "Digital Health / Chatbot / Hotline" = c(
    "\\bchatbots?\\b","\\bvirtual assistants?\\b","\\bhotline\\b","\\bmobile health\\b",
    "\\bdigital health\\b","\\bmhealth\\b","\\btelemedicine\\b","\\bconversational ai\\b",
    "\\bmobile app\\w*","\\bdashboard\\b","\\bweb application\\b")
)
application_regex <- vapply(application_patterns, function(p) paste(p, collapse = "|"), character(1))

application_priority <- c(
  "Prediction / Forecasting", "Surveillance / Event Detection",
  "Risk Mapping / Spatial Analysis", "Diagnosis / Clinical Decision Support",
  "Resource Planning / Response Optimization", "Policy / Decision Support",
  "Laboratory / Genomic Intelligence", "Digital Health / Chatbot / Hotline"
)

count_hits_app <- function(pattern, txt) {
  if (is.na(txt) || !nzchar(txt)) return(0L)
  m <- gregexpr(pattern, txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) 0L else length(m)
}

# title / body (resume + type d'article). Retourne list(primary, all, top_score)
classify_application_one <- function(title, body) {
  title <- ifelse(is.na(title), "", title); body <- ifelse(is.na(body), "", body)
  sc <- vapply(application_priority, function(k) {
    count_hits_app(application_regex[[k]], body) + 2L * count_hits_app(application_regex[[k]], title)
  }, integer(1))
  if (sum(sc) == 0L) return(list(primary = NA_character_, all = NA_character_, top = 0L))
  best <- max(sc)
  primary <- application_priority[which(sc == best)[1]]
  list(primary = primary, all = paste(application_priority[sc > 0L], collapse = "; "), top = as.integer(best))
}

detect_evidence_type <- function(title, abstract, article_type) {
  txt <- tolower(collapse_text(title, abstract, article_type))
  
  # REVIEW
  if (grepl("\\bsystematic review\\b|\\bscoping review\\b|\\bmeta-analysis\\b|\\breview\\b|\\bliterature review\\b", txt, perl = TRUE)) {
    return("REVIEW")
  }
  
  # APPLICATION
  if (grepl("\\breal[- ]world\\b|\\bfield data\\b|\\broutine data\\b|\\bdataset\\b|\\bcase study\\b|\\bimplementation\\b|\\bimplemented\\b|\\bapplied\\b|\\bdeployment\\b|\\bpilot\\b|\\bprogrammatic\\b", txt, perl = TRUE)) {
    return("APPLICATION")
  }
  
  # VALIDATION
  if (grepl("\\bvalidation\\b|\\bevaluation\\b|\\bperformance\\b|\\baccuracy\\b|\\bauc\\b|\\bsensitivity\\b|\\bspecificity\\b|\\bbenchmark\\b|\\bcomparative study\\b|\\bproof of concept\\b", txt, perl = TRUE)) {
    return("VALIDATION")
  }
  
  # THEORY
  if (grepl("\\bmodel\\b|\\bframework\\b|\\bapproach\\b|\\bmethod\\b|\\bpropose\\b|\\bproposed\\b|\\bdevelopment\\b|\\bconceptual\\b|\\bdesign\\b|\\bmethodological\\b", txt, perl = TRUE)) {
    return("THEORY")
  }
  
  "UNKNOWN"
}

trk <- safe_read_csv(TRACKER_IN)
log11("[DIAG] TRACKER_IN lu : ", nrow(trk), " lignes")

# [CACHE v2] Les articles deja classes avec la version de logique courante sont conserves.
already_done <- "application_logic_version" %in% names(trk) &&
  sum(!is.na(trk$application_logic_version) & trk$application_logic_version == APPLICATION_LOGIC_VERSION) > 0
if (already_done) {
  is_done <- !is.na(trk$application_logic_version) & trk$application_logic_version == APPLICATION_LOGIC_VERSION
  trk_done <- trk[is_done, ]
  trk_todo <- trk[!is_done, ]
  log11("Cache v2: ", nrow(trk_done), " deja classes | ", nrow(trk_todo), " a classifier")
  if (nrow(trk_todo) == 0) {
    log11("Tous les articles deja classes (v2) - skip")
    quit(save = "no", status = 0)
  }
  trk <- trk_todo
} else {
  log11("Reclassification complete (version ", APPLICATION_LOGIC_VERSION, ")")
}

title_txt        <- if ("title" %in% names(trk)) clean_txt(trk$title) else rep(NA_character_, nrow(trk))
abstract_txt     <- if ("abstract" %in% names(trk)) clean_txt(trk$abstract) else rep(NA_character_, nrow(trk))
article_type_txt <- if ("article_type" %in% names(trk)) clean_txt(trk$article_type) else rep(NA_character_, nrow(trk))
journal_txt      <- if ("journal" %in% names(trk)) clean_txt(trk$journal) else rep(NA_character_, nrow(trk))

body_txt <- vapply(seq_len(nrow(trk)), function(i) {
  collapse_text(abstract_txt[i], article_type_txt[i])
}, character(1))
title_clean <- ifelse(is.na(title_txt), "", title_txt)

res_app <- vector("list", nrow(trk))
t0 <- Sys.time()
for (i in seq_len(nrow(trk))) {
  res_app[[i]] <- classify_application_one(title_clean[i], body_txt[i])
  if (i %% 1000 == 0) log11("  ... ", i, "/", nrow(trk), " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s)")
}
trk$application_type_primary <- vapply(res_app, function(x) x$primary, character(1))
trk$application_type_all     <- vapply(res_app, function(x) x$all, character(1))
trk$application_score_top    <- vapply(res_app, function(x) x$top, integer(1))
trk$application_logic_version <- APPLICATION_LOGIC_VERSION

log11("[DIAG] debut detect_evidence_type sur ", nrow(trk), " lignes")
trk$ai_evidence_type <- vapply(seq_len(nrow(trk)), function(i) {
  detect_evidence_type(title_txt[i], abstract_txt[i], article_type_txt[i])
}, character(1))
log11("[DIAG] detect_evidence_type termine")

# [CACHE] Réassembler
if (already_done && nrow(trk_done) > 0) {
  trk <- dplyr::bind_rows(trk_done, trk)
}

utils::write.csv(trk, TRACKER_OUT, row.names = FALSE, na = "")

log11("AI applications classified successfully")
log11("Rows: ", nrow(trk))
log11("With primary application: ", sum(!is.na(trk$application_type_primary)))
log11("Evidence type assigned: ", sum(!is.na(trk$ai_evidence_type)))

print(sort(table(trk$application_type_primary, useNA = "ifany"), decreasing = TRUE))
print(sort(table(trk$ai_evidence_type, useNA = "ifany"), decreasing = TRUE))
