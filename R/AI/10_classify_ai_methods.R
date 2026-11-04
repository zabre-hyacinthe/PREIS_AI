# =========================================================
# PREIS-AI - CLASSIFY AI METHODS (HARDENED)
# File: 10_classify_ai_methods.R
# Purpose:
#   - Detect AI method families from title / abstract / article_type
#   - Add method_family_primary and method_family_all
#   - Reduce missing classifications through broader patterns
# =========================================================

options(stringsAsFactors = FALSE, warn = 1)

PROJECT_DIR <- "D:/PREIS_AI_CLEAN"

TRACKER_IN  <- file.path(PROJECT_DIR, "data", "enriched", "ai", "ai_tracker_final.csv")
TRACKER_OUT <- TRACKER_IN

log10 <- function(...) {
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
# METHOD DICTIONARY v2 (2026-10-06)
# Principe : separer la PREUVE d'IA (forte) des termes generiques (faibles).
#  - "optimization", "segmentation", "data-driven", "statistical model",
#    "time series", "logistic regression", "transformer" (electrique...) ne
#    constituent PAS une preuve d'IA a eux seuls.
#  - Les termes statistiques/mecanistes (bayesien, ARIMA, SEIR, MCMC...) sont
#    regroupes dans "Bayesian / Statistical Modeling" avec ai_method_evidence
#    = "statistical_only" (pas de preuve d'IA).
#  - Primaire = famille specifique la plus citee (titre + resume, comptes une fois), ex-aequo
#    departages par ordre DL > ML > CV > Optim > NLP > LLM ; "General AI" si
#    seul le terme generique est present.
# ---------------------------------------------------------
METHOD_LOGIC_VERSION <- "v2_20261006"

method_strong <- list(
  "Large Language Models" = paste(c(
    "\\blarge language models?\\b","\\bllms?\\b","\\bgpt[- ]?[34o]\\b","\\bchatgpt\\b",
    "\\bbert\\b","\\broberta\\b","\\bbio[- ]?bert\\b","\\bclinicalbert\\b",
    "\\bprompt engineering\\b","\\bfoundation models?\\b","\\bgenerative ai\\b",
    "\\bgenerative pre-?trained\\b","\\bclaude\\b","\\bgemini\\b","\\bllama\\b"), collapse = "|"),
  "Natural Language Processing" = paste(c(
    "\\bnlp\\b","\\bnatural language processing\\b","\\btext mining\\b","\\btopic model",
    "\\bnamed entity recognition\\b","\\binformation extraction\\b","\\bsentiment analysis\\b",
    "\\btext classification\\b","\\blanguage models?\\b","\\bword embeddings?\\b"), collapse = "|"),
  "Deep Learning" = paste(c(
    "\\bdeep learning\\b","\\bdeep neural networks?\\b",
    "\\b(?:artificial |recurrent |graph |feed-?forward |spiking )?neural networks?\\b",
    "\\bcnns?\\b","\\bconvolutional\\b","\\brnns?\\b","\\blstm\\b","\\bgru\\b",
    "\\bautoencoders?\\b","\\bmultilayer perceptron\\b","\\bu-?net\\b",
    "\\bgenerative adversarial\\b","\\bgnn\\b",
    "\\bdeep (?:\\w+ ){0,2}learning\\b","\\battention (?:network|mechanism)s?\\b",
    "\\bgraph (?:attention|convolution\\w*)\\b","\\bprediction network\\b"), collapse = "|"),
  "Machine Learning" = paste(c(
    "\\bmachine learning\\b","\\bsupervised learning\\b","\\bunsupervised learning\\b",
    "\\bsemi-supervised\\b","\\brandom forests?\\b","\\bgradient boost\\w*","\\bxgboost\\b",
    "\\blightgbm\\b","\\bcatboost\\b","\\bsupport vector\\b","\\bsvm\\b","\\bk[- ]means\\b",
    "\\bdecision trees?\\b","\\bnaive bayes\\b","\\bk-nearest\\b","\\bknn\\b",
    "\\bensemble learning\\b","\\bautoml\\b","\\bfeature selection\\b",
    "\\bboosted (?:regression )?trees?\\b","\\bgaussian process\\b",
    "\\bclustering algorithm","\\bpredictive analytics\\b","\\btransfer learning\\b",
    "\\bfederated learning\\b","\\bself-supervised\\b","\\bcontrastive learning\\b","\\bactive learning\\b"), collapse = "|"),
  "Computer Vision" = paste(c(
    "\\bcomputer vision\\b","\\bimage (?:analysis|classification|recognition|segmentation)\\b",
    "\\bmedical imag\\w+","\\bobject detection\\b",
    "\\bremote sensing\\b.{0,40}(?:classif|deep|learning)","\\bsatellite imagery\\b"), collapse = "|"),
  "Optimization / Hybrid AI" = paste(c(
    "\\breinforcement learning\\b","\\bgenetic algorithms?\\b","\\bevolutionary algorithms?\\b",
    "\\bswarm optimi[sz]ation\\b","\\bfuzzy (?:logic|inference|system)\\b","\\bmulti-agent\\b",
    "\\bmetaheuristic\\w*","\\bhybrid (?:ai|machine learning|deep learning)\\b"), collapse = "|"),
  "General AI" = paste(c(
    "\\bartificial intelligence\\b","\\bai[- ](?:based|driven|powered|enabled|assisted)\\b",
    "\\bcomputational intelligence\\b","\\bintelligent (?:system|algorithm)s?\\b"), collapse = "|")
)

method_stat <- paste(c(
  "\\bbayesian\\b","\\bmarkov\\b","\\barima\\b","\\btime[- ]series\\b","\\bstate[- ]space\\b",
  "\\bhierarchical model","\\bmixed[- ]effects?\\b","\\bregression\\b","\\bcompartmental\\b",
  "\\bseir\\b|\\bsir model\\b","\\bmechanistic model","\\bstochastic model","\\bmathematical model",
  "\\bstatistical model","\\bgeostatistic","\\bspatio-?temporal model","\\breproduction number",
  "\\bmonte carlo\\b","\\bmcmc\\b","\\bforecast\\w*","\\bagent[- ]based","\\bgeneralized (?:additive|linear)",
  "\\bspatial (?:model|analysis)","\\bniche model","\\bmaxent\\b","\\bkriging\\b"), collapse = "|")

method_specific_order <- c("Deep Learning", "Machine Learning", "Computer Vision",
                           "Optimization / Hybrid AI", "Natural Language Processing",
                           "Large Language Models")

count_hits <- function(pattern, txt) {
  if (is.na(txt) || !nzchar(txt)) return(0L)
  m <- gregexpr(pattern, txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) 0L else length(m)
}

# Retourne list(primary, all, evidence)
classify_method_one <- function(title, abstract_etc) {
  title <- ifelse(is.na(title), "", title)
  body  <- ifelse(is.na(abstract_etc), "", abstract_etc)
  full  <- paste(title, ".", body)
  cnt <- vapply(names(method_strong), function(k) {
    count_hits(method_strong[[k]], full)
  }, integer(1))
  # "transformer" : LLM seulement avec un contexte linguistique/attention
  if (grepl("\\btransformers?\\b", full, ignore.case = TRUE, perl = TRUE) &&
      grepl("\\b(?:attention|language|token\\w*|text|bert|gpt|encoder|decoder|sequence|embedding|pre-?trained)\\b",
            full, ignore.case = TRUE, perl = TRUE)) {
    cnt["Large Language Models"] <- cnt["Large Language Models"] + 1L
  }
  # "AI" / "ML" en majuscules (sensible a la casse)
  if (sum(cnt) == 0L && grepl("\\bAI\\b|\\bML\\b", full, perl = TRUE)) cnt["General AI"] <- 1L

  if (sum(cnt) > 0L) {
    fams <- names(cnt)[cnt > 0L]
    spec <- cnt[names(cnt) %in% method_specific_order & cnt > 0L]
    if (length(spec)) {
      ord <- order(-spec, match(names(spec), method_specific_order))
      primary <- names(spec)[ord][1]
    } else primary <- "General AI"
    in_title <- any(vapply(names(method_strong), function(k) count_hits(method_strong[[k]], title) > 0L, logical(1)))
    evidence <- if (in_title || sum(cnt) >= 2L) "explicit" else "mention_only"
    return(list(primary = primary, all = paste(fams, collapse = "; "), evidence = evidence))
  }
  if (grepl(method_stat, full, ignore.case = TRUE, perl = TRUE)) {
    return(list(primary = "Bayesian / Statistical Modeling",
                all = "Bayesian / Statistical Modeling", evidence = "statistical_only"))
  }
  list(primary = NA_character_, all = NA_character_, evidence = "none")
}

trk <- safe_read_csv(TRACKER_IN)

# [CACHE v2] On ne saute les articles deja classes que s'ils portent la
# version de logique courante (sinon reclassification complete).
already_done <- "method_logic_version" %in% names(trk) &&
  "ai_method_evidence" %in% names(trk) &&
  sum(!is.na(trk$method_logic_version) & trk$method_logic_version == METHOD_LOGIC_VERSION) > 0
if (already_done) {
  is_done <- !is.na(trk$method_logic_version) & trk$method_logic_version == METHOD_LOGIC_VERSION
  trk_done <- trk[is_done, ]
  trk_todo <- trk[!is_done, ]
  log10("Cache v2: ", nrow(trk_done), " deja classes | ", nrow(trk_todo), " a classifier")
  if (nrow(trk_todo) == 0) {
    log10("Tous les articles deja classes (v2) - skip")
    quit(save = "no", status = 0)
  }
  trk <- trk_todo
} else {
  log10("Reclassification complete (version ", METHOD_LOGIC_VERSION, ")")
}

title_txt        <- if ("title" %in% names(trk)) clean_txt(trk$title) else rep(NA_character_, nrow(trk))
abstract_txt     <- if ("abstract" %in% names(trk)) clean_txt(trk$abstract) else rep(NA_character_, nrow(trk))
article_type_txt <- if ("article_type" %in% names(trk)) clean_txt(trk$article_type) else rep(NA_character_, nrow(trk))
journal_txt      <- if ("journal" %in% names(trk)) clean_txt(trk$journal) else rep(NA_character_, nrow(trk))

body_txt <- vapply(seq_len(nrow(trk)), function(i) {
  collapse_text(abstract_txt[i], article_type_txt[i])
}, character(1))
title_clean <- ifelse(is.na(title_txt), "", title_txt)

res <- vector("list", nrow(trk))
t0 <- Sys.time()
for (i in seq_len(nrow(trk))) {
  res[[i]] <- classify_method_one(title_clean[i], body_txt[i])
  if (i %% 1000 == 0) log10("  ... ", i, "/", nrow(trk), " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s)")
}
trk$method_family_primary <- vapply(res, function(x) x$primary, character(1))
trk$method_family_all     <- vapply(res, function(x) x$all, character(1))
trk$ai_method_evidence    <- vapply(res, function(x) x$evidence, character(1))
trk$method_logic_version  <- METHOD_LOGIC_VERSION

# [CACHE] Réassembler avec les articles déjà classifiés
if (already_done && nrow(trk_done) > 0) {
  trk <- dplyr::bind_rows(trk_done, trk)
}

utils::write.csv(trk, TRACKER_OUT, row.names = FALSE, na = "")

log10("AI methods classified successfully")
log10("Rows: ", nrow(trk))
log10("With primary method: ", sum(!is.na(trk$method_family_primary)))
print(sort(table(trk$method_family_primary, useNA = "ifany"), decreasing = TRUE))
print(table(trk$ai_method_evidence, useNA = "ifany"))
