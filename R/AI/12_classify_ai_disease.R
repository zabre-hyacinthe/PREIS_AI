# ============================================================
# PREIS-AI — CLASSIFY DISEASE
# File: D:/PREIS_AI/R/AI/12_classify_ai_disease.R
# ============================================================

classify_ai_disease <- function(
    df,
    dict_file = "D:/PREIS_AI/config/ai_disease_dictionary.csv"
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(tibble)
  })
  
  if (!file.exists(dict_file)) {
    stop("Disease dictionary not found: ", dict_file, call. = FALSE)
  }
  
  dict <- readr::read_csv(dict_file, show_col_types = FALSE) %>%
    dplyr::select(disease_primary, disease_group, pathogen_class, keywords) %>%
    mutate(
      disease_primary = as.character(disease_primary),
      disease_group   = as.character(disease_group),
      pathogen_class  = as.character(pathogen_class),
      keywords        = as.character(keywords)
    )
  
  if (nrow(df) == 0) {
    df$disease_primary   <- character()
    df$disease_group     <- character()
    df$pathogen_class    <- character()
    df$disease_detected  <- logical()
    df$disease_keywords_matched <- character()
    df$disease_mentions <- character()
    df$disease_evidence <- character()
    df$ebola_mentioned <- logical()
    return(df)
  }
  
  normalize_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    stringr::str_to_lower(trimws(x))
  }
  
  title_txt <- if ("title" %in% names(df)) normalize_text(df$title) else rep("", nrow(df))
  abstract_txt <- if ("abstract" %in% names(df)) normalize_text(df$abstract) else rep("", nrow(df))
  keywords_txt <- if ("keywords" %in% names(df)) normalize_text(df$keywords) else rep("", nrow(df))
  
  full_txt <- paste(title_txt, abstract_txt, keywords_txt, sep = " || ")
  
  disease_primary <- rep("Unspecified", nrow(df))
  disease_group <- rep("Unspecified", nrow(df))
  pathogen_class <- rep("Unspecified", nrow(df))
  disease_detected <- rep(FALSE, nrow(df))
  disease_keywords_matched <- rep("", nrow(df))
  
  escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
  }
  
  # CORRECTIF 2026-10-06 : la regle "premier match du dictionnaire gagne"
  # etiquetait Ebola un article sur le chikungunya qui cite Ebola en
  # exemple dans son resume (ex. "Nanopore Sequencing for Chikungunya
  # Virus"). Nouvelle regle : la maladie principale est celle qui apparait
  # dans le TITRE (la plus frequente), sinon celle la plus frequente dans
  # titre+resume+mots-cles ; a egalite, l'ordre du dictionnaire tranche.
  # Les autres maladies citees sont conservees dans disease_mentions, et
  # ebola_mentioned distingue "etude sur Ebola" de "Ebola cite".
  best_score <- rep(0, nrow(df))
  best_idx   <- rep(0L, nrow(df))
  mentions   <- rep("", nrow(df))
  eos_algo <- stringr::str_detect(full_txt, "(?i)ebola\\s+optimi[sz]ation\\s+search")
  ebola_mentioned <- rep(FALSE, nrow(df))
  patterns <- character(nrow(dict))

  for (i in seq_len(nrow(dict))) {
    kws <- unlist(strsplit(dict$keywords[i], ";", fixed = TRUE))
    kws <- trimws(tolower(kws))
    kws <- kws[kws != ""]
    if (length(kws) == 0) next
    kws_escaped <- vapply(kws, escape_regex, character(1), USE.NAMES = FALSE)
    # CORRECTIF 2026-10-05 : delimiteurs de mot \b (evite "tb" dans
    # "outbreak", "flu" dans "influencing", "hiv" dans "archive").
    pattern <- paste0("\\b(", paste(kws_escaped, collapse = "|"), ")\\b")
    patterns[i] <- pattern

    n_title <- stringr::str_count(title_txt, pattern)
    n_all   <- stringr::str_count(full_txt, pattern)
    # CORRECTIF 2026-10-05 : "Ebola Optimization Search" (algorithme) n'est pas la maladie.
    if (identical(dict$disease_primary[i], "Ebola")) {
      n_title[eos_algo] <- 0L
      n_all[eos_algo]   <- 0L
      ebola_mentioned   <- n_all > 0
    }
    score <- n_title * 1000 + n_all
    better <- score > best_score          # strict : l'ordre du dictionnaire departage les egalites
    best_score[better] <- score[better]
    best_idx[better]   <- i
    has <- n_all > 0
    mentions[has] <- ifelse(nzchar(mentions[has]),
                            paste(mentions[has], dict$disease_primary[i], sep = "; "),
                            dict$disease_primary[i])
  }

  for (i in seq_len(nrow(dict))) {
    sel <- best_idx == i
    if (!any(sel)) next
    disease_primary[sel]  <- dict$disease_primary[i]
    disease_group[sel]    <- dict$disease_group[i]
    pathogen_class[sel]   <- dict$pathogen_class[i]
    disease_detected[sel] <- TRUE
    disease_keywords_matched[sel] <- stringr::str_extract(full_txt[sel], patterns[i])
  }
  disease_keywords_matched[is.na(disease_keywords_matched)] <- ""
  # Niveau de preuve de la maladie principale (a utiliser pour definir un
  # sous-corpus "centre sur la maladie" : title ou text_multiple).
  n_t_best <- floor(best_score / 1000)
  n_a_best <- best_score - 1000 * n_t_best
  disease_evidence <- ifelse(best_idx == 0L, "none",
                      ifelse(n_t_best > 0, "title",
                      ifelse(n_a_best >= 2, "text_multiple", "single_mention")))

  # CORRECTIF 2026-10-05 (bis) -- dplyr::mutate(x = x) est ambigu quand df a
  # DEJA une colonne nommee x (ce qui est le cas ici dès le 2e run : les
  # colonnes disease_* existent dans le tracker depuis le tout premier
  # passage). Le data-mask de mutate() donne priorite a la colonne
  # EXISTANTE de df sur la variable locale du meme nom -> tout le calcul de
  # cette fonction (dictionnaire, \b, exclusion EOS) etait silencieusement
  # ignore a chaque run sauf le premier. Confirme par diagnostic direct sur
  # le moteur R reel (cf. Projects/claude/analyse-desaccords-validation-
  # 2026-10-05.md). Remplace par une assignation base R sans ambiguite.
  df$disease_primary <- disease_primary
  df$disease_group <- disease_group
  df$pathogen_class <- pathogen_class
  df$disease_detected <- disease_detected
  df$disease_keywords_matched <- disease_keywords_matched
  df$disease_mentions <- mentions
  df$disease_evidence <- disease_evidence
  df$ebola_mentioned <- ebola_mentioned
  df
}