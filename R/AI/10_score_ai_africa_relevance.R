############################################################
# PREIS-AI OBSERVATORY
# 10_score_ai_africa_relevance.R
# Step 9 — Africa relevance scoring
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

ai_classified_col_types <- readr::cols(
  record_origin = readr::col_character(),
  source_name = readr::col_character(),
  query_scope = readr::col_character(),
  title = readr::col_character(),
  abstract = readr::col_character(),
  text_all = readr::col_character(),
  year = readr::col_integer(),
  journal = readr::col_character(),
  doi = readr::col_character(),
  pmid = readr::col_character(),
  pmcid = readr::col_character(),
  authors = readr::col_character(),
  article_type = readr::col_character(),
  source = readr::col_character(),
  is_open_access = readr::col_character(),
  cited_by_count = readr::col_integer(),
  url = readr::col_character(),
  rec_id_provisional = readr::col_character(),
  merge_batch = readr::col_character(),
  text_for_classification = readr::col_character(),
  method_hits = readr::col_character(),
  application_hits = readr::col_character(),
  method_categories = readr::col_character(),
  application_categories = readr::col_character(),
  method_family_primary = readr::col_character(),
  application_type_primary = readr::col_character(),
  n_method_hits = readr::col_integer(),
  n_application_hits = readr::col_integer(),
  has_method_signal = readr::col_logical(),
  has_application_signal = readr::col_logical()
)

score_ai_africa_relevance <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Scoring Africa relevance...")
  
  in_file  <- file.path(project_root, "data", "enriched", "ai", "ai_classified_stage1.csv")
  out_file <- file.path(project_root, "data", "enriched", "ai", "ai_scored_stage1.csv")
  qc_file  <- file.path(project_root, "data", "outputs", "ai", "ai_scoring_qc.csv")
  
  dat <- read_csv(in_file, show_col_types = FALSE, col_types = ai_classified_col_types) %>%
    mutate(
      across(where(is.character), ~coalesce(., "")),
      score_text = str_to_lower(str_squish(paste(
        title, abstract, text_all,
        method_hits, application_hits,
        method_categories, application_categories,
        method_family_primary, application_type_primary
      )))
    )
  
  africa_terms <- c(
    "africa", "african union", "africa cdc",
    "algeria","angola","benin","botswana","burkina faso","burundi",
    "cabo verde","cameroon","central african republic","chad","comoros",
    "congo","cote d'ivoire","ivory coast","djibouti","dr congo","drc",
    "egypt","equatorial guinea","eritrea","eswatini","ethiopia","gabon",
    "gambia","ghana","guinea","guinea-bissau","kenya","lesotho","liberia",
    "libya","madagascar","malawi","mali","mauritania","mauritius","morocco",
    "mozambique","namibia","niger","nigeria","rwanda","sao tome","senegal",
    "seychelles","sierra leone","somalia","south africa","south sudan",
    "sudan","tanzania","togo","tunisia","uganda","zambia","zimbabwe"
  )

  # [FIX P1] Termes continentaux (cadre Afrique / LMIC) vs termes-pays,
  # pour graduer la pertinence Afrique sans la contaminer avec l'opérationnel.
  continental_terms <- c(
    "africa", "african", "african union", "africa cdc",
    "sub-saharan", "sub saharan", "subsaharan", "sahel",
    "\\blmic\\b", "low- and middle-income", "low and middle income"
  )
  country_terms <- setdiff(africa_terms, c("africa", "african union", "africa cdc"))

  operational_terms <- c(
    "real-time", "real time", "dashboard", "platform", "system", "framework",
    "implementation", "implemented", "deployment", "deployed",
    "operational", "surveillance system", "decision support"
  )
  
  # CORRECTIF 2026-10-05 (Finding #5, revue de validation humaine) :
  # (a) "african"/"guinea" employes hors geographie ne comptent plus comme
  #     signal Afrique : African American(s), African ancestry/descent,
  #     African swine fever, African green monkey, Papua New Guinea, guinea pig.
  #     (surestimations confirmees : Alzheimer/ancestries, ECG-AI, fievre
  #     porcine africaine -> Moderate a tort).
  # (b) Un pays africain n'est "High" que s'il est dans le TITRE ou cite au
  #     moins 2 fois dans titre+resume (etude ancree sur ce pays). Une seule
  #     mention passagere dans le resume (liste de pays, comparaison) donne
  #     "Moderate". Le comptage utilise titre+resume UNIQUEMENT : text_all
  #     recopie deja titre+resume et fausserait le decompte.
  # Evalue sur la revue humaine (113 evaluations, 95 articles) : accord
  # 72,6 % -> 77,9 % (9 cas ameliores, 2 degrades). Regle evaluee sur le
  # meme echantillon : a revalider sur un nouvel echantillon independant.
  nongeo_pat <- paste0(
    "african[- ]americans?|african ancestr(?:y|ies)|african swine fever|",
    "african green monkeys?|african descent|afro-?american|",
    "papua new guinea|new guinea|guinea pigs?|guinea fowl"
  )
  country_rx   <- regex(paste0("\\b(", paste(country_terms, collapse = "|"), ")\\b"),
                        ignore_case = TRUE)
  continent_rx <- regex(paste(continental_terms, collapse = "|"), ignore_case = TRUE)
  MIN_COUNTRY_MENTIONS <- 2L

  dat <- dat %>%
    mutate(
      score_text_geo = str_squish(str_replace_all(score_text, regex(nongeo_pat, ignore_case = TRUE), " ")),
      title_geo = str_squish(str_replace_all(str_to_lower(title), regex(nongeo_pat, ignore_case = TRUE), " ")),
      abstract_geo = str_squish(str_replace_all(str_to_lower(paste(title, abstract)), regex(nongeo_pat, ignore_case = TRUE), " "))
    )

  source(file.path(project_root, "R", "AI", "lib_geo_gazetteer.R"))
  G_geo <- geo_load(project_root)
  geo_res <- geo_resolve(dat$title, dat$abstract, G_geo)

  out <- dat %>%
    mutate(
      # [FIX P1] Détection graduée : pays africain nommé (fort) vs cadre
      # continental / LMIC seul (modéré).
      # [2026-10-06] Detection pays par gazetteer v2 (alias EN/FR/PT, gentiles,
      # villes, homonymes neutralises) : voir lib_geo_gazetteer.R. Le pays est
      # compte sur titre+resume ; "High" = pays dans le titre ou pays principal
      # cite >= 2 fois.
      africa_country_signal = geo_res$n_countries_mentioned > 0,
      africa_continental_signal = str_detect(score_text_geo, continent_rx),
      africa_signal = africa_country_signal | africa_continental_signal,
      africa_country_mentions = geo_res$africa_primary_mentions,
      africa_country_in_title = geo_res$africa_country_in_title,
      africa_country_focus = africa_country_signal &
        (africa_country_in_title | africa_country_mentions >= MIN_COUNTRY_MENTIONS),
      
      one_health_signal = str_detect(score_text, fixed("one health", ignore_case = TRUE)) |
        str_detect(score_text, fixed("zoonotic", ignore_case = TRUE)) |
        str_detect(score_text, fixed("environmental surveillance", ignore_case = TRUE)) |
        str_detect(score_text, fixed("wastewater surveillance", ignore_case = TRUE)),
      
      digital_signal = str_detect(score_text, fixed("digital surveillance", ignore_case = TRUE)) |
        str_detect(score_text, fixed("online surveillance", ignore_case = TRUE)) |
        str_detect(score_text, fixed("internet-based surveillance", ignore_case = TRUE)),
      
      predictive_signal = str_detect(score_text, fixed("forecast", ignore_case = TRUE)) |
        str_detect(score_text, fixed("prediction", ignore_case = TRUE)) |
        str_detect(score_text, fixed("predictive", ignore_case = TRUE)) |
        str_detect(score_text, fixed("nowcasting", ignore_case = TRUE)) |
        str_detect(score_text, fixed("risk assessment", ignore_case = TRUE)),
      
      operational_signal = str_detect(score_text, regex(paste(operational_terms, collapse = "|"), ignore_case = TRUE)),
      
      method_signal_binary = has_method_signal,
      application_signal_binary = has_application_signal,
      
      # [FIX P1] africa_relevance = AXE AFRIQUE UNIQUEMENT (validité de construit).
      #   4 = étude ancrée sur un pays africain ; 2 = cadre continental / LMIC ;
      #   0 = aucun signal Afrique. Les signaux opérationnels ne le contaminent plus.
      africa_relevance_score = dplyr::case_when(
        africa_country_focus                          ~ 4L,
        africa_country_signal | africa_continental_signal ~ 2L,
        TRUE                                          ~ 0L
      ),
      africa_relevance_level = case_when(
        africa_relevance_score >= 4 ~ "High",
        africa_relevance_score >= 2 ~ "Moderate",
        TRUE                        ~ "None"
      ),

      # [FIX P1] Axe SÉPARÉ : valeur opérationnelle (0–6), pour la priorité,
      #   sans se faire passer pour de la pertinence Afrique.
      operational_value_score =
        as.integer(predictive_signal)  +
        as.integer(operational_signal) +
        as.integer(one_health_signal)  +
        as.integer(digital_signal)     +
        as.integer(method_signal_binary) +
        as.integer(application_signal_binary)
    )
  
  write_csv(out, out_file, na = "")
  
  qc <- tibble(
    metric = c(
      "n_records",
      "n_high_relevance",
      "n_moderate_relevance",
      "n_none_relevance",
      "n_africa_signal",
      "n_one_health_signal",
      "n_digital_signal",
      "n_predictive_signal",
      "n_operational_signal"
    ),
    value = c(
      nrow(out),
      sum(out$africa_relevance_level == "High", na.rm = TRUE),
      sum(out$africa_relevance_level == "Moderate", na.rm = TRUE),
      sum(out$africa_relevance_level == "None", na.rm = TRUE),
      sum(out$africa_signal, na.rm = TRUE),
      sum(out$one_health_signal, na.rm = TRUE),
      sum(out$digital_signal, na.rm = TRUE),
      sum(out$predictive_signal, na.rm = TRUE),
      sum(out$operational_signal, na.rm = TRUE)
    )
  )
  
  write_csv(qc, qc_file)
  
  message("[PREIS-AI] Africa relevance scoring completed.")
  print(qc)
  
  invisible(list(
    scored = out,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  score_ai_africa_relevance("D:/PREIS_AI")
}