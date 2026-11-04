# ============================================================
# PREIS_AI — Fetch AI surveillance articles from Europe PMC
# 01_fetch_articles_ai_v10.R
# Corrected production-safe version
# ============================================================

PROJECT_ROOT <- "D:/PREIS_AI_CLEAN"
PROJECT_LABEL <- "PREIS_AI"

DASHBOARD_DIR <- file.path(PROJECT_ROOT, "dashboard", "data", "dashboard")
RAW_DIR <- file.path(PROJECT_ROOT, "data", "raw", "ai")
OUTPUTS_DIR <- file.path(PROJECT_ROOT, "data", "outputs", "ai")
LOG_DIR <- file.path(PROJECT_ROOT, "logs")

if (!dir.exists(DASHBOARD_DIR)) dir.create(DASHBOARD_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(RAW_DIR)) dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(OUTPUTS_DIR)) dir.create(OUTPUTS_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_FILE_DASHBOARD <- file.path(DASHBOARD_DIR, "articles_ai.csv")
OUT_FILE_GLOBAL_RAW <- file.path(RAW_DIR, "europepmc_ai_global_raw.csv")
OUT_FILE_AFRICA_RAW <- file.path(RAW_DIR, "europepmc_ai_africa_raw.csv")
QC_FILE <- file.path(OUTPUTS_DIR, "europepmc_fetch_qc.csv")
LOG_FILE <- file.path(LOG_DIR, "01_fetch_articles_ai_v10.log")

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(tibble)
  library(stringr)
  library(purrr)
})

log_message <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  write(msg, file = LOG_FILE, append = TRUE)
}

norm_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_replace_all(x, "[[:space:]]+", " ")
  x <- str_trim(x)
  x
}

extract_keywords <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (is.list(x)) {
    vals <- unlist(x, use.names = FALSE)
    vals <- vals[!is.na(vals)]
    return(paste(unique(as.character(vals)), collapse = "; "))
  }
  paste(unique(as.character(x)), collapse = "; ")
}

extract_affiliations <- function(aff_info) {
  if (is.null(aff_info) || length(aff_info) == 0) return("")
  vals <- unlist(aff_info, use.names = FALSE)
  vals <- vals[!is.na(vals)]
  paste(unique(as.character(vals)), collapse = " | ")
}

safe_bind_rows <- function(lst) {
  lst <- lst[!vapply(lst, is.null, logical(1))]
  if (length(lst) == 0) return(tibble())
  bind_rows(lst)
}

make_article_url <- function(pmid, pmcid) {
  pmid <- norm_txt(pmid)
  pmcid <- norm_txt(pmcid)
  dplyr::case_when(
    pmid != "" ~ paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"),
    pmcid != "" ~ paste0("https://pmc.ncbi.nlm.nih.gov/articles/", pmcid, "/"),
    TRUE ~ ""
  )
}

deduplicate_articles <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  df %>%
    mutate(
      dedup_key = case_when(
        norm_txt(doi) != "" ~ paste0("doi:", tolower(norm_txt(doi))),
        norm_txt(pmid) != "" ~ paste0("pmid:", norm_txt(pmid)),
        norm_txt(pmcid) != "" ~ paste0("pmcid:", norm_txt(pmcid)),
        norm_txt(title) != "" ~ paste0("title:", tolower(norm_txt(title))),
        TRUE ~ paste0("row:", row_number())
      )
    ) %>%
    distinct(dedup_key, .keep_all = TRUE) %>%
    select(-dedup_key)
}

extract_results_df <- function(payload) {
  if (is.null(payload)) return(NULL)
  if (!"resultList" %in% names(payload)) return(NULL)
  if (is.null(payload$resultList)) return(NULL)
  if (!"result" %in% names(payload$resultList)) return(NULL)
  
  results <- payload$resultList$result
  
  if (is.null(results)) return(NULL)
  
  if (is.data.frame(results)) return(results)
  
  if (is.list(results) && length(results) > 0) {
    out <- tryCatch(as_tibble(results), error = function(e) NULL)
    return(out)
  }
  
  NULL
}

fetch_epmc_query <- function(query, query_id, query_scope, page_size = 200, max_pages = 25) {
  base_url <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
  
  page <- 1L
  all_rows <- list()
  seen_ids <- character(0)
  effective_max_pages <- max_pages
  final_hit_count <- NA_integer_
  
  repeat {
    log_message("Fetching page", page, "| scope:", query_scope, "| query_id:", query_id)
    
    res <- tryCatch(
      GET(
        url = base_url,
        query = list(
          query = query,
          format = "json",
          pageSize = page_size,
          page = page,
          resultType = "core"
        ),
        timeout(120)
      ),
      error = function(e) e
    )
    
    if (inherits(res, "error")) {
      log_message("HTTP error:", res$message)
      break
    }
    
    if (status_code(res) != 200) {
      log_message("Non-200 status code:", status_code(res))
      break
    }
    
    txt <- content(res, as = "text", encoding = "UTF-8")
    
    payload <- tryCatch(
      fromJSON(txt, flatten = TRUE),
      error = function(e) e
    )
    
    if (inherits(payload, "error")) {
      log_message("JSON parse error:", payload$message)
      break
    }
    
    hit_count <- tryCatch(as.integer(payload$hitCount), error = function(e) NA_integer_)
    final_hit_count <- hit_count
    
    if (!is.na(hit_count)) {
      effective_max_pages <- min(max_pages, max(1L, ceiling(hit_count / page_size)))
    }
    
    results <- tryCatch(extract_results_df(payload), error = function(e) NULL)
    
    if (is.null(results) || !is.data.frame(results) || nrow(results) == 0) {
      log_message("No usable results for page", page, "| hitCount:", ifelse(is.na(hit_count), "NA", hit_count))
      break
    }
    
    n_res <- nrow(results)
    
    pmid_vec <- if ("pmid" %in% names(results)) as.character(results$pmid) else rep("", n_res)
    pmcid_vec <- if ("pmcid" %in% names(results)) as.character(results$pmcid) else rep("", n_res)
    first_pub_vec <- if ("firstPublicationDate" %in% names(results)) as.character(results$firstPublicationDate) else rep("", n_res)
    pub_year_vec <- if ("pubYear" %in% names(results)) as.character(results$pubYear) else rep("", n_res)
    
    page_df <- tibble(
      source_query = query,
      query_id = query_id,
      query_scope = query_scope,
      id = if ("id" %in% names(results)) as.character(results$id) else rep("", n_res),
      source = if ("source" %in% names(results)) as.character(results$source) else rep("", n_res),
      source_name = "EuropePMC",
      pmid = pmid_vec,
      pmcid = pmcid_vec,
      doi = if ("doi" %in% names(results)) as.character(results$doi) else rep("", n_res),
      title = if ("title" %in% names(results)) as.character(results$title) else rep("", n_res),
      abstract = if ("abstractText" %in% names(results)) as.character(results$abstractText) else rep("", n_res),
      authors = if ("authorString" %in% names(results)) as.character(results$authorString) else rep("", n_res),
      journal = if ("journalTitle" %in% names(results)) as.character(results$journalTitle) else rep("", n_res),
      pub_year = pub_year_vec,
      article_type = if ("pubType" %in% names(results)) as.character(results$pubType) else rep("", n_res),
      pub_type = if ("pubType" %in% names(results)) as.character(results$pubType) else rep("", n_res),
      is_open_access = if ("isOpenAccess" %in% names(results)) as.character(results$isOpenAccess) else rep("", n_res),
      cited_by_count = if ("citedByCount" %in% names(results)) as.character(results$citedByCount) else rep("", n_res),
      keywords = if ("keywordList.keyword" %in% names(results)) {
        vapply(results$keywordList.keyword, extract_keywords, character(1))
      } else {
        rep("", n_res)
      },
      affiliation = if ("affiliation" %in% names(results)) {
        as.character(results$affiliation)
      } else if ("authorList.author.affiliation" %in% names(results)) {
        vapply(results$authorList.author.affiliation, extract_affiliations, character(1))
      } else {
        rep("", n_res)
      },
      first_publication_date = first_pub_vec,
      publication_date_raw = first_pub_vec,
      publication_date = first_pub_vec,
      epmc_first_publication_date = first_pub_vec,
      url = make_article_url(pmid_vec, pmcid_vec),
      date_fetched = as.character(Sys.Date())
    ) %>%
      mutate(across(everything(), ~ norm_txt(.x))) %>%
      mutate(
        publication_date_present = if_else(publication_date != "", "TRUE", "FALSE")
      )
    
    page_key <- dplyr::case_when(
      page_df$doi != "" ~ paste0("doi:", tolower(page_df$doi)),
      page_df$pmid != "" ~ paste0("pmid:", page_df$pmid),
      page_df$pmcid != "" ~ paste0("pmcid:", page_df$pmcid),
      page_df$title != "" ~ paste0("title:", tolower(page_df$title)),
      TRUE ~ paste0("id:", page_df$id)
    )
    
    new_mask <- !(page_key %in% seen_ids)
    n_new <- sum(new_mask, na.rm = TRUE)
    
    log_message(
      "Rows fetched this page:", nrow(page_df),
      "| New unique rows:", n_new,
      "| Reported hitCount:", ifelse(is.na(hit_count), "NA", hit_count),
      "| Effective max pages:", effective_max_pages
    )
    
    if (n_new == 0) {
      log_message("No new unique rows on this page. Stopping query.")
      break
    }
    
    page_df <- page_df[new_mask, , drop = FALSE]
    seen_ids <- union(seen_ids, page_key[new_mask])
    all_rows[[length(all_rows) + 1]] <- page_df
    
    if (page >= effective_max_pages) {
      log_message("Reached effective max pages for query.")
      break
    }
    
    if (nrow(page_df) < page_size) {
      log_message("Last partial page reached. Stopping query.")
      break
    }
    
    page <- page + 1L
    Sys.sleep(0.4)
  }
  
  list(
    data = safe_bind_rows(all_rows),
    hit_count = final_hit_count,
    effective_max_pages = effective_max_pages
  )
}

log_message("====================================================")
log_message("START 01_fetch_articles_ai_v10.R")
log_message("PROJECT_ROOT:", PROJECT_ROOT)
log_message("OUT_FILE_DASHBOARD:", OUT_FILE_DASHBOARD)
log_message("OUT_FILE_GLOBAL_RAW:", OUT_FILE_GLOBAL_RAW)
log_message("OUT_FILE_AFRICA_RAW:", OUT_FILE_AFRICA_RAW)
log_message("====================================================")

SEARCH_QUERIES_GLOBAL <- c(
  '(TITLE_ABS:"artificial intelligence" OR TITLE_ABS:"machine learning" OR TITLE_ABS:"deep learning" OR TITLE_ABS:"large language model" OR TITLE_ABS:NLP) AND (TITLE_ABS:"public health surveillance" OR TITLE_ABS:"epidemic intelligence" OR TITLE_ABS:"outbreak detection" OR TITLE_ABS:"early warning system" OR TITLE_ABS:"disease forecasting")',
  '(TITLE_ABS:"machine learning" OR TITLE_ABS:"deep learning" OR TITLE_ABS:"artificial intelligence") AND (TITLE_ABS:"epidemic prediction" OR TITLE_ABS:"outbreak prediction" OR TITLE_ABS:"disease prediction" OR TITLE_ABS:"forecasting")',
  '(TITLE_ABS:"natural language processing" OR TITLE_ABS:NLP OR TITLE_ABS:"large language model" OR TITLE_ABS:LLM) AND (TITLE_ABS:"epidemic intelligence" OR TITLE_ABS:"digital surveillance" OR TITLE_ABS:"biosurveillance" OR TITLE_ABS:"public health surveillance")',
  '(TITLE_ABS:"artificial intelligence" OR TITLE_ABS:"machine learning") AND (TITLE_ABS:"signal detection" OR TITLE_ABS:"anomaly detection" OR TITLE_ABS:"event detection") AND (TITLE_ABS:"public health" OR TITLE_ABS:epidemic OR TITLE_ABS:outbreak)',
  '(TITLE_ABS:"artificial intelligence" OR TITLE_ABS:"machine learning") AND (TITLE_ABS:"digital surveillance" OR TITLE_ABS:"media monitoring" OR TITLE_ABS:"news surveillance" OR TITLE_ABS:"social media surveillance") AND (TITLE_ABS:infectious OR TITLE_ABS:epidemic OR TITLE_ABS:outbreak)',
  '(TITLE_ABS:"artificial intelligence" OR TITLE_ABS:"machine learning" OR TITLE_ABS:"deep learning") AND (TITLE_ABS:"risk mapping" OR TITLE_ABS:"risk prediction" OR TITLE_ABS:"pandemic preparedness")'
)

SEARCH_QUERIES_AFRICA <- c(
  paste0("(", SEARCH_QUERIES_GLOBAL[1], ") AND (TITLE_ABS:Africa OR TITLE_ABS:African)"),
  paste0("(", SEARCH_QUERIES_GLOBAL[2], ") AND (TITLE_ABS:Africa OR TITLE_ABS:African)"),
  paste0("(", SEARCH_QUERIES_GLOBAL[3], ") AND (TITLE_ABS:Africa OR TITLE_ABS:African)"),
  paste0("(", SEARCH_QUERIES_GLOBAL[4], ") AND (TITLE_ABS:Africa OR TITLE_ABS:African)"),
  paste0("(", SEARCH_QUERIES_GLOBAL[5], ") AND (TITLE_ABS:Africa OR TITLE_ABS:African)"),
  paste0("(", SEARCH_QUERIES_GLOBAL[6], ") AND (TITLE_ABS:Africa OR TITLE_ABS:African)")
)

# [P1 2026-08-05] Bloc de requêtes spécifique Ebola / fièvres hémorragiques
# virales (VHF). L'IA appliquée à Ebola était sous-représentée : les requêtes
# génériques exigeaient des termes de "surveillance" absents de nombreux
# articles Ebola. On capture ici IA/modélisation ∩ Ebola/VHF.
EBOLA_TERMS <- paste(
  'TITLE_ABS:Ebola', 'TITLE_ABS:"Ebola virus disease"', 'TITLE_ABS:EVD',
  'TITLE_ABS:Ebolavirus', 'TITLE_ABS:filovirus', 'TITLE_ABS:Marburg',
  'TITLE_ABS:"Lassa fever"', 'TITLE_ABS:Lassa', 'TITLE_ABS:"viral hemorrhagic fever"',
  'TITLE_ABS:"viral haemorrhagic fever"', 'TITLE_ABS:"Sudan virus"',
  'TITLE_ABS:Bundibugyo',
  sep = " OR "
)

SEARCH_QUERIES_DISEASE <- c(
  paste0('(TITLE_ABS:"artificial intelligence" OR TITLE_ABS:"machine learning" OR TITLE_ABS:"deep learning" OR TITLE_ABS:"large language model" OR TITLE_ABS:NLP OR TITLE_ABS:"neural network") AND (', EBOLA_TERMS, ')'),
  paste0('(TITLE_ABS:forecasting OR TITLE_ABS:"predictive model" OR TITLE_ABS:"transmission model" OR TITLE_ABS:"risk mapping" OR TITLE_ABS:"spatial model" OR TITLE_ABS:"outbreak prediction" OR TITLE_ABS:"machine learning" OR TITLE_ABS:"deep learning") AND (', EBOLA_TERMS, ')')
)

global_results <- purrr::map2(
  SEARCH_QUERIES_GLOBAL,
  paste0("QG", seq_along(SEARCH_QUERIES_GLOBAL)),
  ~ purrr::possibly(fetch_epmc_query, otherwise = list(data = tibble(), hit_count = NA_integer_, effective_max_pages = NA_integer_))(
    query = .x,
    query_id = .y,
    query_scope = "global_transferable"
  )
)

africa_results <- purrr::map2(
  SEARCH_QUERIES_AFRICA,
  paste0("QA", seq_along(SEARCH_QUERIES_AFRICA)),
  ~ purrr::possibly(fetch_epmc_query, otherwise = list(data = tibble(), hit_count = NA_integer_, effective_max_pages = NA_integer_))(
    query = .x,
    query_id = .y,
    query_scope = "africa_specific"
  )
)

disease_results <- purrr::map2(
  SEARCH_QUERIES_DISEASE,
  paste0("QE", seq_along(SEARCH_QUERIES_DISEASE)),
  ~ purrr::possibly(fetch_epmc_query, otherwise = list(data = tibble(), hit_count = NA_integer_, effective_max_pages = NA_integer_))(
    query = .x,
    query_id = .y,
    query_scope = "ebola_vhf"
  )
)

europepmc_global_raw <- safe_bind_rows(purrr::map(global_results, "data"))
# [P1] Les résultats Ebola/VHF rejoignent le flux global (dédupliqués en aval)
europepmc_disease_raw <- safe_bind_rows(purrr::map(disease_results, "data"))
europepmc_global_raw  <- safe_bind_rows(list(europepmc_global_raw, europepmc_disease_raw))
log_message("Ebola/VHF rows fetched:", nrow(europepmc_disease_raw))
europepmc_africa_raw <- safe_bind_rows(purrr::map(africa_results, "data"))

log_message("Global rows before dedup:", nrow(europepmc_global_raw))
log_message("Africa rows before dedup:", nrow(europepmc_africa_raw))

europepmc_global_raw <- deduplicate_articles(europepmc_global_raw)
europepmc_africa_raw <- deduplicate_articles(europepmc_africa_raw)

log_message("Global rows after dedup:", nrow(europepmc_global_raw))
log_message("Africa rows after dedup:", nrow(europepmc_africa_raw))

combined_dashboard <- bind_rows(europepmc_global_raw, europepmc_africa_raw) %>%
  deduplicate_articles() %>%
  mutate(
    country = "",
    iso3 = "",
    rcc = "",
    region_level = "",
    has_ai_signal = "",
    primary_ai_method = "",
    primary_surveillance_function = "",
    primary_ai_usecase = "",
    primary_data_type = "",
    ai_surveillance_relevance = ""
  )

## [20261028] GARDE-FOU (journal C020) : si l'API repond 5xx et qu'on obtient 0 ligne, NE PAS ecraser les fichiers precedents (perte de donnees constatee depuis le 16/10).
if (nrow(europepmc_global_raw) == 0 && nrow(europepmc_africa_raw) == 0) {
  log_message("!!! ECHEC SOURCE EuropePMC : 0 ligne recuperee (API indisponible ?) - fichiers precedents CONSERVES, rien n'est ecrase !!!")
  fail_file <- file.path(dirname(QC_FILE), "source_failures.csv")
  fl <- data.frame(date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), source = "EuropePMC", rows = 0L, action = "previous_files_kept", stringsAsFactors = FALSE)
  write.table(fl, fail_file, sep = ",", row.names = FALSE, col.names = !file.exists(fail_file), append = file.exists(fail_file), qmethod = "double")
  quit(save = "no", status = 0)
}
write_csv(europepmc_global_raw, OUT_FILE_GLOBAL_RAW, na = "")
write_csv(europepmc_africa_raw, OUT_FILE_AFRICA_RAW, na = "")
write_csv(combined_dashboard, OUT_FILE_DASHBOARD, na = "")

qc <- tibble(
  dataset = c("europepmc_ai_global_raw", "europepmc_ai_africa_raw", "articles_ai_dashboard"),
  n_rows = c(nrow(europepmc_global_raw), nrow(europepmc_africa_raw), nrow(combined_dashboard)),
  n_with_title = c(
    sum(europepmc_global_raw$title != "", na.rm = TRUE),
    sum(europepmc_africa_raw$title != "", na.rm = TRUE),
    sum(combined_dashboard$title != "", na.rm = TRUE)
  ),
  n_with_abstract = c(
    sum(europepmc_global_raw$abstract != "", na.rm = TRUE),
    sum(europepmc_africa_raw$abstract != "", na.rm = TRUE),
    sum(combined_dashboard$abstract != "", na.rm = TRUE)
  ),
  n_with_doi = c(
    sum(europepmc_global_raw$doi != "", na.rm = TRUE),
    sum(europepmc_africa_raw$doi != "", na.rm = TRUE),
    sum(combined_dashboard$doi != "", na.rm = TRUE)
  ),
  n_with_pmid = c(
    sum(europepmc_global_raw$pmid != "", na.rm = TRUE),
    sum(europepmc_africa_raw$pmid != "", na.rm = TRUE),
    sum(combined_dashboard$pmid != "", na.rm = TRUE)
  ),
  n_with_publication_date = c(
    sum(europepmc_global_raw$publication_date != "", na.rm = TRUE),
    sum(europepmc_africa_raw$publication_date != "", na.rm = TRUE),
    sum(combined_dashboard$publication_date != "", na.rm = TRUE)
  ),
  n_with_pub_year = c(
    sum(europepmc_global_raw$pub_year != "", na.rm = TRUE),
    sum(europepmc_africa_raw$pub_year != "", na.rm = TRUE),
    sum(combined_dashboard$pub_year != "", na.rm = TRUE)
  )
)

write_csv(qc, QC_FILE, na = "")

log_message("Saved global raw rows:", nrow(europepmc_global_raw))
log_message("Saved africa raw rows:", nrow(europepmc_africa_raw))
log_message("Saved dashboard rows:", nrow(combined_dashboard))
log_message("Saved files:", OUT_FILE_GLOBAL_RAW, "|", OUT_FILE_AFRICA_RAW, "|", OUT_FILE_DASHBOARD)
log_message("QC file:", QC_FILE)
log_message("END 01_fetch_articles_ai_v10.R")
log_message("Done.")

print(qc)
cat("Rows fetched for dashboard:", nrow(combined_dashboard), "\n")
