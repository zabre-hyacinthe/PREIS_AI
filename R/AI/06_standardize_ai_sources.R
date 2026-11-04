############################################################
# PREIS-AI OBSERVATORY
# 06_standardize_ai_sources.R
# Step 6A — Standardize EuropePMC and PubMed raw files
# Corrected full version
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(lubridate)
  library(tidyr)
})

normalize_blank_to_na <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "N/A", "n/a")] <- NA_character_
  x
}

pick_first_nonmissing <- function(df, candidates, default = NA_character_) {
  out <- rep(NA_character_, nrow(df))
  hits <- candidates[candidates %in% names(df)]
  
  if (length(hits) == 0) {
    return(rep(default, nrow(df)))
  }
  
  for (nm in hits) {
    v <- normalize_blank_to_na(df[[nm]])
    idx <- is.na(out) & !is.na(v)
    out[idx] <- v[idx]
  }
  
  out[is.na(out)] <- default
  out
}

pick_first_numeric <- function(df, candidates, default = NA_integer_) {
  out <- rep(NA_integer_, nrow(df))
  hits <- candidates[candidates %in% names(df)]
  
  if (length(hits) == 0) {
    return(rep(default, nrow(df)))
  }
  
  for (nm in hits) {
    v <- suppressWarnings(as.integer(as.character(df[[nm]])))
    idx <- is.na(out) & !is.na(v)
    out[idx] <- v[idx]
  }
  
  out[is.na(out)] <- default
  out
}

parse_real_publication_date <- function(x) {
  x <- normalize_blank_to_na(x)
  
  if (length(x) == 0) {
    return(character(0))
  }
  
  x <- as.character(x)
  x <- stringr::str_squish(x)
  
  x <- gsub("T", " ", x, fixed = TRUE)
  x <- sub("Z$", "", x)
  x <- sub("\\+00:00$", "", x)
  x <- sub("\\+0000$", "", x)
  x <- sub(" UTC$", "", x)
  x <- gsub("([0-9])\\.([0-9]+)$", "\\1", x)
  x <- gsub("([0-9])\\.([0-9]+) ", "\\1 ", x)
  
  out <- rep(NA_character_, length(x))
  
  try_formats <- c(
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%Y%m%d",
    "%Y-%m-%d %H:%M:%S",
    "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%d/%m/%Y",
    "%m/%d/%Y",
    "%d-%m-%Y",
    "%m-%d-%Y",
    "%d %b %Y",
    "%d %B %Y",
    "%b %d %Y",
    "%B %d %Y"
  )
  
  for (fmt in try_formats) {
    idx <- which(is.na(out) & !is.na(x))
    if (length(idx) == 0) break
    
    parsed <- as.Date(x[idx], format = fmt)
    ok <- !is.na(parsed)
    
    if (any(ok)) {
      out[idx[ok]] <- as.character(parsed[ok])
    }
  }
  
  out
}

detect_date_precision <- function(raw_date) {
  raw_date <- normalize_blank_to_na(raw_date)
  
  dplyr::case_when(
    is.na(raw_date) ~ "unknown",
    grepl("^\\d{4}-\\d{2}-\\d{2}$", raw_date) ~ "day",
    grepl("^\\d{4}/\\d{2}/\\d{2}$", raw_date) ~ "day",
    grepl("^\\d{4}-\\d{2}$", raw_date) ~ "month",
    grepl("^\\d{4}/\\d{2}$", raw_date) ~ "month",
    grepl("^\\d{4}$", raw_date) ~ "year",
    TRUE ~ "unknown"
  )
}

read_ai_raw <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = cols(.default = col_character())
  )
}

safe_min_year <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else min(x)
}

safe_max_year <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else max(x)
}

standardize_ai_df <- function(df, record_origin) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  source_name_std <- pick_first_nonmissing(df, c("source_name", "source"))
  title_std <- pick_first_nonmissing(df, c("title"))
  abstract_std <- pick_first_nonmissing(df, c("abstract", "abstractText"))
  journal_std <- pick_first_nonmissing(df, c("journal", "journalTitle"))
  doi_std <- pick_first_nonmissing(df, c("doi"))
  pmid_std <- pick_first_nonmissing(df, c("pmid"))
  pmcid_std <- pick_first_nonmissing(df, c("pmcid"))
  authors_std <- pick_first_nonmissing(df, c("authors", "authorString"))
  article_type_std <- pick_first_nonmissing(df, c("article_type", "pub_type", "pubType"))
  source_std <- pick_first_nonmissing(df, c("source"))
  is_open_access_std <- pick_first_nonmissing(df, c("is_open_access"))
  url_std <- pick_first_nonmissing(df, c("url"))
  query_scope_std <- pick_first_nonmissing(df, c("query_scope"))
  query_string_std <- pick_first_nonmissing(df, c("query_string"))
  cited_by_count_std <- suppressWarnings(as.integer(
    pick_first_nonmissing(df, c("cited_by_count"))
  ))
  
  publication_date_raw <- pick_first_nonmissing(
    df,
    c(
      "publication_date_raw",
      "article_date_raw",
      "pub_date_raw",
      "medline_date",
      "publication_date",
      "published_date",
      "pub_date",
      "date",
      "article_date",
      "published",
      "online_date",
      "epubdate",
      "sortpubdate",
      "first_publication_date",
      "epmc_first_publication_date",
      "firstPublicationDate",
      "publication_date_epmc",
      "electronic_publication_date",
      "first_index_date",
      "coverDate",
      "dc_date",
      "preprint_date"
    )
  )
  
  publication_date_source <- pick_first_nonmissing(
    df,
    c("publication_date_source")
  )
  
  publication_date <- parse_real_publication_date(publication_date_raw)
  
  year_existing <- pick_first_numeric(
    df,
    c("pub_year", "year", "publication_year")
  )
  
  year_from_date <- suppressWarnings(as.integer(substr(publication_date, 1, 4)))
  pub_year <- dplyr::coalesce(year_from_date, year_existing)
  year <- pub_year
  
  date_precision_existing <- pick_first_nonmissing(
    df,
    c("date_precision")
  )
  
  date_precision <- dplyr::coalesce(
    date_precision_existing,
    detect_date_precision(publication_date_raw)
  )
  
  date_fetched_raw <- pick_first_nonmissing(
    df,
    c("date_fetched", "fetched_date", "download_date", "ingest_date"),
    default = as.character(Sys.Date())
  )
  
  date_fetched <- parse_real_publication_date(date_fetched_raw)
  date_fetched[is.na(date_fetched)] <- as.character(Sys.Date())
  
  out <- df %>%
    mutate(
      source_name = stringr::str_squish(replace_na(source_name_std, "")),
      title = stringr::str_squish(replace_na(title_std, "")),
      abstract = stringr::str_squish(replace_na(abstract_std, "")),
      journal = stringr::str_squish(replace_na(journal_std, "")),
      doi = stringr::str_to_lower(stringr::str_squish(replace_na(doi_std, ""))),
      pmid = stringr::str_squish(replace_na(pmid_std, "")),
      pmcid = stringr::str_squish(replace_na(pmcid_std, "")),
      authors = stringr::str_squish(replace_na(authors_std, "")),
      article_type = stringr::str_squish(replace_na(article_type_std, "")),
      source = stringr::str_squish(replace_na(source_std, "")),
      is_open_access = stringr::str_squish(replace_na(is_open_access_std, "")),
      url = stringr::str_squish(replace_na(url_std, "")),
      query_scope = stringr::str_squish(replace_na(query_scope_std, "")),
      query_string = stringr::str_squish(replace_na(query_string_std, "")),
      cited_by_count = cited_by_count_std,
      record_origin = record_origin,
      
      publication_date_raw = publication_date_raw,
      publication_date_source = publication_date_source,
      publication_date = publication_date,
      publication_date_present = !is.na(publication_date) & publication_date != "",
      date_precision = date_precision,
      pub_year = pub_year,
      year = year,
      date_fetched = date_fetched,
      
      text_all = stringr::str_squish(paste(title, abstract)),
      rec_id_provisional = case_when(
        doi != "" ~ paste0("doi:", doi),
        pmid != "" ~ paste0("pmid:", pmid),
        pmcid != "" ~ paste0("pmcid:", pmcid),
        TRUE ~ paste0(
          "txt:",
          stringr::str_sub(
            stringr::str_replace_all(stringr::str_to_lower(text_all), "[^a-z0-9]+", "_"),
            1, 180
          )
        )
      )
    ) %>%
    select(
      record_origin,
      source_name,
      query_scope,
      query_string,
      title,
      abstract,
      text_all,
      year,
      pub_year,
      publication_date_raw,
      publication_date_source,
      publication_date,
      publication_date_present,
      date_precision,
      date_fetched,
      journal,
      doi,
      pmid,
      pmcid,
      authors,
      article_type,
      source,
      is_open_access,
      cited_by_count,
      url,
      rec_id_provisional,
      everything()
    )
  
  out
}

standardize_ai_sources <- function(project_root = "D:/PREIS_AI") {
  raw_dir <- file.path(project_root, "data", "raw", "ai")
  out_dir <- file.path(project_root, "data", "clean", "ai")
  qc_file <- file.path(project_root, "data", "outputs", "ai", "ai_standardization_qc.csv")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qc_file), recursive = TRUE, showWarnings = FALSE)
  
  epmc_global_fp <- file.path(raw_dir, "europepmc_ai_global_raw.csv")
  epmc_africa_fp <- file.path(raw_dir, "europepmc_ai_africa_raw.csv")
  pubm_global_fp <- file.path(raw_dir, "pubmed_ai_global_raw.csv")
  pubm_africa_fp <- file.path(raw_dir, "pubmed_ai_africa_raw.csv")
  
  if (!file.exists(epmc_global_fp)) stop("Missing file: ", epmc_global_fp)
  if (!file.exists(epmc_africa_fp)) stop("Missing file: ", epmc_africa_fp)
  if (!file.exists(pubm_global_fp)) stop("Missing file: ", pubm_global_fp)
  if (!file.exists(pubm_africa_fp)) stop("Missing file: ", pubm_africa_fp)
  
  epmc_global <- read_ai_raw(epmc_global_fp)
  epmc_africa <- read_ai_raw(epmc_africa_fp)
  pubm_global <- read_ai_raw(pubm_global_fp)
  pubm_africa <- read_ai_raw(pubm_africa_fp)
  
  epmc_std <- bind_rows(epmc_global, epmc_africa) %>%
    standardize_ai_df(record_origin = "EuropePMC")
  
  pubm_std <- bind_rows(pubm_global, pubm_africa) %>%
    standardize_ai_df(record_origin = "PubMed")
  
  readr::write_csv(epmc_std, file.path(out_dir, "europepmc_ai_standardized.csv"), na = "")
  readr::write_csv(pubm_std, file.path(out_dir, "pubmed_ai_standardized.csv"), na = "")
  
  qc <- tibble(
    dataset = c("EuropePMC", "PubMed"),
    n_rows = c(nrow(epmc_std), nrow(pubm_std)),
    n_with_title = c(
      sum(!is.na(epmc_std$title) & epmc_std$title != "", na.rm = TRUE),
      sum(!is.na(pubm_std$title) & pubm_std$title != "", na.rm = TRUE)
    ),
    n_with_abstract = c(
      sum(!is.na(epmc_std$abstract) & epmc_std$abstract != "", na.rm = TRUE),
      sum(!is.na(pubm_std$abstract) & pubm_std$abstract != "", na.rm = TRUE)
    ),
    n_with_doi = c(
      sum(!is.na(epmc_std$doi) & epmc_std$doi != "", na.rm = TRUE),
      sum(!is.na(pubm_std$doi) & pubm_std$doi != "", na.rm = TRUE)
    ),
    n_with_pmid = c(
      sum(!is.na(epmc_std$pmid) & epmc_std$pmid != "", na.rm = TRUE),
      sum(!is.na(pubm_std$pmid) & pubm_std$pmid != "", na.rm = TRUE)
    ),
    n_unique_provisional_id = c(
      dplyr::n_distinct(epmc_std$rec_id_provisional),
      dplyr::n_distinct(pubm_std$rec_id_provisional)
    ),
    n_with_publication_date_raw = c(
      sum(!is.na(epmc_std$publication_date_raw) & epmc_std$publication_date_raw != "", na.rm = TRUE),
      sum(!is.na(pubm_std$publication_date_raw) & pubm_std$publication_date_raw != "", na.rm = TRUE)
    ),
    n_with_publication_date = c(
      sum(!is.na(epmc_std$publication_date) & epmc_std$publication_date != "", na.rm = TRUE),
      sum(!is.na(pubm_std$publication_date) & pubm_std$publication_date != "", na.rm = TRUE)
    ),
    n_day_precision = c(
      sum(epmc_std$date_precision == "day", na.rm = TRUE),
      sum(pubm_std$date_precision == "day", na.rm = TRUE)
    ),
    n_with_pub_year = c(
      sum(!is.na(epmc_std$pub_year), na.rm = TRUE),
      sum(!is.na(pubm_std$pub_year), na.rm = TRUE)
    ),
    min_year = c(
      safe_min_year(epmc_std$pub_year),
      safe_min_year(pubm_std$pub_year)
    ),
    max_year = c(
      safe_max_year(epmc_std$pub_year),
      safe_max_year(pubm_std$pub_year)
    )
  )
  
  readr::write_csv(qc, qc_file, na = "")
  
  invisible(list(
    europepmc = epmc_std,
    pubmed = pubm_std,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  standardize_ai_sources("D:/PREIS_AI")
}