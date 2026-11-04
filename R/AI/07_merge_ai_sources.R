############################################################
# PREIS-AI OBSERVATORY
# 07_merge_ai_sources.R
# Step 6B — Merge standardized AI sources
# FULL CORRECTED VERSION
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(lubridate)
})

if (!exists("project_root", inherits = FALSE)) {
  project_root <- "D:/PREIS_AI"
}
if (!exists("root_dir", inherits = FALSE)) {
  root_dir <- project_root
}

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

read_if_exists <- function(fp) {
  if (file.exists(fp)) {
    readr::read_csv(fp, show_col_types = FALSE, progress = FALSE)
  } else {
    tibble::tibble()
  }
}

normalize_blank <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN")] <- NA_character_
  x
}

coalesce_existing_clean <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  
  vals <- lapply(hit, function(nm) normalize_blank(df[[nm]]))
  out <- vals[[1]]
  
  if (length(vals) > 1) {
    for (i in 2:length(vals)) {
      out <- dplyr::coalesce(out, vals[[i]])
    }
  }
  
  out[is.na(out)] <- default
  out
}

coalesce_existing_int <- function(df, candidates, default = NA_integer_) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  
  vals <- lapply(hit, function(nm) suppressWarnings(as.integer(as.character(df[[nm]]))))
  out <- vals[[1]]
  
  if (length(vals) > 1) {
    for (i in 2:length(vals)) {
      out <- dplyr::coalesce(out, vals[[i]])
    }
  }
  
  out
}

parse_publication_date_safe <- function(x) {
  x <- normalize_blank(x)
  if (length(x) == 0) return(character(0))
  
  x <- gsub("T", " ", x, fixed = TRUE)
  x <- gsub("Z$", "", x)
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
  raw_date <- normalize_blank(raw_date)
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

safe_min_year <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else min(x)
}

safe_max_year <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else max(x)
}

harmonize_ai_source_types <- function(dat) {
  if (nrow(dat) == 0) return(dat)
  
  char_cols <- intersect(
    c(
      "record_origin", "source_name", "query_scope", "query_string",
      "title", "abstract", "text_all",
      "publication_date_raw", "publication_date_source", "publication_date",
      "date_precision", "journal", "doi", "pmid", "pmcid", "authors",
      "article_type", "source", "is_open_access", "url", "pdf_url",
      "rec_id_provisional", "provisional_id", "record_id",
      "merge_batch", "source_file",
      "published_date", "updated_date",
      "article_date_raw", "pub_date_raw", "medline_date",
      "first_publication_date", "publication_date_epmc",
      "electronic_publication_date", "first_index_date"
    ),
    names(dat)
  )
  
  int_cols <- intersect(
    c("pub_year", "year", "cited_by_count"),
    names(dat)
  )
  
  dat %>%
    mutate(
      across(all_of(char_cols), ~ as.character(.x)),
      across(all_of(int_cols), ~ suppressWarnings(as.integer(as.character(.x))))
    )
}

merge_ai_sources <- function(project_root = "D:/PREIS_AI") {
  
  message("[PREIS-AI] Merging standardized AI sources...")
  
  clean_dir <- file.path(project_root, "data", "clean", "ai")
  raw_dir   <- file.path(project_root, "data", "raw", "ai")
  out_dir   <- file.path(project_root, "data", "clean", "ai")
  qc_dir    <- file.path(project_root, "data", "outputs", "ai")
  
  safe_dir_create(out_dir)
  safe_dir_create(qc_dir)
  
  source_files <- tibble::tribble(
    ~source_label, ~file_path,
    "EuropePMC", file.path(clean_dir, "europepmc_ai_standardized.csv"),
    "PubMed",    file.path(clean_dir, "pubmed_ai_standardized.csv"),
    "arXiv",     file.path(raw_dir,   "arxiv_standardized.csv"),
    "medRxiv",   file.path(raw_dir,   "medrxiv_standardized.csv"),
    "bioRxiv",   file.path(raw_dir,   "biorxiv_standardized.csv")
  ) %>%
    mutate(exists = file.exists(file_path))
  
  src_list <- lapply(seq_len(nrow(source_files)), function(i) {
    fp <- source_files$file_path[i]
    src <- source_files$source_label[i]
    
    dat <- read_if_exists(fp)
    if (nrow(dat) == 0) return(dat)
    
    dat <- harmonize_ai_source_types(dat)
    
    dat %>%
      mutate(
        merge_batch = basename(fp),
        source_file = fp,
        source_name = dplyr::coalesce(
          normalize_blank(if ("source_name" %in% names(.)) source_name else NA_character_),
          src
        ),
        record_origin = dplyr::coalesce(
          normalize_blank(if ("record_origin" %in% names(.)) record_origin else NA_character_),
          source_name
        )
      )
  })
  
  all_sources <- dplyr::bind_rows(src_list)
  
  if (nrow(all_sources) == 0) {
    stop("No standardized source files were found or all were empty.")
  }
  
  merged <- all_sources %>%
    mutate(
      title = coalesce_existing_clean(., c("title"), default = ""),
      abstract = coalesce_existing_clean(., c("abstract"), default = ""),
      authors = coalesce_existing_clean(., c("authors"), default = ""),
      journal = coalesce_existing_clean(., c("journal"), default = ""),
      doi = coalesce_existing_clean(., c("doi"), default = ""),
      pmid = coalesce_existing_clean(., c("pmid"), default = ""),
      pmcid = coalesce_existing_clean(., c("pmcid"), default = ""),
      article_type = coalesce_existing_clean(., c("article_type"), default = ""),
      source = coalesce_existing_clean(., c("source"), default = ""),
      is_open_access = coalesce_existing_clean(., c("is_open_access"), default = ""),
      url = coalesce_existing_clean(., c("url"), default = ""),
      pdf_url = coalesce_existing_clean(., c("pdf_url"), default = ""),
      query_scope = coalesce_existing_clean(., c("query_scope"), default = ""),
      query_string = coalesce_existing_clean(., c("query_string"), default = ""),
      
      publication_date_raw = coalesce_existing_clean(
        .,
        c(
          "publication_date_raw",
          "article_date_raw",
          "pub_date_raw",
          "medline_date",
          "publication_date",
          "published_date",
          "updated_date",
          "published",
          "date",
          "article_date",
          "online_date",
          "epubdate",
          "sortpubdate",
          "first_publication_date",
          "publication_date_epmc",
          "electronic_publication_date",
          "first_index_date"
        )
      ),
      
      publication_date_source = coalesce_existing_clean(
        .,
        c("publication_date_source"),
        default = NA_character_
      ),
      
      publication_date = dplyr::coalesce(
        parse_publication_date_safe(publication_date_raw),
        normalize_blank(coalesce_existing_clean(., c("publication_date")))
      ),
      
      pub_year = dplyr::coalesce(
        suppressWarnings(as.integer(substr(publication_date, 1, 4))),
        coalesce_existing_int(., c("pub_year", "year"))
      ),
      
      year = pub_year,
      
      date_precision = dplyr::coalesce(
        normalize_blank(coalesce_existing_clean(., c("date_precision"))),
        detect_date_precision(publication_date_raw)
      ),
      
      rec_id_provisional = dplyr::coalesce(
        coalesce_existing_clean(., c("rec_id_provisional", "provisional_id", "record_id")),
        ifelse(
          doi != "", paste0("doi:", doi),
          ifelse(
            pmid != "", paste0("pmid:", pmid),
            ifelse(
              pmcid != "", paste0("pmcid:", pmcid),
              paste0("row_", dplyr::row_number())
            )
          )
        )
      ),
      
      publication_date_present = !is.na(publication_date) & publication_date != "",
      date_fetched = normalize_blank(coalesce_existing_clean(., c("date_fetched"))),
      
      text_all = stringr::str_squish(
        dplyr::coalesce(
          normalize_blank(coalesce_existing_clean(., c("text_all"))),
          paste(title, abstract, authors)
        )
      )
    ) %>%
    select(
      record_origin, source_name, query_scope, query_string,
      title, abstract, text_all,
      pub_year, year,
      publication_date_raw, publication_date_source, publication_date,
      publication_date_present, date_precision, date_fetched,
      journal, doi, pmid, pmcid,
      authors, article_type, source, is_open_access,
      cited_by_count, url, pdf_url,
      rec_id_provisional, merge_batch, source_file,
      everything()
    )
  
  out_file <- file.path(out_dir, "ai_multisource_merged.csv")
  qc_file  <- file.path(qc_dir, "ai_merge_qc.csv")
  
  readr::write_csv(merged, out_file, na = "")
  
  qc <- tibble(
    metric = c(
      "n_sources_declared",
      "n_source_files_found",
      "n_records_merged",
      "n_unique_rec_id_provisional",
      "n_with_title",
      "n_with_abstract",
      "n_with_doi",
      "n_with_pmid",
      "n_with_publication_date_raw",
      "n_with_publication_date",
      "n_day_precision",
      "min_year",
      "max_year"
    ),
    value = c(
      nrow(source_files),
      sum(source_files$exists),
      nrow(merged),
      dplyr::n_distinct(merged$rec_id_provisional),
      sum(merged$title != "", na.rm = TRUE),
      sum(merged$abstract != "", na.rm = TRUE),
      sum(merged$doi != "", na.rm = TRUE),
      sum(merged$pmid != "", na.rm = TRUE),
      sum(!is.na(merged$publication_date_raw) & merged$publication_date_raw != "", na.rm = TRUE),
      sum(!is.na(merged$publication_date) & merged$publication_date != "", na.rm = TRUE),
      sum(merged$date_precision == "day", na.rm = TRUE),
      safe_min_year(merged$pub_year),
      safe_max_year(merged$pub_year)
    )
  )
  
  readr::write_csv(qc, qc_file, na = "")
  
  message("[PREIS-AI] Merge completed.")
  print(qc)
  
  invisible(list(
    merged = merged,
    qc = qc,
    source_files = source_files,
    out_file = out_file,
    qc_file = qc_file
  ))
}

if (sys.nframe() == 0) {
  merge_ai_sources(project_root)
}