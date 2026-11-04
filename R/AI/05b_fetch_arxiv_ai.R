############################################################
# PREIS-AI OBSERVATORY
# 05b_fetch_arxiv_ai.R
# Step 5C — Fetch AI surveillance records from arXiv
# CORRECTED VERSION WITH FULL PUBLICATION DATE CAPTURE
############################################################

suppressPackageStartupMessages({
  library(xml2)
  library(httr2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(readr)
  library(lubridate)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

clean_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  stringr::str_squish(stringr::str_replace_all(as.character(x), "[\r\n\t]+", " "))
}

normalize_blank <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN")] <- NA_character_
  x
}

safe_xml_text <- function(node) {
  if (length(node) == 0 || inherits(node, "xml_missing")) return(NA_character_)
  clean_text(xml2::xml_text(node))
}

safe_xml_attr <- function(node, attr) {
  if (length(node) == 0 || inherits(node, "xml_missing")) return(NA_character_)
  out <- xml2::xml_attr(node, attr)
  if (length(out) == 0) return(NA_character_)
  clean_text(out)
}

parse_publication_date_safe <- function(x) {
  x <- normalize_blank(x)
  if (length(x) == 0) return(character(0))
  
  x <- gsub("T", " ", x, fixed = TRUE)
  x <- gsub("Z$", "", x)
  x <- gsub("([0-9])\\.([0-9]+)$", "\\1", x)
  x <- gsub("([0-9])\\.([0-9]+) ", "\\1 ", x)
  
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c(
        "Y-m-d H:M:S",
        "Y/m/d H:M:S",
        "Y-m-d H:M",
        "Y/m/d H:M",
        "Y-m-d",
        "Y/m/d",
        "Ymd",
        "Y-m",
        "Y/m",
        "Ym",
        "d/m/Y",
        "m/d/Y",
        "d-m-Y",
        "m-d-Y",
        "d b Y",
        "d B Y",
        "b d Y",
        "B d Y",
        "Y"
      ),
      tz = "UTC",
      quiet = TRUE
    )
  )
  
  out <- as.character(as.Date(parsed))
  out[out %in% c("NA", "")] <- NA_character_
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

extract_arxiv_authors <- function(entry) {
  nodes <- xml2::xml_find_all(entry, ".//*[local-name()='author']/*[local-name()='name']")
  if (length(nodes) == 0) return(NA_character_)
  paste(vapply(nodes, safe_xml_text, character(1)), collapse = "; ")
}

extract_arxiv_doi <- function(entry) {
  doi_node <- xml2::xml_find_first(entry, ".//*[local-name()='doi' or local-name()='arxiv:doi']")
  safe_xml_text(doi_node)
}

extract_primary_category <- function(entry) {
  node <- xml2::xml_find_first(entry, ".//*[local-name()='primary_category']")
  safe_xml_attr(node, "term")
}

extract_pdf_url <- function(entry) {
  link_nodes <- xml2::xml_find_all(entry, ".//*[local-name()='link']")
  if (length(link_nodes) == 0) return(NA_character_)
  
  hrefs <- vapply(link_nodes, function(x) safe_xml_attr(x, "href"), character(1))
  titles <- vapply(link_nodes, function(x) safe_xml_attr(x, "title"), character(1))
  types  <- vapply(link_nodes, function(x) safe_xml_attr(x, "type"), character(1))
  
  idx <- which(titles == "pdf")
  if (length(idx) == 0) idx <- which(types == "application/pdf")
  if (length(idx) == 0) return(NA_character_)
  
  hrefs[idx[1]]
}

fetch_arxiv_once <- function(query, start = 0, max_results = 100) {
  resp <- httr2::request("https://export.arxiv.org/api/query") |>
    httr2::req_user_agent("PREIS-AI/1.0") |>
    httr2::req_url_query(
      search_query = query,
      start = start,
      max_results = max_results,
      sortBy = "submittedDate",
      sortOrder = "descending"
    ) |>
    httr2::req_perform()
  
  txt <- httr2::resp_body_string(resp)
  doc <- xml2::read_xml(txt)
  
  entries <- xml2::xml_find_all(doc, ".//*[local-name()='entry']")
  if (length(entries) == 0) return(tibble())
  
  purrr::map_dfr(entries, function(e) {
    record_id <- safe_xml_text(xml2::xml_find_first(e, ".//*[local-name()='id']"))
    title <- safe_xml_text(xml2::xml_find_first(e, ".//*[local-name()='title']"))
    abstract <- safe_xml_text(xml2::xml_find_first(e, ".//*[local-name()='summary']"))
    published_date <- safe_xml_text(xml2::xml_find_first(e, ".//*[local-name()='published']"))
    updated_date <- safe_xml_text(xml2::xml_find_first(e, ".//*[local-name()='updated']"))
    
    cat_nodes <- xml2::xml_find_all(e, ".//*[local-name()='category']")
    category_all <- if (length(cat_nodes) == 0) {
      NA_character_
    } else {
      paste(vapply(cat_nodes, function(x) safe_xml_attr(x, "term"), character(1)), collapse = "; ")
    }
    
    arxiv_id <- stringr::str_replace(record_id %||% "", "^https?://arxiv.org/abs/", "")
    if (identical(arxiv_id, "")) arxiv_id <- NA_character_
    
    publication_date_raw <- dplyr::coalesce(
      normalize_blank(published_date),
      normalize_blank(updated_date)
    )
    
    publication_date_source <- dplyr::case_when(
      !is.na(normalize_blank(published_date)) ~ "published",
      !is.na(normalize_blank(updated_date)) ~ "updated",
      TRUE ~ NA_character_
    )
    
    publication_date <- parse_publication_date_safe(publication_date_raw)
    
    pub_year <- dplyr::coalesce(
      suppressWarnings(lubridate::year(as.Date(publication_date))),
      suppressWarnings(as.integer(substr(published_date, 1, 4))),
      suppressWarnings(as.integer(substr(updated_date, 1, 4)))
    )
    
    tibble(
      source_name = "arXiv",
      source = "arxiv",
      server = "arXiv",
      record_id = record_id,
      provisional_id = dplyr::coalesce(
        arxiv_id,
        record_id,
        paste0("arxiv_", start, "_", sample.int(1e9, 1))
      ),
      title = title,
      abstract = abstract,
      authors = extract_arxiv_authors(e),
      journal = "arXiv",
      article_type = "preprint",
      doi = extract_arxiv_doi(e),
      pmid = NA_character_,
      pmcid = NA_character_,
      url = record_id,
      pdf_url = extract_pdf_url(e),
      
      published_date = published_date,
      updated_date = updated_date,
      publication_date_raw = publication_date_raw,
      publication_date_source = publication_date_source,
      publication_date = publication_date,
      date_precision = detect_date_precision(publication_date_raw),
      pub_year = pub_year,
      year = pub_year,
      
      category_primary = extract_primary_category(e),
      category_all = category_all,
      query_scope = NA_character_,
      query_string = NA_character_,
      record_origin = "preprint",
      text_all = stringr::str_squish(
        paste(title, abstract, extract_arxiv_authors(e), category_all)
      )
    )
  })
}

fetch_arxiv_all <- function(queries, max_pages = 10, page_size = 100, sleep_sec = 3,
                            query_scope = "global_transferable") {
  out <- list()
  
  for (i in seq_along(queries)) {
    q <- queries[[i]]
    message(sprintf("[arXiv] Query %s/%s", i, length(queries)))
    
    for (pg in seq_len(max_pages)) {
      start_n <- (pg - 1) * page_size
      message(sprintf("[arXiv] page=%s start=%s", pg, start_n))
      
      dat <- tryCatch(
        fetch_arxiv_once(q, start = start_n, max_results = page_size),
        error = function(e) {
          message("[arXiv] ERROR: ", e$message)
          tibble()
        }
      )
      
      if (nrow(dat) == 0) break
      
      dat$query_scope <- query_scope
      dat$query_string <- q
      out[[length(out) + 1]] <- dat
      
      if (nrow(dat) < page_size) break
      Sys.sleep(sleep_sec)
    }
  }
  
  dplyr::bind_rows(out) %>%
    distinct()
}

run_fetch_arxiv_ai <- function(project_root = "D:/PREIS_AI", queries = NULL,
                               max_pages = 10, page_size = 100) {
  raw_dir <- file.path(project_root, "data", "raw", "ai")
  out_dir <- file.path(project_root, "data", "outputs", "ai")
  safe_dir_create(raw_dir)
  safe_dir_create(out_dir)
  
  if (is.null(queries)) {
    queries <- c(
      '(all:"artificial intelligence" OR all:"machine learning" OR all:"deep learning" OR all:"large language model") AND (all:"public health" OR all:"disease surveillance" OR all:"epidemic intelligence" OR all:"outbreak prediction")',
      '(cat:cs.AI OR cat:cs.LG OR cat:cs.CL OR cat:q-bio.QM OR cat:q-bio.PE) AND (all:"surveillance" OR all:"epidemic" OR all:"outbreak" OR all:"public health")'
    )
  }
  
  std <- fetch_arxiv_all(
    queries = queries,
    max_pages = max_pages,
    page_size = page_size
  )
  
  std_fp <- file.path(raw_dir, "arxiv_standardized.csv")
  qc_fp  <- file.path(out_dir, "arxiv_fetch_qc.csv")
  
  readr::write_csv(std, std_fp, na = "")
  readr::write_csv(
    tibble(
      source = "arXiv",
      n_standardized = nrow(std),
      n_with_abstract = if ("abstract" %in% names(std)) sum(!is.na(std$abstract) & std$abstract != "") else 0,
      n_with_doi = if ("doi" %in% names(std)) sum(!is.na(std$doi) & std$doi != "") else 0,
      n_with_publication_date_raw = if ("publication_date_raw" %in% names(std)) sum(!is.na(std$publication_date_raw) & std$publication_date_raw != "") else 0,
      n_with_publication_date = if ("publication_date" %in% names(std)) sum(!is.na(std$publication_date) & std$publication_date != "") else 0,
      n_day_precision = if ("date_precision" %in% names(std)) sum(std$date_precision == "day", na.rm = TRUE) else 0,
      min_year = if ("pub_year" %in% names(std)) safe_min_year(std$pub_year) else NA_integer_,
      max_year = if ("pub_year" %in% names(std)) safe_max_year(std$pub_year) else NA_integer_,
      n_unique_provisional_id = if ("provisional_id" %in% names(std)) dplyr::n_distinct(std$provisional_id) else 0,
      file_standardized = std_fp
    ),
    qc_fp,
    na = ""
  )
  
  invisible(std_fp)
}

if (sys.nframe() == 0) {
  run_fetch_arxiv_ai("D:/PREIS_AI")
}