############################################################
# PREIS-AI OBSERVATORY
# 04_fetch_europepmc_ai.R
# Step 5A — Fetch AI surveillance records from Europe PMC
# CORRECTED VERSION WITH FULL PUBLICATION DATE CAPTURE
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(lubridate)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_col_chr <- function(df, col_name, default = "") {
  if (col_name %in% names(df)) {
    x <- df[[col_name]]
    if (length(x) == 0) rep(default, nrow(df)) else as.character(x)
  } else {
    rep(default, nrow(df))
  }
}

safe_col_int <- function(df, col_name) {
  if (col_name %in% names(df)) {
    suppressWarnings(as.integer(df[[col_name]]))
  } else {
    rep(NA_integer_, nrow(df))
  }
}

normalize_blank <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN")] <- NA_character_
  x
}

parse_publication_date_safe <- function(x) {
  x <- normalize_blank(x)
  if (length(x) == 0) return(character(0))
  
  # Normalize common ISO-8601 forms before parsing
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

fetch_epmc_cursor_page <- function(query_text, cursor_mark = "*", page_size = 1000) {
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=",
    URLencode(query_text, reserved = TRUE),
    "&format=json",
    "&resultType=core",
    "&pageSize=", page_size,
    "&cursorMark=", URLencode(cursor_mark, reserved = TRUE)
  )
  
  r <- GET(url, timeout(90), user_agent("PREIS_AI/1.0"))
  stop_for_status(r)
  
  txt <- content(r, as = "text", encoding = "UTF-8")
  js  <- fromJSON(txt, flatten = TRUE)
  
  res <- js$resultList$result
  hit_count <- suppressWarnings(as.integer(js$hitCount %||% NA))
  next_cursor <- js$nextCursorMark %||% NA_character_
  
  if (is.null(res)) {
    return(list(
      data = tibble(),
      hit_count = hit_count,
      next_cursor = next_cursor
    ))
  }
  
  df <- as_tibble(res)
  
  if (nrow(df) == 0) {
    return(list(
      data = tibble(),
      hit_count = hit_count,
      next_cursor = next_cursor
    ))
  }
  
  out <- tibble(
    source_name    = rep("EuropePMC", nrow(df)),
    title          = safe_col_chr(df, "title"),
    abstract       = safe_col_chr(df, "abstractText"),
    year           = safe_col_int(df, "pubYear"),
    journal        = safe_col_chr(df, "journalTitle"),
    doi            = safe_col_chr(df, "doi"),
    pmid           = safe_col_chr(df, "pmid"),
    pmcid          = safe_col_chr(df, "pmcid"),
    authors        = safe_col_chr(df, "authorString"),
    article_type   = safe_col_chr(df, "pubType"),
    source         = safe_col_chr(df, "source"),
    is_open_access = safe_col_chr(df, "isOpenAccess"),
    cited_by_count = safe_col_int(df, "citedByCount"),
    
    first_publication_date      = safe_col_chr(df, "firstPublicationDate"),
    publication_date_epmc       = safe_col_chr(df, "publicationDate"),
    electronic_publication_date = safe_col_chr(df, "electronicPublicationDate"),
    first_index_date            = safe_col_chr(df, "firstIndexDate"),
    pub_year_raw                = safe_col_chr(df, "pubYear")
  )
  
  url1 <- safe_col_chr(df, "fullTextUrlList.fullTextUrl.url")
  url2 <- safe_col_chr(df, "authorManuscriptDetails.manuscriptId")
  url3 <- safe_col_chr(df, "doi")
  
  out <- out %>%
    mutate(
      url = dplyr::coalesce(
        na_if(url1, ""),
        na_if(url2, ""),
        ifelse(url3 != "", paste0("https://doi.org/", url3), NA_character_)
      ),
      
      publication_date_raw = dplyr::coalesce(
        normalize_blank(first_publication_date),
        normalize_blank(publication_date_epmc),
        normalize_blank(electronic_publication_date),
        normalize_blank(first_index_date)
      ),
      
      publication_date_source = dplyr::case_when(
        !is.na(normalize_blank(first_publication_date)) ~ "firstPublicationDate",
        !is.na(normalize_blank(publication_date_epmc)) ~ "publicationDate",
        !is.na(normalize_blank(electronic_publication_date)) ~ "electronicPublicationDate",
        !is.na(normalize_blank(first_index_date)) ~ "firstIndexDate",
        TRUE ~ NA_character_
      ),
      
      publication_date = parse_publication_date_safe(publication_date_raw),
      
      pub_year = dplyr::coalesce(
        suppressWarnings(lubridate::year(as.Date(publication_date))),
        suppressWarnings(as.integer(pub_year_raw)),
        year
      ),
      
      date_precision = detect_date_precision(publication_date_raw),
      
      abstract = ifelse(is.na(abstract), "", abstract),
      title    = ifelse(is.na(title), "", title),
      journal  = ifelse(is.na(journal), "", journal),
      doi      = ifelse(is.na(doi), "", doi),
      pmid     = ifelse(is.na(pmid), "", pmid),
      pmcid    = ifelse(is.na(pmcid), "", pmcid),
      authors  = ifelse(is.na(authors), "", authors)
    ) %>%
    select(
      source_name, title, abstract,
      pub_year, year,
      publication_date_raw, publication_date_source, publication_date, date_precision,
      first_publication_date, publication_date_epmc, electronic_publication_date, first_index_date, pub_year_raw,
      journal, doi, pmid, pmcid, authors, article_type, source, is_open_access, cited_by_count, url,
      everything()
    )
  
  list(
    data = out,
    hit_count = hit_count,
    next_cursor = next_cursor
  )
}

fetch_epmc_all <- function(query_text, page_size = 1000, max_pages = 50) {
  all_pages <- list()
  hit_count_final <- NA_integer_
  cursor <- "*"
  
  for (i in seq_len(max_pages)) {
    res <- fetch_epmc_cursor_page(
      query_text = query_text,
      cursor_mark = cursor,
      page_size = page_size
    )
    
    if (is.na(hit_count_final)) {
      hit_count_final <- res$hit_count
    }
    
    dat <- res$data
    next_cursor <- res$next_cursor
    
    if (nrow(dat) == 0) break
    
    all_pages[[length(all_pages) + 1]] <- dat
    
    if (is.na(next_cursor) || identical(next_cursor, cursor)) break
    
    cursor <- next_cursor
    
    if (nrow(dat) < page_size) break
  }
  
  out <- bind_rows(all_pages) %>%
    distinct()
  
  list(
    data = out,
    hit_count = hit_count_final,
    pages_fetched = length(all_pages)
  )
}

fetch_europepmc_ai <- function(project_root = "D:/PREIS_AI", page_size = 1000, max_pages = 50) {
  message("[PREIS-AI] Fetching Europe PMC with cursor pagination...")
  
  queries_file <- file.path(project_root, "config", "ai_queries_master.csv")
  out_dir      <- file.path(project_root, "data", "raw", "ai")
  qc_file      <- file.path(project_root, "data", "outputs", "ai", "europepmc_fetch_qc.csv")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qc_file), recursive = TRUE, showWarnings = FALSE)
  
  queries <- read_csv(queries_file, show_col_types = FALSE) %>%
    filter(source_name == "EuropePMC")
  
  q_global <- queries %>%
    filter(query_scope == "global_transferable") %>%
    pull(query_text)
  
  q_africa <- queries %>%
    filter(query_scope == "africa_specific") %>%
    pull(query_text)
  
  if (length(q_global) != 1) stop("Expected exactly one EuropePMC global query.")
  if (length(q_africa) != 1) stop("Expected exactly one EuropePMC Africa query.")
  
  res_global <- fetch_epmc_all(q_global, page_size = page_size, max_pages = max_pages)
  res_africa <- fetch_epmc_all(q_africa, page_size = page_size, max_pages = max_pages)
  
  dat_global <- res_global$data %>% mutate(query_scope = "global_transferable")
  dat_africa <- res_africa$data %>% mutate(query_scope = "africa_specific")
  
  write_csv(dat_global, file.path(out_dir, "europepmc_ai_global_raw.csv"), na = "")
  write_csv(dat_africa, file.path(out_dir, "europepmc_ai_africa_raw.csv"), na = "")
  
  qc <- tibble(
    source_name = "EuropePMC",
    query_scope = c("global_transferable", "africa_specific"),
    hit_count_reported = c(res_global$hit_count, res_africa$hit_count),
    n_records_downloaded = c(nrow(dat_global), nrow(dat_africa)),
    pages_fetched = c(res_global$pages_fetched, res_africa$pages_fetched),
    n_with_title = c(sum(dat_global$title != ""), sum(dat_africa$title != "")),
    n_with_abstract = c(sum(dat_global$abstract != ""), sum(dat_africa$abstract != "")),
    n_with_doi = c(sum(dat_global$doi != ""), sum(dat_africa$doi != "")),
    n_with_publication_date_raw = c(
      sum(!is.na(dat_global$publication_date_raw)),
      sum(!is.na(dat_africa$publication_date_raw))
    ),
    n_with_publication_date = c(
      sum(!is.na(dat_global$publication_date)),
      sum(!is.na(dat_africa$publication_date))
    ),
    n_day_precision = c(
      sum(dat_global$date_precision == "day", na.rm = TRUE),
      sum(dat_africa$date_precision == "day", na.rm = TRUE)
    ),
    min_year = c(safe_min_year(dat_global$pub_year), safe_min_year(dat_africa$pub_year)),
    max_year = c(safe_max_year(dat_global$pub_year), safe_max_year(dat_africa$pub_year))
  )
  
  write_csv(qc, qc_file)
  
  message("[PREIS-AI] Europe PMC fetch completed.")
  print(qc)
  
  invisible(list(global = dat_global, africa = dat_africa, qc = qc))
}

if (sys.nframe() == 0) {
  fetch_europepmc_ai("D:/PREIS_AI")
}