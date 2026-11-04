############################################################
# PREIS-AI OBSERVATORY
# 05_fetch_pubmed_ai.R
# Step 5B — Fetch AI surveillance records from PubMed
# CORRECTED VERSION WITH FULL PUBLICATION DATE CAPTURE
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(xml2)
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(lubridate)
})

safe_text <- function(node, xpath_expr) {
  x <- xml_find_first(node, xpath_expr)
  if (inherits(x, "xml_missing") || length(x) == 0) "" else xml_text(x)
}

collapse_texts <- function(nodes) {
  if (length(nodes) == 0) return("")
  txt <- xml_text(nodes)
  txt <- txt[!is.na(txt) & txt != ""]
  paste(txt, collapse = "; ")
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

build_ymd <- function(y, m = "", d = "") {
  y <- stringr::str_squish(as.character(y))
  m <- stringr::str_squish(as.character(m))
  d <- stringr::str_squish(as.character(d))
  
  if (is.na(y) || y == "") return("")
  
  month_map <- c(
    Jan = "01", Feb = "02", Mar = "03", Apr = "04",
    May = "05", Jun = "06", Jul = "07", Aug = "08",
    Sep = "09", Sept = "09", Oct = "10", Nov = "11", Dec = "12",
    January = "01", February = "02", March = "03", April = "04",
    June = "06", July = "07", August = "08", September = "09",
    October = "10", November = "11", December = "12"
  )
  
  if (!is.na(m) && m %in% names(month_map)) m <- month_map[[m]]
  if (grepl("^[0-9]$", m)) m <- paste0("0", m)
  if (grepl("^[0-9]$", d)) d <- paste0("0", d)
  
  if (is.na(m) || m == "") return(y)
  if (is.na(d) || d == "") return(paste(y, m, sep = "-"))
  paste(y, m, d, sep = "-")
}

extract_pubmed_article <- function(article_node) {
  pmid <- safe_text(article_node, ".//MedlineCitation/PMID")
  article_title <- safe_text(article_node, ".//Article/ArticleTitle")
  
  abstract_nodes <- xml_find_all(article_node, ".//Article/Abstract/AbstractText")
  abstract <- collapse_texts(abstract_nodes)
  
  journal <- safe_text(article_node, ".//Article/Journal/Title")
  
  pub_year_txt  <- safe_text(article_node, ".//Article/Journal/JournalIssue/PubDate/Year")
  pub_month_txt <- safe_text(article_node, ".//Article/Journal/JournalIssue/PubDate/Month")
  pub_day_txt   <- safe_text(article_node, ".//Article/Journal/JournalIssue/PubDate/Day")
  
  medline_date <- safe_text(article_node, ".//Article/Journal/JournalIssue/PubDate/MedlineDate")
  
  article_year_txt  <- safe_text(article_node, ".//Article/ArticleDate/Year")
  article_month_txt <- safe_text(article_node, ".//Article/ArticleDate/Month")
  article_day_txt   <- safe_text(article_node, ".//Article/ArticleDate/Day")
  
  article_date_raw <- build_ymd(article_year_txt, article_month_txt, article_day_txt)
  pub_date_raw     <- build_ymd(pub_year_txt, pub_month_txt, pub_day_txt)
  
  publication_date_raw <- dplyr::coalesce(
    dplyr::na_if(article_date_raw, ""),
    dplyr::na_if(pub_date_raw, ""),
    normalize_blank(medline_date),
    dplyr::na_if(pub_year_txt, "")
  )
  
  publication_date_source <- dplyr::case_when(
    !is.na(dplyr::na_if(article_date_raw, "")) ~ "ArticleDate",
    !is.na(dplyr::na_if(pub_date_raw, "")) ~ "PubDate",
    !is.na(normalize_blank(medline_date)) ~ "MedlineDate",
    !is.na(dplyr::na_if(pub_year_txt, "")) ~ "PubDateYear",
    TRUE ~ NA_character_
  )
  
  publication_date <- parse_publication_date_safe(publication_date_raw)
  
  year <- dplyr::coalesce(
    suppressWarnings(lubridate::year(as.Date(publication_date))),
    suppressWarnings(as.integer(pub_year_txt)),
    suppressWarnings(as.integer(article_year_txt)),
    suppressWarnings(as.integer(str_extract(medline_date, "\\b(19|20)\\d{2}\\b")))
  )
  
  authors_nodes <- xml_find_all(article_node, ".//Article/AuthorList/Author")
  authors <- map_chr(authors_nodes, function(a) {
    ln <- safe_text(a, "./LastName")
    fn <- safe_text(a, "./ForeName")
    cn <- safe_text(a, "./CollectiveName")
    if (cn != "") return(cn)
    str_trim(paste(fn, ln))
  })
  authors <- paste(authors[authors != ""], collapse = "; ")
  
  article_types <- xml_find_all(article_node, ".//Article/PublicationTypeList/PublicationType")
  article_type <- collapse_texts(article_types)
  
  doi_nodes <- xml_find_all(article_node, './/PubmedData/ArticleIdList/ArticleId[@IdType="doi"]')
  doi <- if (length(doi_nodes) == 0) "" else xml_text(doi_nodes[[1]])
  
  pmcid_nodes <- xml_find_all(article_node, './/PubmedData/ArticleIdList/ArticleId[@IdType="pmc"]')
  pmcid <- if (length(pmcid_nodes) == 0) "" else xml_text(pmcid_nodes[[1]])
  
  url <- if (doi != "") {
    paste0("https://doi.org/", doi)
  } else if (pmid != "") {
    paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
  } else {
    ""
  }
  
  tibble(
    source_name    = "PubMed",
    title          = article_title,
    abstract       = abstract,
    pub_year       = year,
    year           = year,
    publication_date_raw = publication_date_raw,
    publication_date_source = publication_date_source,
    publication_date = publication_date,
    date_precision = detect_date_precision(publication_date_raw),
    article_date_raw = article_date_raw,
    pub_date_raw = pub_date_raw,
    medline_date = medline_date,
    journal        = journal,
    doi            = doi,
    pmid           = pmid,
    pmcid          = pmcid,
    authors        = authors,
    article_type   = article_type,
    source         = "PubMed",
    is_open_access = ifelse(pmcid != "", "Y", ""),
    cited_by_count = NA_integer_,
    url            = url
  )
}

pubmed_esearch <- function(query_text, retmax = 9999) {
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  
  r <- GET(
    base,
    query = list(
      db = "pubmed",
      term = query_text,
      retmode = "xml",
      retmax = retmax,
      usehistory = "n"
    ),
    timeout(90),
    user_agent("PREIS_AI/1.0")
  )
  stop_for_status(r)
  
  doc <- read_xml(content(r, as = "text", encoding = "UTF-8"))
  
  count <- suppressWarnings(as.integer(xml_text(xml_find_first(doc, ".//Count"))))
  ids <- xml_text(xml_find_all(doc, ".//IdList/Id"))
  
  list(count = count, ids = ids)
}

pubmed_efetch_batch <- function(id_batch) {
  if (length(id_batch) == 0) return(tibble())
  
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  
  r <- GET(
    base,
    query = list(
      db = "pubmed",
      id = paste(id_batch, collapse = ","),
      retmode = "xml"
    ),
    timeout(120),
    user_agent("PREIS_AI/1.0")
  )
  stop_for_status(r)
  
  doc <- read_xml(content(r, as = "text", encoding = "UTF-8"))
  articles <- xml_find_all(doc, ".//PubmedArticle")
  
  if (length(articles) == 0) return(tibble())
  
  bind_rows(map(articles, extract_pubmed_article))
}

fetch_pubmed_all <- function(query_text, batch_size = 200, max_records = 5000) {
  es <- pubmed_esearch(query_text, retmax = max_records)
  
  ids <- unique(es$ids)
  if (length(ids) == 0) {
    return(list(
      data = tibble(),
      hit_count = es$count,
      ids_found = 0,
      batches_fetched = 0
    ))
  }
  
  batches <- split(ids, ceiling(seq_along(ids) / batch_size))
  dat_list <- vector("list", length(batches))
  
  for (i in seq_along(batches)) {
    dat_list[[i]] <- pubmed_efetch_batch(batches[[i]])
    Sys.sleep(0.34)
  }
  
  dat <- bind_rows(dat_list) %>%
    distinct()
  
  list(
    data = dat,
    hit_count = es$count,
    ids_found = length(ids),
    batches_fetched = length(batches)
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

fetch_pubmed_ai <- function(project_root = "D:/PREIS_AI", batch_size = 200, max_records = 5000) {
  message("[PREIS-AI] Fetching PubMed...")
  
  queries_file <- file.path(project_root, "config", "ai_queries_master.csv")
  out_dir      <- file.path(project_root, "data", "raw", "ai")
  qc_file      <- file.path(project_root, "data", "outputs", "ai", "pubmed_fetch_qc.csv")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qc_file), recursive = TRUE, showWarnings = FALSE)
  
  queries <- read_csv(queries_file, show_col_types = FALSE) %>%
    filter(source_name == "PubMed")
  
  q_global <- queries %>%
    filter(query_scope == "global_transferable") %>%
    pull(query_text)
  
  q_africa <- queries %>%
    filter(query_scope == "africa_specific") %>%
    pull(query_text)
  
  if (length(q_global) != 1) stop("Expected exactly one PubMed global query.")
  if (length(q_africa) != 1) stop("Expected exactly one PubMed Africa query.")
  
  res_global <- fetch_pubmed_all(q_global, batch_size = batch_size, max_records = max_records)
  res_africa <- fetch_pubmed_all(q_africa, batch_size = batch_size, max_records = max_records)
  
  dat_global <- res_global$data %>% mutate(query_scope = "global_transferable")
  dat_africa <- res_africa$data %>% mutate(query_scope = "africa_specific")
  
  # [P1] Bloc Ebola / FHV ajouté en dur (le fetcher n'accepte qu'une requête
  # par scope via la config) — mêmes termes que pour EuropePMC.
  q_ebola <- paste0(
    '(artificial intelligence[Title/Abstract] OR machine learning[Title/Abstract] ',
    'OR deep learning[Title/Abstract] OR neural network[Title/Abstract] ',
    'OR forecasting[Title/Abstract] OR "predictive model"[Title/Abstract]) ',
    'AND (Ebola[Title/Abstract] OR Ebolavirus[Title/Abstract] ',
    'OR "Ebola virus disease"[Title/Abstract] OR filovirus[Title/Abstract] ',
    'OR Marburg[Title/Abstract] OR Lassa[Title/Abstract] ',
    'OR "viral hemorrhagic fever"[Title/Abstract])'
  )
  dat_ebola <- tryCatch({
    res_ebola <- fetch_pubmed_all(q_ebola, batch_size = batch_size, max_records = max_records)
    res_ebola$data %>% mutate(query_scope = "ebola_vhf")
  }, error = function(e) {
    message("[PREIS-AI] PubMed Ebola query failed: ", conditionMessage(e)); dat_global[0, ]
  })
  dat_global <- dplyr::bind_rows(dat_global, dat_ebola)

  write_csv(dat_global, file.path(out_dir, "pubmed_ai_global_raw.csv"), na = "")
  write_csv(dat_africa, file.path(out_dir, "pubmed_ai_africa_raw.csv"), na = "")
  
  qc <- tibble(
    source_name = "PubMed",
    query_scope = c("global_transferable", "africa_specific"),
    hit_count_reported = c(res_global$hit_count, res_africa$hit_count),
    ids_found = c(res_global$ids_found, res_africa$ids_found),
    n_records_downloaded = c(nrow(dat_global), nrow(dat_africa)),
    batches_fetched = c(res_global$batches_fetched, res_africa$batches_fetched),
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
  
  message("[PREIS-AI] PubMed fetch completed.")
  print(qc)
  
  invisible(list(
    global = dat_global,
    africa = dat_africa,
    qc = qc
  ))
}

if (sys.nframe() == 0) {
  fetch_pubmed_ai(Sys.getenv("PREIS_AI_ROOT", unset = "D:/PREIS_AI_CLEAN"))
}