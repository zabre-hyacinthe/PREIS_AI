############################################################
# PREIS-AI OBSERVATORY
# 05c_fetch_medrxiv_ai.R
# Step 5D — Fetch AI surveillance records from medRxiv
# CORRECTED VERSION WITH FULL PUBLICATION DATE CAPTURE
# PATCH 2026-09-27 : retry + timeout + HTTP/1.1 force + capture
#                     d'erreur complete + protection anti-ecrasement
#                     (voir bloc "PATCH" ci-dessous pour le detail)
############################################################

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readr)
  library(lubridate)
})

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

clean_text <- function(x) {
  if (length(x) == 0 || is.null(x)) return(NA_character_)
  x |>
    as.character() |>
    stringr::str_replace_all("[\r\n\t]+", " ") |>
    stringr::str_squish()
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

safe_min_year <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else min(x)
}

safe_max_year <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else max(x)
}

# ============================================================
# PATCH 2026-09-27
# Contexte : depuis ~5 mois, les fetchers bioRxiv/medRxiv echouaient
# silencieusement ("Failed to perform HTTP request") alors que :
#   - Test-NetConnection sur le port 443 reussit (pas de blocage reseau/pare-feu bas niveau)
#   - Invoke-WebRequest (PowerShell/WinHTTP) sur la MEME URL renvoie 200 OK
#     mais avec Content-Length: 0 (corps vide)
# Cette combinaison (200 OK + corps vide, cote WinHTTP ; echec cote
# libcurl/httr2) est une signature typique d'un probleme de negociation
# HTTP/2 derriere un proxy ou pare-feu d'inspection TLS -- WinHTTP retombe
# sans bruit sur HTTP/1.1 alors que libcurl (utilise par httr2) peut rester
# bloque en HTTP/2. Ce correctif :
#   1. force explicitement HTTP/1.1 (req_options(http_version = 2L)),
#   2. utilise un User-Agent de navigateur standard (au lieu de "PREIS-AI/1.0"
#      qui peut etre filtre par un pare-feu applicatif),
#   3. ajoute un timeout explicite et jusqu'a 3 tentatives avec backoff,
#   4. journalise l'erreur COMPLETE (classe + message + parent) en cas
#      d'echec final, au lieu du seul e$message generique.
# Ce correctif est une hypothese de correction fondee sur les preuves
# collectees a ce jour ; le script diagnostic_rxiv_http.R doit etre
# execute en parallele pour confirmer la cause exacte.
# ============================================================

fetch_rxiv_interval <- function(server = "medrxiv", from_date, to_date, cursor = 0,
                                 max_attempts = 3, timeout_sec = 60,
                                 user_agent_str = paste0(
                                   "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                                   "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
                                 )) {
  url <- sprintf("https://api.biorxiv.org/details/%s/%s/%s/%s", server, from_date, to_date, cursor)

  last_err <- NULL
  for (attempt in seq_len(max_attempts)) {
    resp <- tryCatch({
      request(url) |>
        req_user_agent(user_agent_str) |>
        req_options(http_version = 2L) |>   # force HTTP/1.1
        req_timeout(timeout_sec) |>
        req_perform()
    }, error = function(e) {
      last_err <<- e
      NULL
    })
    if (!is.null(resp)) {
      return(jsonlite::fromJSON(resp_body_string(resp), flatten = TRUE))
    }
    if (attempt < max_attempts) {
      message(sprintf("  [%s] tentative %s/%s en echec (%s) -- nouvel essai dans %ss",
                       server, attempt, max_attempts, class(last_err)[1], 2 * attempt))
      Sys.sleep(2 * attempt)
    }
  }
  # Toutes les tentatives ont echoue : on journalise l'erreur COMPLETE avant
  # de la re-signaler, pour permettre un vrai diagnostic (pas seulement le
  # resume generique "Failed to perform HTTP request").
  message(sprintf(
    "  [%s] ERREUR COMPLETE -- classe: %s | message: %s",
    server, paste(class(last_err), collapse = "/"), conditionMessage(last_err)
  ))
  stop(last_err)
}

parse_rxiv <- function(x, server = "medRxiv", query_scope = "global_transferable") {
  coll <- x$collection
  if (is.null(coll) || length(coll) == 0) return(tibble())

  df <- as_tibble(coll)

  needed <- c(
    "doi", "title", "authors", "date", "version", "type", "license", "category", "abstract",
    "published", "author_corresponding", "author_corresponding_institution", "jatsxml"
  )
  for (nm in needed) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
  }

  df |>
    transmute(
      source_name = server,
      source = tolower(server),
      server = server,

      record_id = doi,
      provisional_id = doi,

      title = clean_text(title),
      abstract = clean_text(abstract),
      authors = clean_text(authors),
      journal = server,
      article_type = "preprint",

      doi = doi,
      pmid = NA_character_,
      pmcid = NA_character_,

      url = paste0("https://www.", tolower(server), ".org/content/", doi, "v", version),
      pdf_url = paste0("https://www.", tolower(server), ".org/content/", doi, "v", version, ".full.pdf"),

      published_date = clean_text(date),
      updated_date = clean_text(date),

      publication_date_raw = dplyr::coalesce(
        normalize_blank(clean_text(date)),
        normalize_blank(clean_text(published))
      ),

      publication_date_source = dplyr::case_when(
        !is.na(normalize_blank(clean_text(date))) ~ "date",
        !is.na(normalize_blank(clean_text(published))) ~ "published",
        TRUE ~ NA_character_
      ),

      publication_date = parse_publication_date_safe(publication_date_raw),

      pub_year = dplyr::coalesce(
        suppressWarnings(lubridate::year(as.Date(publication_date))),
        suppressWarnings(as.integer(substr(clean_text(date), 1, 4))),
        suppressWarnings(as.integer(substr(clean_text(published), 1, 4)))
      ),

      year = pub_year,
      date_precision = detect_date_precision(publication_date_raw),

      category_primary = clean_text(category),
      category_all = clean_text(category),

      query_scope = query_scope,
      query_string = NA_character_,
      record_origin = "preprint",

      version = clean_text(version),
      rxiv_type = clean_text(type),
      license = clean_text(license),
      published_flag = clean_text(published),
      author_corresponding = clean_text(author_corresponding),
      author_corresponding_institution = clean_text(author_corresponding_institution),
      jatsxml = clean_text(jatsxml),

      text_all = stringr::str_squish(paste(title, abstract, authors, category))
    )
}

fetch_medrxiv_all <- function(from_date = Sys.Date() - 30, to_date = Sys.Date(),
                              max_pages = 20) {
  out <- list()
  cursor <- 0
  hard_failure <- FALSE   # PATCH : vrai si la 1ere page (ou une page intermediaire) echoue vraiment (erreur HTTP), pas juste "plus de resultats"

  for (i in seq_len(max_pages)) {
    message(sprintf("[medRxiv] page=%s cursor=%s", i, cursor))

    dat <- tryCatch(
      fetch_rxiv_interval("medrxiv", as.character(from_date), as.character(to_date), cursor = cursor),
      error = function(e) {
        message("[medRxiv] ERREUR (apres tentatives) : ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(dat)) {
      hard_failure <- TRUE   # PATCH : echec reel de la requete HTTP, pas une fin normale de pagination
      break
    }

    parsed <- parse_rxiv(dat, server = "medRxiv")
    if (nrow(parsed) == 0) break

    out[[length(out) + 1]] <- parsed

    count_val <- suppressWarnings(as.integer(dat$messages$count[[1]]))
    if (is.na(count_val) || count_val < 100) break

    cursor <- cursor + 100
    Sys.sleep(1)   # PATCH : courtoisie envers l'API partagee bioRxiv/medRxiv, limite le risque de rate-limiting
  }

  result <- dplyr::bind_rows(out) |>
    dplyr::distinct()
  attr(result, "fetch_had_hard_failure") <- hard_failure   # PATCH
  attr(result, "fetch_pages_ok") <- length(out)            # PATCH
  result
}

run_fetch_medrxiv_ai <- function(project_root = "D:/PREIS_AI",
                                 from_date = Sys.Date() - 30,
                                 to_date = Sys.Date(),
                                 max_pages = 20) {
  raw_dir <- file.path(project_root, "data", "raw", "ai")
  out_dir <- file.path(project_root, "data", "outputs", "ai")
  safe_dir_create(raw_dir)
  safe_dir_create(out_dir)

  std <- fetch_medrxiv_all(
    from_date = from_date,
    to_date = to_date,
    max_pages = max_pages
  )

  std_fp <- file.path(raw_dir, "medrxiv_standardized.csv")
  qc_fp  <- file.path(out_dir, "medrxiv_fetch_qc.csv")

  hard_failure <- isTRUE(attr(std, "fetch_had_hard_failure"))
  pages_ok <- attr(std, "fetch_pages_ok")
  if (is.null(pages_ok)) pages_ok <- NA_integer_

  # ============================================================
  # PATCH 2026-09-27 -- PROTECTION ANTI-ECRASEMENT
  # Si la collecte a echoue COMPLETEMENT (0 page reussie -> 0 ligne),
  # on NE remplace PAS le fichier standardise existant par un fichier
  # vide. Avant ce correctif, un echec HTTP total ecrasait quand meme
  # medrxiv_standardized.csv avec un tableau vide, ce qui pouvait faire
  # perdre silencieusement l'historique deja collecte (cf. anomalie de
  # -106 lignes observee sur bioRxiv dans les memes conditions).
  # ============================================================
  if (hard_failure && nrow(std) == 0 && file.exists(std_fp)) {
    message(sprintf(
      "[medRxiv] ECHEC COMPLET DE LA COLLECTE (0 page reussie sur %s max). Fichier existant CONSERVE SANS MODIFICATION : %s",
      max_pages, std_fp
    ))
    readr::write_csv(
      tibble(
        source = "medRxiv",
        date_from = as.character(from_date),
        date_to = as.character(to_date),
        status = "FETCH_FAILED_FILE_PRESERVED",
        pages_ok = pages_ok,
        n_standardized = 0,
        file_standardized = std_fp,
        run_time = as.character(Sys.time())
      ),
      qc_fp,
      na = ""
    )
    return(invisible(NA_character_))
  }

  readr::write_csv(std, std_fp, na = "")
  readr::write_csv(
    tibble(
      source = "medRxiv",
      date_from = as.character(from_date),
      date_to = as.character(to_date),
      status = if (hard_failure) "FETCH_PARTIAL_FAILURE" else "OK",   # PATCH
      pages_ok = pages_ok,                                            # PATCH
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
  run_fetch_medrxiv_ai("D:/PREIS_AI")
}
