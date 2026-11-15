## ============================================================
## PREIS-AI — DASHBOARD v3 (PRODUCTION)
## Corrections v3 :
##   [C1] Source données : alerts_ai.csv (scores corrects)
##   [C2] Filtre disease_primary dans sidebar + server
##   [C3] Onglet IA × Maladie × Pays (nouveau)
##   [C4] disease_primary + alert_level dans toutes les tables
##   [C5] Évolution temporelle dans Focus Africa
##   [C6] Suppression doublon pub_dates + fonctions dupliquées
##   [C7] Logo chemin relatif www/ + reactiveFileReader auto-refresh
##   [C8] Onglet Priority Intel nettoyé (pas de doublon Methods)
## ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(DT)
  library(leaflet)
  library(sf)
  library(scales)
  library(htmltools)
  library(lubridate)
  library(tibble)
  library(tidyr)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

## ------------------------------------------------------------
## PATHS
## ------------------------------------------------------------
project_dir <- if (dir.exists(file.path(getwd(), "dashboard", "data"))) getwd() else "D:/PREIS_AI_CLEAN"   # server: bundle folder; local: project folder
# [REGISTRY] PREIS AI Atlas tab (added 14 Nov 2026). If the module file is absent the tab shows a notice and nothing else changes.
reg_dir <- { .c1 <- file.path(project_dir, "registry_data"); .c2 <- file.path(project_dir, "PREIS_REGISTRY", "registry_data"); if (dir.exists(.c1)) .c1 else .c2 }
.reg_mod <- Filter(file.exists, c(file.path(project_dir, "registry_module.R"), file.path(project_dir, "dashboard", "registry_module.R")))
if (length(.reg_mod)) {
  source(.reg_mod[1], local = TRUE)
} else {
  registry_ui <- function() tabItem(tabName = "registry", box(width = 12, title = "PREIS AI Atlas", "Module not installed on this server."))
  registry_server <- function(input, output, session, reg_dir, filtered_data) invisible(NULL)
}

# [C1] Source principale : alerts_ai.csv (scores recalculés corrects)
data_file <- file.path(
  project_dir, "dashboard", "data", "dashboard", "alerts_ai.csv"
)

# Fallback si alerts_ai absent
data_file_fallback <- file.path(
  project_dir, "data", "dashboard", "ai_intelligence_dashboard_dataset.csv"
)

if (!file.exists(data_file)) {
  if (file.exists(data_file_fallback)) {
    data_file <- data_file_fallback
    message("Using fallback dataset: ", data_file)
  } else {
    stop("No dashboard dataset found. Checked:\n  ", data_file,
         "\n  ", data_file_fallback)
  }
}

tracker_file <- c(
  file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final.csv"),
  file.path(project_dir, "data", "outputs",  "ai", "ai_tracker_final.csv")
) |> (\(x) x[file.exists(x)][1])()

map_file <- c(
  file.path(project_dir, "maps",      "africa_countries_rcc.geojson"),
  file.path(project_dir, "dashboard", "maps", "africa_countries_rcc.geojson")
) |> (\(x) x[file.exists(x)][1])()

centroid_file <- c(
  file.path(project_dir, "maps",      "country_coords.csv"),
  file.path(project_dir, "maps",      "country_centroids.csv"),
  file.path(project_dir, "dashboard", "maps", "country_coords.csv")
) |> (\(x) x[file.exists(x)][1])()

## ------------------------------------------------------------
## HELPERS (fonctions uniques — [C6] pas de doublons)
## ------------------------------------------------------------
clean_txt <- function(x, default = "Unspecified") {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_squish(x)
  x[x == ""] <- default
  x
}

normalize_blank_to_na <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "NULL", "N/A", "na", "null", "NaN", "None")] <- NA_character_
  x
}

safe_chr <- function(x) normalize_blank_to_na(as.character(x))

safe_label_vec <- function(x) {
  x <- as.character(x)
  x[is.na(x) | str_squish(x) == ""] <- "Unspecified"
  x
}

pick_first <- function(df, candidates, default = "Unspecified") {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  out <- safe_chr(df[[hit[1]]])
  out[is.na(out)] <- default
  out
}

pick_first_num <- function(df, candidates, default = NA_real_) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  out <- suppressWarnings(as.numeric(safe_chr(df[[hit[1]]])))
  out[is.na(out)] <- default
  out
}

parse_binary_signal <- function(x, default = 0) {
  y <- safe_chr(x)
  out <- rep(default, length(y))
  out[tolower(y) %in% c("1","true","yes","y","present")] <- 1
  out[tolower(y) %in% c("0","false","no","n","absent")]  <- 0
  num_y <- suppressWarnings(as.numeric(y))
  out[!is.na(num_y)] <- ifelse(num_y[!is.na(num_y)] > 0, 1, 0)
  as.numeric(out)
}

normalize_key <- function(x) {
  x <- tolower(as.character(x))
  x <- str_squish(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

clean_url_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^https?://", "", x)
  x <- sub("^www\\.", "", x)
  x <- sub("\\?.*$", "", x)
  x <- sub("/+$", "", x)
  x[x == ""] <- NA_character_
  x
}

safe_parse_dates <- function(x) {
  if (inherits(x, "Date"))   return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  n <- length(x)
  if (n == 0) return(as.Date(character()))
  out <- rep(as.Date(NA), n)
  x2  <- str_squish(safe_chr(x))
  x2  <- gsub("T", " ", x2, fixed=TRUE)
  x2  <- sub("Z$|\\+00:00$|\\+0000$| UTC$| GMT$", "", x2)
  x2  <- trimws(x2)
  for (fmt in c("%Y-%m-%d","%Y/%m/%d","%d/%m/%Y","%m/%d/%Y",
                "%d-%m-%Y","%d %b %Y","%b %d %Y","%B %d %Y")) {
    idx <- which(is.na(out) & !is.na(x2))
    if (!length(idx)) break
    p <- suppressWarnings(as.Date(x2[idx], format=fmt))
    out[idx[!is.na(p)]] <- p[!is.na(p)]
  }
  idx <- which(is.na(out) & !is.na(x2) & str_detect(x2,"^[12][0-9]{3}$"))
  if (length(idx)) out[idx] <- as.Date(paste0(x2[idx],"-01-01"))
  out
}

## ------------------------------------------------------------
## LOAD DATA — [C7] reactiveFileReader pour auto-refresh
## ------------------------------------------------------------
## [LANG 20261115] Langue de la publication, detectee sur le titre (mots-outils). Un champ "language" de la source sera prefere quand il existera.
preis_detect_lang <- function(x) {
  sw <- list(
    French     = c("les","des","du","et","pour","dans","avec","une","sur","par","aux","chez","selon","entre","est","sont","ont","ainsi","lors","apr\u00e8s"),
    Spanish    = c("los","las","del","para","con","una","por","sobre","como","mediante","entre","desde","hacia","seg\u00fan"),
    Portuguese = c("dos","das","uma","pelo","pela","em","n\u00e3o","com","mediante","atrav\u00e9s","durante","entre","sobre"),
    German     = c("der","die","das","und","mit","von","zur","zum","f\u00fcr","im","den","dem","ein","eine","bei","auf"),
    Italian    = c("gli","della","delle","dei","degli","nel","nella","per","con","una","sul","sulla","tra"),
    Swahili    = c("kwa","katika","kutoka","cha","za","wa","hii","huu")
  )
  en <- c("the","of","and","in","for","with","to","on","using","based","from","by","an","is","are","as","via","among","at","its","their")
  one <- function(t) {
    if (is.na(t) || !nzchar(t)) return("English")
    t <- tolower(enc2utf8(t))
    if (grepl("\\p{Arabic}", t, perl=TRUE)) return("Arabic")
    if (grepl("[\\p{Arabic}\\p{Han}\\p{Cyrillic}\\p{Hiragana}\\p{Katakana}\\p{Hangul}]", t, perl=TRUE)) return("Other")
    toks <- regmatches(t, gregexpr("\\p{L}+", t, perl=TRUE))[[1]]
    n_en <- sum(toks %in% en)
    sc <- vapply(names(sw), function(k) {
      s <- sum(toks %in% sw[[k]])
      if (k == "French") s <- s + sum(lengths(regmatches(t, gregexpr("\\b[ld]['\u2019]", t, perl=TRUE))))
      as.numeric(s)
    }, numeric(1))
    best <- names(sc)[which.max(sc)]; bs <- max(sc)
    if (bs >= 2 && bs > n_en) return(best)
    if (bs >= 1 && n_en == 0 && length(toks) >= 4) return(best)
    "English"
  }
  tryCatch(vapply(as.character(x), one, character(1), USE.NAMES = FALSE),
           error = function(e) { message("preis_detect_lang: ", conditionMessage(e)); rep("English", length(x)) })
}

load_ai_data <- function(path) {
  message("Loading: ", path)
  
  # ── 1. Lire alerts_ai ──────────────────────────────────────
  ai_raw <- read_csv(path, show_col_types=FALSE,
                     col_types=cols(.default=col_character()))
  if (nrow(ai_raw) == 0) stop("Dataset empty: ", path)
  # [20261010] Garde-fou: toute date/annee future (> annee courante) est ignoree (erreur de metadonnees)
  .cur_y <- as.integer(format(Sys.Date(), "%Y"))
  for (.cc in intersect(c("publication_date","pub_date","date_published","year","pub_year"), names(ai_raw))) {
    .yy <- suppressWarnings(as.integer(substr(trimws(ai_raw[[.cc]]), 1, 4)))
    ai_raw[[.cc]][which(!is.na(.yy) & .yy > .cur_y)] <- NA_character_
  }
  
  # ── 2. Construire table geo depuis dashboard dataset ────────
  # Jointure sur doi/pmid car record_id incompatibles entre fichiers
  dash_geo_path <- file.path(project_dir, "data", "dashboard",
                             "ai_intelligence_dashboard_dataset.csv")
  geo_tbl <- NULL
  # [20261010] Le fichier geo historique (avril 2026) n'est utilise que si alerts_ai n'a pas de colonne country,
  # sinon il reinjecterait l'ancienne classification pour les articles 'Unspecified'.
  if (file.exists(dash_geo_path) && !("country" %in% names(ai_raw))) {
    message("Geo: loading ai_intelligence_dashboard_dataset.csv")
    geo_tbl <- read_csv(dash_geo_path, show_col_types=FALSE,
                        col_types=cols(.default=col_character())) %>%
      select(doi, pmid, any_of(c("country","iso3","rcc"))) %>%
      filter(!is.na(country) & trimws(country) != "") %>%
      mutate(
        doi_k  = tolower(trimws(doi)),
        pmid_k = trimws(pmid)
      ) %>%
      select(doi_k, pmid_k, country, iso3, rcc) %>%
      distinct(doi_k, .keep_all=TRUE)
    message("  -> ", nrow(geo_tbl), " geo rows")
  }
  
  # ── 3. Construire clés de jointure pour alerts_ai ──────────
  ai_keys <- ai_raw %>%
    mutate(
      doi_k = tolower(trimws(dplyr::coalesce(
        dplyr::if_else(grepl("^doi:", record_id, ignore.case=TRUE),
                       sub("(?i)^doi:", "", record_id), NA_character_),
        doi
      ))),
      pmid_k = trimws(dplyr::coalesce(
        dplyr::if_else(grepl("^pmid:", record_id, ignore.case=TRUE),
                       sub("(?i)^pmid:", "", record_id), NA_character_),
        pmid
      ))
    ) %>%
    select(record_id, doi_k, pmid_k)
  
  # ── 4. Résoudre geo pour chaque record ─────────────────────
  if (!is.null(geo_tbl)) {
    # Jointure doi
    geo_matched <- ai_keys %>%
      left_join(geo_tbl %>% select(doi_k, country, iso3, rcc),
                by="doi_k")
    
    # Fallback pmid pour les non-matchés
    miss_idx <- which(is.na(geo_matched$country) | geo_matched$country == "")
    if (length(miss_idx) > 0) {
      geo_pmid <- geo_tbl %>%
        filter(!is.na(pmid_k) & pmid_k != "") %>%
        select(pmid_k, c2=country, i2=iso3, r2=rcc) %>%
        distinct(pmid_k, .keep_all=TRUE)
      fb <- geo_matched[miss_idx,] %>%
        left_join(geo_pmid, by="pmid_k")
      geo_matched$country[miss_idx] <- fb$c2
      geo_matched$iso3[miss_idx]    <- fb$i2
      geo_matched$rcc[miss_idx]     <- fb$r2
    }
    
    geo_resolved <- geo_matched %>%
      select(record_id,
             country_geo = country,
             iso3_geo    = iso3,
             rcc_geo     = rcc)
    
    n_ok <- sum(!is.na(geo_resolved$country_geo) &
                  geo_resolved$country_geo != "", na.rm=TRUE)
    message("Geo resolved: ", n_ok, " / ", nrow(geo_resolved), " records")
  } else {
    geo_resolved <- ai_keys %>%
      select(record_id) %>%
      mutate(country_geo=NA_character_, iso3_geo=NA_character_, rcc_geo=NA_character_)
  }
  
  # ── 5. Mutate principal (sans country/iso3/rcc) ─────────────
  today         <- Sys.Date()
  cur_yr        <- lubridate::year(today)
  cur_mo_start  <- lubridate::floor_date(today, "month")
  # EpiWeek ISO : commence lundi, finit dimanche
  cur_wk_start  <- lubridate::floor_date(today, "week", week_start=1)
  cur_wk_end    <- cur_wk_start + 6L
  cur_epiweek   <- lubridate::epiweek(today)   # 1-53, semaine CDC/ISO
  cur_epiyear   <- lubridate::epiyear(today)
  
  pub_col <- intersect(
    c("publication_date","date_published","pub_date","year"),
    names(ai_raw)
  )[1]
  pub_dates <- if (!is.na(pub_col)) safe_parse_dates(ai_raw[[pub_col]]) else
    rep(as.Date(NA), nrow(ai_raw))
  
  ai <- ai_raw %>%
    mutate(
      across(everything(), safe_chr),
      record_id   = pick_first(., "record_id", ""),
      record_id   = dplyr::if_else(record_id=="",
                                   paste0("rec_",dplyr::row_number()), record_id),
      title       = pick_first(., "title", "Untitled"),
      year        = pick_first(., c("year","pub_year"), NA_character_),
      journal     = pick_first(., "journal", "Unspecified"),
      doi         = pick_first(., "doi", ""),
      pmid        = pick_first(., "pmid", ""),
      url         = pick_first(., "url", ""),
      authors     = pick_first(., "authors", "Unspecified"),
      source_name = pick_first(., c("source_name","source"), "Unspecified"),
      article_type= pick_first(., "article_type", "Unspecified"),
      
      disease_primary          = { dp <- pick_first(., "disease_primary", "Non sp\u00e9cifi\u00e9"); dp[dp %in% c("Non specifi\u00e9", "Non sp\u00e9cifi\u00e9", "Non specifie", "Non sp\u00e9cifie", "non specifi\u00e9", "Unspecified")] <- "Non sp\u00e9cifi\u00e9"; dp },
      disease_confidence       = pick_first(., "disease_confidence", ""),   # [20261014] Low = mention unique, a verifier
      disease_mentioned        = pick_first(., "disease_mentioned", ""),    # [20261014] etiquette retrogradee (non validee)
      countries_mentioned      = pick_first(., "countries_mentioned", ""),  # [20261014] pays cites dans le texte, non valides
      method_family_primary    = pick_first(., "method_family_primary", "Unspecified"),
      application_type_primary = pick_first(., "application_type_primary", "Unspecified"),
      
      africa_relevance_level = pick_first(., "africa_relevance_level", "Unspecified"),
      novelty_level          = pick_first(., "novelty_level", "Unspecified"),
      priority_level         = pick_first(., "priority_level", "Unspecified"),
      alert_level            = priority_level,  # jamais renseigne en amont -> alias sur priority_level reel (High/Moderate/Low)
      signal_profile         = pick_first(., "signal_profile", "Unspecified"),
      tracker_recommendation = pick_first(., "tracker_recommendation", "Unspecified"),
      dashboard_bucket       = pick_first(., "dashboard_bucket", "Unspecified"),
      
      africa_relevance_score = pick_first_num(., "africa_relevance_score", 0),
      novelty_score          = pick_first_num(., "novelty_score", 0),
      priority_score         = pick_first_num(., "priority_score", 0),
      
      predictive_signal  = parse_binary_signal(
        if("predictive_signal"  %in% names(.)) predictive_signal  else 0),
      operational_signal = parse_binary_signal(
        if("operational_signal" %in% names(.)) operational_signal else 0),
      digital_signal     = parse_binary_signal(
        if("digital_signal"     %in% names(.)) digital_signal     else 0),
      one_health_signal  = parse_binary_signal(
        if("one_health_signal"  %in% names(.)) one_health_signal  else 0),
      
      year_num = suppressWarnings(as.integer(year)),
      pub_date = as.Date(pub_dates),
      
      is_new_this_year  = (!is.na(pub_date) & lubridate::year(pub_date)==cur_yr) |
        (is.na(pub_date) & !is.na(year_num) & year_num==cur_yr),
      is_new_this_month = !is.na(pub_date) & pub_date >= cur_mo_start & pub_date <= today,
      # EpiWeek ISO : Lundi → Dimanche (W1-W53)
      # Fenêtre = lundi de la semaine courante à dimanche inclus
      is_new_this_week  = !is.na(pub_date) & pub_date >= (today - 7L),



      epiweek_num  = lubridate::epiweek(pub_date),
      epiyear_num  = lubridate::epiyear(pub_date),
      epiweek_label = dplyr::if_else(
        !is.na(pub_date),
        paste0("W", formatC(lubridate::epiweek(pub_date), width=2, flag="0"),
               "-", lubridate::epiyear(pub_date)),
        NA_character_
      )
    )
  
  if (any(is.na(ai$year_num)) && any(!is.na(ai$pub_date)))
    ai <- ai %>%
    mutate(year_num = ifelse(is.na(year_num),
                             lubridate::year(pub_date), year_num))
  
  # ── 6. Joindre geo APRÈS mutate ──────────────────────────────
  # Utiliser row_number comme clé intermédiaire (stable, pas modifié)
  ai <- ai %>%
    mutate(.row_id = dplyr::row_number())
  
  # geo_resolved a le record_id ORIGINAL (depuis ai_raw)
  # Créer un mapping row → geo via ai_raw$record_id
  ai_raw_ids <- tibble::tibble(
    .row_id    = seq_len(nrow(ai_raw)),
    record_id_orig = ai_raw$record_id,
    country_raw = if ("country" %in% names(ai_raw)) as.character(ai_raw$country) else NA_character_,
    iso3_raw    = if ("iso3" %in% names(ai_raw)) as.character(ai_raw$iso3) else NA_character_,
    rcc_raw     = if ("rcc" %in% names(ai_raw)) as.character(ai_raw$rcc) else NA_character_
  )
  
  geo_with_row <- ai_raw_ids %>%
    left_join(geo_resolved %>% rename(record_id_orig=record_id),
              by="record_id_orig") %>%
    select(.row_id, country_raw, iso3_raw, rcc_raw, country_geo, iso3_geo, rcc_geo)
  
  ai <- ai %>%
    left_join(geo_with_row, by=".row_id") %>%
    mutate(
      country = dplyr::case_when(
        !is.na(country_raw) & trimws(country_raw) != "" & trimws(country_raw) != "Unspecified" ~ trimws(country_raw),
        !is.na(country_geo) & trimws(country_geo) != "" ~ trimws(country_geo),
        TRUE ~ "Unspecified"
      ),
      iso3 = toupper(dplyr::case_when(
        !is.na(iso3_raw) & trimws(iso3_raw) != "" & trimws(iso3_raw) != "Unspecified" ~ trimws(iso3_raw),
        !is.na(iso3_geo) & trimws(iso3_geo) != "" ~ trimws(iso3_geo),
        TRUE ~ "Unspecified"
      )),
      rcc = dplyr::case_when(
        !is.na(rcc_raw) & trimws(rcc_raw) != "" & trimws(rcc_raw) != "Unspecified" ~ trimws(rcc_raw),
        !is.na(rcc_geo) & trimws(rcc_geo) != "" ~ trimws(rcc_geo),
        TRUE ~ "Unspecified"
      )
    ) %>%
    select(-any_of(c("country_raw","iso3_raw","rcc_raw","country_geo","iso3_geo","rcc_geo",".row_id")))
  
  n_final <- sum(ai$country != "Unspecified", na.rm=TRUE)
  message("Geo final: ", n_final, " records avec country (",
          round(n_final/nrow(ai)*100, 1), "%)")
  
  # [LANG 20261115] langue de la publication (detectee sur le titre)
  ai$language <- preis_detect_lang(if ("title" %in% names(ai)) ai$title else rep("", nrow(ai)))
  message("Languages: ", paste(names(table(ai$language)), as.integer(table(ai$language)), sep="=", collapse=", "))
  
  # Debug dates
  today_ref <- Sys.Date()
  wk_start  <- lubridate::floor_date(today_ref, "week", week_start=1)
  wk_end    <- wk_start + 6L
  n_pub_ok  <- sum(!is.na(ai$pub_date))
  n_yr_ok   <- sum(ai$is_new_this_year,  na.rm=TRUE)
  n_mo_ok   <- sum(ai$is_new_this_month, na.rm=TRUE)
  n_wk_ok   <- sum(ai$is_new_this_week,  na.rm=TRUE)
  message("pub_date non-NA: ", n_pub_ok, " | this year: ", n_yr_ok,
          " | this month: ", n_mo_ok, " | this week: ", n_wk_ok)
  message("EpiWeek W", formatC(lubridate::epiweek(today_ref),width=2,flag="0"),
          "/", lubridate::epiyear(today_ref),
          " window: ", wk_start, " to ", wk_end)
  if (n_pub_ok > 0) {
    message("pub_date range: ",
            format(min(ai$pub_date, na.rm=TRUE)),
            " to ",
            format(max(ai$pub_date, na.rm=TRUE)))
  }
  
  ai
}

ai_static <- load_ai_data(data_file)

valid_years <- sort(unique(ai_static$year_num[!is.na(ai_static$year_num)]))
if (!length(valid_years)) valid_years <- c(2000L, as.integer(format(Sys.Date(),"%Y")))
min_year <- min(valid_years); max_year <- max(valid_years)

## ------------------------------------------------------------
## COULEURS
## ------------------------------------------------------------
rcc_colors <- c(
  # [20261010] Palette Africa CDC (vert, or, bordeaux) + 2 teintes distinctes pour lisibilite
  Northern="#1F5F6B", Western="#0B6B3A", Central="#D4A017",
  Eastern="#8B1E2D",  Southern="#C8622B", Unspecified="#B8C0BE"
)
priority_colors <- c(
  Critical="#4A0000", High="#8B0000", Moderate="#F39C12", Medium="#F39C12",
  Low="#2980B9", Unspecified="#95A5A6"
)
alert_colors <- c(
  Critical="#4A0000", High="#8B0000", Moderate="#F39C12", Medium="#F39C12",
  Low="#2980B9", Unspecified="#95A5A6"
)
africa_rel_colors <- c(
  High="#C0392B", Moderate="#F39C12", Low="#2980B9",
  None="#BDC3C7", Unspecified="#95A5A6"
)
novelty_colors <- c(
  High="#8E44AD", Moderate="#16A085", Medium="#16A085", Low="#7F8C8D", Minimal="#BDC3C7",
  Unspecified="#BDC3C7"
)

## ------------------------------------------------------------
## CARTE
## ------------------------------------------------------------
## [20261115] controle: valeurs de donnees sans couleur definie (evite les barres grises / legendes incompletes)
for (cc in list(c("priority_level","priority_colors"), c("alert_level","alert_colors"), c("africa_relevance_level","africa_rel_colors"), c("novelty_level","novelty_colors"), c("rcc","rcc_colors"))) {
  miss <- setdiff(unique(as.character(ai_static[[cc[1]]])), names(get(cc[2])))
  if (length(miss) > 0) message("WARNING colors: ", cc[1], " values without color in ", cc[2], ": ", paste(miss, collapse=", "))
}

africa_map <- tryCatch(
  if (!is.null(map_file) && !is.na(map_file) && file.exists(map_file))
    sf::st_read(map_file, quiet=TRUE) else NULL,
  error=function(e) NULL
)

country_points <- NULL
if (!is.null(centroid_file) && !is.na(centroid_file) && file.exists(centroid_file)) {
  cent <- read_csv(centroid_file, show_col_types=FALSE)
  lat_c <- names(cent)[tolower(names(cent)) %in% c("lat","latitude")][1]
  lon_c <- names(cent)[tolower(names(cent)) %in% c("lon","lng","longitude")][1]
  cty_c <- names(cent)[tolower(names(cent)) %in% c("country","country_name","name","country_std")][1]
  iso_c <- names(cent)[tolower(names(cent)) %in% c("iso3","adm0_a3","country_iso3")][1]
  if (!is.na(lat_c) && !is.na(lon_c)) {
    country_points <- cent %>%
      mutate(
        lat          = suppressWarnings(as.numeric(.data[[lat_c]])),
        lon          = suppressWarnings(as.numeric(.data[[lon_c]])),
        country_join = if(!is.na(cty_c)) clean_txt(.data[[cty_c]]) else "Unspecified",
        iso3_join    = if(!is.na(iso_c)) toupper(clean_txt(.data[[iso_c]])) else "Unspecified"
      ) %>% filter(!is.na(lat), !is.na(lon))
  }
}

## ------------------------------------------------------------
## UI
## ------------------------------------------------------------
## [LANG 20261115] Traduction de l interface EN/FR cote navigateur (dictionnaire exact + motifs). Les valeurs des filtres ne sont pas modifiees.
## [LANG 20261115] vocabulaire des valeurs des donnees (axes et legendes des graphiques)
## Arabe : les images de graphiques n'ont des lettres liees que si le paquet ragg est installe;
## sinon les libelles de graphiques restent en anglais en arabe (jamais de lettres disjointes).
PREIS_RAGG <- requireNamespace("ragg", quietly = TRUE)
if (PREIS_RAGG) options(shiny.useragg = TRUE)
PREIS_VOCAB <- list(
  fr = c("Priority level"="Niveau de priorit\u00e9", "Machine Learning"="Apprentissage automatique", "Bayesian / Statistical Modeling"="Mod\u00e9lisation bay\u00e9sienne / statistique", "Deep Learning"="Apprentissage profond", "General AI"="IA g\u00e9n\u00e9rale", "Large Language Models"="Grands mod\u00e8les de langage", "Optimization / Hybrid AI"="Optimisation / IA hybride", "Natural Language Processing"="Traitement du langage naturel", "Computer Vision"="Vision par ordinateur", "Surveillance / Event Detection"="Surveillance / d\u00e9tection d'\u00e9v\u00e9nements", "Prediction / Forecasting"="Pr\u00e9diction / pr\u00e9vision", "Policy / Decision Support"="Appui aux politiques / aux d\u00e9cisions", "Laboratory / Genomic Intelligence"="Laboratoire / renseignement g\u00e9nomique", "Diagnosis / Clinical Decision Support"="Diagnostic / aide \u00e0 la d\u00e9cision clinique", "Risk Mapping / Spatial Analysis"="Cartographie du risque / analyse spatiale", "Digital Health / Chatbot / Hotline"="Sant\u00e9 num\u00e9rique / chatbot / ligne d'assistance", "Resource Planning / Response Optimization"="Planification des ressources / optimisation de la riposte", "High"="\u00c9lev\u00e9", "HIGH"="\u00c9lev\u00e9", "Moderate"="Mod\u00e9r\u00e9", "Medium"="Moyen", "MEDIUM"="Moyen", "Low"="Faible", "LOW"="Faible", "None"="Aucun", "Minimal"="Minimal", "Critical"="Critique", "CRITICAL"="Critique", "Unspecified"="Non sp\u00e9cifi\u00e9", "Non specifi\u00e9"="Non sp\u00e9cifi\u00e9", "Non sp\u00e9cifi\u00e9"="Non sp\u00e9cifi\u00e9", "Non specifie"="Non sp\u00e9cifi\u00e9", "NA"="NA", "Predictive"="Pr\u00e9dictif", "Operational"="Op\u00e9rationnel", "One Health"="Une seule sant\u00e9", "General"="G\u00e9n\u00e9ral", "Immediate review required"="Examen imm\u00e9diat requis", "Monitor closely"="Surveiller de pr\u00e8s", "Priority review"="Examen prioritaire", "Archive for background tracking"="Archiver pour suivi de fond", "Top Priority"="Priorit\u00e9 maximale", "Watch List"="Liste de surveillance", "High Impact"="Fort impact", "Background"="Fond", "Western"="Ouest", "Eastern"="Est", "Central"="Centre", "Southern"="Sud", "Northern"="Nord", "Mpox"="Mpox", "Ebola"="Ebola", "Marburg"="Marburg", "COVID-19"="COVID-19", "HIV/AIDS"="VIH/sida", "Malaria"="Paludisme", "Influenza"="Grippe", "Yellow Fever"="Fi\u00e8vre jaune", "Dengue"="Dengue", "Cholera"="Chol\u00e9ra", "Meningitis"="M\u00e9ningite", "Tuberculosis"="Tuberculose", "Lassa fever"="Fi\u00e8vre de Lassa", "Polio"="Poliomy\u00e9lite", "Measles"="Rougeole", "Rift Valley Fever"="Fi\u00e8vre de la vall\u00e9e du Rift", "Algeria"="Alg\u00e9rie", "Angola"="Angola", "Benin"="B\u00e9nin", "Botswana"="Botswana", "Burkina Faso"="Burkina Faso", "Burundi"="Burundi", "Cabo Verde"="Cap-Vert", "Republic of Cabo Verde"="Cap-Vert", "Cameroon"="Cameroun", "Central African Republic"="R\u00e9publique centrafricaine", "Chad"="Tchad", "Comoros"="Comores", "Congo"="Congo", "Republic of the Congo"="R\u00e9publique du Congo", "Democratic Republic of the Congo"="R\u00e9publique d\u00e9mocratique du Congo", "DR Congo"="RD Congo", "Djibouti"="Djibouti", "Egypt"="\u00c9gypte", "Equatorial Guinea"="Guin\u00e9e \u00e9quatoriale", "Eritrea"="\u00c9rythr\u00e9e", "Eswatini"="Eswatini", "Kingdom of eSwatini"="Eswatini", "Ethiopia"="\u00c9thiopie", "Gabon"="Gabon", "Gambia"="Gambie", "The Gambia"="Gambie", "Ghana"="Ghana", "Guinea"="Guin\u00e9e", "Guinea-Bissau"="Guin\u00e9e-Bissau", "C\u00f4te d'Ivoire"="C\u00f4te d'Ivoire", "Ivory Coast"="C\u00f4te d'Ivoire", "Kenya"="Kenya", "Lesotho"="Lesotho", "Liberia"="Lib\u00e9ria", "Libya"="Libye", "Madagascar"="Madagascar", "Malawi"="Malawi", "Mali"="Mali", "Mauritania"="Mauritanie", "Mauritius"="Maurice", "Morocco"="Maroc", "Mozambique"="Mozambique", "Namibia"="Namibie", "Niger"="Niger", "Nigeria"="Nig\u00e9ria", "Rwanda"="Rwanda", "Sao Tome and Principe"="Sao Tom\u00e9-et-Principe", "S\u00e3o Tom\u00e9 and Principe"="Sao Tom\u00e9-et-Principe", "Senegal"="S\u00e9n\u00e9gal", "Seychelles"="Seychelles", "Sierra Leone"="Sierra Leone", "Somalia"="Somalie", "South Africa"="Afrique du Sud", "South Sudan"="Soudan du Sud", "Sudan"="Soudan", "Tanzania"="Tanzanie", "Togo"="Togo", "Tunisia"="Tunisie", "Uganda"="Ouganda", "Zambia"="Zambie", "Zimbabwe"="Zimbabwe", "Western Sahara"="Sahara occidental", "African geography detected"="G\u00e9ographie africaine d\u00e9tect\u00e9e", "No geography detected"="Aucune g\u00e9ographie d\u00e9tect\u00e9e", "No data with country information"="Aucune donn\u00e9e avec information de pays", "preprint"="Pr\u00e9publication", "PubMed"="PubMed", "Core"="C\u0153ur", "Adjacent"="Adjacent", "explicit"="explicite", "statistical_only"="statistique seulement", "mention_only"="mention seulement", "none"="aucun", "title"="titre", "single_mention"="mention unique", "text_multiple"="mentions multiples", "ambiguous"="ambigu", "dominant"="dominant", "TRUE"="VRAI", "FALSE"="FAUX", "Journal Article"="Article de revue", "Review"="Revue de la litt\u00e9rature", "Systematic Review"="Revue syst\u00e9matique", "Scoping Review"="Revue de cadrage", "Editorial"="\u00c9ditorial", "Retracted Publication"="Publication r\u00e9tract\u00e9e", "Preprint"="Pr\u00e9publication", "Research Support, Non-U.S. Gov't"="Soutien \u00e0 la recherche (hors gouvernement am\u00e9ricain)", "Research Support, N.I.H., Extramural"="Soutien \u00e0 la recherche (NIH, extramural)", "Research Support, U.S. Gov't, Non-P.H.S."="Soutien \u00e0 la recherche (gouvernement am\u00e9ricain, hors PHS)", "Antimicrobial resistance"="R\u00e9sistance aux antimicrobiens", "HIV"="VIH", "Hepatitis"="H\u00e9patite", "Diphtheria"="Dipht\u00e9rie", "Pertussis"="Coqueluche"),
  pt = c("Priority level"="N\u00edvel de prioridade", "Machine Learning"="Aprendizagem autom\u00e1tica", "Bayesian / Statistical Modeling"="Modela\u00e7\u00e3o bayesiana / estat\u00edstica", "Deep Learning"="Aprendizagem profunda", "General AI"="IA geral", "Large Language Models"="Grandes modelos de linguagem", "Optimization / Hybrid AI"="Otimiza\u00e7\u00e3o / IA h\u00edbrida", "Natural Language Processing"="Processamento de linguagem natural", "Computer Vision"="Vis\u00e3o computacional", "Surveillance / Event Detection"="Vigil\u00e2ncia / dete\u00e7\u00e3o de eventos", "Prediction / Forecasting"="Previs\u00e3o / proje\u00e7\u00e3o", "Policy / Decision Support"="Apoio a pol\u00edticas / decis\u00f5es", "Laboratory / Genomic Intelligence"="Laborat\u00f3rio / intelig\u00eancia gen\u00f3mica", "Diagnosis / Clinical Decision Support"="Diagn\u00f3stico / apoio \u00e0 decis\u00e3o cl\u00ednica", "Risk Mapping / Spatial Analysis"="Mapeamento de risco / an\u00e1lise espacial", "Digital Health / Chatbot / Hotline"="Sa\u00fade digital / chatbot / linha de apoio", "Resource Planning / Response Optimization"="Planeamento de recursos / otimiza\u00e7\u00e3o da resposta", "High"="Alto", "HIGH"="Alto", "Moderate"="Moderado", "Medium"="M\u00e9dio", "MEDIUM"="M\u00e9dio", "Low"="Baixo", "LOW"="Baixo", "None"="Nenhum", "Minimal"="M\u00ednimo", "Critical"="Cr\u00edtico", "CRITICAL"="Cr\u00edtico", "Unspecified"="N\u00e3o especificado", "Non specifi\u00e9"="N\u00e3o especificado", "Non sp\u00e9cifi\u00e9"="N\u00e3o especificado", "Non specifie"="N\u00e3o especificado", "NA"="NA", "Predictive"="Preditivo", "Operational"="Operacional", "One Health"="Sa\u00fade \u00danica", "General"="Geral", "Immediate review required"="Revis\u00e3o imediata necess\u00e1ria", "Monitor closely"="Monitorizar de perto", "Priority review"="Revis\u00e3o priorit\u00e1ria", "Archive for background tracking"="Arquivar para acompanhamento de fundo", "Top Priority"="Prioridade m\u00e1xima", "Watch List"="Lista de vigil\u00e2ncia", "High Impact"="Alto impacto", "Background"="Contexto", "Western"="Ocidental", "Eastern"="Oriental", "Central"="Central", "Southern"="Austral", "Northern"="Setentrional", "Mpox"="Mpox", "Ebola"="\u00c9bola", "Marburg"="Marburgo", "COVID-19"="COVID-19", "HIV/AIDS"="VIH/sida", "Malaria"="Mal\u00e1ria", "Influenza"="Gripe", "Yellow Fever"="Febre amarela", "Dengue"="Dengue", "Cholera"="C\u00f3lera", "Meningitis"="Meningite", "Tuberculosis"="Tuberculose", "Lassa fever"="Febre de Lassa", "Polio"="Poliomielite", "Measles"="Sarampo", "Rift Valley Fever"="Febre do Vale do Rift", "Algeria"="Arg\u00e9lia", "Angola"="Angola", "Benin"="Benim", "Botswana"="Botsuana", "Burkina Faso"="Burquina Faso", "Burundi"="Bur\u00fandi", "Cabo Verde"="Cabo Verde", "Republic of Cabo Verde"="Cabo Verde", "Cameroon"="Camar\u00f5es", "Central African Republic"="Rep\u00fablica Centro-Africana", "Chad"="Chade", "Comoros"="Comores", "Congo"="Congo", "Republic of the Congo"="Rep\u00fablica do Congo", "Democratic Republic of the Congo"="Rep\u00fablica Democr\u00e1tica do Congo", "DR Congo"="RD Congo", "Djibouti"="Jibuti", "Egypt"="Egito", "Equatorial Guinea"="Guin\u00e9 Equatorial", "Eritrea"="Eritreia", "Eswatini"="Essuat\u00edni", "Kingdom of eSwatini"="Essuat\u00edni", "Ethiopia"="Eti\u00f3pia", "Gabon"="Gab\u00e3o", "Gambia"="G\u00e2mbia", "The Gambia"="G\u00e2mbia", "Ghana"="Gana", "Guinea"="Guin\u00e9", "Guinea-Bissau"="Guin\u00e9-Bissau", "C\u00f4te d'Ivoire"="Costa do Marfim", "Ivory Coast"="Costa do Marfim", "Kenya"="Qu\u00e9nia", "Lesotho"="Lesoto", "Liberia"="Lib\u00e9ria", "Libya"="L\u00edbia", "Madagascar"="Madag\u00e1scar", "Malawi"="Mal\u00e1ui", "Mali"="Mali", "Mauritania"="Maurit\u00e2nia", "Mauritius"="Maur\u00edcia", "Morocco"="Marrocos", "Mozambique"="Mo\u00e7ambique", "Namibia"="Nam\u00edbia", "Niger"="N\u00edger", "Nigeria"="Nig\u00e9ria", "Rwanda"="Ruanda", "Sao Tome and Principe"="S\u00e3o Tom\u00e9 e Pr\u00edncipe", "S\u00e3o Tom\u00e9 and Principe"="S\u00e3o Tom\u00e9 e Pr\u00edncipe", "Senegal"="Senegal", "Seychelles"="Seicheles", "Sierra Leone"="Serra Leoa", "Somalia"="Som\u00e1lia", "South Africa"="\u00c1frica do Sul", "South Sudan"="Sud\u00e3o do Sul", "Sudan"="Sud\u00e3o", "Tanzania"="Tanz\u00e2nia", "Togo"="Togo", "Tunisia"="Tun\u00edsia", "Uganda"="Uganda", "Zambia"="Z\u00e2mbia", "Zimbabwe"="Zimbabu\u00e9", "Western Sahara"="Saara Ocidental", "African geography detected"="Geografia africana detetada", "No geography detected"="Nenhuma geografia detetada", "No data with country information"="Sem dados com informa\u00e7\u00e3o de pa\u00eds", "preprint"="Pr\u00e9-publica\u00e7\u00e3o", "PubMed"="PubMed", "Core"="N\u00facleo", "Adjacent"="Adjacente", "explicit"="expl\u00edcito", "statistical_only"="apenas estat\u00edstico", "mention_only"="apenas men\u00e7\u00e3o", "none"="nenhum", "title"="t\u00edtulo", "single_mention"="men\u00e7\u00e3o \u00fanica", "text_multiple"="men\u00e7\u00f5es m\u00faltiplas", "ambiguous"="amb\u00edguo", "dominant"="dominante", "TRUE"="VERDADEIRO", "FALSE"="FALSO", "Journal Article"="Artigo de revista", "Review"="Revis\u00e3o", "Systematic Review"="Revis\u00e3o sistem\u00e1tica", "Scoping Review"="Revis\u00e3o de \u00e2mbito", "Editorial"="Editorial", "Retracted Publication"="Publica\u00e7\u00e3o retratada", "Preprint"="Pr\u00e9-publica\u00e7\u00e3o", "Research Support, Non-U.S. Gov't"="Apoio \u00e0 investiga\u00e7\u00e3o (n\u00e3o governo dos EUA)", "Research Support, N.I.H., Extramural"="Apoio \u00e0 investiga\u00e7\u00e3o (NIH, extramural)", "Research Support, U.S. Gov't, Non-P.H.S."="Apoio \u00e0 investiga\u00e7\u00e3o (governo dos EUA, n\u00e3o PHS)", "Antimicrobial resistance"="Resist\u00eancia antimicrobiana", "HIV"="VIH", "Hepatitis"="Hepatite", "Diphtheria"="Difteria", "Pertussis"="Tosse convulsa"),
  es = c("Priority level"="Nivel de prioridad", "Machine Learning"="Aprendizaje autom\u00e1tico", "Bayesian / Statistical Modeling"="Modelizaci\u00f3n bayesiana / estad\u00edstica", "Deep Learning"="Aprendizaje profundo", "General AI"="IA general", "Large Language Models"="Grandes modelos de lenguaje", "Optimization / Hybrid AI"="Optimizaci\u00f3n / IA h\u00edbrida", "Natural Language Processing"="Procesamiento del lenguaje natural", "Computer Vision"="Visi\u00f3n por computadora", "Surveillance / Event Detection"="Vigilancia / detecci\u00f3n de eventos", "Prediction / Forecasting"="Predicci\u00f3n / pron\u00f3stico", "Policy / Decision Support"="Apoyo a pol\u00edticas / decisiones", "Laboratory / Genomic Intelligence"="Laboratorio / inteligencia gen\u00f3mica", "Diagnosis / Clinical Decision Support"="Diagn\u00f3stico / apoyo a la decisi\u00f3n cl\u00ednica", "Risk Mapping / Spatial Analysis"="Mapeo de riesgos / an\u00e1lisis espacial", "Digital Health / Chatbot / Hotline"="Salud digital / chatbot / l\u00ednea de ayuda", "Resource Planning / Response Optimization"="Planificaci\u00f3n de recursos / optimizaci\u00f3n de la respuesta", "High"="Alto", "HIGH"="Alto", "Moderate"="Moderado", "Medium"="Medio", "MEDIUM"="Medio", "Low"="Bajo", "LOW"="Bajo", "None"="Ninguno", "Minimal"="M\u00ednimo", "Critical"="Cr\u00edtico", "CRITICAL"="Cr\u00edtico", "Unspecified"="No especificado", "Non specifi\u00e9"="No especificado", "Non sp\u00e9cifi\u00e9"="No especificado", "Non specifie"="No especificado", "NA"="NA", "Predictive"="Predictivo", "Operational"="Operativo", "One Health"="Una sola salud", "General"="General", "Immediate review required"="Revisi\u00f3n inmediata requerida", "Monitor closely"="Vigilar de cerca", "Priority review"="Revisi\u00f3n prioritaria", "Archive for background tracking"="Archivar para seguimiento de fondo", "Top Priority"="Prioridad m\u00e1xima", "Watch List"="Lista de vigilancia", "High Impact"="Alto impacto", "Background"="Contexto", "Western"="Occidental", "Eastern"="Oriental", "Central"="Central", "Southern"="Austral", "Northern"="Septentrional", "Mpox"="Mpox", "Ebola"="\u00c9bola", "Marburg"="Marburgo", "COVID-19"="COVID-19", "HIV/AIDS"="VIH/sida", "Malaria"="Paludismo", "Influenza"="Gripe", "Yellow Fever"="Fiebre amarilla", "Dengue"="Dengue", "Cholera"="C\u00f3lera", "Meningitis"="Meningitis", "Tuberculosis"="Tuberculosis", "Lassa fever"="Fiebre de Lassa", "Polio"="Poliomielitis", "Measles"="Sarampi\u00f3n", "Rift Valley Fever"="Fiebre del Valle del Rift", "Algeria"="Argelia", "Angola"="Angola", "Benin"="Ben\u00edn", "Botswana"="Botsuana", "Burkina Faso"="Burkina Faso", "Burundi"="Burundi", "Cabo Verde"="Cabo Verde", "Republic of Cabo Verde"="Cabo Verde", "Cameroon"="Camer\u00fan", "Central African Republic"="Rep\u00fablica Centroafricana", "Chad"="Chad", "Comoros"="Comoras", "Congo"="Congo", "Republic of the Congo"="Rep\u00fablica del Congo", "Democratic Republic of the Congo"="Rep\u00fablica Democr\u00e1tica del Congo", "DR Congo"="RD Congo", "Djibouti"="Yibuti", "Egypt"="Egipto", "Equatorial Guinea"="Guinea Ecuatorial", "Eritrea"="Eritrea", "Eswatini"="Esuatini", "Kingdom of eSwatini"="Esuatini", "Ethiopia"="Etiop\u00eda", "Gabon"="Gab\u00f3n", "Gambia"="Gambia", "The Gambia"="Gambia", "Ghana"="Ghana", "Guinea"="Guinea", "Guinea-Bissau"="Guinea-Bis\u00e1u", "C\u00f4te d'Ivoire"="C\u00f4te d'Ivoire", "Ivory Coast"="C\u00f4te d'Ivoire", "Kenya"="Kenia", "Lesotho"="Lesoto", "Liberia"="Liberia", "Libya"="Libia", "Madagascar"="Madagascar", "Malawi"="Malaui", "Mali"="Mal\u00ed", "Mauritania"="Mauritania", "Mauritius"="Mauricio", "Morocco"="Marruecos", "Mozambique"="Mozambique", "Namibia"="Namibia", "Niger"="N\u00edger", "Nigeria"="Nigeria", "Rwanda"="Ruanda", "Sao Tome and Principe"="Santo Tom\u00e9 y Pr\u00edncipe", "S\u00e3o Tom\u00e9 and Principe"="Santo Tom\u00e9 y Pr\u00edncipe", "Senegal"="Senegal", "Seychelles"="Seychelles", "Sierra Leone"="Sierra Leona", "Somalia"="Somalia", "South Africa"="Sud\u00e1frica", "South Sudan"="Sud\u00e1n del Sur", "Sudan"="Sud\u00e1n", "Tanzania"="Tanzania", "Togo"="Togo", "Tunisia"="T\u00fanez", "Uganda"="Uganda", "Zambia"="Zambia", "Zimbabwe"="Zimbabue", "Western Sahara"="S\u00e1hara Occidental", "African geography detected"="Geograf\u00eda africana detectada", "No geography detected"="Ninguna geograf\u00eda detectada", "No data with country information"="Sin datos con informaci\u00f3n de pa\u00eds", "preprint"="Preimpresi\u00f3n", "PubMed"="PubMed", "Core"="N\u00facleo", "Adjacent"="Adyacente", "explicit"="expl\u00edcito", "statistical_only"="solo estad\u00edstico", "mention_only"="solo menci\u00f3n", "none"="ninguno", "title"="t\u00edtulo", "single_mention"="menci\u00f3n \u00fanica", "text_multiple"="menciones m\u00faltiples", "ambiguous"="ambiguo", "dominant"="dominante", "TRUE"="VERDADERO", "FALSE"="FALSO", "Journal Article"="Art\u00edculo de revista", "Review"="Revisi\u00f3n", "Systematic Review"="Revisi\u00f3n sistem\u00e1tica", "Scoping Review"="Revisi\u00f3n de alcance", "Editorial"="Editorial", "Retracted Publication"="Publicaci\u00f3n retractada", "Preprint"="Preimpresi\u00f3n", "Research Support, Non-U.S. Gov't"="Apoyo a la investigaci\u00f3n (no gobierno de EE. UU.)", "Research Support, N.I.H., Extramural"="Apoyo a la investigaci\u00f3n (NIH, extramural)", "Research Support, U.S. Gov't, Non-P.H.S."="Apoyo a la investigaci\u00f3n (gobierno de EE. UU., no PHS)", "Antimicrobial resistance"="Resistencia a los antimicrobianos", "HIV"="VIH", "Hepatitis"="Hepatitis", "Diphtheria"="Difteria", "Pertussis"="Tos ferina"),
  ar = c("Priority level"="\u0645\u0633\u062a\u0648\u0649 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "Machine Learning"="\u0627\u0644\u062a\u0639\u0644\u0645 \u0627\u0644\u0622\u0644\u064a", "Bayesian / Statistical Modeling"="\u0627\u0644\u0646\u0645\u0630\u062c\u0629 \u0627\u0644\u0628\u0627\u064a\u0632\u064a\u0629 / \u0627\u0644\u0625\u062d\u0635\u0627\u0626\u064a\u0629", "Deep Learning"="\u0627\u0644\u062a\u0639\u0644\u0645 \u0627\u0644\u0639\u0645\u064a\u0642", "General AI"="\u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0639\u0627\u0645", "Large Language Models"="\u0646\u0645\u0627\u0630\u062c \u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0643\u0628\u064a\u0631\u0629", "Optimization / Hybrid AI"="\u0627\u0644\u062a\u062d\u0633\u064a\u0646 / \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0647\u062c\u064a\u0646", "Natural Language Processing"="\u0645\u0639\u0627\u0644\u062c\u0629 \u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0637\u0628\u064a\u0639\u064a\u0629", "Computer Vision"="\u0627\u0644\u0631\u0624\u064a\u0629 \u0627\u0644\u062d\u0627\u0633\u0648\u0628\u064a\u0629", "Surveillance / Event Detection"="\u0627\u0644\u062a\u0631\u0635\u062f / \u0643\u0634\u0641 \u0627\u0644\u0623\u062d\u062f\u0627\u062b", "Prediction / Forecasting"="\u0627\u0644\u062a\u0646\u0628\u0624 / \u0627\u0644\u0627\u0633\u062a\u0634\u0631\u0627\u0641", "Policy / Decision Support"="\u062f\u0639\u0645 \u0627\u0644\u0633\u064a\u0627\u0633\u0627\u062a / \u0627\u0644\u0642\u0631\u0627\u0631\u0627\u062a", "Laboratory / Genomic Intelligence"="\u0627\u0644\u0645\u062e\u062a\u0628\u0631 / \u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0627\u0644\u062c\u064a\u0646\u0648\u0645\u064a\u0629", "Diagnosis / Clinical Decision Support"="\u0627\u0644\u062a\u0634\u062e\u064a\u0635 / \u062f\u0639\u0645 \u0627\u0644\u0642\u0631\u0627\u0631 \u0627\u0644\u0633\u0631\u064a\u0631\u064a", "Risk Mapping / Spatial Analysis"="\u0631\u0633\u0645 \u062e\u0631\u0627\u0626\u0637 \u0627\u0644\u0645\u062e\u0627\u0637\u0631 / \u0627\u0644\u062a\u062d\u0644\u064a\u0644 \u0627\u0644\u0645\u0643\u0627\u0646\u064a", "Digital Health / Chatbot / Hotline"="\u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0631\u0642\u0645\u064a\u0629 / \u0631\u0648\u0628\u0648\u062a \u0627\u0644\u0645\u062d\u0627\u062f\u062b\u0629 / \u0627\u0644\u062e\u0637 \u0627\u0644\u0633\u0627\u062e\u0646", "Resource Planning / Response Optimization"="\u062a\u062e\u0637\u064a\u0637 \u0627\u0644\u0645\u0648\u0627\u0631\u062f / \u062a\u062d\u0633\u064a\u0646 \u0627\u0644\u0627\u0633\u062a\u062c\u0627\u0628\u0629", "High"="\u0645\u0631\u062a\u0641\u0639", "HIGH"="\u0645\u0631\u062a\u0641\u0639", "Moderate"="\u0645\u0639\u062a\u062f\u0644", "Medium"="\u0645\u062a\u0648\u0633\u0637", "MEDIUM"="\u0645\u062a\u0648\u0633\u0637", "Low"="\u0645\u0646\u062e\u0641\u0636", "LOW"="\u0645\u0646\u062e\u0641\u0636", "None"="\u0644\u0627 \u0634\u064a\u0621", "Minimal"="\u0636\u0626\u064a\u0644", "Critical"="\u062d\u0631\u062c", "CRITICAL"="\u062d\u0631\u062c", "Unspecified"="\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "Non specifi\u00e9"="\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "Non sp\u00e9cifi\u00e9"="\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "Non specifie"="\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "NA"="NA", "Predictive"="\u062a\u0646\u0628\u0624\u064a", "Operational"="\u062a\u0634\u063a\u064a\u0644\u064a", "One Health"="\u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0648\u0627\u062d\u062f\u0629", "General"="\u0639\u0627\u0645", "Immediate review required"="\u0645\u0637\u0644\u0648\u0628 \u0645\u0631\u0627\u062c\u0639\u0629 \u0641\u0648\u0631\u064a\u0629", "Monitor closely"="\u0627\u0644\u0645\u062a\u0627\u0628\u0639\u0629 \u0639\u0646 \u0643\u062b\u0628", "Priority review"="\u0645\u0631\u0627\u062c\u0639\u0629 \u0630\u0627\u062a \u0623\u0648\u0644\u0648\u064a\u0629", "Archive for background tracking"="\u0623\u0631\u0634\u0641\u0629 \u0644\u0644\u0645\u062a\u0627\u0628\u0639\u0629 \u0627\u0644\u062e\u0644\u0641\u064a\u0629", "Top Priority"="\u0623\u0648\u0644\u0648\u064a\u0629 \u0642\u0635\u0648\u0649", "Watch List"="\u0642\u0627\u0626\u0645\u0629 \u0627\u0644\u0645\u0631\u0627\u0642\u0628\u0629", "High Impact"="\u062a\u0623\u062b\u064a\u0631 \u0643\u0628\u064a\u0631", "Background"="\u062e\u0644\u0641\u064a\u0629", "Western"="\u0627\u0644\u063a\u0631\u0628\u064a", "Eastern"="\u0627\u0644\u0634\u0631\u0642\u064a", "Central"="\u0627\u0644\u0623\u0648\u0633\u0637", "Southern"="\u0627\u0644\u062c\u0646\u0648\u0628\u064a", "Northern"="\u0627\u0644\u0634\u0645\u0627\u0644\u064a", "Mpox"="\u062c\u062f\u0631\u064a \u0627\u0644\u0642\u0631\u062f\u0629 (\u0625\u0645\u0628\u0648\u0643\u0633)", "Ebola"="\u0625\u064a\u0628\u0648\u0644\u0627", "Marburg"="\u0645\u0627\u0631\u0628\u0648\u0631\u063a", "COVID-19"="\u0643\u0648\u0641\u064a\u062f-19", "HIV/AIDS"="\u0641\u064a\u0631\u0648\u0633 \u0627\u0644\u0639\u0648\u0632 \u0627\u0644\u0645\u0646\u0627\u0639\u064a \u0627\u0644\u0628\u0634\u0631\u064a/\u0627\u0644\u0625\u064a\u062f\u0632", "Malaria"="\u0627\u0644\u0645\u0644\u0627\u0631\u064a\u0627", "Influenza"="\u0627\u0644\u0625\u0646\u0641\u0644\u0648\u0646\u0632\u0627", "Yellow Fever"="\u0627\u0644\u062d\u0645\u0649 \u0627\u0644\u0635\u0641\u0631\u0627\u0621", "Dengue"="\u062d\u0645\u0649 \u0627\u0644\u0636\u0646\u0643", "Cholera"="\u0627\u0644\u0643\u0648\u0644\u064a\u0631\u0627", "Meningitis"="\u0627\u0644\u062a\u0647\u0627\u0628 \u0627\u0644\u0633\u062d\u0627\u064a\u0627", "Tuberculosis"="\u0627\u0644\u0633\u0644", "Lassa fever"="\u062d\u0645\u0649 \u0644\u0627\u0633\u0627", "Polio"="\u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644", "Measles"="\u0627\u0644\u062d\u0635\u0628\u0629", "Rift Valley Fever"="\u062d\u0645\u0649 \u0627\u0644\u0648\u0627\u062f\u064a \u0627\u0644\u0645\u062a\u0635\u062f\u0639", "Algeria"="\u0627\u0644\u062c\u0632\u0627\u0626\u0631", "Angola"="\u0623\u0646\u063a\u0648\u0644\u0627", "Benin"="\u0628\u0646\u064a\u0646", "Botswana"="\u0628\u0648\u062a\u0633\u0648\u0627\u0646\u0627", "Burkina Faso"="\u0628\u0648\u0631\u0643\u064a\u0646\u0627 \u0641\u0627\u0633\u0648", "Burundi"="\u0628\u0648\u0631\u0648\u0646\u062f\u064a", "Cabo Verde"="\u0627\u0644\u0631\u0623\u0633 \u0627\u0644\u0623\u062e\u0636\u0631", "Republic of Cabo Verde"="\u0627\u0644\u0631\u0623\u0633 \u0627\u0644\u0623\u062e\u0636\u0631", "Cameroon"="\u0627\u0644\u0643\u0627\u0645\u064a\u0631\u0648\u0646", "Central African Republic"="\u062c\u0645\u0647\u0648\u0631\u064a\u0629 \u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u0627\u0644\u0648\u0633\u0637\u0649", "Chad"="\u062a\u0634\u0627\u062f", "Comoros"="\u062c\u0632\u0631 \u0627\u0644\u0642\u0645\u0631", "Congo"="\u0627\u0644\u0643\u0648\u0646\u063a\u0648", "Republic of the Congo"="\u062c\u0645\u0647\u0648\u0631\u064a\u0629 \u0627\u0644\u0643\u0648\u0646\u063a\u0648", "Democratic Republic of the Congo"="\u062c\u0645\u0647\u0648\u0631\u064a\u0629 \u0627\u0644\u0643\u0648\u0646\u063a\u0648 \u0627\u0644\u062f\u064a\u0645\u0642\u0631\u0627\u0637\u064a\u0629", "DR Congo"="\u0627\u0644\u0643\u0648\u0646\u063a\u0648 \u0627\u0644\u062f\u064a\u0645\u0642\u0631\u0627\u0637\u064a\u0629", "Djibouti"="\u062c\u064a\u0628\u0648\u062a\u064a", "Egypt"="\u0645\u0635\u0631", "Equatorial Guinea"="\u063a\u064a\u0646\u064a\u0627 \u0627\u0644\u0627\u0633\u062a\u0648\u0627\u0626\u064a\u0629", "Eritrea"="\u0625\u0631\u064a\u062a\u0631\u064a\u0627", "Eswatini"="\u0625\u0633\u0648\u0627\u062a\u064a\u0646\u064a", "Kingdom of eSwatini"="\u0625\u0633\u0648\u0627\u062a\u064a\u0646\u064a", "Ethiopia"="\u0625\u062b\u064a\u0648\u0628\u064a\u0627", "Gabon"="\u0627\u0644\u063a\u0627\u0628\u0648\u0646", "Gambia"="\u063a\u0627\u0645\u0628\u064a\u0627", "The Gambia"="\u063a\u0627\u0645\u0628\u064a\u0627", "Ghana"="\u063a\u0627\u0646\u0627", "Guinea"="\u063a\u064a\u0646\u064a\u0627", "Guinea-Bissau"="\u063a\u064a\u0646\u064a\u0627 \u0628\u064a\u0633\u0627\u0648", "C\u00f4te d'Ivoire"="\u0643\u0648\u062a \u062f\u064a\u0641\u0648\u0627\u0631", "Ivory Coast"="\u0643\u0648\u062a \u062f\u064a\u0641\u0648\u0627\u0631", "Kenya"="\u0643\u064a\u0646\u064a\u0627", "Lesotho"="\u0644\u064a\u0633\u0648\u062a\u0648", "Liberia"="\u0644\u064a\u0628\u064a\u0631\u064a\u0627", "Libya"="\u0644\u064a\u0628\u064a\u0627", "Madagascar"="\u0645\u062f\u063a\u0634\u0642\u0631", "Malawi"="\u0645\u0644\u0627\u0648\u064a", "Mali"="\u0645\u0627\u0644\u064a", "Mauritania"="\u0645\u0648\u0631\u064a\u062a\u0627\u0646\u064a\u0627", "Mauritius"="\u0645\u0648\u0631\u064a\u0634\u064a\u0648\u0633", "Morocco"="\u0627\u0644\u0645\u063a\u0631\u0628", "Mozambique"="\u0645\u0648\u0632\u0645\u0628\u064a\u0642", "Namibia"="\u0646\u0627\u0645\u064a\u0628\u064a\u0627", "Niger"="\u0627\u0644\u0646\u064a\u062c\u0631", "Nigeria"="\u0646\u064a\u062c\u064a\u0631\u064a\u0627", "Rwanda"="\u0631\u0648\u0627\u0646\u062f\u0627", "Sao Tome and Principe"="\u0633\u0627\u0646 \u062a\u0648\u0645\u064a \u0648\u0628\u0631\u064a\u0646\u0633\u064a\u0628\u064a", "S\u00e3o Tom\u00e9 and Principe"="\u0633\u0627\u0646 \u062a\u0648\u0645\u064a \u0648\u0628\u0631\u064a\u0646\u0633\u064a\u0628\u064a", "Senegal"="\u0627\u0644\u0633\u0646\u063a\u0627\u0644", "Seychelles"="\u0633\u064a\u0634\u064a\u0644", "Sierra Leone"="\u0633\u064a\u0631\u0627\u0644\u064a\u0648\u0646", "Somalia"="\u0627\u0644\u0635\u0648\u0645\u0627\u0644", "South Africa"="\u062c\u0646\u0648\u0628 \u0623\u0641\u0631\u064a\u0642\u064a\u0627", "South Sudan"="\u062c\u0646\u0648\u0628 \u0627\u0644\u0633\u0648\u062f\u0627\u0646", "Sudan"="\u0627\u0644\u0633\u0648\u062f\u0627\u0646", "Tanzania"="\u062a\u0646\u0632\u0627\u0646\u064a\u0627", "Togo"="\u062a\u0648\u063a\u0648", "Tunisia"="\u062a\u0648\u0646\u0633", "Uganda"="\u0623\u0648\u063a\u0646\u062f\u0627", "Zambia"="\u0632\u0627\u0645\u0628\u064a\u0627", "Zimbabwe"="\u0632\u064a\u0645\u0628\u0627\u0628\u0648\u064a", "Western Sahara"="\u0627\u0644\u0635\u062d\u0631\u0627\u0621 \u0627\u0644\u063a\u0631\u0628\u064a\u0629", "African geography detected"="\u062a\u0645 \u0631\u0635\u062f \u062c\u063a\u0631\u0627\u0641\u064a\u0627 \u0623\u0641\u0631\u064a\u0642\u064a\u0629", "No geography detected"="\u0644\u0645 \u064a\u062a\u0645 \u0631\u0635\u062f \u062c\u063a\u0631\u0627\u0641\u064a\u0627", "No data with country information"="\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u062a\u062a\u0636\u0645\u0646 \u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0639\u0646 \u0627\u0644\u0628\u0644\u062f", "preprint"="\u0646\u0633\u062e\u0629 \u0623\u0648\u0644\u064a\u0629 (\u0645\u0627 \u0642\u0628\u0644 \u0627\u0644\u0646\u0634\u0631)", "PubMed"="PubMed", "Core"="\u0623\u0633\u0627\u0633\u064a", "Adjacent"="\u0630\u0648 \u0635\u0644\u0629", "explicit"="\u0635\u0631\u064a\u062d", "statistical_only"="\u0625\u062d\u0635\u0627\u0626\u064a \u0641\u0642\u0637", "mention_only"="\u0630\u0643\u0631 \u0641\u0642\u0637", "none"="\u0644\u0627 \u0634\u064a\u0621", "title"="\u0627\u0644\u0639\u0646\u0648\u0627\u0646", "single_mention"="\u0630\u0643\u0631 \u0648\u0627\u062d\u062f", "text_multiple"="\u0630\u0643\u0631 \u0645\u062a\u0639\u062f\u062f", "ambiguous"="\u063a\u0627\u0645\u0636", "dominant"="\u063a\u0627\u0644\u0628", "TRUE"="\u0635\u062d\u064a\u062d", "FALSE"="\u062e\u0637\u0623", "Journal Article"="\u0645\u0642\u0627\u0644 \u0641\u064a \u0645\u062c\u0644\u0629", "Review"="\u0645\u0631\u0627\u062c\u0639\u0629", "Systematic Review"="\u0645\u0631\u0627\u062c\u0639\u0629 \u0645\u0646\u0647\u062c\u064a\u0629", "Scoping Review"="\u0645\u0631\u0627\u062c\u0639\u0629 \u0627\u0633\u062a\u0643\u0634\u0627\u0641\u064a\u0629", "Editorial"="\u0627\u0641\u062a\u062a\u0627\u062d\u064a\u0629", "Retracted Publication"="\u0645\u0646\u0634\u0648\u0631 \u0645\u0633\u062d\u0648\u0628", "Preprint"="\u0646\u0633\u062e\u0629 \u0623\u0648\u0644\u064a\u0629 (\u0645\u0627 \u0642\u0628\u0644 \u0627\u0644\u0646\u0634\u0631)", "Research Support, Non-U.S. Gov't"="\u062f\u0639\u0645 \u0628\u062d\u062b\u064a (\u063a\u064a\u0631 \u062d\u0643\u0648\u0645\u064a \u0623\u0645\u0631\u064a\u0643\u064a)", "Research Support, N.I.H., Extramural"="\u062f\u0639\u0645 \u0628\u062d\u062b\u064a (\u0627\u0644\u0645\u0639\u0627\u0647\u062f \u0627\u0644\u0648\u0637\u0646\u064a\u0629 \u0644\u0644\u0635\u062d\u0629\u060c \u062e\u0627\u0631\u062c\u064a)", "Research Support, U.S. Gov't, Non-P.H.S."="\u062f\u0639\u0645 \u0628\u062d\u062b\u064a (\u062d\u0643\u0648\u0645\u0629 \u0627\u0644\u0648\u0644\u0627\u064a\u0627\u062a \u0627\u0644\u0645\u062a\u062d\u062f\u0629\u060c \u063a\u064a\u0631 PHS)", "Antimicrobial resistance"="\u0645\u0642\u0627\u0648\u0645\u0629 \u0645\u0636\u0627\u062f\u0627\u062a \u0627\u0644\u0645\u064a\u0643\u0631\u0648\u0628\u0627\u062a", "HIV"="\u0641\u064a\u0631\u0648\u0633 \u0646\u0642\u0635 \u0627\u0644\u0645\u0646\u0627\u0639\u0629 \u0627\u0644\u0628\u0634\u0631\u064a\u0629", "Hepatitis"="\u0627\u0644\u062a\u0647\u0627\u0628 \u0627\u0644\u0643\u0628\u062f", "Diphtheria"="\u0627\u0644\u062e\u0646\u0627\u0642 (\u0627\u0644\u062f\u0641\u062a\u064a\u0631\u064a\u0627)", "Pertussis"="\u0627\u0644\u0633\u0639\u0627\u0644 \u0627\u0644\u062f\u064a\u0643\u064a"),
  sw = c("Priority level"="Kiwango cha kipaumbele", "Machine Learning"="Ujifunzaji wa mashine", "Bayesian / Statistical Modeling"="Uundaji wa Bayesian / takwimu", "Deep Learning"="Ujifunzaji wa kina", "General AI"="AI ya jumla", "Large Language Models"="Mifano mikubwa ya lugha", "Optimization / Hybrid AI"="Uboreshaji / AI mseto", "Natural Language Processing"="Uchakataji wa lugha asilia", "Computer Vision"="Maono ya kompyuta", "Surveillance / Event Detection"="Uchunguzi / ugunduzi wa matukio", "Prediction / Forecasting"="Utabiri / makadirio", "Policy / Decision Support"="Msaada wa sera / maamuzi", "Laboratory / Genomic Intelligence"="Maabara / taarifa za jenomu", "Diagnosis / Clinical Decision Support"="Utambuzi / msaada wa maamuzi ya kitabibu", "Risk Mapping / Spatial Analysis"="Ramani ya hatari / uchambuzi wa anga", "Digital Health / Chatbot / Hotline"="Afya ya kidijitali / chatbot / simu ya msaada", "Resource Planning / Response Optimization"="Mipango ya rasilimali / uboreshaji wa mwitikio", "High"="Juu", "HIGH"="Juu", "Moderate"="Wastani", "Medium"="Kati", "MEDIUM"="Kati", "Low"="Chini", "LOW"="Chini", "None"="Hakuna", "Minimal"="Ndogo sana", "Critical"="Muhimu sana", "CRITICAL"="Muhimu sana", "Unspecified"="Haijabainishwa", "Non specifi\u00e9"="Haijabainishwa", "Non sp\u00e9cifi\u00e9"="Haijabainishwa", "Non specifie"="Haijabainishwa", "NA"="NA", "Predictive"="Utabiri", "Operational"="Kiutendaji", "One Health"="Afya Moja", "General"="Jumla", "Immediate review required"="Mapitio ya haraka yanahitajika", "Monitor closely"="Fuatilia kwa karibu", "Priority review"="Mapitio ya kipaumbele", "Archive for background tracking"="Hifadhi kwa ufuatiliaji wa msingi", "Top Priority"="Kipaumbele cha juu kabisa", "Watch List"="Orodha ya uangalizi", "High Impact"="Athari kubwa", "Background"="Usuli", "Western"="Magharibi", "Eastern"="Mashariki", "Central"="Kati", "Southern"="Kusini", "Northern"="Kaskazini", "Mpox"="Mpox", "Ebola"="Ebola", "Marburg"="Marburg", "COVID-19"="UVIKO-19", "HIV/AIDS"="VVU/UKIMWI", "Malaria"="Malaria", "Influenza"="Mafua makali", "Yellow Fever"="Homa ya manjano", "Dengue"="Dengi", "Cholera"="Kipindupindu", "Meningitis"="Homa ya uti wa mgongo", "Tuberculosis"="Kifua kikuu", "Lassa fever"="Homa ya Lassa", "Polio"="Polio", "Measles"="Surua", "Rift Valley Fever"="Homa ya Bonde la Ufa", "Algeria"="Algeria", "Angola"="Angola", "Benin"="Benin", "Botswana"="Botswana", "Burkina Faso"="Burkina Faso", "Burundi"="Burundi", "Cabo Verde"="Cabo Verde", "Republic of Cabo Verde"="Cabo Verde", "Cameroon"="Kamerun", "Central African Republic"="Jamhuri ya Afrika ya Kati", "Chad"="Chadi", "Comoros"="Komoro", "Congo"="Kongo", "Republic of the Congo"="Jamhuri ya Kongo", "Democratic Republic of the Congo"="Jamhuri ya Kidemokrasia ya Kongo", "DR Congo"="DR Kongo", "Djibouti"="Jibuti", "Egypt"="Misri", "Equatorial Guinea"="Guinea ya Ikweta", "Eritrea"="Eritrea", "Eswatini"="Eswatini", "Kingdom of eSwatini"="Eswatini", "Ethiopia"="Ethiopia", "Gabon"="Gabon", "Gambia"="Gambia", "The Gambia"="Gambia", "Ghana"="Ghana", "Guinea"="Guinea", "Guinea-Bissau"="Guinea-Bissau", "C\u00f4te d'Ivoire"="C\u00f4te d'Ivoire", "Ivory Coast"="C\u00f4te d'Ivoire", "Kenya"="Kenya", "Lesotho"="Lesotho", "Liberia"="Liberia", "Libya"="Libya", "Madagascar"="Madagaska", "Malawi"="Malawi", "Mali"="Mali", "Mauritania"="Mauritania", "Mauritius"="Mauritius", "Morocco"="Moroko", "Mozambique"="Msumbiji", "Namibia"="Namibia", "Niger"="Niger", "Nigeria"="Nigeria", "Rwanda"="Rwanda", "Sao Tome and Principe"="Sao Tome na Principe", "S\u00e3o Tom\u00e9 and Principe"="Sao Tome na Principe", "Senegal"="Senegali", "Seychelles"="Shelisheli", "Sierra Leone"="Sierra Leone", "Somalia"="Somalia", "South Africa"="Afrika Kusini", "South Sudan"="Sudan Kusini", "Sudan"="Sudan", "Tanzania"="Tanzania", "Togo"="Togo", "Tunisia"="Tunisia", "Uganda"="Uganda", "Zambia"="Zambia", "Zimbabwe"="Zimbabwe", "Western Sahara"="Sahara Magharibi", "African geography detected"="Jiografia ya Afrika imebainika", "No geography detected"="Hakuna jiografia iliyobainika", "No data with country information"="Hakuna data yenye taarifa za nchi", "preprint"="Chapisho la awali", "PubMed"="PubMed", "Core"="Kiini", "Adjacent"="Jirani", "explicit"="wazi", "statistical_only"="takwimu pekee", "mention_only"="kutajwa tu", "none"="hakuna", "title"="kichwa", "single_mention"="kutajwa mara moja", "text_multiple"="kutajwa mara nyingi", "ambiguous"="haieleweki", "dominant"="kuu", "TRUE"="NDIYO", "FALSE"="HAPANA", "Journal Article"="Makala ya jarida", "Review"="Mapitio", "Systematic Review"="Mapitio ya kimfumo", "Scoping Review"="Mapitio ya wigo", "Editorial"="Tahariri", "Retracted Publication"="Chapisho lililoondolewa", "Preprint"="Chapisho la awali", "Research Support, Non-U.S. Gov't"="Ufadhili wa utafiti (si serikali ya Marekani)", "Research Support, N.I.H., Extramural"="Ufadhili wa utafiti (NIH, nje)", "Research Support, U.S. Gov't, Non-P.H.S."="Ufadhili wa utafiti (serikali ya Marekani, si PHS)", "Antimicrobial resistance"="Usugu wa dawa za viua vijidudu", "HIV"="VVU", "Hepatitis"="Homa ya ini", "Diphtheria"="Dondakoo", "Pertussis"="Kifaduro")
)
preis_tv <- function(x, lang) {
  if (is.null(lang) || !(lang %in% names(PREIS_VOCAB))) return(x)
  if (identical(lang, "ar") && !PREIS_RAGG) return(x)
  v <- PREIS_VOCAB[[lang]]
  one <- function(s) {
    if (is.na(s)) return(s)
    if (s %in% names(v)) return(unname(v[[s]]))
    for (p in c(" \u2014 ", "; ")) {
      if (grepl(p, s, fixed = TRUE)) {
        parts <- strsplit(s, p, fixed = TRUE)[[1]]
        parts <- vapply(parts, function(q) if (q %in% names(v)) unname(v[[q]]) else q, character(1), USE.NAMES = FALSE)
        return(paste(parts, collapse = p))
      }
    }
    s
  }
  vapply(as.character(x), one, character(1), USE.NAMES = FALSE)
}

PREIS_I18N_JS <- r"---((function(){
  var L = {"fr": {"Overview": "Vue d'ensemble", "AI x Disease x Country": "IA x Maladie x Pays", "Methods & Applications": "M\u00e9thodes et applications", "Focus Africa": "Focus Afrique", "Priority Intelligence": "Renseignement prioritaire", "All Records": "Tous les enregistrements", "Downloads": "T\u00e9l\u00e9chargements", "PREIS AI Atlas": "Atlas PREIS IA", "Methods and limits": "M\u00e9thodes et limites", "Filters": "Filtres", "Year": "Ann\u00e9e", "Alert level": "Niveau d'alerte", "Disease": "Maladie", "AI method": "M\u00e9thode d'IA", "Application": "Application", "RCC": "CCR", "Country": "Pays", "Signal profile": "Profil de signal", "Language of publication": "Langue de la publication", "All": "Tous", "English": "Anglais", "French": "Fran\u00e7ais", "Spanish": "Espagnol", "Portuguese": "Portugais", "German": "Allemand", "Italian": "Italien", "Other": "Autre", "High": "\u00c9lev\u00e9", "Moderate": "Mod\u00e9r\u00e9", "Low": "Faible", "Unspecified": "Non sp\u00e9cifi\u00e9", "Critical": "Critique", "PREIS AI Tracker \u2014 Dashboard": "PREIS AI Tracker \u2014 Tableau de bord", "Monitoring AI applications for public health surveillance in Africa |": "Suivi des applications de l'IA pour la surveillance de sant\u00e9 publique en Afrique |", "Source:": "Source :", "Data generated:": "Donn\u00e9es g\u00e9n\u00e9r\u00e9es :", "unknown": "inconnu", "Automated classification, not yet validated by human review.": "Classification automatique, non encore valid\u00e9e par une relecture humaine.", "Disease, method, application and Africa-relevance labels and scores are produced by keyword rules and may contain errors.": "Les \u00e9tiquettes et scores de maladie, de m\u00e9thode, d'application et de pertinence pour l'Afrique sont produits par des r\u00e8gles de mots-cl\u00e9s et peuvent contenir des erreurs.", "Records outside the infectious-disease/epidemic scope are excluded from this view.": "Les enregistrements hors du p\u00e9rim\u00e8tre des maladies infectieuses et \u00e9pid\u00e9mies sont exclus de cette vue.", "Do not use as a basis for operational decisions without expert verification.": "Ne pas utiliser comme base de d\u00e9cisions op\u00e9rationnelles sans v\u00e9rification par un expert.", "AI records": "Enregistrements IA", "High-priority records": "Enregistrements \u00e0 priorit\u00e9 \u00e9lev\u00e9e", "Diseases under surveillance": "Maladies sous surveillance", "Priority distribution": "R\u00e9partition par priorit\u00e9", "Alert level distribution": "R\u00e9partition par niveau d'alerte", "AI methods": "M\u00e9thodes d'IA", "AI applications": "Applications de l'IA", "Africa relevance": "Pertinence pour l'Afrique", "Novelty": "Nouveaut\u00e9", "Diseases under AI surveillance \u2014 top priority": "Maladies sous surveillance par IA \u2014 priorit\u00e9 maximale", "AI method x Disease matrix": "Matrice m\u00e9thode d'IA x maladie", "Application x Disease matrix": "Matrice application x maladie", "Top countries per disease (Africa-linked)": "Principaux pays par maladie (li\u00e9s \u00e0 l'Afrique)", "Disease trend over time": "\u00c9volution des maladies dans le temps", "Disease intelligence records": "Enregistrements de renseignement par maladie", "Methods by priority": "M\u00e9thodes par priorit\u00e9", "Applications by priority": "Applications par priorit\u00e9", "Methods \u00d7 Applications": "M\u00e9thodes \u00d7 applications", "Africa-linked AI records \u2014 geographic distribution": "Enregistrements d'IA li\u00e9s \u00e0 l'Afrique \u2014 r\u00e9partition g\u00e9ographique", "Show country names on the map": "Afficher les noms de pays sur la carte", "By RCC": "Par CCR", "Top countries": "Principaux pays", "Geography status": "Statut g\u00e9ographique", "Africa-linked records \u2014 trend over time": "Enregistrements li\u00e9s \u00e0 l'Afrique \u2014 \u00e9volution dans le temps", "Critical alerts": "Alertes critiques", "Priority score distribution by disease": "R\u00e9partition du score de priorit\u00e9 par maladie", "High priority intelligence records": "Enregistrements de renseignement \u00e0 priorit\u00e9 \u00e9lev\u00e9e", "All intelligence records": "Tous les enregistrements de renseignement", "Filtered dataset": "Jeu de donn\u00e9es filtr\u00e9", "All records matching current filters.": "Tous les enregistrements correspondant aux filtres actuels.", "Africa-linked dataset": "Jeu de donn\u00e9es li\u00e9 \u00e0 l'Afrique", "Records with African geographic signal.": "Enregistrements avec un signal g\u00e9ographique africain.", "High priority alerts": "Alertes \u00e0 priorit\u00e9 \u00e9lev\u00e9e", "High-priority records (top priority tier).": "Enregistrements \u00e0 priorit\u00e9 \u00e9lev\u00e9e (niveau de priorit\u00e9 maximal).", "Download CSV": "T\u00e9l\u00e9charger le CSV", "Dataset information": "Informations sur le jeu de donn\u00e9es", "Rows = AI method | Columns = disease": "Lignes = m\u00e9thode d'IA | Colonnes = maladie", "Rows = Application | Columns = disease": "Lignes = application | Colonnes = maladie", "No data with country information": "Aucune donn\u00e9e avec information de pays", "Top 10 high-priority records": "Top 10 des enregistrements \u00e0 priorit\u00e9 \u00e9lev\u00e9e", "link": "lien", "Light map": "Fond clair", "Satellite": "Satellite", "Regional Coordinating Centre (fill)": "Centre de coordination r\u00e9gionale (remplissage)", "African geography detected": "G\u00e9ographie africaine d\u00e9tect\u00e9e", "No geography detected": "Aucune g\u00e9ographie d\u00e9tect\u00e9e", "Loess trend line": "Ligne de tendance Loess", "Search:": "Rechercher :", "Previous": "Pr\u00e9c\u00e9dent", "Next": "Suivant", "First": "Premier", "Last": "Dernier", "Show": "Afficher", "entries": "entr\u00e9es", "No data available in table": "Aucune donn\u00e9e dans le tableau", "No matching records found": "Aucun enregistrement correspondant", "disease_primary": "Maladie", "disease_confidence": "Confiance maladie", "disease_mentioned": "Maladie mentionn\u00e9e", "alert_level": "Niveau d'alerte", "title": "titre", "year": "Ann\u00e9e", "country": "Pays", "countries_mentioned": "Pays cit\u00e9s", "rcc": "CCR", "method_family_primary": "M\u00e9thode d'IA", "application_type_primary": "Application", "priority_score": "Score de priorit\u00e9", "africa_relevance_level": "Pertinence Afrique", "novelty_level": "Nouveaut\u00e9", "tracker_recommendation": "Recommandation", "signal_profile": "Profil de signal", "url": "Lien", "Status: research prototype.": "Statut : prototype de recherche.", "Automatic screening of public bibliographic metadata (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Titles, authors, journal, year and links are shown; abstracts are not reproduced. Follow the link to read the source.": "Tri automatique de m\u00e9tadonn\u00e9es bibliographiques publiques (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Les titres, auteurs, revues, ann\u00e9es et liens sont affich\u00e9s ; les r\u00e9sum\u00e9s ne sont pas reproduits. Suivez le lien pour lire la source.", "All labels (AI method, application, disease, country, public-health scope, priority) are produced automatically by rules and have NOT been validated for operational decisions.": "Toutes les \u00e9tiquettes (m\u00e9thode d'IA, application, maladie, pays, p\u00e9rim\u00e8tre de sant\u00e9 publique, priorit\u00e9) sont produites automatiquement par des r\u00e8gles et n'ont PAS \u00e9t\u00e9 valid\u00e9es pour des d\u00e9cisions op\u00e9rationnelles.", "Pilot validation: two samples of 50 articles, one reviewer, stratified and unweighted; agreement for AI as study method 43/50 in each sample. Scope and geography are the weakest dimensions. No result should be read as a prevalence.": "Validation pilote : deux \u00e9chantillons de 50 articles, un seul relecteur, stratifi\u00e9s et non pond\u00e9r\u00e9s ; concordance pour l'IA comme m\u00e9thode d'\u00e9tude 43/50 dans chaque \u00e9chantillon. Le p\u00e9rim\u00e8tre et la g\u00e9ographie sont les dimensions les plus faibles. Aucun r\u00e9sultat ne doit \u00eatre lu comme une pr\u00e9valence.", "Preprints are the majority of records and are not peer reviewed. Europe PMC was unavailable from 16 to 30 October 2026 and was reloaded on 31 October 2026; records added by the source between runs can be missed until the next run.": "Les preprints forment la majorit\u00e9 des enregistrements et ne sont pas \u00e9valu\u00e9s par les pairs. Europe PMC \u00e9tait indisponible du 16 au 30 octobre 2026 et a \u00e9t\u00e9 recharg\u00e9 le 31 octobre 2026 ; les enregistrements ajout\u00e9s par la source entre deux ex\u00e9cutions peuvent \u00eatre manqu\u00e9s jusqu'\u00e0 l'ex\u00e9cution suivante.", "A study country is assigned to only a small share of records; country counts are lower bounds, not the distribution of research.": "Un pays d'\u00e9tude n'est attribu\u00e9 qu'\u00e0 une petite part des enregistrements ; les comptes par pays sont des minimums, pas la r\u00e9partition de la recherche.", "Associations are not causal and predictions are not certainties. Alerts are prepared in test mode; test emails are sent only to the author's own addresses. An outbreak mode (active for the 2026 Bundibugyo virus disease outbreak) widens routing of filovirus articles to the response team.": "Les associations ne sont pas causales et les pr\u00e9dictions ne sont pas des certitudes. Les alertes sont pr\u00e9par\u00e9es en mode test ; les e-mails de test sont envoy\u00e9s uniquement aux adresses de l'auteur. Un mode flamb\u00e9e (actif pour l'\u00e9pid\u00e9mie de maladie \u00e0 virus Bundibugyo de 2026) \u00e9largit l'acheminement des articles sur les filovirus vers l'\u00e9quipe de r\u00e9ponse.", "This tool is not an official product of, and is not endorsed by, any institution. The views are those of the author(s).": "Cet outil n'est pas un produit officiel d'une institution et n'est approuv\u00e9 par aucune. Les opinions sont celles de l'auteur ou des auteurs.", "Last data load:": "Dernier chargement des donn\u00e9es :", "PREIS AI Atlas: which AI, on what, where, at what stage?": "Atlas PREIS IA : quelle IA, sur quoi, o\u00f9, \u00e0 quel stade ?", "Objective:": "Objectif :", "map every AI application used for epidemic surveillance in Africa:": "cartographier chaque application d'IA utilis\u00e9e pour la surveillance \u00e9pid\u00e9miologique en Afrique :", "WHAT": "QUOI", "it does,": "ce qu'elle fait,", "ON WHAT": "SUR QUOI", "disease,": "maladie,", "WHERE": "O\u00d9", "(RCC, country),": "(CCR, pays),", "WHEN": "QUAND", "and at what stage.": "et \u00e0 quel stade.", "Literature signals and known platforms; filters on the left apply.": "Signaux de la litt\u00e9rature et plateformes connues ; les filtres \u00e0 gauche s'appliquent.", "Candidates are keyword signals, not verified systems; registry entries: 0.": "Les candidats sont des signaux par mots-cl\u00e9s, pas des syst\u00e8mes v\u00e9rifi\u00e9s ; entr\u00e9es du registre : 0.", "Tracker data in view:": "Donn\u00e9es du tracker affich\u00e9es :", "records after filters. Candidates file updated:": "enregistrements apr\u00e8s filtres. Fichier des candidats mis \u00e0 jour :", "THE MATRIX: which AI application, on which disease? (number of records)": "LA MATRICE : quelle application d'IA, sur quelle maladie ? (nombre d'enregistrements)", "WHAT: which AI applications?": "QUOI : quelles applications d'IA ?", "ON WHAT: which diseases?": "SUR QUOI : quelles maladies ?", "KNOWN AI SURVEILLANCE PLATFORMS (documented at source, not validated)": "PLATEFORMES D'IA POUR LA SURVEILLANCE CONNUES (document\u00e9es \u00e0 la source, non valid\u00e9es)", "Each line cites its public source. Stage is quoted from that source. Documented = stated by the owner or implementer; it is not verified by the registry and says nothing about performance. Country, RCC and disease filters apply when they match.": "Chaque ligne cite sa source publique. Le stade est cit\u00e9 depuis cette source. Document\u00e9 = affirm\u00e9 par le propri\u00e9taire ou l'impl\u00e9menteur ; ce n'est pas v\u00e9rifi\u00e9 par le registre et ne dit rien de la performance. Les filtres pays, CCR et maladie s'appliquent lorsqu'ils correspondent.", "WHERE: Regional Coordinating Centres (fill) and Member States (outline)": "O\u00d9 : Centres de coordination r\u00e9gionale (remplissage) et \u00c9tats membres (contour)", "Fill = RCC. Dark outline and circle = Member State with at least one record. Click a country for its applications. Only records with a validated study country are placed; the others are counted in the totals but not on the map.": "Remplissage = CCR. Contour fonc\u00e9 et cercle = \u00c9tat membre avec au moins un enregistrement. Cliquez sur un pays pour voir ses applications. Seuls les enregistrements avec un pays d'\u00e9tude valid\u00e9 sont plac\u00e9s ; les autres sont compt\u00e9s dans les totaux mais pas sur la carte.", "WHERE: records by RCC": "O\u00d9 : enregistrements par CCR", "WHERE: top Member States": "O\u00d9 : principaux \u00c9tats membres", "WHEN: records by year of publication and application": "QUAND : enregistrements par ann\u00e9e de publication et application", "The current year is partial. Publication year is not the year of deployment.": "L'ann\u00e9e en cours est partielle. L'ann\u00e9e de publication n'est pas l'ann\u00e9e de d\u00e9ploiement.", "Candidate systems found automatically (stage signal from the wording only)": "Syst\u00e8mes candidats trouv\u00e9s automatiquement (signal de stade tir\u00e9 du seul libell\u00e9)", "Strength reflects the wording of the title and abstract (for example 'deployed', 'pilot'), not a verified stage. Each candidate must be checked by two independent validators.": "La force refl\u00e8te le libell\u00e9 du titre et du r\u00e9sum\u00e9 (par exemple \u00ab d\u00e9ploy\u00e9 \u00bb, \u00ab pilote \u00bb), pas un stade v\u00e9rifi\u00e9. Chaque candidat doit \u00eatre v\u00e9rifi\u00e9 par deux validateurs ind\u00e9pendants.", "Leads: applications read by one reader (preliminary, not validated)": "Pistes : applications lues par un seul lecteur (pr\u00e9liminaire, non valid\u00e9)", "None is confirmed as routine use. A lead is a reason to ask the team, not a finding. Year, country and disease filters apply.": "Aucune n'est confirm\u00e9e comme usage de routine. Une piste est une raison d'interroger l'\u00e9quipe, pas un r\u00e9sultat. Les filtres ann\u00e9e, pays et maladie s'appliquent.", "Registry status and declaration": "Statut du registre et d\u00e9claration", "Statuses:": "Statuts :", "declared": "d\u00e9clar\u00e9", "(the owner says so),": "(le propri\u00e9taire l'affirme),", "documented": "document\u00e9", "(a public document shows it),": "(un document public l'atteste),", "verified": "v\u00e9rifi\u00e9", "(two independent validators agree). Entries now: declared 0, documented 0, verified 0.": "(deux validateurs ind\u00e9pendants concordent). Entr\u00e9es actuelles : d\u00e9clar\u00e9 0, document\u00e9 0, v\u00e9rifi\u00e9 0.", "Phase 1 (done): automatic candidates from the literature. Phase 1b (planned): grey literature. Phase 2 (planned): declarations by RCCs, ministries and partners, then cross-validation.": "Phase 1 (termin\u00e9e) : candidats automatiques issus de la litt\u00e9rature. Phase 1b (pr\u00e9vue) : litt\u00e9rature grise. Phase 2 (pr\u00e9vue) : d\u00e9clarations par les CCR, minist\u00e8res et partenaires, puis validation crois\u00e9e.", "Declare an AI system (about 5 minutes)": "D\u00e9clarer un syst\u00e8me d'IA (environ 5 minutes)", "- recorded as declared, not verified. Nothing is published with a name or country without consent.": "- enregistr\u00e9 comme d\u00e9clar\u00e9, non v\u00e9rifi\u00e9. Rien n'est publi\u00e9 avec un nom ou un pays sans consentement.", "Africa-linked records, explicit AI (after filters)": "Enregistrements li\u00e9s \u00e0 l'Afrique, IA explicite (apr\u00e8s filtres)", "Known AI platforms in Africa (documented, core)": "Plateformes d'IA connues en Afrique (document\u00e9es, centrales)", "Candidates in view (not verified)": "Candidats affich\u00e9s (non v\u00e9rifi\u00e9s)", "Strong wording signals": "Signaux de libell\u00e9 forts", "Registry entries (declared, documented, verified)": "Entr\u00e9es du registre (d\u00e9clar\u00e9es, document\u00e9es, v\u00e9rifi\u00e9es)", "No record matches the current filters. Widen the filters in the left bar.": "Aucun enregistrement ne correspond aux filtres actuels. \u00c9largissez les filtres dans la barre de gauche.", "No candidate matches the current filters.": "Aucun candidat ne correspond aux filtres actuels.", "No platform matches the current filters.": "Aucune plateforme ne correspond aux filtres actuels.", "No lead matches the current filters.": "Aucune piste ne correspond aux filtres actuels.", "Platforms file not found.": "Fichier des plateformes introuvable.", "Leads file not found.": "Fichier des pistes introuvable.", "Documented": "Document\u00e9", "Reported": "Rapport\u00e9", "Arabic": "Arabe", "Swahili": "Swahili", "Machine Learning": "Apprentissage automatique", "Bayesian / Statistical Modeling": "Mod\u00e9lisation bay\u00e9sienne / statistique", "Deep Learning": "Apprentissage profond", "General AI": "IA g\u00e9n\u00e9rale", "Large Language Models": "Grands mod\u00e8les de langage", "Optimization / Hybrid AI": "Optimisation / IA hybride", "Natural Language Processing": "Traitement du langage naturel", "Computer Vision": "Vision par ordinateur", "Surveillance / Event Detection": "Surveillance / d\u00e9tection d'\u00e9v\u00e9nements", "Prediction / Forecasting": "Pr\u00e9diction / pr\u00e9vision", "Policy / Decision Support": "Appui aux politiques / aux d\u00e9cisions", "Laboratory / Genomic Intelligence": "Laboratoire / renseignement g\u00e9nomique", "Diagnosis / Clinical Decision Support": "Diagnostic / aide \u00e0 la d\u00e9cision clinique", "Risk Mapping / Spatial Analysis": "Cartographie du risque / analyse spatiale", "Digital Health / Chatbot / Hotline": "Sant\u00e9 num\u00e9rique / chatbot / ligne d'assistance", "Resource Planning / Response Optimization": "Planification des ressources / optimisation de la riposte", "HIGH": "\u00c9lev\u00e9", "Medium": "Moyen", "MEDIUM": "Moyen", "LOW": "Faible", "None": "Aucun", "Minimal": "Minimal", "CRITICAL": "Critique", "Non specifi\u00e9": "Non sp\u00e9cifi\u00e9", "Non sp\u00e9cifi\u00e9": "Non sp\u00e9cifi\u00e9", "Non specifie": "Non sp\u00e9cifi\u00e9", "NA": "NA", "Predictive": "Pr\u00e9dictif", "Operational": "Op\u00e9rationnel", "One Health": "Une seule sant\u00e9", "General": "G\u00e9n\u00e9ral", "Immediate review required": "Examen imm\u00e9diat requis", "Monitor closely": "Surveiller de pr\u00e8s", "Priority review": "Examen prioritaire", "Archive for background tracking": "Archiver pour suivi de fond", "Top Priority": "Priorit\u00e9 maximale", "Watch List": "Liste de surveillance", "High Impact": "Fort impact", "Background": "Fond", "Western": "Ouest", "Eastern": "Est", "Central": "Centre", "Southern": "Sud", "Northern": "Nord", "Mpox": "Mpox", "Ebola": "Ebola", "Marburg": "Marburg", "COVID-19": "COVID-19", "HIV/AIDS": "VIH/sida", "Malaria": "Paludisme", "Influenza": "Grippe", "Yellow Fever": "Fi\u00e8vre jaune", "Dengue": "Dengue", "Cholera": "Chol\u00e9ra", "Meningitis": "M\u00e9ningite", "Tuberculosis": "Tuberculose", "Lassa fever": "Fi\u00e8vre de Lassa", "Polio": "Poliomy\u00e9lite", "Measles": "Rougeole", "Rift Valley Fever": "Fi\u00e8vre de la vall\u00e9e du Rift", "Algeria": "Alg\u00e9rie", "Angola": "Angola", "Benin": "B\u00e9nin", "Botswana": "Botswana", "Burkina Faso": "Burkina Faso", "Burundi": "Burundi", "Cabo Verde": "Cap-Vert", "Republic of Cabo Verde": "Cap-Vert", "Cameroon": "Cameroun", "Central African Republic": "R\u00e9publique centrafricaine", "Chad": "Tchad", "Comoros": "Comores", "Congo": "Congo", "Republic of the Congo": "R\u00e9publique du Congo", "Democratic Republic of the Congo": "R\u00e9publique d\u00e9mocratique du Congo", "DR Congo": "RD Congo", "Djibouti": "Djibouti", "Egypt": "\u00c9gypte", "Equatorial Guinea": "Guin\u00e9e \u00e9quatoriale", "Eritrea": "\u00c9rythr\u00e9e", "Eswatini": "Eswatini", "Kingdom of eSwatini": "Eswatini", "Ethiopia": "\u00c9thiopie", "Gabon": "Gabon", "Gambia": "Gambie", "The Gambia": "Gambie", "Ghana": "Ghana", "Guinea": "Guin\u00e9e", "Guinea-Bissau": "Guin\u00e9e-Bissau", "C\u00f4te d'Ivoire": "C\u00f4te d'Ivoire", "Ivory Coast": "C\u00f4te d'Ivoire", "Kenya": "Kenya", "Lesotho": "Lesotho", "Liberia": "Lib\u00e9ria", "Libya": "Libye", "Madagascar": "Madagascar", "Malawi": "Malawi", "Mali": "Mali", "Mauritania": "Mauritanie", "Mauritius": "Maurice", "Morocco": "Maroc", "Mozambique": "Mozambique", "Namibia": "Namibie", "Niger": "Niger", "Nigeria": "Nig\u00e9ria", "Rwanda": "Rwanda", "Sao Tome and Principe": "Sao Tom\u00e9-et-Principe", "S\u00e3o Tom\u00e9 and Principe": "Sao Tom\u00e9-et-Principe", "Senegal": "S\u00e9n\u00e9gal", "Seychelles": "Seychelles", "Sierra Leone": "Sierra Leone", "Somalia": "Somalie", "South Africa": "Afrique du Sud", "South Sudan": "Soudan du Sud", "Sudan": "Soudan", "Tanzania": "Tanzanie", "Togo": "Togo", "Tunisia": "Tunisie", "Uganda": "Ouganda", "Zambia": "Zambie", "Zimbabwe": "Zimbabwe", "Western Sahara": "Sahara occidental", "preprint": "Pr\u00e9publication", "PubMed": "PubMed", "Core": "C\u0153ur", "Adjacent": "Adjacent", "explicit": "explicite", "statistical_only": "statistique seulement", "mention_only": "mention seulement", "none": "aucun", "single_mention": "mention unique", "text_multiple": "mentions multiples", "ambiguous": "ambigu", "dominant": "dominant", "TRUE": "VRAI", "FALSE": "FAUX", "Journal Article": "Article de revue", "Review": "Revue de la litt\u00e9rature", "Systematic Review": "Revue syst\u00e9matique", "Scoping Review": "Revue de cadrage", "Editorial": "\u00c9ditorial", "Retracted Publication": "Publication r\u00e9tract\u00e9e", "Preprint": "Pr\u00e9publication", "Research Support, Non-U.S. Gov't": "Soutien \u00e0 la recherche (hors gouvernement am\u00e9ricain)", "Research Support, N.I.H., Extramural": "Soutien \u00e0 la recherche (NIH, extramural)", "Research Support, U.S. Gov't, Non-P.H.S.": "Soutien \u00e0 la recherche (gouvernement am\u00e9ricain, hors PHS)", "Antimicrobial resistance": "R\u00e9sistance aux antimicrobiens", "HIV": "VIH", "Hepatitis": "H\u00e9patite", "Diphtheria": "Dipht\u00e9rie", "Pertussis": "Coqueluche"}, "pt": {"Overview": "Vis\u00e3o geral", "AI x Disease x Country": "IA x Doen\u00e7a x Pa\u00eds", "Methods & Applications": "M\u00e9todos e aplica\u00e7\u00f5es", "Focus Africa": "Foco em \u00c1frica", "Priority Intelligence": "Intelig\u00eancia priorit\u00e1ria", "All Records": "Todos os registos", "Downloads": "Descarregamentos", "PREIS AI Atlas": "Atlas PREIS IA", "Methods and limits": "M\u00e9todos e limites", "Filters": "Filtros", "Year": "Ano", "Alert level": "N\u00edvel de alerta", "Disease": "Doen\u00e7a", "AI method": "M\u00e9todo de IA", "Application": "Aplica\u00e7\u00e3o", "RCC": "CCR", "Country": "Pa\u00eds", "Signal profile": "Perfil de sinal", "Language of publication": "Idioma da publica\u00e7\u00e3o", "All": "Todos", "English": "Ingl\u00eas", "French": "Franc\u00eas", "Spanish": "Espanhol", "Portuguese": "Portugu\u00eas", "German": "Alem\u00e3o", "Italian": "Italiano", "Other": "Outro", "Arabic": "\u00c1rabe", "Swahili": "Sua\u00edli", "High": "Alto", "Moderate": "Moderado", "Low": "Baixo", "Unspecified": "N\u00e3o especificado", "Critical": "Cr\u00edtico", "PREIS AI Tracker \u2014 Dashboard": "PREIS AI Tracker \u2014 Painel", "Monitoring AI applications for public health surveillance in Africa |": "Monitoriza\u00e7\u00e3o de aplica\u00e7\u00f5es de IA para a vigil\u00e2ncia da sa\u00fade p\u00fablica em \u00c1frica |", "Source:": "Fonte:", "Data generated:": "Dados gerados:", "unknown": "desconhecido", "Automated classification, not yet validated by human review.": "Classifica\u00e7\u00e3o autom\u00e1tica, ainda n\u00e3o validada por revis\u00e3o humana.", "Disease, method, application and Africa-relevance labels and scores are produced by keyword rules and may contain errors.": "As etiquetas e pontua\u00e7\u00f5es de doen\u00e7a, m\u00e9todo, aplica\u00e7\u00e3o e relev\u00e2ncia para \u00c1frica s\u00e3o produzidas por regras de palavras-chave e podem conter erros.", "Records outside the infectious-disease/epidemic scope are excluded from this view.": "Os registos fora do \u00e2mbito das doen\u00e7as infecciosas e epidemias s\u00e3o exclu\u00eddos desta vista.", "Do not use as a basis for operational decisions without expert verification.": "N\u00e3o utilizar como base para decis\u00f5es operacionais sem verifica\u00e7\u00e3o por um especialista.", "AI records": "Registos de IA", "High-priority records": "Registos de alta prioridade", "Diseases under surveillance": "Doen\u00e7as sob vigil\u00e2ncia", "Priority distribution": "Distribui\u00e7\u00e3o por prioridade", "Alert level distribution": "Distribui\u00e7\u00e3o por n\u00edvel de alerta", "AI methods": "M\u00e9todos de IA", "AI applications": "Aplica\u00e7\u00f5es de IA", "Africa relevance": "Relev\u00e2ncia para \u00c1frica", "Novelty": "Novidade", "Diseases under AI surveillance \u2014 top priority": "Doen\u00e7as sob vigil\u00e2ncia por IA \u2014 m\u00e1xima prioridade", "AI method x Disease matrix": "Matriz m\u00e9todo de IA x doen\u00e7a", "Application x Disease matrix": "Matriz aplica\u00e7\u00e3o x doen\u00e7a", "Top countries per disease (Africa-linked)": "Principais pa\u00edses por doen\u00e7a (ligados a \u00c1frica)", "Disease trend over time": "Evolu\u00e7\u00e3o das doen\u00e7as ao longo do tempo", "Disease intelligence records": "Registos de intelig\u00eancia por doen\u00e7a", "Methods by priority": "M\u00e9todos por prioridade", "Applications by priority": "Aplica\u00e7\u00f5es por prioridade", "Methods \u00d7 Applications": "M\u00e9todos \u00d7 aplica\u00e7\u00f5es", "Africa-linked AI records \u2014 geographic distribution": "Registos de IA ligados a \u00c1frica \u2014 distribui\u00e7\u00e3o geogr\u00e1fica", "Show country names on the map": "Mostrar nomes dos pa\u00edses no mapa", "By RCC": "Por CCR", "Top countries": "Principais pa\u00edses", "Geography status": "Estado geogr\u00e1fico", "Africa-linked records \u2014 trend over time": "Registos ligados a \u00c1frica \u2014 evolu\u00e7\u00e3o ao longo do tempo", "Critical alerts": "Alertas cr\u00edticos", "Priority score distribution by disease": "Distribui\u00e7\u00e3o da pontua\u00e7\u00e3o de prioridade por doen\u00e7a", "High priority intelligence records": "Registos de intelig\u00eancia de alta prioridade", "All intelligence records": "Todos os registos de intelig\u00eancia", "Filtered dataset": "Conjunto de dados filtrado", "All records matching current filters.": "Todos os registos que correspondem aos filtros atuais.", "Africa-linked dataset": "Conjunto de dados ligado a \u00c1frica", "Records with African geographic signal.": "Registos com sinal geogr\u00e1fico africano.", "High priority alerts": "Alertas de alta prioridade", "High-priority records (top priority tier).": "Registos de alta prioridade (n\u00edvel m\u00e1ximo).", "Download CSV": "Descarregar CSV", "Dataset information": "Informa\u00e7\u00f5es sobre o conjunto de dados", "Rows = AI method | Columns = disease": "Linhas = m\u00e9todo de IA | Colunas = doen\u00e7a", "Rows = Application | Columns = disease": "Linhas = aplica\u00e7\u00e3o | Colunas = doen\u00e7a", "No data with country information": "Sem dados com informa\u00e7\u00e3o de pa\u00eds", "Top 10 high-priority records": "Top 10 de registos de alta prioridade", "link": "liga\u00e7\u00e3o", "Light map": "Mapa claro", "Satellite": "Sat\u00e9lite", "Regional Coordinating Centre (fill)": "Centro de Coordena\u00e7\u00e3o Regional (preenchimento)", "African geography detected": "Geografia africana detetada", "No geography detected": "Nenhuma geografia detetada", "Loess trend line": "Linha de tend\u00eancia Loess", "Search:": "Pesquisar:", "Previous": "Anterior", "Next": "Seguinte", "First": "Primeiro", "Last": "\u00daltimo", "Show": "Mostrar", "entries": "entradas", "No data available in table": "Sem dados na tabela", "No matching records found": "Nenhum registo correspondente", "disease_primary": "Doen\u00e7a", "disease_confidence": "Confian\u00e7a da doen\u00e7a", "disease_mentioned": "Doen\u00e7a mencionada", "alert_level": "N\u00edvel de alerta", "title": "t\u00edtulo", "year": "Ano", "country": "Pa\u00eds", "countries_mentioned": "Pa\u00edses citados", "rcc": "CCR", "method_family_primary": "M\u00e9todo de IA", "application_type_primary": "Aplica\u00e7\u00e3o", "priority_score": "Pontua\u00e7\u00e3o de prioridade", "africa_relevance_level": "Relev\u00e2ncia \u00c1frica", "novelty_level": "Novidade", "tracker_recommendation": "Recomenda\u00e7\u00e3o", "signal_profile": "Perfil de sinal", "url": "Liga\u00e7\u00e3o", "Status: research prototype.": "Estado: prot\u00f3tipo de investiga\u00e7\u00e3o.", "Automatic screening of public bibliographic metadata (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Titles, authors, journal, year and links are shown; abstracts are not reproduced. Follow the link to read the source.": "Triagem autom\u00e1tica de metadados bibliogr\u00e1ficos p\u00fablicos (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). S\u00e3o apresentados t\u00edtulos, autores, revista, ano e liga\u00e7\u00f5es; os resumos n\u00e3o s\u00e3o reproduzidos. Siga a liga\u00e7\u00e3o para ler a fonte.", "All labels (AI method, application, disease, country, public-health scope, priority) are produced automatically by rules and have NOT been validated for operational decisions.": "Todas as etiquetas (m\u00e9todo de IA, aplica\u00e7\u00e3o, doen\u00e7a, pa\u00eds, \u00e2mbito de sa\u00fade p\u00fablica, prioridade) s\u00e3o produzidas automaticamente por regras e N\u00c3O foram validadas para decis\u00f5es operacionais.", "Pilot validation: two samples of 50 articles, one reviewer, stratified and unweighted; agreement for AI as study method 43/50 in each sample. Scope and geography are the weakest dimensions. No result should be read as a prevalence.": "Valida\u00e7\u00e3o piloto: duas amostras de 50 artigos, um revisor, estratificadas e n\u00e3o ponderadas; concord\u00e2ncia para a IA como m\u00e9todo de estudo 43/50 em cada amostra. O \u00e2mbito e a geografia s\u00e3o as dimens\u00f5es mais fracas. Nenhum resultado deve ser lido como uma preval\u00eancia.", "Preprints are the majority of records and are not peer reviewed. Europe PMC was unavailable from 16 to 30 October 2026 and was reloaded on 31 October 2026; records added by the source between runs can be missed until the next run.": "Os preprints s\u00e3o a maioria dos registos e n\u00e3o s\u00e3o revistos por pares. O Europe PMC esteve indispon\u00edvel de 16 a 30 de outubro de 2026 e foi recarregado a 31 de outubro de 2026; os registos acrescentados pela fonte entre execu\u00e7\u00f5es podem ser omitidos at\u00e9 \u00e0 execu\u00e7\u00e3o seguinte.", "A study country is assigned to only a small share of records; country counts are lower bounds, not the distribution of research.": "S\u00f3 uma pequena parte dos registos tem um pa\u00eds de estudo atribu\u00eddo; as contagens por pa\u00eds s\u00e3o limites m\u00ednimos, n\u00e3o a distribui\u00e7\u00e3o da investiga\u00e7\u00e3o.", "Associations are not causal and predictions are not certainties. Alerts are prepared in test mode; test emails are sent only to the author's own addresses. An outbreak mode (active for the 2026 Bundibugyo virus disease outbreak) widens routing of filovirus articles to the response team.": "As associa\u00e7\u00f5es n\u00e3o s\u00e3o causais e as previs\u00f5es n\u00e3o s\u00e3o certezas. Os alertas s\u00e3o preparados em modo de teste; os e-mails de teste s\u00e3o enviados apenas para os endere\u00e7os do autor. Um modo de surto (ativo para o surto de doen\u00e7a por v\u00edrus Bundibugyo de 2026) alarga o encaminhamento de artigos sobre filov\u00edrus para a equipa de resposta.", "This tool is not an official product of, and is not endorsed by, any institution. The views are those of the author(s).": "Esta ferramenta n\u00e3o \u00e9 um produto oficial nem \u00e9 endossada por nenhuma institui\u00e7\u00e3o. As opini\u00f5es s\u00e3o as do(s) autor(es).", "Last data load:": "\u00daltimo carregamento de dados:", "PREIS AI Atlas: which AI, on what, where, at what stage?": "Atlas PREIS IA: que IA, sobre qu\u00ea, onde, em que fase?", "Objective:": "Objetivo:", "map every AI application used for epidemic surveillance in Africa:": "mapear cada aplica\u00e7\u00e3o de IA utilizada na vigil\u00e2ncia epidemiol\u00f3gica em \u00c1frica:", "WHAT": "O QU\u00ca", "it does,": "faz,", "ON WHAT": "SOBRE QU\u00ca", "disease,": "doen\u00e7a,", "WHERE": "ONDE", "(RCC, country),": "(CCR, pa\u00eds),", "WHEN": "QUANDO", "and at what stage.": "e em que fase.", "Literature signals and known platforms; filters on the left apply.": "Sinais da literatura e plataformas conhecidas; os filtros \u00e0 esquerda aplicam-se.", "Candidates are keyword signals, not verified systems; registry entries: 0.": "Os candidatos s\u00e3o sinais por palavras-chave, n\u00e3o sistemas verificados; entradas do registo: 0.", "Tracker data in view:": "Dados do tracker em vista:", "records after filters. Candidates file updated:": "registos ap\u00f3s filtros. Ficheiro de candidatos atualizado:", "THE MATRIX: which AI application, on which disease? (number of records)": "A MATRIZ: que aplica\u00e7\u00e3o de IA, sobre que doen\u00e7a? (n\u00famero de registos)", "WHAT: which AI applications?": "O QU\u00ca: que aplica\u00e7\u00f5es de IA?", "ON WHAT: which diseases?": "SOBRE QU\u00ca: que doen\u00e7as?", "KNOWN AI SURVEILLANCE PLATFORMS (documented at source, not validated)": "PLATAFORMAS DE IA PARA VIGIL\u00c2NCIA CONHECIDAS (documentadas na fonte, n\u00e3o validadas)", "Each line cites its public source. Stage is quoted from that source. Documented = stated by the owner or implementer; it is not verified by the registry and says nothing about performance. Country, RCC and disease filters apply when they match.": "Cada linha cita a sua fonte p\u00fablica. A fase \u00e9 citada dessa fonte. Documentado = declarado pelo propriet\u00e1rio ou implementador; n\u00e3o \u00e9 verificado pelo registo e nada diz sobre o desempenho. Os filtros de pa\u00eds, CCR e doen\u00e7a aplicam-se quando correspondem.", "WHERE: Regional Coordinating Centres (fill) and Member States (outline)": "ONDE: Centros de Coordena\u00e7\u00e3o Regional (preenchimento) e Estados-Membros (contorno)", "Fill = RCC. Dark outline and circle = Member State with at least one record. Click a country for its applications. Only records with a validated study country are placed; the others are counted in the totals but not on the map.": "Preenchimento = CCR. Contorno escuro e c\u00edrculo = Estado-Membro com pelo menos um registo. Clique num pa\u00eds para ver as suas aplica\u00e7\u00f5es. S\u00f3 s\u00e3o colocados os registos com pa\u00eds de estudo validado; os restantes contam nos totais mas n\u00e3o aparecem no mapa.", "WHERE: records by RCC": "ONDE: registos por CCR", "WHERE: top Member States": "ONDE: principais Estados-Membros", "WHEN: records by year of publication and application": "QUANDO: registos por ano de publica\u00e7\u00e3o e aplica\u00e7\u00e3o", "The current year is partial. Publication year is not the year of deployment.": "O ano corrente \u00e9 parcial. O ano de publica\u00e7\u00e3o n\u00e3o \u00e9 o ano de implementa\u00e7\u00e3o.", "Candidate systems found automatically (stage signal from the wording only)": "Sistemas candidatos encontrados automaticamente (sinal de fase apenas pela reda\u00e7\u00e3o)", "Strength reflects the wording of the title and abstract (for example 'deployed', 'pilot'), not a verified stage. Each candidate must be checked by two independent validators.": "A for\u00e7a reflete a reda\u00e7\u00e3o do t\u00edtulo e do resumo (por exemplo \u00abimplementado\u00bb, \u00abpiloto\u00bb), n\u00e3o uma fase verificada. Cada candidato tem de ser verificado por dois validadores independentes.", "Leads: applications read by one reader (preliminary, not validated)": "Pistas: aplica\u00e7\u00f5es lidas por um s\u00f3 leitor (preliminar, n\u00e3o validado)", "None is confirmed as routine use. A lead is a reason to ask the team, not a finding. Year, country and disease filters apply.": "Nenhuma est\u00e1 confirmada como uso de rotina. Uma pista \u00e9 um motivo para perguntar \u00e0 equipa, n\u00e3o uma conclus\u00e3o. Os filtros de ano, pa\u00eds e doen\u00e7a aplicam-se.", "Registry status and declaration": "Estado do registo e declara\u00e7\u00e3o", "Statuses:": "Estados:", "declared": "declarado", "(the owner says so),": "(o propriet\u00e1rio afirma-o),", "documented": "documentado", "(a public document shows it),": "(um documento p\u00fablico demonstra-o),", "verified": "verificado", "(two independent validators agree). Entries now: declared 0, documented 0, verified 0.": "(dois validadores independentes concordam). Entradas atuais: declarado 0, documentado 0, verificado 0.", "Phase 1 (done): automatic candidates from the literature. Phase 1b (planned): grey literature. Phase 2 (planned): declarations by RCCs, ministries and partners, then cross-validation.": "Fase 1 (conclu\u00edda): candidatos autom\u00e1ticos a partir da literatura. Fase 1b (prevista): literatura cinzenta. Fase 2 (prevista): declara\u00e7\u00f5es dos CCR, minist\u00e9rios e parceiros, depois valida\u00e7\u00e3o cruzada.", "Declare an AI system (about 5 minutes)": "Declarar um sistema de IA (cerca de 5 minutos)", "- recorded as declared, not verified. Nothing is published with a name or country without consent.": "- registado como declarado, n\u00e3o verificado. Nada \u00e9 publicado com nome ou pa\u00eds sem consentimento.", "Africa-linked records, explicit AI (after filters)": "Registos ligados a \u00c1frica, IA expl\u00edcita (ap\u00f3s filtros)", "Known AI platforms in Africa (documented, core)": "Plataformas de IA conhecidas em \u00c1frica (documentadas, centrais)", "Candidates in view (not verified)": "Candidatos em vista (n\u00e3o verificados)", "Strong wording signals": "Sinais de reda\u00e7\u00e3o fortes", "Registry entries (declared, documented, verified)": "Entradas do registo (declaradas, documentadas, verificadas)", "No record matches the current filters. Widen the filters in the left bar.": "Nenhum registo corresponde aos filtros atuais. Alargue os filtros na barra da esquerda.", "No candidate matches the current filters.": "Nenhum candidato corresponde aos filtros atuais.", "No platform matches the current filters.": "Nenhuma plataforma corresponde aos filtros atuais.", "No lead matches the current filters.": "Nenhuma pista corresponde aos filtros atuais.", "Platforms file not found.": "Ficheiro de plataformas n\u00e3o encontrado.", "Leads file not found.": "Ficheiro de pistas n\u00e3o encontrado.", "Documented": "Documentado", "Reported": "Reportado", "Machine Learning": "Aprendizagem autom\u00e1tica", "Bayesian / Statistical Modeling": "Modela\u00e7\u00e3o bayesiana / estat\u00edstica", "Deep Learning": "Aprendizagem profunda", "General AI": "IA geral", "Large Language Models": "Grandes modelos de linguagem", "Optimization / Hybrid AI": "Otimiza\u00e7\u00e3o / IA h\u00edbrida", "Natural Language Processing": "Processamento de linguagem natural", "Computer Vision": "Vis\u00e3o computacional", "Surveillance / Event Detection": "Vigil\u00e2ncia / dete\u00e7\u00e3o de eventos", "Prediction / Forecasting": "Previs\u00e3o / proje\u00e7\u00e3o", "Policy / Decision Support": "Apoio a pol\u00edticas / decis\u00f5es", "Laboratory / Genomic Intelligence": "Laborat\u00f3rio / intelig\u00eancia gen\u00f3mica", "Diagnosis / Clinical Decision Support": "Diagn\u00f3stico / apoio \u00e0 decis\u00e3o cl\u00ednica", "Risk Mapping / Spatial Analysis": "Mapeamento de risco / an\u00e1lise espacial", "Digital Health / Chatbot / Hotline": "Sa\u00fade digital / chatbot / linha de apoio", "Resource Planning / Response Optimization": "Planeamento de recursos / otimiza\u00e7\u00e3o da resposta", "HIGH": "Alto", "Medium": "M\u00e9dio", "MEDIUM": "M\u00e9dio", "LOW": "Baixo", "None": "Nenhum", "Minimal": "M\u00ednimo", "CRITICAL": "Cr\u00edtico", "Non specifi\u00e9": "N\u00e3o especificado", "Non sp\u00e9cifi\u00e9": "N\u00e3o especificado", "Non specifie": "N\u00e3o especificado", "NA": "NA", "Predictive": "Preditivo", "Operational": "Operacional", "One Health": "Sa\u00fade \u00danica", "General": "Geral", "Immediate review required": "Revis\u00e3o imediata necess\u00e1ria", "Monitor closely": "Monitorizar de perto", "Priority review": "Revis\u00e3o priorit\u00e1ria", "Archive for background tracking": "Arquivar para acompanhamento de fundo", "Top Priority": "Prioridade m\u00e1xima", "Watch List": "Lista de vigil\u00e2ncia", "High Impact": "Alto impacto", "Background": "Contexto", "Western": "Ocidental", "Eastern": "Oriental", "Central": "Central", "Southern": "Austral", "Northern": "Setentrional", "Mpox": "Mpox", "Ebola": "\u00c9bola", "Marburg": "Marburgo", "COVID-19": "COVID-19", "HIV/AIDS": "VIH/sida", "Malaria": "Mal\u00e1ria", "Influenza": "Gripe", "Yellow Fever": "Febre amarela", "Dengue": "Dengue", "Cholera": "C\u00f3lera", "Meningitis": "Meningite", "Tuberculosis": "Tuberculose", "Lassa fever": "Febre de Lassa", "Polio": "Poliomielite", "Measles": "Sarampo", "Rift Valley Fever": "Febre do Vale do Rift", "Algeria": "Arg\u00e9lia", "Angola": "Angola", "Benin": "Benim", "Botswana": "Botsuana", "Burkina Faso": "Burquina Faso", "Burundi": "Bur\u00fandi", "Cabo Verde": "Cabo Verde", "Republic of Cabo Verde": "Cabo Verde", "Cameroon": "Camar\u00f5es", "Central African Republic": "Rep\u00fablica Centro-Africana", "Chad": "Chade", "Comoros": "Comores", "Congo": "Congo", "Republic of the Congo": "Rep\u00fablica do Congo", "Democratic Republic of the Congo": "Rep\u00fablica Democr\u00e1tica do Congo", "DR Congo": "RD Congo", "Djibouti": "Jibuti", "Egypt": "Egito", "Equatorial Guinea": "Guin\u00e9 Equatorial", "Eritrea": "Eritreia", "Eswatini": "Essuat\u00edni", "Kingdom of eSwatini": "Essuat\u00edni", "Ethiopia": "Eti\u00f3pia", "Gabon": "Gab\u00e3o", "Gambia": "G\u00e2mbia", "The Gambia": "G\u00e2mbia", "Ghana": "Gana", "Guinea": "Guin\u00e9", "Guinea-Bissau": "Guin\u00e9-Bissau", "C\u00f4te d'Ivoire": "Costa do Marfim", "Ivory Coast": "Costa do Marfim", "Kenya": "Qu\u00e9nia", "Lesotho": "Lesoto", "Liberia": "Lib\u00e9ria", "Libya": "L\u00edbia", "Madagascar": "Madag\u00e1scar", "Malawi": "Mal\u00e1ui", "Mali": "Mali", "Mauritania": "Maurit\u00e2nia", "Mauritius": "Maur\u00edcia", "Morocco": "Marrocos", "Mozambique": "Mo\u00e7ambique", "Namibia": "Nam\u00edbia", "Niger": "N\u00edger", "Nigeria": "Nig\u00e9ria", "Rwanda": "Ruanda", "Sao Tome and Principe": "S\u00e3o Tom\u00e9 e Pr\u00edncipe", "S\u00e3o Tom\u00e9 and Principe": "S\u00e3o Tom\u00e9 e Pr\u00edncipe", "Senegal": "Senegal", "Seychelles": "Seicheles", "Sierra Leone": "Serra Leoa", "Somalia": "Som\u00e1lia", "South Africa": "\u00c1frica do Sul", "South Sudan": "Sud\u00e3o do Sul", "Sudan": "Sud\u00e3o", "Tanzania": "Tanz\u00e2nia", "Togo": "Togo", "Tunisia": "Tun\u00edsia", "Uganda": "Uganda", "Zambia": "Z\u00e2mbia", "Zimbabwe": "Zimbabu\u00e9", "Western Sahara": "Saara Ocidental", "preprint": "Pr\u00e9-publica\u00e7\u00e3o", "PubMed": "PubMed", "Core": "N\u00facleo", "Adjacent": "Adjacente", "explicit": "expl\u00edcito", "statistical_only": "apenas estat\u00edstico", "mention_only": "apenas men\u00e7\u00e3o", "none": "nenhum", "single_mention": "men\u00e7\u00e3o \u00fanica", "text_multiple": "men\u00e7\u00f5es m\u00faltiplas", "ambiguous": "amb\u00edguo", "dominant": "dominante", "TRUE": "VERDADEIRO", "FALSE": "FALSO", "Journal Article": "Artigo de revista", "Review": "Revis\u00e3o", "Systematic Review": "Revis\u00e3o sistem\u00e1tica", "Scoping Review": "Revis\u00e3o de \u00e2mbito", "Editorial": "Editorial", "Retracted Publication": "Publica\u00e7\u00e3o retratada", "Preprint": "Pr\u00e9-publica\u00e7\u00e3o", "Research Support, Non-U.S. Gov't": "Apoio \u00e0 investiga\u00e7\u00e3o (n\u00e3o governo dos EUA)", "Research Support, N.I.H., Extramural": "Apoio \u00e0 investiga\u00e7\u00e3o (NIH, extramural)", "Research Support, U.S. Gov't, Non-P.H.S.": "Apoio \u00e0 investiga\u00e7\u00e3o (governo dos EUA, n\u00e3o PHS)", "Antimicrobial resistance": "Resist\u00eancia antimicrobiana", "HIV": "VIH", "Hepatitis": "Hepatite", "Diphtheria": "Difteria", "Pertussis": "Tosse convulsa"}, "es": {"Overview": "Resumen", "AI x Disease x Country": "IA x Enfermedad x Pa\u00eds", "Methods & Applications": "M\u00e9todos y aplicaciones", "Focus Africa": "Enfoque en \u00c1frica", "Priority Intelligence": "Inteligencia prioritaria", "All Records": "Todos los registros", "Downloads": "Descargas", "PREIS AI Atlas": "Atlas PREIS IA", "Methods and limits": "M\u00e9todos y l\u00edmites", "Filters": "Filtros", "Year": "A\u00f1o", "Alert level": "Nivel de alerta", "Disease": "Enfermedad", "AI method": "M\u00e9todo de IA", "Application": "Aplicaci\u00f3n", "RCC": "CCR", "Country": "Pa\u00eds", "Signal profile": "Perfil de se\u00f1al", "Language of publication": "Idioma de la publicaci\u00f3n", "All": "Todos", "English": "Ingl\u00e9s", "French": "Franc\u00e9s", "Spanish": "Espa\u00f1ol", "Portuguese": "Portugu\u00e9s", "German": "Alem\u00e1n", "Italian": "Italiano", "Other": "Otro", "Arabic": "\u00c1rabe", "Swahili": "Suajili", "High": "Alto", "Moderate": "Moderado", "Low": "Bajo", "Unspecified": "No especificado", "Critical": "Cr\u00edtico", "PREIS AI Tracker \u2014 Dashboard": "PREIS AI Tracker \u2014 Panel", "Monitoring AI applications for public health surveillance in Africa |": "Seguimiento de aplicaciones de IA para la vigilancia de la salud p\u00fablica en \u00c1frica |", "Source:": "Fuente:", "Data generated:": "Datos generados:", "unknown": "desconocido", "Automated classification, not yet validated by human review.": "Clasificaci\u00f3n autom\u00e1tica, a\u00fan no validada por revisi\u00f3n humana.", "Disease, method, application and Africa-relevance labels and scores are produced by keyword rules and may contain errors.": "Las etiquetas y puntuaciones de enfermedad, m\u00e9todo, aplicaci\u00f3n y relevancia para \u00c1frica se generan con reglas de palabras clave y pueden contener errores.", "Records outside the infectious-disease/epidemic scope are excluded from this view.": "Los registros fuera del \u00e1mbito de las enfermedades infecciosas y epidemias se excluyen de esta vista.", "Do not use as a basis for operational decisions without expert verification.": "No utilizar como base para decisiones operativas sin verificaci\u00f3n de un experto.", "AI records": "Registros de IA", "High-priority records": "Registros de alta prioridad", "Diseases under surveillance": "Enfermedades bajo vigilancia", "Priority distribution": "Distribuci\u00f3n por prioridad", "Alert level distribution": "Distribuci\u00f3n por nivel de alerta", "AI methods": "M\u00e9todos de IA", "AI applications": "Aplicaciones de IA", "Africa relevance": "Relevancia para \u00c1frica", "Novelty": "Novedad", "Diseases under AI surveillance \u2014 top priority": "Enfermedades bajo vigilancia con IA \u2014 m\u00e1xima prioridad", "AI method x Disease matrix": "Matriz m\u00e9todo de IA x enfermedad", "Application x Disease matrix": "Matriz aplicaci\u00f3n x enfermedad", "Top countries per disease (Africa-linked)": "Principales pa\u00edses por enfermedad (vinculados a \u00c1frica)", "Disease trend over time": "Evoluci\u00f3n de las enfermedades en el tiempo", "Disease intelligence records": "Registros de inteligencia por enfermedad", "Methods by priority": "M\u00e9todos por prioridad", "Applications by priority": "Aplicaciones por prioridad", "Methods \u00d7 Applications": "M\u00e9todos \u00d7 aplicaciones", "Africa-linked AI records \u2014 geographic distribution": "Registros de IA vinculados a \u00c1frica \u2014 distribuci\u00f3n geogr\u00e1fica", "Show country names on the map": "Mostrar nombres de pa\u00edses en el mapa", "By RCC": "Por CCR", "Top countries": "Principales pa\u00edses", "Geography status": "Estado geogr\u00e1fico", "Africa-linked records \u2014 trend over time": "Registros vinculados a \u00c1frica \u2014 evoluci\u00f3n en el tiempo", "Critical alerts": "Alertas cr\u00edticas", "Priority score distribution by disease": "Distribuci\u00f3n de la puntuaci\u00f3n de prioridad por enfermedad", "High priority intelligence records": "Registros de inteligencia de alta prioridad", "All intelligence records": "Todos los registros de inteligencia", "Filtered dataset": "Conjunto de datos filtrado", "All records matching current filters.": "Todos los registros que coinciden con los filtros actuales.", "Africa-linked dataset": "Conjunto de datos vinculado a \u00c1frica", "Records with African geographic signal.": "Registros con se\u00f1al geogr\u00e1fica africana.", "High priority alerts": "Alertas de alta prioridad", "High-priority records (top priority tier).": "Registros de alta prioridad (nivel m\u00e1ximo).", "Download CSV": "Descargar CSV", "Dataset information": "Informaci\u00f3n sobre el conjunto de datos", "Rows = AI method | Columns = disease": "Filas = m\u00e9todo de IA | Columnas = enfermedad", "Rows = Application | Columns = disease": "Filas = aplicaci\u00f3n | Columnas = enfermedad", "No data with country information": "Sin datos con informaci\u00f3n de pa\u00eds", "Top 10 high-priority records": "Top 10 de registros de alta prioridad", "link": "enlace", "Light map": "Mapa claro", "Satellite": "Sat\u00e9lite", "Regional Coordinating Centre (fill)": "Centro de Coordinaci\u00f3n Regional (relleno)", "African geography detected": "Geograf\u00eda africana detectada", "No geography detected": "Ninguna geograf\u00eda detectada", "Loess trend line": "L\u00ednea de tendencia Loess", "Search:": "Buscar:", "Previous": "Anterior", "Next": "Siguiente", "First": "Primero", "Last": "\u00daltimo", "Show": "Mostrar", "entries": "entradas", "No data available in table": "Sin datos en la tabla", "No matching records found": "No se encontraron registros coincidentes", "disease_primary": "Enfermedad", "disease_confidence": "Confianza de la enfermedad", "disease_mentioned": "Enfermedad mencionada", "alert_level": "Nivel de alerta", "title": "t\u00edtulo", "year": "A\u00f1o", "country": "Pa\u00eds", "countries_mentioned": "Pa\u00edses citados", "rcc": "CCR", "method_family_primary": "M\u00e9todo de IA", "application_type_primary": "Aplicaci\u00f3n", "priority_score": "Puntuaci\u00f3n de prioridad", "africa_relevance_level": "Relevancia \u00c1frica", "novelty_level": "Novedad", "tracker_recommendation": "Recomendaci\u00f3n", "signal_profile": "Perfil de se\u00f1al", "url": "Enlace", "Status: research prototype.": "Estado: prototipo de investigaci\u00f3n.", "Automatic screening of public bibliographic metadata (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Titles, authors, journal, year and links are shown; abstracts are not reproduced. Follow the link to read the source.": "Cribado autom\u00e1tico de metadatos bibliogr\u00e1ficos p\u00fablicos (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Se muestran t\u00edtulos, autores, revista, a\u00f1o y enlaces; los res\u00famenes no se reproducen. Siga el enlace para leer la fuente.", "All labels (AI method, application, disease, country, public-health scope, priority) are produced automatically by rules and have NOT been validated for operational decisions.": "Todas las etiquetas (m\u00e9todo de IA, aplicaci\u00f3n, enfermedad, pa\u00eds, \u00e1mbito de salud p\u00fablica, prioridad) se generan autom\u00e1ticamente con reglas y NO han sido validadas para decisiones operativas.", "Pilot validation: two samples of 50 articles, one reviewer, stratified and unweighted; agreement for AI as study method 43/50 in each sample. Scope and geography are the weakest dimensions. No result should be read as a prevalence.": "Validaci\u00f3n piloto: dos muestras de 50 art\u00edculos, un revisor, estratificadas y sin ponderar; concordancia para la IA como m\u00e9todo de estudio 43/50 en cada muestra. El \u00e1mbito y la geograf\u00eda son las dimensiones m\u00e1s d\u00e9biles. Ning\u00fan resultado debe interpretarse como una prevalencia.", "Preprints are the majority of records and are not peer reviewed. Europe PMC was unavailable from 16 to 30 October 2026 and was reloaded on 31 October 2026; records added by the source between runs can be missed until the next run.": "Los preprints son la mayor\u00eda de los registros y no est\u00e1n revisados por pares. Europe PMC no estuvo disponible del 16 al 30 de octubre de 2026 y se recarg\u00f3 el 31 de octubre de 2026; los registros a\u00f1adidos por la fuente entre ejecuciones pueden omitirse hasta la siguiente ejecuci\u00f3n.", "A study country is assigned to only a small share of records; country counts are lower bounds, not the distribution of research.": "Solo una peque\u00f1a parte de los registros tiene un pa\u00eds de estudio asignado; los recuentos por pa\u00eds son m\u00ednimos, no la distribuci\u00f3n de la investigaci\u00f3n.", "Associations are not causal and predictions are not certainties. Alerts are prepared in test mode; test emails are sent only to the author's own addresses. An outbreak mode (active for the 2026 Bundibugyo virus disease outbreak) widens routing of filovirus articles to the response team.": "Las asociaciones no son causales y las predicciones no son certezas. Las alertas se preparan en modo de prueba; los correos de prueba se env\u00edan solo a las direcciones del autor. Un modo de brote (activo para el brote de enfermedad por virus Bundibugyo de 2026) ampl\u00eda el encaminamiento de art\u00edculos sobre filovirus al equipo de respuesta.", "This tool is not an official product of, and is not endorsed by, any institution. The views are those of the author(s).": "Esta herramienta no es un producto oficial ni est\u00e1 respaldada por ninguna instituci\u00f3n. Las opiniones son las del autor o autores.", "Last data load:": "\u00daltima carga de datos:", "PREIS AI Atlas: which AI, on what, where, at what stage?": "Atlas PREIS IA: \u00bfqu\u00e9 IA, sobre qu\u00e9, d\u00f3nde, en qu\u00e9 etapa?", "Objective:": "Objetivo:", "map every AI application used for epidemic surveillance in Africa:": "cartografiar cada aplicaci\u00f3n de IA utilizada en la vigilancia epidemiol\u00f3gica en \u00c1frica:", "WHAT": "QU\u00c9", "it does,": "hace,", "ON WHAT": "SOBRE QU\u00c9", "disease,": "enfermedad,", "WHERE": "D\u00d3NDE", "(RCC, country),": "(CCR, pa\u00eds),", "WHEN": "CU\u00c1NDO", "and at what stage.": "y en qu\u00e9 etapa.", "Literature signals and known platforms; filters on the left apply.": "Se\u00f1ales de la literatura y plataformas conocidas; se aplican los filtros de la izquierda.", "Candidates are keyword signals, not verified systems; registry entries: 0.": "Los candidatos son se\u00f1ales por palabras clave, no sistemas verificados; entradas del registro: 0.", "Tracker data in view:": "Datos del tracker en vista:", "records after filters. Candidates file updated:": "registros tras los filtros. Archivo de candidatos actualizado:", "THE MATRIX: which AI application, on which disease? (number of records)": "LA MATRIZ: \u00bfqu\u00e9 aplicaci\u00f3n de IA, sobre qu\u00e9 enfermedad? (n\u00famero de registros)", "WHAT: which AI applications?": "QU\u00c9: \u00bfqu\u00e9 aplicaciones de IA?", "ON WHAT: which diseases?": "SOBRE QU\u00c9: \u00bfqu\u00e9 enfermedades?", "KNOWN AI SURVEILLANCE PLATFORMS (documented at source, not validated)": "PLATAFORMAS DE IA PARA VIGILANCIA CONOCIDAS (documentadas en la fuente, no validadas)", "Each line cites its public source. Stage is quoted from that source. Documented = stated by the owner or implementer; it is not verified by the registry and says nothing about performance. Country, RCC and disease filters apply when they match.": "Cada l\u00ednea cita su fuente p\u00fablica. La etapa se cita de esa fuente. Documentado = declarado por el propietario o implementador; el registro no lo verifica y no dice nada sobre el rendimiento. Los filtros de pa\u00eds, CCR y enfermedad se aplican cuando coinciden.", "WHERE: Regional Coordinating Centres (fill) and Member States (outline)": "D\u00d3NDE: Centros de Coordinaci\u00f3n Regional (relleno) y Estados miembros (contorno)", "Fill = RCC. Dark outline and circle = Member State with at least one record. Click a country for its applications. Only records with a validated study country are placed; the others are counted in the totals but not on the map.": "Relleno = CCR. Contorno oscuro y c\u00edrculo = Estado miembro con al menos un registro. Haga clic en un pa\u00eds para ver sus aplicaciones. Solo se ubican los registros con pa\u00eds de estudio validado; los dem\u00e1s se cuentan en los totales pero no aparecen en el mapa.", "WHERE: records by RCC": "D\u00d3NDE: registros por CCR", "WHERE: top Member States": "D\u00d3NDE: principales Estados miembros", "WHEN: records by year of publication and application": "CU\u00c1NDO: registros por a\u00f1o de publicaci\u00f3n y aplicaci\u00f3n", "The current year is partial. Publication year is not the year of deployment.": "El a\u00f1o en curso es parcial. El a\u00f1o de publicaci\u00f3n no es el a\u00f1o de despliegue.", "Candidate systems found automatically (stage signal from the wording only)": "Sistemas candidatos encontrados autom\u00e1ticamente (se\u00f1al de etapa solo por la redacci\u00f3n)", "Strength reflects the wording of the title and abstract (for example 'deployed', 'pilot'), not a verified stage. Each candidate must be checked by two independent validators.": "La fuerza refleja la redacci\u00f3n del t\u00edtulo y del resumen (por ejemplo \u00abdesplegado\u00bb, \u00abpiloto\u00bb), no una etapa verificada. Cada candidato debe ser comprobado por dos validadores independientes.", "Leads: applications read by one reader (preliminary, not validated)": "Pistas: aplicaciones le\u00eddas por un solo lector (preliminar, no validado)", "None is confirmed as routine use. A lead is a reason to ask the team, not a finding. Year, country and disease filters apply.": "Ninguna est\u00e1 confirmada como uso rutinario. Una pista es un motivo para preguntar al equipo, no un hallazgo. Se aplican los filtros de a\u00f1o, pa\u00eds y enfermedad.", "Registry status and declaration": "Estado del registro y declaraci\u00f3n", "Statuses:": "Estados:", "declared": "declarado", "(the owner says so),": "(el propietario lo afirma),", "documented": "documentado", "(a public document shows it),": "(un documento p\u00fablico lo demuestra),", "verified": "verificado", "(two independent validators agree). Entries now: declared 0, documented 0, verified 0.": "(dos validadores independientes coinciden). Entradas actuales: declarado 0, documentado 0, verificado 0.", "Phase 1 (done): automatic candidates from the literature. Phase 1b (planned): grey literature. Phase 2 (planned): declarations by RCCs, ministries and partners, then cross-validation.": "Fase 1 (hecha): candidatos autom\u00e1ticos a partir de la literatura. Fase 1b (prevista): literatura gris. Fase 2 (prevista): declaraciones de los CCR, ministerios y socios, y luego validaci\u00f3n cruzada.", "Declare an AI system (about 5 minutes)": "Declarar un sistema de IA (unos 5 minutos)", "- recorded as declared, not verified. Nothing is published with a name or country without consent.": "- registrado como declarado, no verificado. No se publica nada con nombre o pa\u00eds sin consentimiento.", "Africa-linked records, explicit AI (after filters)": "Registros vinculados a \u00c1frica, IA expl\u00edcita (tras filtros)", "Known AI platforms in Africa (documented, core)": "Plataformas de IA conocidas en \u00c1frica (documentadas, centrales)", "Candidates in view (not verified)": "Candidatos en vista (no verificados)", "Strong wording signals": "Se\u00f1ales de redacci\u00f3n fuertes", "Registry entries (declared, documented, verified)": "Entradas del registro (declaradas, documentadas, verificadas)", "No record matches the current filters. Widen the filters in the left bar.": "Ning\u00fan registro coincide con los filtros actuales. Ampl\u00ede los filtros de la barra izquierda.", "No candidate matches the current filters.": "Ning\u00fan candidato coincide con los filtros actuales.", "No platform matches the current filters.": "Ninguna plataforma coincide con los filtros actuales.", "No lead matches the current filters.": "Ninguna pista coincide con los filtros actuales.", "Platforms file not found.": "Archivo de plataformas no encontrado.", "Leads file not found.": "Archivo de pistas no encontrado.", "Documented": "Documentado", "Reported": "Reportado", "Machine Learning": "Aprendizaje autom\u00e1tico", "Bayesian / Statistical Modeling": "Modelizaci\u00f3n bayesiana / estad\u00edstica", "Deep Learning": "Aprendizaje profundo", "General AI": "IA general", "Large Language Models": "Grandes modelos de lenguaje", "Optimization / Hybrid AI": "Optimizaci\u00f3n / IA h\u00edbrida", "Natural Language Processing": "Procesamiento del lenguaje natural", "Computer Vision": "Visi\u00f3n por computadora", "Surveillance / Event Detection": "Vigilancia / detecci\u00f3n de eventos", "Prediction / Forecasting": "Predicci\u00f3n / pron\u00f3stico", "Policy / Decision Support": "Apoyo a pol\u00edticas / decisiones", "Laboratory / Genomic Intelligence": "Laboratorio / inteligencia gen\u00f3mica", "Diagnosis / Clinical Decision Support": "Diagn\u00f3stico / apoyo a la decisi\u00f3n cl\u00ednica", "Risk Mapping / Spatial Analysis": "Mapeo de riesgos / an\u00e1lisis espacial", "Digital Health / Chatbot / Hotline": "Salud digital / chatbot / l\u00ednea de ayuda", "Resource Planning / Response Optimization": "Planificaci\u00f3n de recursos / optimizaci\u00f3n de la respuesta", "HIGH": "Alto", "Medium": "Medio", "MEDIUM": "Medio", "LOW": "Bajo", "None": "Ninguno", "Minimal": "M\u00ednimo", "CRITICAL": "Cr\u00edtico", "Non specifi\u00e9": "No especificado", "Non sp\u00e9cifi\u00e9": "No especificado", "Non specifie": "No especificado", "NA": "NA", "Predictive": "Predictivo", "Operational": "Operativo", "One Health": "Una sola salud", "General": "General", "Immediate review required": "Revisi\u00f3n inmediata requerida", "Monitor closely": "Vigilar de cerca", "Priority review": "Revisi\u00f3n prioritaria", "Archive for background tracking": "Archivar para seguimiento de fondo", "Top Priority": "Prioridad m\u00e1xima", "Watch List": "Lista de vigilancia", "High Impact": "Alto impacto", "Background": "Contexto", "Western": "Occidental", "Eastern": "Oriental", "Central": "Central", "Southern": "Austral", "Northern": "Septentrional", "Mpox": "Mpox", "Ebola": "\u00c9bola", "Marburg": "Marburgo", "COVID-19": "COVID-19", "HIV/AIDS": "VIH/sida", "Malaria": "Paludismo", "Influenza": "Gripe", "Yellow Fever": "Fiebre amarilla", "Dengue": "Dengue", "Cholera": "C\u00f3lera", "Meningitis": "Meningitis", "Tuberculosis": "Tuberculosis", "Lassa fever": "Fiebre de Lassa", "Polio": "Poliomielitis", "Measles": "Sarampi\u00f3n", "Rift Valley Fever": "Fiebre del Valle del Rift", "Algeria": "Argelia", "Angola": "Angola", "Benin": "Ben\u00edn", "Botswana": "Botsuana", "Burkina Faso": "Burkina Faso", "Burundi": "Burundi", "Cabo Verde": "Cabo Verde", "Republic of Cabo Verde": "Cabo Verde", "Cameroon": "Camer\u00fan", "Central African Republic": "Rep\u00fablica Centroafricana", "Chad": "Chad", "Comoros": "Comoras", "Congo": "Congo", "Republic of the Congo": "Rep\u00fablica del Congo", "Democratic Republic of the Congo": "Rep\u00fablica Democr\u00e1tica del Congo", "DR Congo": "RD Congo", "Djibouti": "Yibuti", "Egypt": "Egipto", "Equatorial Guinea": "Guinea Ecuatorial", "Eritrea": "Eritrea", "Eswatini": "Esuatini", "Kingdom of eSwatini": "Esuatini", "Ethiopia": "Etiop\u00eda", "Gabon": "Gab\u00f3n", "Gambia": "Gambia", "The Gambia": "Gambia", "Ghana": "Ghana", "Guinea": "Guinea", "Guinea-Bissau": "Guinea-Bis\u00e1u", "C\u00f4te d'Ivoire": "C\u00f4te d'Ivoire", "Ivory Coast": "C\u00f4te d'Ivoire", "Kenya": "Kenia", "Lesotho": "Lesoto", "Liberia": "Liberia", "Libya": "Libia", "Madagascar": "Madagascar", "Malawi": "Malaui", "Mali": "Mal\u00ed", "Mauritania": "Mauritania", "Mauritius": "Mauricio", "Morocco": "Marruecos", "Mozambique": "Mozambique", "Namibia": "Namibia", "Niger": "N\u00edger", "Nigeria": "Nigeria", "Rwanda": "Ruanda", "Sao Tome and Principe": "Santo Tom\u00e9 y Pr\u00edncipe", "S\u00e3o Tom\u00e9 and Principe": "Santo Tom\u00e9 y Pr\u00edncipe", "Senegal": "Senegal", "Seychelles": "Seychelles", "Sierra Leone": "Sierra Leona", "Somalia": "Somalia", "South Africa": "Sud\u00e1frica", "South Sudan": "Sud\u00e1n del Sur", "Sudan": "Sud\u00e1n", "Tanzania": "Tanzania", "Togo": "Togo", "Tunisia": "T\u00fanez", "Uganda": "Uganda", "Zambia": "Zambia", "Zimbabwe": "Zimbabue", "Western Sahara": "S\u00e1hara Occidental", "preprint": "Preimpresi\u00f3n", "PubMed": "PubMed", "Core": "N\u00facleo", "Adjacent": "Adyacente", "explicit": "expl\u00edcito", "statistical_only": "solo estad\u00edstico", "mention_only": "solo menci\u00f3n", "none": "ninguno", "single_mention": "menci\u00f3n \u00fanica", "text_multiple": "menciones m\u00faltiples", "ambiguous": "ambiguo", "dominant": "dominante", "TRUE": "VERDADERO", "FALSE": "FALSO", "Journal Article": "Art\u00edculo de revista", "Review": "Revisi\u00f3n", "Systematic Review": "Revisi\u00f3n sistem\u00e1tica", "Scoping Review": "Revisi\u00f3n de alcance", "Editorial": "Editorial", "Retracted Publication": "Publicaci\u00f3n retractada", "Preprint": "Preimpresi\u00f3n", "Research Support, Non-U.S. Gov't": "Apoyo a la investigaci\u00f3n (no gobierno de EE. UU.)", "Research Support, N.I.H., Extramural": "Apoyo a la investigaci\u00f3n (NIH, extramural)", "Research Support, U.S. Gov't, Non-P.H.S.": "Apoyo a la investigaci\u00f3n (gobierno de EE. UU., no PHS)", "Antimicrobial resistance": "Resistencia a los antimicrobianos", "HIV": "VIH", "Hepatitis": "Hepatitis", "Diphtheria": "Difteria", "Pertussis": "Tos ferina"}, "ar": {"Overview": "\u0646\u0638\u0631\u0629 \u0639\u0627\u0645\u0629", "AI x Disease x Country": "\u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a x \u0627\u0644\u0645\u0631\u0636 x \u0627\u0644\u0628\u0644\u062f", "Methods & Applications": "\u0627\u0644\u0623\u0633\u0627\u0644\u064a\u0628 \u0648\u0627\u0644\u062a\u0637\u0628\u064a\u0642\u0627\u062a", "Focus Africa": "\u0627\u0644\u062a\u0631\u0643\u064a\u0632 \u0639\u0644\u0649 \u0623\u0641\u0631\u064a\u0642\u064a\u0627", "Priority Intelligence": "\u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0630\u0627\u062a \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "All Records": "\u062c\u0645\u064a\u0639 \u0627\u0644\u0633\u062c\u0644\u0627\u062a", "Downloads": "\u0627\u0644\u062a\u0646\u0632\u064a\u0644\u0627\u062a", "PREIS AI Atlas": "\u0623\u0637\u0644\u0633 PREIS \u0644\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a", "Methods and limits": "\u0627\u0644\u0623\u0633\u0627\u0644\u064a\u0628 \u0648\u0627\u0644\u062d\u062f\u0648\u062f", "Filters": "\u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629", "Year": "\u0627\u0644\u0633\u0646\u0629", "Alert level": "\u0645\u0633\u062a\u0648\u0649 \u0627\u0644\u062a\u0646\u0628\u064a\u0647", "Disease": "\u0627\u0644\u0645\u0631\u0636", "AI method": "\u0623\u0633\u0644\u0648\u0628 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a", "Application": "\u0627\u0644\u062a\u0637\u0628\u064a\u0642", "RCC": "\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a", "Country": "\u0627\u0644\u0628\u0644\u062f", "Signal profile": "\u0646\u0648\u0639 \u0627\u0644\u0625\u0634\u0627\u0631\u0629", "Language of publication": "\u0644\u063a\u0629 \u0627\u0644\u0645\u0646\u0634\u0648\u0631", "All": "\u0627\u0644\u0643\u0644", "English": "\u0627\u0644\u0625\u0646\u062c\u0644\u064a\u0632\u064a\u0629", "French": "\u0627\u0644\u0641\u0631\u0646\u0633\u064a\u0629", "Spanish": "\u0627\u0644\u0625\u0633\u0628\u0627\u0646\u064a\u0629", "Portuguese": "\u0627\u0644\u0628\u0631\u062a\u063a\u0627\u0644\u064a\u0629", "German": "\u0627\u0644\u0623\u0644\u0645\u0627\u0646\u064a\u0629", "Italian": "\u0627\u0644\u0625\u064a\u0637\u0627\u0644\u064a\u0629", "Other": "\u0623\u062e\u0631\u0649", "Arabic": "\u0627\u0644\u0639\u0631\u0628\u064a\u0629", "Swahili": "\u0627\u0644\u0633\u0648\u0627\u062d\u064a\u0644\u064a\u0629", "High": "\u0645\u0631\u062a\u0641\u0639", "Moderate": "\u0645\u0639\u062a\u062f\u0644", "Low": "\u0645\u0646\u062e\u0641\u0636", "Unspecified": "\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "Critical": "\u062d\u0631\u062c", "PREIS AI Tracker \u2014 Dashboard": "PREIS AI Tracker \u2014 \u0644\u0648\u062d\u0629 \u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a", "Monitoring AI applications for public health surveillance in Africa |": "\u0631\u0635\u062f \u062a\u0637\u0628\u064a\u0642\u0627\u062a \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0641\u064a \u062a\u0631\u0635\u0651\u062f \u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0639\u0627\u0645\u0629 \u0641\u064a \u0623\u0641\u0631\u064a\u0642\u064a\u0627 |", "Source:": "\u0627\u0644\u0645\u0635\u062f\u0631:", "Data generated:": "\u062a\u0627\u0631\u064a\u062e \u0625\u0646\u0634\u0627\u0621 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a:", "unknown": "\u063a\u064a\u0631 \u0645\u0639\u0631\u0648\u0641", "Automated classification, not yet validated by human review.": "\u062a\u0635\u0646\u064a\u0641 \u0622\u0644\u064a \u0644\u0645 \u064a\u064f\u062a\u062d\u0642\u0642 \u0645\u0646\u0647 \u0628\u0639\u062f \u0628\u0645\u0631\u0627\u062c\u0639\u0629 \u0628\u0634\u0631\u064a\u0629.", "Disease, method, application and Africa-relevance labels and scores are produced by keyword rules and may contain errors.": "\u062a\u064f\u0646\u062a\u064e\u062c \u062a\u0633\u0645\u064a\u0627\u062a \u0648\u062f\u0631\u062c\u0627\u062a \u0627\u0644\u0645\u0631\u0636 \u0648\u0627\u0644\u0623\u0633\u0644\u0648\u0628 \u0648\u0627\u0644\u062a\u0637\u0628\u064a\u0642 \u0648\u0627\u0644\u0635\u0644\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u0628\u0642\u0648\u0627\u0639\u062f \u0627\u0644\u0643\u0644\u0645\u0627\u062a \u0627\u0644\u0645\u0641\u062a\u0627\u062d\u064a\u0629 \u0648\u0642\u062f \u062a\u062a\u0636\u0645\u0646 \u0623\u062e\u0637\u0627\u0621.", "Records outside the infectious-disease/epidemic scope are excluded from this view.": "\u062a\u064f\u0633\u062a\u0628\u0639\u062f \u0645\u0646 \u0647\u0630\u0627 \u0627\u0644\u0639\u0631\u0636 \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u062e\u0627\u0631\u062c\u0629 \u0639\u0646 \u0646\u0637\u0627\u0642 \u0627\u0644\u0623\u0645\u0631\u0627\u0636 \u0627\u0644\u0645\u0639\u062f\u064a\u0629 \u0648\u0627\u0644\u0623\u0648\u0628\u0626\u0629.", "Do not use as a basis for operational decisions without expert verification.": "\u0644\u0627 \u064a\u062c\u0648\u0632 \u0627\u0633\u062a\u062e\u062f\u0627\u0645\u0647 \u0623\u0633\u0627\u0633\u0627\u064b \u0644\u0642\u0631\u0627\u0631\u0627\u062a \u062a\u0634\u063a\u064a\u0644\u064a\u0629 \u062f\u0648\u0646 \u062a\u062d\u0642\u0642 \u0645\u0646 \u062e\u0628\u064a\u0631.", "AI records": "\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a", "High-priority records": "\u0633\u062c\u0644\u0627\u062a \u0639\u0627\u0644\u064a\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "Diseases under surveillance": "\u0627\u0644\u0623\u0645\u0631\u0627\u0636 \u062a\u062d\u062a \u0627\u0644\u062a\u0631\u0635\u062f", "Priority distribution": "\u0627\u0644\u062a\u0648\u0632\u064a\u0639 \u062d\u0633\u0628 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "Alert level distribution": "\u0627\u0644\u062a\u0648\u0632\u064a\u0639 \u062d\u0633\u0628 \u0645\u0633\u062a\u0648\u0649 \u0627\u0644\u062a\u0646\u0628\u064a\u0647", "AI methods": "\u0623\u0633\u0627\u0644\u064a\u0628 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a", "AI applications": "\u062a\u0637\u0628\u064a\u0642\u0627\u062a \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a", "Africa relevance": "\u0627\u0644\u0635\u0644\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627", "Novelty": "\u0627\u0644\u062c\u062f\u0651\u0629", "Diseases under AI surveillance \u2014 top priority": "\u0627\u0644\u0623\u0645\u0631\u0627\u0636 \u062a\u062d\u062a \u062a\u0631\u0635\u0651\u062f \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u2014 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629 \u0627\u0644\u0642\u0635\u0648\u0649", "AI method x Disease matrix": "\u0645\u0635\u0641\u0648\u0641\u0629 \u0623\u0633\u0644\u0648\u0628 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a x \u0627\u0644\u0645\u0631\u0636", "Application x Disease matrix": "\u0645\u0635\u0641\u0648\u0641\u0629 \u0627\u0644\u062a\u0637\u0628\u064a\u0642 x \u0627\u0644\u0645\u0631\u0636", "Top countries per disease (Africa-linked)": "\u0623\u0647\u0645 \u0627\u0644\u0628\u0644\u062f\u0627\u0646 \u0644\u0643\u0644 \u0645\u0631\u0636 (\u0627\u0644\u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627)", "Disease trend over time": "\u0627\u062a\u062c\u0627\u0647 \u0627\u0644\u0623\u0645\u0631\u0627\u0636 \u0639\u0628\u0631 \u0627\u0644\u0632\u0645\u0646", "Disease intelligence records": "\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a \u062d\u0633\u0628 \u0627\u0644\u0645\u0631\u0636", "Methods by priority": "\u0627\u0644\u0623\u0633\u0627\u0644\u064a\u0628 \u062d\u0633\u0628 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "Applications by priority": "\u0627\u0644\u062a\u0637\u0628\u064a\u0642\u0627\u062a \u062d\u0633\u0628 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "Methods \u00d7 Applications": "\u0627\u0644\u0623\u0633\u0627\u0644\u064a\u0628 \u00d7 \u0627\u0644\u062a\u0637\u0628\u064a\u0642\u0627\u062a", "Africa-linked AI records \u2014 geographic distribution": "\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u2014 \u0627\u0644\u062a\u0648\u0632\u064a\u0639 \u0627\u0644\u062c\u063a\u0631\u0627\u0641\u064a", "Show country names on the map": "\u0625\u0638\u0647\u0627\u0631 \u0623\u0633\u0645\u0627\u0621 \u0627\u0644\u0628\u0644\u062f\u0627\u0646 \u0639\u0644\u0649 \u0627\u0644\u062e\u0631\u064a\u0637\u0629", "By RCC": "\u062d\u0633\u0628 \u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a", "Top countries": "\u0623\u0647\u0645 \u0627\u0644\u0628\u0644\u062f\u0627\u0646", "Geography status": "\u0627\u0644\u0648\u0636\u0639 \u0627\u0644\u062c\u063a\u0631\u0627\u0641\u064a", "Africa-linked records \u2014 trend over time": "\u0627\u0644\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u2014 \u0627\u0644\u0627\u062a\u062c\u0627\u0647 \u0639\u0628\u0631 \u0627\u0644\u0632\u0645\u0646", "Critical alerts": "\u062a\u0646\u0628\u064a\u0647\u0627\u062a \u062d\u0631\u062c\u0629", "Priority score distribution by disease": "\u062a\u0648\u0632\u064a\u0639 \u062f\u0631\u062c\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629 \u062d\u0633\u0628 \u0627\u0644\u0645\u0631\u0636", "High priority intelligence records": "\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0639\u0627\u0644\u064a\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "All intelligence records": "\u062c\u0645\u064a\u0639 \u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a", "Filtered dataset": "\u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0645\u0635\u0641\u0651\u0627\u0629", "All records matching current filters.": "\u062c\u0645\u064a\u0639 \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u0645\u0637\u0627\u0628\u0642\u0629 \u0644\u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0627\u0644\u062d\u0627\u0644\u064a\u0629.", "Africa-linked dataset": "\u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627", "Records with African geographic signal.": "\u0633\u062c\u0644\u0627\u062a \u0630\u0627\u062a \u0625\u0634\u0627\u0631\u0629 \u062c\u063a\u0631\u0627\u0641\u064a\u0629 \u0623\u0641\u0631\u064a\u0642\u064a\u0629.", "High priority alerts": "\u062a\u0646\u0628\u064a\u0647\u0627\u062a \u0639\u0627\u0644\u064a\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "High-priority records (top priority tier).": "\u0633\u062c\u0644\u0627\u062a \u0639\u0627\u0644\u064a\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629 (\u0623\u0639\u0644\u0649 \u0645\u0633\u062a\u0648\u0649).", "Download CSV": "\u062a\u0646\u0632\u064a\u0644 CSV", "Dataset information": "\u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0639\u0646 \u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a", "Rows = AI method | Columns = disease": "\u0627\u0644\u0635\u0641\u0648\u0641 = \u0623\u0633\u0644\u0648\u0628 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a | \u0627\u0644\u0623\u0639\u0645\u062f\u0629 = \u0627\u0644\u0645\u0631\u0636", "Rows = Application | Columns = disease": "\u0627\u0644\u0635\u0641\u0648\u0641 = \u0627\u0644\u062a\u0637\u0628\u064a\u0642 | \u0627\u0644\u0623\u0639\u0645\u062f\u0629 = \u0627\u0644\u0645\u0631\u0636", "No data with country information": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u062a\u062a\u0636\u0645\u0646 \u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0639\u0646 \u0627\u0644\u0628\u0644\u062f", "Top 10 high-priority records": "\u0623\u0639\u0644\u0649 10 \u0633\u062c\u0644\u0627\u062a \u0630\u0627\u062a \u0623\u0648\u0644\u0648\u064a\u0629 \u0639\u0627\u0644\u064a\u0629", "link": "\u0631\u0627\u0628\u0637", "Light map": "\u062e\u0631\u064a\u0637\u0629 \u0641\u0627\u062a\u062d\u0629", "Satellite": "\u0642\u0645\u0631 \u0635\u0646\u0627\u0639\u064a", "Regional Coordinating Centre (fill)": "\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a (\u062a\u0639\u0628\u0626\u0629)", "African geography detected": "\u062a\u0645 \u0631\u0635\u062f \u062c\u063a\u0631\u0627\u0641\u064a\u0627 \u0623\u0641\u0631\u064a\u0642\u064a\u0629", "No geography detected": "\u0644\u0645 \u064a\u062a\u0645 \u0631\u0635\u062f \u062c\u063a\u0631\u0627\u0641\u064a\u0627", "Loess trend line": "\u062e\u0637 \u0627\u062a\u062c\u0627\u0647 \u0644\u0648\u064a\u0633", "Search:": "\u0628\u062d\u062b:", "Previous": "\u0627\u0644\u0633\u0627\u0628\u0642", "Next": "\u0627\u0644\u062a\u0627\u0644\u064a", "First": "\u0627\u0644\u0623\u0648\u0644", "Last": "\u0627\u0644\u0623\u062e\u064a\u0631", "Show": "\u0639\u0631\u0636", "entries": "\u0625\u062f\u062e\u0627\u0644\u0627\u062a", "No data available in table": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u0641\u064a \u0627\u0644\u062c\u062f\u0648\u0644", "No matching records found": "\u0644\u0645 \u064a\u064f\u0639\u062b\u0631 \u0639\u0644\u0649 \u0633\u062c\u0644\u0627\u062a \u0645\u0637\u0627\u0628\u0642\u0629", "disease_primary": "\u0627\u0644\u0645\u0631\u0636", "disease_confidence": "\u0645\u0648\u062b\u0648\u0642\u064a\u0629 \u0627\u0644\u0645\u0631\u0636", "disease_mentioned": "\u0627\u0644\u0645\u0631\u0636 \u0627\u0644\u0645\u0630\u0643\u0648\u0631", "alert_level": "\u0645\u0633\u062a\u0648\u0649 \u0627\u0644\u062a\u0646\u0628\u064a\u0647", "title": "\u0627\u0644\u0639\u0646\u0648\u0627\u0646", "year": "\u0627\u0644\u0633\u0646\u0629", "country": "\u0627\u0644\u0628\u0644\u062f", "countries_mentioned": "\u0627\u0644\u0628\u0644\u062f\u0627\u0646 \u0627\u0644\u0645\u0630\u0643\u0648\u0631\u0629", "rcc": "\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642", "method_family_primary": "\u0623\u0633\u0644\u0648\u0628 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a", "application_type_primary": "\u0627\u0644\u062a\u0637\u0628\u064a\u0642", "priority_score": "\u062f\u0631\u062c\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629", "africa_relevance_level": "\u0627\u0644\u0635\u0644\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627", "novelty_level": "\u0627\u0644\u062c\u062f\u0651\u0629", "tracker_recommendation": "\u0627\u0644\u062a\u0648\u0635\u064a\u0629", "signal_profile": "\u0646\u0648\u0639 \u0627\u0644\u0625\u0634\u0627\u0631\u0629", "url": "\u0627\u0644\u0631\u0627\u0628\u0637", "Status: research prototype.": "\u0627\u0644\u062d\u0627\u0644\u0629: \u0646\u0645\u0648\u0630\u062c \u0623\u0648\u0644\u064a \u0628\u062d\u062b\u064a.", "Automatic screening of public bibliographic metadata (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Titles, authors, journal, year and links are shown; abstracts are not reproduced. Follow the link to read the source.": "\u0641\u0631\u0632 \u0622\u0644\u064a \u0644\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0648\u0635\u0641\u064a\u0629 \u0627\u0644\u0628\u0628\u0644\u064a\u0648\u063a\u0631\u0627\u0641\u064a\u0629 \u0627\u0644\u0639\u0627\u0645\u0629 (PubMed \u0648Europe PMC \u0648bioRxiv \u0648medRxiv \u0648arXiv). \u062a\u064f\u0639\u0631\u0636 \u0627\u0644\u0639\u0646\u0627\u0648\u064a\u0646 \u0648\u0627\u0644\u0645\u0624\u0644\u0641\u0648\u0646 \u0648\u0627\u0644\u0645\u062c\u0644\u0629 \u0648\u0627\u0644\u0633\u0646\u0629 \u0648\u0627\u0644\u0631\u0648\u0627\u0628\u0637\u060c \u0648\u0644\u0627 \u062a\u064f\u0639\u0627\u062f \u0627\u0644\u0645\u0644\u062e\u0635\u0627\u062a. \u0627\u062a\u0628\u0639 \u0627\u0644\u0631\u0627\u0628\u0637 \u0644\u0642\u0631\u0627\u0621\u0629 \u0627\u0644\u0645\u0635\u062f\u0631.", "All labels (AI method, application, disease, country, public-health scope, priority) are produced automatically by rules and have NOT been validated for operational decisions.": "\u062c\u0645\u064a\u0639 \u0627\u0644\u062a\u0633\u0645\u064a\u0627\u062a (\u0623\u0633\u0644\u0648\u0628 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a\u060c \u0627\u0644\u062a\u0637\u0628\u064a\u0642\u060c \u0627\u0644\u0645\u0631\u0636\u060c \u0627\u0644\u0628\u0644\u062f\u060c \u0646\u0637\u0627\u0642 \u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0639\u0627\u0645\u0629\u060c \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629) \u062a\u064f\u0646\u062a\u064e\u062c \u0622\u0644\u064a\u0627\u064b \u0628\u0642\u0648\u0627\u0639\u062f \u0648\u0644\u0645 \u064a\u064f\u062a\u062d\u0642\u0642 \u0645\u0646\u0647\u0627 \u0644\u0627\u062a\u062e\u0627\u0630 \u0642\u0631\u0627\u0631\u0627\u062a \u062a\u0634\u063a\u064a\u0644\u064a\u0629.", "Pilot validation: two samples of 50 articles, one reviewer, stratified and unweighted; agreement for AI as study method 43/50 in each sample. Scope and geography are the weakest dimensions. No result should be read as a prevalence.": "\u0627\u0644\u062a\u062d\u0642\u0642 \u0627\u0644\u062a\u062c\u0631\u064a\u0628\u064a: \u0639\u064a\u0646\u062a\u0627\u0646 \u0645\u0646 50 \u0645\u0642\u0627\u0644\u0627\u064b\u060c \u0645\u0631\u0627\u062c\u0639 \u0648\u0627\u062d\u062f\u060c \u0637\u0628\u0642\u064a\u062a\u0627\u0646 \u0648\u063a\u064a\u0631 \u0645\u0631\u062c\u0651\u062d\u062a\u064a\u0646\u061b \u0627\u0644\u0627\u062a\u0641\u0627\u0642 \u0639\u0644\u0649 \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0643\u0623\u0633\u0644\u0648\u0628 \u062f\u0631\u0627\u0633\u0629 43/50 \u0641\u064a \u0643\u0644 \u0639\u064a\u0646\u0629. \u0627\u0644\u0646\u0637\u0627\u0642 \u0648\u0627\u0644\u062c\u063a\u0631\u0627\u0641\u064a\u0627 \u0623\u0636\u0639\u0641 \u0627\u0644\u0623\u0628\u0639\u0627\u062f. \u0644\u0627 \u064a\u0646\u0628\u063a\u064a \u0642\u0631\u0627\u0621\u0629 \u0623\u064a \u0646\u062a\u064a\u062c\u0629 \u0639\u0644\u0649 \u0623\u0646\u0647\u0627 \u0645\u0639\u062f\u0644 \u0627\u0646\u062a\u0634\u0627\u0631.", "Preprints are the majority of records and are not peer reviewed. Europe PMC was unavailable from 16 to 30 October 2026 and was reloaded on 31 October 2026; records added by the source between runs can be missed until the next run.": "\u0627\u0644\u0646\u0633\u062e \u0627\u0644\u062a\u0645\u0647\u064a\u062f\u064a\u0629 \u0647\u064a \u063a\u0627\u0644\u0628\u064a\u0629 \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u0648\u0644\u0645 \u062a\u062e\u0636\u0639 \u0644\u0645\u0631\u0627\u062c\u0639\u0629 \u0627\u0644\u0623\u0642\u0631\u0627\u0646. \u0643\u0627\u0646 Europe PMC \u063a\u064a\u0631 \u0645\u062a\u0627\u062d \u0645\u0646 16 \u0625\u0644\u0649 30 \u0623\u0643\u062a\u0648\u0628\u0631 2026 \u0648\u0623\u064f\u0639\u064a\u062f \u062a\u062d\u0645\u064a\u0644\u0647 \u0641\u064a 31 \u0623\u0643\u062a\u0648\u0628\u0631 2026\u061b \u0648\u0642\u062f \u062a\u0641\u0648\u062a \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u0627\u0644\u062a\u064a \u064a\u0636\u064a\u0641\u0647\u0627 \u0627\u0644\u0645\u0635\u062f\u0631 \u0628\u064a\u0646 \u0627\u0644\u062a\u0634\u063a\u064a\u0644\u0627\u062a \u062d\u062a\u0649 \u0627\u0644\u062a\u0634\u063a\u064a\u0644 \u0627\u0644\u062a\u0627\u0644\u064a.", "A study country is assigned to only a small share of records; country counts are lower bounds, not the distribution of research.": "\u0644\u0627 \u064a\u064f\u0633\u0646\u064e\u062f \u0628\u0644\u062f \u062f\u0631\u0627\u0633\u0629 \u0625\u0644\u0627 \u0625\u0644\u0649 \u0646\u0633\u0628\u0629 \u0635\u063a\u064a\u0631\u0629 \u0645\u0646 \u0627\u0644\u0633\u062c\u0644\u0627\u062a\u061b \u0648\u0623\u0639\u062f\u0627\u062f \u0627\u0644\u0628\u0644\u062f\u0627\u0646 \u062d\u062f\u0648\u062f \u062f\u0646\u064a\u0627 \u0648\u0644\u064a\u0633\u062a \u062a\u0648\u0632\u064a\u0639 \u0627\u0644\u0628\u062d\u0648\u062b.", "Associations are not causal and predictions are not certainties. Alerts are prepared in test mode; test emails are sent only to the author's own addresses. An outbreak mode (active for the 2026 Bundibugyo virus disease outbreak) widens routing of filovirus articles to the response team.": "\u0627\u0644\u0627\u0631\u062a\u0628\u0627\u0637\u0627\u062a \u0644\u064a\u0633\u062a \u0633\u0628\u0628\u064a\u0629 \u0648\u0627\u0644\u062a\u0646\u0628\u0624\u0627\u062a \u0644\u064a\u0633\u062a \u064a\u0642\u064a\u0646\u064a\u0629. \u062a\u064f\u0639\u064e\u062f\u0651 \u0627\u0644\u062a\u0646\u0628\u064a\u0647\u0627\u062a \u0641\u064a \u0648\u0636\u0639 \u0627\u0644\u0627\u062e\u062a\u0628\u0627\u0631\u061b \u0648\u062a\u064f\u0631\u0633\u0644 \u0631\u0633\u0627\u0626\u0644 \u0627\u0644\u0627\u062e\u062a\u0628\u0627\u0631 \u0625\u0644\u0649 \u0639\u0646\u0627\u0648\u064a\u0646 \u0627\u0644\u0645\u0624\u0644\u0641 \u0648\u062d\u062f\u0647. \u0648\u064a\u0648\u0633\u0651\u0639 \u0648\u0636\u0639 \u0627\u0644\u0641\u0627\u0634\u064a\u0629 (\u0627\u0644\u0645\u0641\u0639\u0651\u0644 \u0644\u0641\u0627\u0634\u064a\u0629 \u0645\u0631\u0636 \u0641\u064a\u0631\u0648\u0633 \u0628\u0648\u0646\u062f\u064a\u0628\u0648\u063a\u064a\u0648 2026) \u062a\u0648\u062c\u064a\u0647 \u0627\u0644\u0645\u0642\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u062a\u0639\u0644\u0642\u0629 \u0628\u0627\u0644\u0641\u064a\u0631\u0648\u0633\u0627\u062a \u0627\u0644\u062e\u064a\u0637\u064a\u0629 \u0625\u0644\u0649 \u0641\u0631\u064a\u0642 \u0627\u0644\u0627\u0633\u062a\u062c\u0627\u0628\u0629.", "This tool is not an official product of, and is not endorsed by, any institution. The views are those of the author(s).": "\u0647\u0630\u0647 \u0627\u0644\u0623\u062f\u0627\u0629 \u0644\u064a\u0633\u062a \u0645\u0646\u062a\u062c\u0627\u064b \u0631\u0633\u0645\u064a\u0627\u064b \u0644\u0623\u064a \u0645\u0624\u0633\u0633\u0629 \u0648\u0644\u0627 \u062a\u062d\u0638\u0649 \u0628\u062a\u0623\u064a\u064a\u062f\u0647\u0627. \u0627\u0644\u0622\u0631\u0627\u0621 \u0627\u0644\u0645\u0639\u0628\u064e\u0651\u0631 \u0639\u0646\u0647\u0627 \u0647\u064a \u0622\u0631\u0627\u0621 \u0627\u0644\u0645\u0624\u0644\u0641 \u0623\u0648 \u0627\u0644\u0645\u0624\u0644\u0641\u064a\u0646.", "Last data load:": "\u0622\u062e\u0631 \u062a\u062d\u0645\u064a\u0644 \u0644\u0644\u0628\u064a\u0627\u0646\u0627\u062a:", "PREIS AI Atlas: which AI, on what, where, at what stage?": "\u0623\u0637\u0644\u0633 PREIS \u0644\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a: \u0623\u064a \u0630\u0643\u0627\u0621 \u0627\u0635\u0637\u0646\u0627\u0639\u064a\u060c \u0648\u0639\u0644\u0649 \u0645\u0627\u0630\u0627\u060c \u0648\u0623\u064a\u0646\u060c \u0648\u0641\u064a \u0623\u064a \u0645\u0631\u062d\u0644\u0629\u061f", "Objective:": "\u0627\u0644\u0647\u062f\u0641:", "map every AI application used for epidemic surveillance in Africa:": "\u0631\u0633\u0645 \u062e\u0631\u064a\u0637\u0629 \u0644\u0643\u0644 \u062a\u0637\u0628\u064a\u0642 \u0644\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u064a\u064f\u0633\u062a\u062e\u062f\u0645 \u0641\u064a \u0627\u0644\u062a\u0631\u0635\u062f \u0627\u0644\u0648\u0628\u0627\u0626\u064a \u0641\u064a \u0623\u0641\u0631\u064a\u0642\u064a\u0627:", "WHAT": "\u0645\u0627\u0630\u0627", "it does,": "\u062a\u0641\u0639\u0644\u060c", "ON WHAT": "\u0639\u0644\u0649 \u0645\u0627\u0630\u0627", "disease,": "\u0627\u0644\u0645\u0631\u0636\u060c", "WHERE": "\u0623\u064a\u0646", "(RCC, country),": "(\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u060c \u0627\u0644\u0628\u0644\u062f)\u060c", "WHEN": "\u0645\u062a\u0649", "and at what stage.": "\u0648\u0641\u064a \u0623\u064a \u0645\u0631\u062d\u0644\u0629.", "Literature signals and known platforms; filters on the left apply.": "\u0625\u0634\u0627\u0631\u0627\u062a \u0645\u0646 \u0627\u0644\u0623\u062f\u0628\u064a\u0627\u062a \u0648\u0627\u0644\u0645\u0646\u0635\u0627\u062a \u0627\u0644\u0645\u0639\u0631\u0648\u0641\u0629\u061b \u0648\u062a\u0646\u0637\u0628\u0642 \u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0639\u0644\u0649 \u0627\u0644\u064a\u0633\u0627\u0631.", "Candidates are keyword signals, not verified systems; registry entries: 0.": "\u0627\u0644\u0645\u0631\u0634\u062d\u0648\u0646 \u0625\u0634\u0627\u0631\u0627\u062a \u0628\u0627\u0644\u0643\u0644\u0645\u0627\u062a \u0627\u0644\u0645\u0641\u062a\u0627\u062d\u064a\u0629 \u0648\u0644\u064a\u0633\u0648\u0627 \u0623\u0646\u0638\u0645\u0629 \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642\u0627\u064b \u0645\u0646\u0647\u0627\u061b \u0625\u062f\u062e\u0627\u0644\u0627\u062a \u0627\u0644\u0633\u062c\u0644: 0.", "Tracker data in view:": "\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0645\u062a\u062a\u0628\u0639 \u0627\u0644\u0645\u0639\u0631\u0648\u0636\u0629:", "records after filters. Candidates file updated:": "\u0633\u062c\u0644\u0627\u064b \u0628\u0639\u062f \u0627\u0644\u062a\u0635\u0641\u064a\u0629. \u062a\u0627\u0631\u064a\u062e \u062a\u062d\u062f\u064a\u062b \u0645\u0644\u0641 \u0627\u0644\u0645\u0631\u0634\u062d\u064a\u0646:", "THE MATRIX: which AI application, on which disease? (number of records)": "\u0627\u0644\u0645\u0635\u0641\u0648\u0641\u0629: \u0623\u064a \u062a\u0637\u0628\u064a\u0642 \u0644\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a\u060c \u0648\u0639\u0644\u0649 \u0623\u064a \u0645\u0631\u0636\u061f (\u0639\u062f\u062f \u0627\u0644\u0633\u062c\u0644\u0627\u062a)", "WHAT: which AI applications?": "\u0645\u0627\u0630\u0627: \u0623\u064a \u062a\u0637\u0628\u064a\u0642\u0627\u062a \u0644\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a\u061f", "ON WHAT: which diseases?": "\u0639\u0644\u0649 \u0645\u0627\u0630\u0627: \u0623\u064a \u0623\u0645\u0631\u0627\u0636\u061f", "KNOWN AI SURVEILLANCE PLATFORMS (documented at source, not validated)": "\u0645\u0646\u0635\u0627\u062a \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0645\u0639\u0631\u0648\u0641\u0629 \u0644\u0644\u062a\u0631\u0635\u062f (\u0645\u0648\u062b\u0651\u0642\u0629 \u0641\u064a \u0627\u0644\u0645\u0635\u062f\u0631 \u0648\u063a\u064a\u0631 \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642 \u0645\u0646\u0647\u0627)", "Each line cites its public source. Stage is quoted from that source. Documented = stated by the owner or implementer; it is not verified by the registry and says nothing about performance. Country, RCC and disease filters apply when they match.": "\u064a\u0630\u0643\u0631 \u0643\u0644 \u0633\u0637\u0631 \u0645\u0635\u062f\u0631\u0647 \u0627\u0644\u0639\u0627\u0645. \u062a\u064f\u0642\u062a\u0628\u0633 \u0627\u0644\u0645\u0631\u062d\u0644\u0629 \u0645\u0646 \u0630\u0644\u0643 \u0627\u0644\u0645\u0635\u062f\u0631. \u0645\u0648\u062b\u0651\u0642 = \u0635\u0631\u0651\u062d \u0628\u0647 \u0627\u0644\u0645\u0627\u0644\u0643 \u0623\u0648 \u0627\u0644\u0645\u0646\u0641\u0651\u0630\u061b \u0648\u0644\u0627 \u064a\u062a\u062d\u0642\u0642 \u0645\u0646\u0647 \u0627\u0644\u0633\u062c\u0644 \u0648\u0644\u0627 \u064a\u062f\u0644 \u0639\u0644\u0649 \u0627\u0644\u0623\u062f\u0627\u0621. \u0648\u062a\u0646\u0637\u0628\u0642 \u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0628\u0627\u0644\u0628\u0644\u062f \u0648\u0627\u0644\u0645\u0631\u0643\u0632 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a \u0648\u0627\u0644\u0645\u0631\u0636 \u0639\u0646\u062f \u0627\u0644\u062a\u0637\u0627\u0628\u0642.", "WHERE: Regional Coordinating Centres (fill) and Member States (outline)": "\u0623\u064a\u0646: \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 (\u062a\u0639\u0628\u0626\u0629) \u0648\u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621 (\u062d\u062f\u0648\u062f)", "Fill = RCC. Dark outline and circle = Member State with at least one record. Click a country for its applications. Only records with a validated study country are placed; the others are counted in the totals but not on the map.": "\u0627\u0644\u062a\u0639\u0628\u0626\u0629 = \u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a. \u0627\u0644\u062d\u062f \u0627\u0644\u062f\u0627\u0643\u0646 \u0648\u0627\u0644\u062f\u0627\u0626\u0631\u0629 = \u062f\u0648\u0644\u0629 \u0639\u0636\u0648 \u0644\u062f\u064a\u0647\u0627 \u0633\u062c\u0644 \u0648\u0627\u062d\u062f \u0639\u0644\u0649 \u0627\u0644\u0623\u0642\u0644. \u0627\u0646\u0642\u0631 \u0639\u0644\u0649 \u0628\u0644\u062f \u0644\u0639\u0631\u0636 \u062a\u0637\u0628\u064a\u0642\u0627\u062a\u0647. \u0644\u0627 \u062a\u064f\u0648\u0636\u064e\u0639 \u0639\u0644\u0649 \u0627\u0644\u062e\u0631\u064a\u0637\u0629 \u0625\u0644\u0627 \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u0630\u0627\u062a \u0628\u0644\u062f \u0627\u0644\u062f\u0631\u0627\u0633\u0629 \u0627\u0644\u0645\u0639\u062a\u0645\u062f\u061b \u0648\u062a\u064f\u062d\u062a\u0633\u0628 \u0627\u0644\u0623\u062e\u0631\u0649 \u0641\u064a \u0627\u0644\u0625\u062c\u0645\u0627\u0644\u064a\u0627\u062a \u062f\u0648\u0646 \u0638\u0647\u0648\u0631\u0647\u0627 \u0639\u0644\u0649 \u0627\u0644\u062e\u0631\u064a\u0637\u0629.", "WHERE: records by RCC": "\u0623\u064a\u0646: \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u062d\u0633\u0628 \u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a", "WHERE: top Member States": "\u0623\u064a\u0646: \u0623\u0628\u0631\u0632 \u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621", "WHEN: records by year of publication and application": "\u0645\u062a\u0649: \u0627\u0644\u0633\u062c\u0644\u0627\u062a \u062d\u0633\u0628 \u0633\u0646\u0629 \u0627\u0644\u0646\u0634\u0631 \u0648\u0627\u0644\u062a\u0637\u0628\u064a\u0642", "The current year is partial. Publication year is not the year of deployment.": "\u0627\u0644\u0633\u0646\u0629 \u0627\u0644\u062d\u0627\u0644\u064a\u0629 \u062c\u0632\u0626\u064a\u0629. \u0648\u0633\u0646\u0629 \u0627\u0644\u0646\u0634\u0631 \u0644\u064a\u0633\u062a \u0633\u0646\u0629 \u0627\u0644\u0646\u0634\u0631 \u0627\u0644\u0645\u064a\u062f\u0627\u0646\u064a.", "Candidate systems found automatically (stage signal from the wording only)": "\u0623\u0646\u0638\u0645\u0629 \u0645\u0631\u0634\u062d\u0629 \u0639\u064f\u062b\u0631 \u0639\u0644\u064a\u0647\u0627 \u0622\u0644\u064a\u0627\u064b (\u0625\u0634\u0627\u0631\u0629 \u0627\u0644\u0645\u0631\u062d\u0644\u0629 \u0645\u0646 \u0627\u0644\u0635\u064a\u0627\u063a\u0629 \u0641\u0642\u0637)", "Strength reflects the wording of the title and abstract (for example 'deployed', 'pilot'), not a verified stage. Each candidate must be checked by two independent validators.": "\u062a\u0639\u0643\u0633 \u0627\u0644\u0642\u0648\u0629 \u0635\u064a\u0627\u063a\u0629 \u0627\u0644\u0639\u0646\u0648\u0627\u0646 \u0648\u0627\u0644\u0645\u0644\u062e\u0635 (\u0645\u062b\u0644 \u00ab\u0645\u0646\u0634\u0648\u0631\u00bb \u0648\u00ab\u062a\u062c\u0631\u064a\u0628\u064a\u00bb) \u0648\u0644\u064a\u0633 \u0645\u0631\u062d\u0644\u0629 \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642\u0627\u064b \u0645\u0646\u0647\u0627. \u0648\u064a\u062c\u0628 \u0623\u0646 \u064a\u062a\u062d\u0642\u0642 \u0645\u0646 \u0643\u0644 \u0645\u0631\u0634\u062d \u0645\u062f\u0642\u0651\u0642\u0627\u0646 \u0645\u0633\u062a\u0642\u0644\u0627\u0646.", "Leads: applications read by one reader (preliminary, not validated)": "\u062e\u064a\u0648\u0637 \u0623\u0648\u0644\u064a\u0629: \u062a\u0637\u0628\u064a\u0642\u0627\u062a \u0642\u0631\u0623\u0647\u0627 \u0642\u0627\u0631\u0626 \u0648\u0627\u062d\u062f (\u0623\u0648\u0644\u064a \u0648\u063a\u064a\u0631 \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642 \u0645\u0646\u0647)", "None is confirmed as routine use. A lead is a reason to ask the team, not a finding. Year, country and disease filters apply.": "\u0644\u0627 \u064a\u064f\u0624\u0643\u064e\u0651\u062f \u0623\u064a \u0645\u0646\u0647\u0627 \u0627\u0633\u062a\u062e\u062f\u0627\u0645\u0627\u064b \u0631\u0648\u062a\u064a\u0646\u064a\u0627\u064b. \u0627\u0644\u062e\u064a\u0637 \u0627\u0644\u0623\u0648\u0644\u064a \u0633\u0628\u0628 \u0644\u0633\u0624\u0627\u0644 \u0627\u0644\u0641\u0631\u064a\u0642 \u0648\u0644\u064a\u0633 \u0646\u062a\u064a\u062c\u0629. \u0648\u062a\u0646\u0637\u0628\u0642 \u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0628\u0627\u0644\u0633\u0646\u0629 \u0648\u0627\u0644\u0628\u0644\u062f \u0648\u0627\u0644\u0645\u0631\u0636.", "Registry status and declaration": "\u062d\u0627\u0644\u0629 \u0627\u0644\u0633\u062c\u0644 \u0648\u0627\u0644\u0625\u0642\u0631\u0627\u0631", "Statuses:": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a:", "declared": "\u0645\u064f\u0639\u0644\u064e\u0646", "(the owner says so),": "(\u0627\u0644\u0645\u0627\u0644\u0643 \u064a\u0642\u0648\u0644 \u0630\u0644\u0643)\u060c", "documented": "\u0645\u0648\u062b\u0651\u0642", "(a public document shows it),": "(\u0648\u062b\u064a\u0642\u0629 \u0639\u0627\u0645\u0629 \u062a\u062b\u0628\u062a\u0647)\u060c", "verified": "\u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642 \u0645\u0646\u0647", "(two independent validators agree). Entries now: declared 0, documented 0, verified 0.": "(\u064a\u062a\u0641\u0642 \u0645\u062f\u0642\u0651\u0642\u0627\u0646 \u0645\u0633\u062a\u0642\u0644\u0627\u0646). \u0627\u0644\u0625\u062f\u062e\u0627\u0644\u0627\u062a \u0627\u0644\u062d\u0627\u0644\u064a\u0629: \u0645\u064f\u0639\u0644\u064e\u0646 0\u060c \u0645\u0648\u062b\u0651\u0642 0\u060c \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642 \u0645\u0646\u0647 0.", "Phase 1 (done): automatic candidates from the literature. Phase 1b (planned): grey literature. Phase 2 (planned): declarations by RCCs, ministries and partners, then cross-validation.": "\u0627\u0644\u0645\u0631\u062d\u0644\u0629 1 (\u0623\u064f\u0646\u062c\u0632\u062a): \u0645\u0631\u0634\u062d\u0648\u0646 \u0622\u0644\u064a\u0648\u0646 \u0645\u0646 \u0627\u0644\u0623\u062f\u0628\u064a\u0627\u062a. \u0627\u0644\u0645\u0631\u062d\u0644\u0629 1\u0628 (\u0645\u062e\u0637\u0637\u0629): \u0627\u0644\u0623\u062f\u0628\u064a\u0627\u062a \u0627\u0644\u0631\u0645\u0627\u062f\u064a\u0629. \u0627\u0644\u0645\u0631\u062d\u0644\u0629 2 (\u0645\u062e\u0637\u0637\u0629): \u0625\u0642\u0631\u0627\u0631\u0627\u062a \u0645\u0646 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0648\u0627\u0644\u0648\u0632\u0627\u0631\u0627\u062a \u0648\u0627\u0644\u0634\u0631\u0643\u0627\u0621\u060c \u062b\u0645 \u0627\u0644\u062a\u062d\u0642\u0642 \u0627\u0644\u0645\u062a\u0628\u0627\u062f\u0644.", "Declare an AI system (about 5 minutes)": "\u0627\u0644\u0625\u0642\u0631\u0627\u0631 \u0628\u0646\u0638\u0627\u0645 \u0630\u0643\u0627\u0621 \u0627\u0635\u0637\u0646\u0627\u0639\u064a (\u0646\u062d\u0648 5 \u062f\u0642\u0627\u0626\u0642)", "- recorded as declared, not verified. Nothing is published with a name or country without consent.": "- \u064a\u064f\u0633\u062c\u064e\u0651\u0644 \u0643\u0645\u0627 \u0623\u064f\u0639\u0644\u0646 \u0648\u0644\u064a\u0633 \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642\u0627\u064b \u0645\u0646\u0647. \u0644\u0627 \u064a\u064f\u0646\u0634\u0631 \u0634\u064a\u0621 \u0628\u0627\u0633\u0645 \u0623\u0648 \u0628\u0644\u062f \u062f\u0648\u0646 \u0645\u0648\u0627\u0641\u0642\u0629.", "Africa-linked records, explicit AI (after filters)": "\u0633\u062c\u0644\u0627\u062a \u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627\u060c \u0630\u0643\u0627\u0621 \u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0635\u0631\u064a\u062d (\u0628\u0639\u062f \u0627\u0644\u062a\u0635\u0641\u064a\u0629)", "Known AI platforms in Africa (documented, core)": "\u0645\u0646\u0635\u0627\u062a \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0645\u0639\u0631\u0648\u0641\u0629 \u0641\u064a \u0623\u0641\u0631\u064a\u0642\u064a\u0627 (\u0645\u0648\u062b\u0651\u0642\u0629\u060c \u0623\u0633\u0627\u0633\u064a\u0629)", "Candidates in view (not verified)": "\u0627\u0644\u0645\u0631\u0634\u062d\u0648\u0646 \u0627\u0644\u0645\u0639\u0631\u0648\u0636\u0648\u0646 (\u063a\u064a\u0631 \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642 \u0645\u0646\u0647\u0645)", "Strong wording signals": "\u0625\u0634\u0627\u0631\u0627\u062a \u0635\u064a\u0627\u063a\u0629 \u0642\u0648\u064a\u0629", "Registry entries (declared, documented, verified)": "\u0625\u062f\u062e\u0627\u0644\u0627\u062a \u0627\u0644\u0633\u062c\u0644 (\u0645\u064f\u0639\u0644\u064e\u0646\u0629\u060c \u0645\u0648\u062b\u0651\u0642\u0629\u060c \u0645\u064f\u062a\u062d\u0642\u064e\u0651\u0642 \u0645\u0646\u0647\u0627)", "No record matches the current filters. Widen the filters in the left bar.": "\u0644\u0627 \u064a\u0648\u062c\u062f \u0633\u062c\u0644 \u0645\u0637\u0627\u0628\u0642 \u0644\u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0627\u0644\u062d\u0627\u0644\u064a\u0629. \u0648\u0633\u0651\u0639 \u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0641\u064a \u0627\u0644\u0634\u0631\u064a\u0637 \u0627\u0644\u0623\u064a\u0633\u0631.", "No candidate matches the current filters.": "\u0644\u0627 \u064a\u0648\u062c\u062f \u0645\u0631\u0634\u062d \u0645\u0637\u0627\u0628\u0642 \u0644\u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0627\u0644\u062d\u0627\u0644\u064a\u0629.", "No platform matches the current filters.": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0645\u0646\u0635\u0629 \u0645\u0637\u0627\u0628\u0642\u0629 \u0644\u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0627\u0644\u062d\u0627\u0644\u064a\u0629.", "No lead matches the current filters.": "\u0644\u0627 \u064a\u0648\u062c\u062f \u062e\u064a\u0637 \u0623\u0648\u0644\u064a \u0645\u0637\u0627\u0628\u0642 \u0644\u0639\u0648\u0627\u0645\u0644 \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0627\u0644\u062d\u0627\u0644\u064a\u0629.", "Platforms file not found.": "\u0645\u0644\u0641 \u0627\u0644\u0645\u0646\u0635\u0627\u062a \u063a\u064a\u0631 \u0645\u0648\u062c\u0648\u062f.", "Leads file not found.": "\u0645\u0644\u0641 \u0627\u0644\u062e\u064a\u0648\u0637 \u0627\u0644\u0623\u0648\u0644\u064a\u0629 \u063a\u064a\u0631 \u0645\u0648\u062c\u0648\u062f.", "Documented": "\u0645\u0648\u062b\u0651\u0642", "Reported": "\u0645\u064f\u0628\u0644\u064e\u0651\u063a \u0639\u0646\u0647", "Machine Learning": "\u0627\u0644\u062a\u0639\u0644\u0645 \u0627\u0644\u0622\u0644\u064a", "Bayesian / Statistical Modeling": "\u0627\u0644\u0646\u0645\u0630\u062c\u0629 \u0627\u0644\u0628\u0627\u064a\u0632\u064a\u0629 / \u0627\u0644\u0625\u062d\u0635\u0627\u0626\u064a\u0629", "Deep Learning": "\u0627\u0644\u062a\u0639\u0644\u0645 \u0627\u0644\u0639\u0645\u064a\u0642", "General AI": "\u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0639\u0627\u0645", "Large Language Models": "\u0646\u0645\u0627\u0630\u062c \u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0643\u0628\u064a\u0631\u0629", "Optimization / Hybrid AI": "\u0627\u0644\u062a\u062d\u0633\u064a\u0646 / \u0627\u0644\u0630\u0643\u0627\u0621 \u0627\u0644\u0627\u0635\u0637\u0646\u0627\u0639\u064a \u0627\u0644\u0647\u062c\u064a\u0646", "Natural Language Processing": "\u0645\u0639\u0627\u0644\u062c\u0629 \u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0637\u0628\u064a\u0639\u064a\u0629", "Computer Vision": "\u0627\u0644\u0631\u0624\u064a\u0629 \u0627\u0644\u062d\u0627\u0633\u0648\u0628\u064a\u0629", "Surveillance / Event Detection": "\u0627\u0644\u062a\u0631\u0635\u062f / \u0643\u0634\u0641 \u0627\u0644\u0623\u062d\u062f\u0627\u062b", "Prediction / Forecasting": "\u0627\u0644\u062a\u0646\u0628\u0624 / \u0627\u0644\u0627\u0633\u062a\u0634\u0631\u0627\u0641", "Policy / Decision Support": "\u062f\u0639\u0645 \u0627\u0644\u0633\u064a\u0627\u0633\u0627\u062a / \u0627\u0644\u0642\u0631\u0627\u0631\u0627\u062a", "Laboratory / Genomic Intelligence": "\u0627\u0644\u0645\u062e\u062a\u0628\u0631 / \u0627\u0644\u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0627\u0644\u062c\u064a\u0646\u0648\u0645\u064a\u0629", "Diagnosis / Clinical Decision Support": "\u0627\u0644\u062a\u0634\u062e\u064a\u0635 / \u062f\u0639\u0645 \u0627\u0644\u0642\u0631\u0627\u0631 \u0627\u0644\u0633\u0631\u064a\u0631\u064a", "Risk Mapping / Spatial Analysis": "\u0631\u0633\u0645 \u062e\u0631\u0627\u0626\u0637 \u0627\u0644\u0645\u062e\u0627\u0637\u0631 / \u0627\u0644\u062a\u062d\u0644\u064a\u0644 \u0627\u0644\u0645\u0643\u0627\u0646\u064a", "Digital Health / Chatbot / Hotline": "\u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0631\u0642\u0645\u064a\u0629 / \u0631\u0648\u0628\u0648\u062a \u0627\u0644\u0645\u062d\u0627\u062f\u062b\u0629 / \u0627\u0644\u062e\u0637 \u0627\u0644\u0633\u0627\u062e\u0646", "Resource Planning / Response Optimization": "\u062a\u062e\u0637\u064a\u0637 \u0627\u0644\u0645\u0648\u0627\u0631\u062f / \u062a\u062d\u0633\u064a\u0646 \u0627\u0644\u0627\u0633\u062a\u062c\u0627\u0628\u0629", "HIGH": "\u0645\u0631\u062a\u0641\u0639", "Medium": "\u0645\u062a\u0648\u0633\u0637", "MEDIUM": "\u0645\u062a\u0648\u0633\u0637", "LOW": "\u0645\u0646\u062e\u0641\u0636", "None": "\u0644\u0627 \u0634\u064a\u0621", "Minimal": "\u0636\u0626\u064a\u0644", "CRITICAL": "\u062d\u0631\u062c", "Non specifi\u00e9": "\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "Non sp\u00e9cifi\u00e9": "\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "Non specifie": "\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "NA": "NA", "Predictive": "\u062a\u0646\u0628\u0624\u064a", "Operational": "\u062a\u0634\u063a\u064a\u0644\u064a", "One Health": "\u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0648\u0627\u062d\u062f\u0629", "General": "\u0639\u0627\u0645", "Immediate review required": "\u0645\u0637\u0644\u0648\u0628 \u0645\u0631\u0627\u062c\u0639\u0629 \u0641\u0648\u0631\u064a\u0629", "Monitor closely": "\u0627\u0644\u0645\u062a\u0627\u0628\u0639\u0629 \u0639\u0646 \u0643\u062b\u0628", "Priority review": "\u0645\u0631\u0627\u062c\u0639\u0629 \u0630\u0627\u062a \u0623\u0648\u0644\u0648\u064a\u0629", "Archive for background tracking": "\u0623\u0631\u0634\u0641\u0629 \u0644\u0644\u0645\u062a\u0627\u0628\u0639\u0629 \u0627\u0644\u062e\u0644\u0641\u064a\u0629", "Top Priority": "\u0623\u0648\u0644\u0648\u064a\u0629 \u0642\u0635\u0648\u0649", "Watch List": "\u0642\u0627\u0626\u0645\u0629 \u0627\u0644\u0645\u0631\u0627\u0642\u0628\u0629", "High Impact": "\u062a\u0623\u062b\u064a\u0631 \u0643\u0628\u064a\u0631", "Background": "\u062e\u0644\u0641\u064a\u0629", "Western": "\u0627\u0644\u063a\u0631\u0628\u064a", "Eastern": "\u0627\u0644\u0634\u0631\u0642\u064a", "Central": "\u0627\u0644\u0623\u0648\u0633\u0637", "Southern": "\u0627\u0644\u062c\u0646\u0648\u0628\u064a", "Northern": "\u0627\u0644\u0634\u0645\u0627\u0644\u064a", "Mpox": "\u062c\u062f\u0631\u064a \u0627\u0644\u0642\u0631\u062f\u0629 (\u0625\u0645\u0628\u0648\u0643\u0633)", "Ebola": "\u0625\u064a\u0628\u0648\u0644\u0627", "Marburg": "\u0645\u0627\u0631\u0628\u0648\u0631\u063a", "COVID-19": "\u0643\u0648\u0641\u064a\u062f-19", "HIV/AIDS": "\u0641\u064a\u0631\u0648\u0633 \u0627\u0644\u0639\u0648\u0632 \u0627\u0644\u0645\u0646\u0627\u0639\u064a \u0627\u0644\u0628\u0634\u0631\u064a/\u0627\u0644\u0625\u064a\u062f\u0632", "Malaria": "\u0627\u0644\u0645\u0644\u0627\u0631\u064a\u0627", "Influenza": "\u0627\u0644\u0625\u0646\u0641\u0644\u0648\u0646\u0632\u0627", "Yellow Fever": "\u0627\u0644\u062d\u0645\u0649 \u0627\u0644\u0635\u0641\u0631\u0627\u0621", "Dengue": "\u062d\u0645\u0649 \u0627\u0644\u0636\u0646\u0643", "Cholera": "\u0627\u0644\u0643\u0648\u0644\u064a\u0631\u0627", "Meningitis": "\u0627\u0644\u062a\u0647\u0627\u0628 \u0627\u0644\u0633\u062d\u0627\u064a\u0627", "Tuberculosis": "\u0627\u0644\u0633\u0644", "Lassa fever": "\u062d\u0645\u0649 \u0644\u0627\u0633\u0627", "Polio": "\u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644", "Measles": "\u0627\u0644\u062d\u0635\u0628\u0629", "Rift Valley Fever": "\u062d\u0645\u0649 \u0627\u0644\u0648\u0627\u062f\u064a \u0627\u0644\u0645\u062a\u0635\u062f\u0639", "Algeria": "\u0627\u0644\u062c\u0632\u0627\u0626\u0631", "Angola": "\u0623\u0646\u063a\u0648\u0644\u0627", "Benin": "\u0628\u0646\u064a\u0646", "Botswana": "\u0628\u0648\u062a\u0633\u0648\u0627\u0646\u0627", "Burkina Faso": "\u0628\u0648\u0631\u0643\u064a\u0646\u0627 \u0641\u0627\u0633\u0648", "Burundi": "\u0628\u0648\u0631\u0648\u0646\u062f\u064a", "Cabo Verde": "\u0627\u0644\u0631\u0623\u0633 \u0627\u0644\u0623\u062e\u0636\u0631", "Republic of Cabo Verde": "\u0627\u0644\u0631\u0623\u0633 \u0627\u0644\u0623\u062e\u0636\u0631", "Cameroon": "\u0627\u0644\u0643\u0627\u0645\u064a\u0631\u0648\u0646", "Central African Republic": "\u062c\u0645\u0647\u0648\u0631\u064a\u0629 \u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u0627\u0644\u0648\u0633\u0637\u0649", "Chad": "\u062a\u0634\u0627\u062f", "Comoros": "\u062c\u0632\u0631 \u0627\u0644\u0642\u0645\u0631", "Congo": "\u0627\u0644\u0643\u0648\u0646\u063a\u0648", "Republic of the Congo": "\u062c\u0645\u0647\u0648\u0631\u064a\u0629 \u0627\u0644\u0643\u0648\u0646\u063a\u0648", "Democratic Republic of the Congo": "\u062c\u0645\u0647\u0648\u0631\u064a\u0629 \u0627\u0644\u0643\u0648\u0646\u063a\u0648 \u0627\u0644\u062f\u064a\u0645\u0642\u0631\u0627\u0637\u064a\u0629", "DR Congo": "\u0627\u0644\u0643\u0648\u0646\u063a\u0648 \u0627\u0644\u062f\u064a\u0645\u0642\u0631\u0627\u0637\u064a\u0629", "Djibouti": "\u062c\u064a\u0628\u0648\u062a\u064a", "Egypt": "\u0645\u0635\u0631", "Equatorial Guinea": "\u063a\u064a\u0646\u064a\u0627 \u0627\u0644\u0627\u0633\u062a\u0648\u0627\u0626\u064a\u0629", "Eritrea": "\u0625\u0631\u064a\u062a\u0631\u064a\u0627", "Eswatini": "\u0625\u0633\u0648\u0627\u062a\u064a\u0646\u064a", "Kingdom of eSwatini": "\u0625\u0633\u0648\u0627\u062a\u064a\u0646\u064a", "Ethiopia": "\u0625\u062b\u064a\u0648\u0628\u064a\u0627", "Gabon": "\u0627\u0644\u063a\u0627\u0628\u0648\u0646", "Gambia": "\u063a\u0627\u0645\u0628\u064a\u0627", "The Gambia": "\u063a\u0627\u0645\u0628\u064a\u0627", "Ghana": "\u063a\u0627\u0646\u0627", "Guinea": "\u063a\u064a\u0646\u064a\u0627", "Guinea-Bissau": "\u063a\u064a\u0646\u064a\u0627 \u0628\u064a\u0633\u0627\u0648", "C\u00f4te d'Ivoire": "\u0643\u0648\u062a \u062f\u064a\u0641\u0648\u0627\u0631", "Ivory Coast": "\u0643\u0648\u062a \u062f\u064a\u0641\u0648\u0627\u0631", "Kenya": "\u0643\u064a\u0646\u064a\u0627", "Lesotho": "\u0644\u064a\u0633\u0648\u062a\u0648", "Liberia": "\u0644\u064a\u0628\u064a\u0631\u064a\u0627", "Libya": "\u0644\u064a\u0628\u064a\u0627", "Madagascar": "\u0645\u062f\u063a\u0634\u0642\u0631", "Malawi": "\u0645\u0644\u0627\u0648\u064a", "Mali": "\u0645\u0627\u0644\u064a", "Mauritania": "\u0645\u0648\u0631\u064a\u062a\u0627\u0646\u064a\u0627", "Mauritius": "\u0645\u0648\u0631\u064a\u0634\u064a\u0648\u0633", "Morocco": "\u0627\u0644\u0645\u063a\u0631\u0628", "Mozambique": "\u0645\u0648\u0632\u0645\u0628\u064a\u0642", "Namibia": "\u0646\u0627\u0645\u064a\u0628\u064a\u0627", "Niger": "\u0627\u0644\u0646\u064a\u062c\u0631", "Nigeria": "\u0646\u064a\u062c\u064a\u0631\u064a\u0627", "Rwanda": "\u0631\u0648\u0627\u0646\u062f\u0627", "Sao Tome and Principe": "\u0633\u0627\u0646 \u062a\u0648\u0645\u064a \u0648\u0628\u0631\u064a\u0646\u0633\u064a\u0628\u064a", "S\u00e3o Tom\u00e9 and Principe": "\u0633\u0627\u0646 \u062a\u0648\u0645\u064a \u0648\u0628\u0631\u064a\u0646\u0633\u064a\u0628\u064a", "Senegal": "\u0627\u0644\u0633\u0646\u063a\u0627\u0644", "Seychelles": "\u0633\u064a\u0634\u064a\u0644", "Sierra Leone": "\u0633\u064a\u0631\u0627\u0644\u064a\u0648\u0646", "Somalia": "\u0627\u0644\u0635\u0648\u0645\u0627\u0644", "South Africa": "\u062c\u0646\u0648\u0628 \u0623\u0641\u0631\u064a\u0642\u064a\u0627", "South Sudan": "\u062c\u0646\u0648\u0628 \u0627\u0644\u0633\u0648\u062f\u0627\u0646", "Sudan": "\u0627\u0644\u0633\u0648\u062f\u0627\u0646", "Tanzania": "\u062a\u0646\u0632\u0627\u0646\u064a\u0627", "Togo": "\u062a\u0648\u063a\u0648", "Tunisia": "\u062a\u0648\u0646\u0633", "Uganda": "\u0623\u0648\u063a\u0646\u062f\u0627", "Zambia": "\u0632\u0627\u0645\u0628\u064a\u0627", "Zimbabwe": "\u0632\u064a\u0645\u0628\u0627\u0628\u0648\u064a", "Western Sahara": "\u0627\u0644\u0635\u062d\u0631\u0627\u0621 \u0627\u0644\u063a\u0631\u0628\u064a\u0629", "preprint": "\u0646\u0633\u062e\u0629 \u0623\u0648\u0644\u064a\u0629 (\u0645\u0627 \u0642\u0628\u0644 \u0627\u0644\u0646\u0634\u0631)", "PubMed": "PubMed", "Core": "\u0623\u0633\u0627\u0633\u064a", "Adjacent": "\u0630\u0648 \u0635\u0644\u0629", "explicit": "\u0635\u0631\u064a\u062d", "statistical_only": "\u0625\u062d\u0635\u0627\u0626\u064a \u0641\u0642\u0637", "mention_only": "\u0630\u0643\u0631 \u0641\u0642\u0637", "none": "\u0644\u0627 \u0634\u064a\u0621", "single_mention": "\u0630\u0643\u0631 \u0648\u0627\u062d\u062f", "text_multiple": "\u0630\u0643\u0631 \u0645\u062a\u0639\u062f\u062f", "ambiguous": "\u063a\u0627\u0645\u0636", "dominant": "\u063a\u0627\u0644\u0628", "TRUE": "\u0635\u062d\u064a\u062d", "FALSE": "\u062e\u0637\u0623", "Journal Article": "\u0645\u0642\u0627\u0644 \u0641\u064a \u0645\u062c\u0644\u0629", "Review": "\u0645\u0631\u0627\u062c\u0639\u0629", "Systematic Review": "\u0645\u0631\u0627\u062c\u0639\u0629 \u0645\u0646\u0647\u062c\u064a\u0629", "Scoping Review": "\u0645\u0631\u0627\u062c\u0639\u0629 \u0627\u0633\u062a\u0643\u0634\u0627\u0641\u064a\u0629", "Editorial": "\u0627\u0641\u062a\u062a\u0627\u062d\u064a\u0629", "Retracted Publication": "\u0645\u0646\u0634\u0648\u0631 \u0645\u0633\u062d\u0648\u0628", "Preprint": "\u0646\u0633\u062e\u0629 \u0623\u0648\u0644\u064a\u0629 (\u0645\u0627 \u0642\u0628\u0644 \u0627\u0644\u0646\u0634\u0631)", "Research Support, Non-U.S. Gov't": "\u062f\u0639\u0645 \u0628\u062d\u062b\u064a (\u063a\u064a\u0631 \u062d\u0643\u0648\u0645\u064a \u0623\u0645\u0631\u064a\u0643\u064a)", "Research Support, N.I.H., Extramural": "\u062f\u0639\u0645 \u0628\u062d\u062b\u064a (\u0627\u0644\u0645\u0639\u0627\u0647\u062f \u0627\u0644\u0648\u0637\u0646\u064a\u0629 \u0644\u0644\u0635\u062d\u0629\u060c \u062e\u0627\u0631\u062c\u064a)", "Research Support, U.S. Gov't, Non-P.H.S.": "\u062f\u0639\u0645 \u0628\u062d\u062b\u064a (\u062d\u0643\u0648\u0645\u0629 \u0627\u0644\u0648\u0644\u0627\u064a\u0627\u062a \u0627\u0644\u0645\u062a\u062d\u062f\u0629\u060c \u063a\u064a\u0631 PHS)", "Antimicrobial resistance": "\u0645\u0642\u0627\u0648\u0645\u0629 \u0645\u0636\u0627\u062f\u0627\u062a \u0627\u0644\u0645\u064a\u0643\u0631\u0648\u0628\u0627\u062a", "HIV": "\u0641\u064a\u0631\u0648\u0633 \u0646\u0642\u0635 \u0627\u0644\u0645\u0646\u0627\u0639\u0629 \u0627\u0644\u0628\u0634\u0631\u064a\u0629", "Hepatitis": "\u0627\u0644\u062a\u0647\u0627\u0628 \u0627\u0644\u0643\u0628\u062f", "Diphtheria": "\u0627\u0644\u062e\u0646\u0627\u0642 (\u0627\u0644\u062f\u0641\u062a\u064a\u0631\u064a\u0627)", "Pertussis": "\u0627\u0644\u0633\u0639\u0627\u0644 \u0627\u0644\u062f\u064a\u0643\u064a"}, "sw": {"Overview": "Muhtasari", "AI x Disease x Country": "AI x Ugonjwa x Nchi", "Methods & Applications": "Mbinu na matumizi", "Focus Africa": "Mkazo kwa Afrika", "Priority Intelligence": "Taarifa za kipaumbele", "All Records": "Rekodi zote", "Downloads": "Upakuaji", "PREIS AI Atlas": "Atlasi ya PREIS AI", "Methods and limits": "Mbinu na mipaka", "Filters": "Vichujio", "Year": "Mwaka", "Alert level": "Kiwango cha tahadhari", "Disease": "Ugonjwa", "AI method": "Mbinu ya AI", "Application": "Matumizi", "RCC": "RCC", "Country": "Nchi", "Signal profile": "Wasifu wa ishara", "Language of publication": "Lugha ya chapisho", "All": "Zote", "English": "Kiingereza", "French": "Kifaransa", "Spanish": "Kihispania", "Portuguese": "Kireno", "German": "Kijerumani", "Italian": "Kiitaliano", "Other": "Nyingine", "Arabic": "Kiarabu", "Swahili": "Kiswahili", "High": "Juu", "Moderate": "Wastani", "Low": "Chini", "Unspecified": "Haijabainishwa", "Critical": "Muhimu sana", "PREIS AI Tracker \u2014 Dashboard": "PREIS AI Tracker \u2014 Dashibodi", "Monitoring AI applications for public health surveillance in Africa |": "Ufuatiliaji wa matumizi ya AI katika uchunguzi wa afya ya umma barani Afrika |", "Source:": "Chanzo:", "Data generated:": "Data zilizozalishwa:", "unknown": "haijulikani", "Automated classification, not yet validated by human review.": "Uainishaji wa kiotomatiki, bado haujahakikiwa na mapitio ya binadamu.", "Disease, method, application and Africa-relevance labels and scores are produced by keyword rules and may contain errors.": "Lebo na alama za ugonjwa, mbinu, matumizi na umuhimu kwa Afrika hutolewa kwa kanuni za maneno muhimu na zinaweza kuwa na makosa.", "Records outside the infectious-disease/epidemic scope are excluded from this view.": "Rekodi zilizo nje ya wigo wa magonjwa ya kuambukiza na milipuko hazijumuishwi kwenye mwonekano huu.", "Do not use as a basis for operational decisions without expert verification.": "Usitumie kama msingi wa maamuzi ya kiutendaji bila uhakiki wa mtaalamu.", "AI records": "Rekodi za AI", "High-priority records": "Rekodi za kipaumbele cha juu", "Diseases under surveillance": "Magonjwa yanayochunguzwa", "Priority distribution": "Mgawanyo kwa kipaumbele", "Alert level distribution": "Mgawanyo kwa kiwango cha tahadhari", "AI methods": "Mbinu za AI", "AI applications": "Matumizi ya AI", "Africa relevance": "Umuhimu kwa Afrika", "Novelty": "Uvumbuzi", "Diseases under AI surveillance \u2014 top priority": "Magonjwa yanayochunguzwa kwa AI \u2014 kipaumbele cha juu kabisa", "AI method x Disease matrix": "Jedwali la mbinu ya AI x ugonjwa", "Application x Disease matrix": "Jedwali la matumizi x ugonjwa", "Top countries per disease (Africa-linked)": "Nchi kuu kwa kila ugonjwa (zinazohusiana na Afrika)", "Disease trend over time": "Mwenendo wa magonjwa kwa wakati", "Disease intelligence records": "Rekodi za taarifa kwa ugonjwa", "Methods by priority": "Mbinu kwa kipaumbele", "Applications by priority": "Matumizi kwa kipaumbele", "Methods \u00d7 Applications": "Mbinu \u00d7 matumizi", "Africa-linked AI records \u2014 geographic distribution": "Rekodi za AI zinazohusiana na Afrika \u2014 mgawanyo wa kijiografia", "Show country names on the map": "Onyesha majina ya nchi kwenye ramani", "By RCC": "Kwa RCC", "Top countries": "Nchi kuu", "Geography status": "Hali ya kijiografia", "Africa-linked records \u2014 trend over time": "Rekodi zinazohusiana na Afrika \u2014 mwenendo kwa wakati", "Critical alerts": "Tahadhari muhimu sana", "Priority score distribution by disease": "Mgawanyo wa alama za kipaumbele kwa ugonjwa", "High priority intelligence records": "Rekodi za taarifa za kipaumbele cha juu", "All intelligence records": "Rekodi zote za taarifa", "Filtered dataset": "Seti ya data iliyochujwa", "All records matching current filters.": "Rekodi zote zinazolingana na vichujio vya sasa.", "Africa-linked dataset": "Seti ya data inayohusiana na Afrika", "Records with African geographic signal.": "Rekodi zenye ishara ya kijiografia ya Afrika.", "High priority alerts": "Tahadhari za kipaumbele cha juu", "High-priority records (top priority tier).": "Rekodi za kipaumbele cha juu (kiwango cha juu kabisa).", "Download CSV": "Pakua CSV", "Dataset information": "Taarifa kuhusu seti ya data", "Rows = AI method | Columns = disease": "Safu mlalo = mbinu ya AI | Safu wima = ugonjwa", "Rows = Application | Columns = disease": "Safu mlalo = matumizi | Safu wima = ugonjwa", "No data with country information": "Hakuna data yenye taarifa za nchi", "Top 10 high-priority records": "Rekodi 10 bora za kipaumbele cha juu", "link": "kiungo", "Light map": "Ramani nyepesi", "Satellite": "Setilaiti", "Regional Coordinating Centre (fill)": "Kituo cha Uratibu wa Kikanda (rangi)", "African geography detected": "Jiografia ya Afrika imebainika", "No geography detected": "Hakuna jiografia iliyobainika", "Loess trend line": "Mstari wa mwelekeo wa Loess", "Search:": "Tafuta:", "Previous": "Iliyotangulia", "Next": "Inayofuata", "First": "Ya kwanza", "Last": "Ya mwisho", "Show": "Onyesha", "entries": "maingizo", "No data available in table": "Hakuna data kwenye jedwali", "No matching records found": "Hakuna rekodi zinazolingana", "disease_primary": "Ugonjwa", "disease_confidence": "Uhakika wa ugonjwa", "disease_mentioned": "Ugonjwa uliotajwa", "alert_level": "Kiwango cha tahadhari", "title": "kichwa", "year": "Mwaka", "country": "Nchi", "countries_mentioned": "Nchi zilizotajwa", "rcc": "RCC", "method_family_primary": "Mbinu ya AI", "application_type_primary": "Matumizi", "priority_score": "Alama ya kipaumbele", "africa_relevance_level": "Umuhimu kwa Afrika", "novelty_level": "Uvumbuzi", "tracker_recommendation": "Pendekezo", "signal_profile": "Wasifu wa ishara", "url": "Kiungo", "Status: research prototype.": "Hali: mfano wa utafiti.", "Automatic screening of public bibliographic metadata (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Titles, authors, journal, year and links are shown; abstracts are not reproduced. Follow the link to read the source.": "Uchujaji wa kiotomatiki wa metadata za kibibliografia za umma (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Vichwa, waandishi, jarida, mwaka na viungo vinaonyeshwa; muhtasari hauchapishwi tena. Fuata kiungo kusoma chanzo.", "All labels (AI method, application, disease, country, public-health scope, priority) are produced automatically by rules and have NOT been validated for operational decisions.": "Lebo zote (mbinu ya AI, matumizi, ugonjwa, nchi, wigo wa afya ya umma, kipaumbele) hutolewa kiotomatiki kwa kanuni na HAZIJAHAKIKIWA kwa maamuzi ya kiutendaji.", "Pilot validation: two samples of 50 articles, one reviewer, stratified and unweighted; agreement for AI as study method 43/50 in each sample. Scope and geography are the weakest dimensions. No result should be read as a prevalence.": "Uhakiki wa majaribio: sampuli mbili za makala 50, mhakiki mmoja, zilizopangwa kwa matabaka na zisizopimwa uzito; ulinganifu wa AI kama mbinu ya utafiti 43/50 katika kila sampuli. Wigo na jiografia ndizo nyanja dhaifu zaidi. Hakuna matokeo yanayopaswa kusomwa kama kiwango cha maambukizi.", "Preprints are the majority of records and are not peer reviewed. Europe PMC was unavailable from 16 to 30 October 2026 and was reloaded on 31 October 2026; records added by the source between runs can be missed until the next run.": "Machapisho ya awali ndiyo wengi wa rekodi na hayajapitiwa na wenzao. Europe PMC haikupatikana kuanzia tarehe 16 hadi 30 Oktoba 2026 na ilipakiwa upya tarehe 31 Oktoba 2026; rekodi zilizoongezwa na chanzo kati ya uendeshaji zinaweza kukosekana hadi uendeshaji unaofuata.", "A study country is assigned to only a small share of records; country counts are lower bounds, not the distribution of research.": "Nchi ya utafiti imepewa sehemu ndogo tu ya rekodi; hesabu za nchi ni viwango vya chini, si mgawanyo wa utafiti.", "Associations are not causal and predictions are not certainties. Alerts are prepared in test mode; test emails are sent only to the author's own addresses. An outbreak mode (active for the 2026 Bundibugyo virus disease outbreak) widens routing of filovirus articles to the response team.": "Uhusiano si sababu na utabiri si uhakika. Tahadhari huandaliwa katika hali ya majaribio; barua pepe za majaribio hutumwa kwa anwani za mwandishi pekee. Hali ya mlipuko (inayotumika kwa mlipuko wa ugonjwa wa virusi vya Bundibugyo 2026) hupanua uelekezaji wa makala kuhusu filovirusi kwa timu ya kukabiliana.", "This tool is not an official product of, and is not endorsed by, any institution. The views are those of the author(s).": "Zana hii si bidhaa rasmi ya taasisi yoyote wala haijaidhinishwa na taasisi yoyote. Maoni ni ya mwandishi au waandishi.", "Last data load:": "Upakiaji wa mwisho wa data:", "PREIS AI Atlas: which AI, on what, where, at what stage?": "Atlasi ya PREIS AI: AI ipi, kwa nini, wapi, katika hatua gani?", "Objective:": "Lengo:", "map every AI application used for epidemic surveillance in Africa:": "kuchora ramani ya kila matumizi ya AI katika uchunguzi wa milipuko barani Afrika:", "WHAT": "NINI", "it does,": "inafanya,", "ON WHAT": "KWA NINI", "disease,": "ugonjwa,", "WHERE": "WAPI", "(RCC, country),": "(RCC, nchi),", "WHEN": "LINI", "and at what stage.": "na katika hatua gani.", "Literature signals and known platforms; filters on the left apply.": "Ishara kutoka machapisho na majukwaa yanayojulikana; vichujio vya kushoto vinatumika.", "Candidates are keyword signals, not verified systems; registry entries: 0.": "Wagombea ni ishara za maneno muhimu, si mifumo iliyohakikiwa; maingizo ya rejista: 0.", "Tracker data in view:": "Data za tracker zinazoonyeshwa:", "records after filters. Candidates file updated:": "rekodi baada ya vichujio. Faili ya wagombea imesasishwa:", "THE MATRIX: which AI application, on which disease? (number of records)": "JEDWALI: matumizi gani ya AI, kwa ugonjwa gani? (idadi ya rekodi)", "WHAT: which AI applications?": "NINI: matumizi gani ya AI?", "ON WHAT: which diseases?": "KWA NINI: magonjwa gani?", "KNOWN AI SURVEILLANCE PLATFORMS (documented at source, not validated)": "MAJUKWAA YA AI YANAYOJULIKANA KWA UCHUNGUZI (yaliyoandikwa chanzoni, hayajahakikiwa)", "Each line cites its public source. Stage is quoted from that source. Documented = stated by the owner or implementer; it is not verified by the registry and says nothing about performance. Country, RCC and disease filters apply when they match.": "Kila mstari unataja chanzo chake cha umma. Hatua inanukuliwa kutoka chanzo hicho. Imeandikwa = imetajwa na mmiliki au mtekelezaji; haijahakikiwa na rejista na haisemi lolote kuhusu utendaji. Vichujio vya nchi, RCC na ugonjwa vinatumika vinapolingana.", "WHERE: Regional Coordinating Centres (fill) and Member States (outline)": "WAPI: Vituo vya Uratibu wa Kikanda (rangi) na Nchi Wanachama (mpaka)", "Fill = RCC. Dark outline and circle = Member State with at least one record. Click a country for its applications. Only records with a validated study country are placed; the others are counted in the totals but not on the map.": "Rangi = RCC. Mpaka mweusi na duara = Nchi Mwanachama yenye angalau rekodi moja. Bofya nchi kuona matumizi yake. Ni rekodi zenye nchi ya utafiti iliyothibitishwa pekee zinazowekwa; zingine zinahesabiwa kwenye jumla lakini hazionekani kwenye ramani.", "WHERE: records by RCC": "WAPI: rekodi kwa RCC", "WHERE: top Member States": "WAPI: Nchi Wanachama kuu", "WHEN: records by year of publication and application": "LINI: rekodi kwa mwaka wa chapisho na matumizi", "The current year is partial. Publication year is not the year of deployment.": "Mwaka wa sasa haujakamilika. Mwaka wa chapisho si mwaka wa utekelezaji.", "Candidate systems found automatically (stage signal from the wording only)": "Mifumo ya wagombea iliyopatikana kiotomatiki (ishara ya hatua kutoka maneno tu)", "Strength reflects the wording of the title and abstract (for example 'deployed', 'pilot'), not a verified stage. Each candidate must be checked by two independent validators.": "Nguvu inaakisi maneno ya kichwa na muhtasari (kwa mfano 'imetekelezwa', 'majaribio'), si hatua iliyohakikiwa. Kila mgombea lazima ahakikiwe na wahakiki wawili huru.", "Leads: applications read by one reader (preliminary, not validated)": "Viongozi: matumizi yaliyosomwa na msomaji mmoja (ya awali, hayajahakikiwa)", "None is confirmed as routine use. A lead is a reason to ask the team, not a finding. Year, country and disease filters apply.": "Hakuna kinachothibitishwa kuwa matumizi ya kawaida. Kiongozi ni sababu ya kuuliza timu, si matokeo. Vichujio vya mwaka, nchi na ugonjwa vinatumika.", "Registry status and declaration": "Hali ya rejista na tamko", "Statuses:": "Hali:", "declared": "imetangazwa", "(the owner says so),": "(mmiliki anasema hivyo),", "documented": "imeandikwa", "(a public document shows it),": "(waraka wa umma unaonyesha),", "verified": "imehakikiwa", "(two independent validators agree). Entries now: declared 0, documented 0, verified 0.": "(wahakiki wawili huru wanakubaliana). Maingizo sasa: imetangazwa 0, imeandikwa 0, imehakikiwa 0.", "Phase 1 (done): automatic candidates from the literature. Phase 1b (planned): grey literature. Phase 2 (planned): declarations by RCCs, ministries and partners, then cross-validation.": "Awamu ya 1 (imekamilika): wagombea wa kiotomatiki kutoka machapisho. Awamu ya 1b (imepangwa): machapisho ya kijivu. Awamu ya 2 (imepangwa): matamko kutoka RCC, wizara na washirika, kisha uhakiki mtambuka.", "Declare an AI system (about 5 minutes)": "Tangaza mfumo wa AI (takriban dakika 5)", "- recorded as declared, not verified. Nothing is published with a name or country without consent.": "- inarekodiwa kama ilivyotangazwa, haijahakikiwa. Hakuna kinachochapishwa kwa jina au nchi bila ridhaa.", "Africa-linked records, explicit AI (after filters)": "Rekodi zinazohusiana na Afrika, AI dhahiri (baada ya vichujio)", "Known AI platforms in Africa (documented, core)": "Majukwaa ya AI yanayojulikana Afrika (yaliyoandikwa, ya msingi)", "Candidates in view (not verified)": "Wagombea wanaoonyeshwa (hawajahakikiwa)", "Strong wording signals": "Ishara kali za maneno", "Registry entries (declared, documented, verified)": "Maingizo ya rejista (yaliyotangazwa, yaliyoandikwa, yaliyohakikiwa)", "No record matches the current filters. Widen the filters in the left bar.": "Hakuna rekodi inayolingana na vichujio vya sasa. Panua vichujio kwenye upau wa kushoto.", "No candidate matches the current filters.": "Hakuna mgombea anayelingana na vichujio vya sasa.", "No platform matches the current filters.": "Hakuna jukwaa linalolingana na vichujio vya sasa.", "No lead matches the current filters.": "Hakuna kiongozi anayelingana na vichujio vya sasa.", "Platforms file not found.": "Faili ya majukwaa haijapatikana.", "Leads file not found.": "Faili ya viongozi haijapatikana.", "Documented": "Imeandikwa", "Reported": "Imeripotiwa", "Machine Learning": "Ujifunzaji wa mashine", "Bayesian / Statistical Modeling": "Uundaji wa Bayesian / takwimu", "Deep Learning": "Ujifunzaji wa kina", "General AI": "AI ya jumla", "Large Language Models": "Mifano mikubwa ya lugha", "Optimization / Hybrid AI": "Uboreshaji / AI mseto", "Natural Language Processing": "Uchakataji wa lugha asilia", "Computer Vision": "Maono ya kompyuta", "Surveillance / Event Detection": "Uchunguzi / ugunduzi wa matukio", "Prediction / Forecasting": "Utabiri / makadirio", "Policy / Decision Support": "Msaada wa sera / maamuzi", "Laboratory / Genomic Intelligence": "Maabara / taarifa za jenomu", "Diagnosis / Clinical Decision Support": "Utambuzi / msaada wa maamuzi ya kitabibu", "Risk Mapping / Spatial Analysis": "Ramani ya hatari / uchambuzi wa anga", "Digital Health / Chatbot / Hotline": "Afya ya kidijitali / chatbot / simu ya msaada", "Resource Planning / Response Optimization": "Mipango ya rasilimali / uboreshaji wa mwitikio", "HIGH": "Juu", "Medium": "Kati", "MEDIUM": "Kati", "LOW": "Chini", "None": "Hakuna", "Minimal": "Ndogo sana", "CRITICAL": "Muhimu sana", "Non specifi\u00e9": "Haijabainishwa", "Non sp\u00e9cifi\u00e9": "Haijabainishwa", "Non specifie": "Haijabainishwa", "NA": "NA", "Predictive": "Utabiri", "Operational": "Kiutendaji", "One Health": "Afya Moja", "General": "Jumla", "Immediate review required": "Mapitio ya haraka yanahitajika", "Monitor closely": "Fuatilia kwa karibu", "Priority review": "Mapitio ya kipaumbele", "Archive for background tracking": "Hifadhi kwa ufuatiliaji wa msingi", "Top Priority": "Kipaumbele cha juu kabisa", "Watch List": "Orodha ya uangalizi", "High Impact": "Athari kubwa", "Background": "Usuli", "Western": "Magharibi", "Eastern": "Mashariki", "Central": "Kati", "Southern": "Kusini", "Northern": "Kaskazini", "Mpox": "Mpox", "Ebola": "Ebola", "Marburg": "Marburg", "COVID-19": "UVIKO-19", "HIV/AIDS": "VVU/UKIMWI", "Malaria": "Malaria", "Influenza": "Mafua makali", "Yellow Fever": "Homa ya manjano", "Dengue": "Dengi", "Cholera": "Kipindupindu", "Meningitis": "Homa ya uti wa mgongo", "Tuberculosis": "Kifua kikuu", "Lassa fever": "Homa ya Lassa", "Polio": "Polio", "Measles": "Surua", "Rift Valley Fever": "Homa ya Bonde la Ufa", "Algeria": "Algeria", "Angola": "Angola", "Benin": "Benin", "Botswana": "Botswana", "Burkina Faso": "Burkina Faso", "Burundi": "Burundi", "Cabo Verde": "Cabo Verde", "Republic of Cabo Verde": "Cabo Verde", "Cameroon": "Kamerun", "Central African Republic": "Jamhuri ya Afrika ya Kati", "Chad": "Chadi", "Comoros": "Komoro", "Congo": "Kongo", "Republic of the Congo": "Jamhuri ya Kongo", "Democratic Republic of the Congo": "Jamhuri ya Kidemokrasia ya Kongo", "DR Congo": "DR Kongo", "Djibouti": "Jibuti", "Egypt": "Misri", "Equatorial Guinea": "Guinea ya Ikweta", "Eritrea": "Eritrea", "Eswatini": "Eswatini", "Kingdom of eSwatini": "Eswatini", "Ethiopia": "Ethiopia", "Gabon": "Gabon", "Gambia": "Gambia", "The Gambia": "Gambia", "Ghana": "Ghana", "Guinea": "Guinea", "Guinea-Bissau": "Guinea-Bissau", "C\u00f4te d'Ivoire": "C\u00f4te d'Ivoire", "Ivory Coast": "C\u00f4te d'Ivoire", "Kenya": "Kenya", "Lesotho": "Lesotho", "Liberia": "Liberia", "Libya": "Libya", "Madagascar": "Madagaska", "Malawi": "Malawi", "Mali": "Mali", "Mauritania": "Mauritania", "Mauritius": "Mauritius", "Morocco": "Moroko", "Mozambique": "Msumbiji", "Namibia": "Namibia", "Niger": "Niger", "Nigeria": "Nigeria", "Rwanda": "Rwanda", "Sao Tome and Principe": "Sao Tome na Principe", "S\u00e3o Tom\u00e9 and Principe": "Sao Tome na Principe", "Senegal": "Senegali", "Seychelles": "Shelisheli", "Sierra Leone": "Sierra Leone", "Somalia": "Somalia", "South Africa": "Afrika Kusini", "South Sudan": "Sudan Kusini", "Sudan": "Sudan", "Tanzania": "Tanzania", "Togo": "Togo", "Tunisia": "Tunisia", "Uganda": "Uganda", "Zambia": "Zambia", "Zimbabwe": "Zimbabwe", "Western Sahara": "Sahara Magharibi", "preprint": "Chapisho la awali", "PubMed": "PubMed", "Core": "Kiini", "Adjacent": "Jirani", "explicit": "wazi", "statistical_only": "takwimu pekee", "mention_only": "kutajwa tu", "none": "hakuna", "single_mention": "kutajwa mara moja", "text_multiple": "kutajwa mara nyingi", "ambiguous": "haieleweki", "dominant": "kuu", "TRUE": "NDIYO", "FALSE": "HAPANA", "Journal Article": "Makala ya jarida", "Review": "Mapitio", "Systematic Review": "Mapitio ya kimfumo", "Scoping Review": "Mapitio ya wigo", "Editorial": "Tahariri", "Retracted Publication": "Chapisho lililoondolewa", "Preprint": "Chapisho la awali", "Research Support, Non-U.S. Gov't": "Ufadhili wa utafiti (si serikali ya Marekani)", "Research Support, N.I.H., Extramural": "Ufadhili wa utafiti (NIH, nje)", "Research Support, U.S. Gov't, Non-P.H.S.": "Ufadhili wa utafiti (serikali ya Marekani, si PHS)", "Antimicrobial resistance": "Usugu wa dawa za viua vijidudu", "HIV": "VVU", "Hepatitis": "Homa ya ini", "Diphtheria": "Dondakoo", "Pertussis": "Kifaduro"}};
  var MO = {"fr": {"January": "janvier", "February": "f\u00e9vrier", "March": "mars", "April": "avril", "May": "mai", "June": "juin", "July": "juillet", "August": "ao\u00fbt", "September": "septembre", "October": "octobre", "November": "novembre", "December": "d\u00e9cembre"}, "pt": {"January": "janeiro", "February": "fevereiro", "March": "mar\u00e7o", "April": "abril", "May": "maio", "June": "junho", "July": "julho", "August": "agosto", "September": "setembro", "October": "outubro", "November": "novembro", "December": "dezembro"}, "es": {"January": "enero", "February": "febrero", "March": "marzo", "April": "abril", "May": "mayo", "June": "junio", "July": "julio", "August": "agosto", "September": "septiembre", "October": "octubre", "November": "noviembre", "December": "diciembre"}, "ar": {"January": "\u064a\u0646\u0627\u064a\u0631", "February": "\u0641\u0628\u0631\u0627\u064a\u0631", "March": "\u0645\u0627\u0631\u0633", "April": "\u0623\u0628\u0631\u064a\u0644", "May": "\u0645\u0627\u064a\u0648", "June": "\u064a\u0648\u0646\u064a\u0648", "July": "\u064a\u0648\u0644\u064a\u0648", "August": "\u0623\u063a\u0633\u0637\u0633", "September": "\u0633\u0628\u062a\u0645\u0628\u0631", "October": "\u0623\u0643\u062a\u0648\u0628\u0631", "November": "\u0646\u0648\u0641\u0645\u0628\u0631", "December": "\u062f\u064a\u0633\u0645\u0628\u0631"}, "sw": {"January": "Januari", "February": "Februari", "March": "Machi", "April": "Aprili", "May": "Mei", "June": "Juni", "July": "Julai", "August": "Agosti", "September": "Septemba", "October": "Oktoba", "November": "Novemba", "December": "Desemba"}};
  var MT = {"fr": " / Afrique : ", "pt": " / \u00c1frica: ", "es": " / \u00c1frica: ", "ar": " / \u0623\u0641\u0631\u064a\u0642\u064a\u0627: ", "sw": " / Afrika: "}, MS = {"fr": " : ", "pt": ": ", "es": ": ", "ar": ": ", "sw": ": "}, NOTE = {"pt": "Tradu\u00e7\u00e3o provis\u00f3ria, a rever por falantes nativos.", "es": "Traducci\u00f3n provisional, pendiente de revisi\u00f3n por hablantes nativos.", "ar": "\u062a\u0631\u062c\u0645\u0629 \u0623\u0648\u0644\u064a\u0629 \u0628\u0627\u0646\u062a\u0638\u0627\u0631 \u0645\u0631\u0627\u062c\u0639\u0629 \u0646\u0627\u0637\u0642\u064a\u0646 \u0623\u0635\u0644\u064a\u064a\u0646.", "sw": "Tafsiri ya awali, inasubiri mapitio ya wazungumzaji asilia."};
  var PR = [
  [/^Showing (\S+) to (\S+) of (\S+) entries \(filtered from (\S+) total entries\)$/, 'info_f', {"fr": "Affichage de $1 \u00e0 $2 sur $3 entr\u00e9es (filtr\u00e9es sur un total de $4)", "pt": "Mostrando $1 a $2 de $3 entradas (filtradas de um total de $4)", "es": "Mostrando $1 a $2 de $3 entradas (filtrado de un total de $4)", "ar": "\u0639\u0631\u0636 $1 \u0625\u0644\u0649 $2 \u0645\u0646 $3 \u0625\u062f\u062e\u0627\u0644\u0627\u064b (\u062a\u0645\u062a \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0645\u0646 \u0625\u062c\u0645\u0627\u0644\u064a $4)", "sw": "Inaonyesha $1 hadi $2 kati ya maingizo $3 (yamechujwa kutoka jumla ya $4)"}],
  [/^Showing (\S+) to (\S+) of (\S+) entries$/, 'info', {"fr": "Affichage de $1 \u00e0 $2 sur $3 entr\u00e9es", "pt": "Mostrando $1 a $2 de $3 entradas", "es": "Mostrando $1 a $2 de $3 entradas", "ar": "\u0639\u0631\u0636 $1 \u0625\u0644\u0649 $2 \u0645\u0646 $3 \u0625\u062f\u062e\u0627\u0644\u0627\u064b", "sw": "Inaonyesha $1 hadi $2 kati ya maingizo $3"}],
  [/^EpiWeek (W\d+\/\d+)$/, 'epi', {"fr": "Semaine \u00e9pid\u00e9m. $1", "pt": "Semana epidemiol\u00f3gica $1", "es": "Semana epidemiol\u00f3gica $1", "ar": "\u0627\u0644\u0623\u0633\u0628\u0648\u0639 \u0627\u0644\u0648\u0628\u0627\u0626\u064a $1", "sw": "Wiki ya epidemiolojia $1"}],
  [/^Year (\d{4}): (.+) \/ Africa: (.+)$/, 'year', {"fr": "Ann\u00e9e $1 : $2 / Afrique : $3", "pt": "Ano $1: $2 / \u00c1frica: $3", "es": "A\u00f1o $1: $2 / \u00c1frica: $3", "ar": "\u0627\u0644\u0633\u0646\u0629 $1: $2 / \u0623\u0641\u0631\u064a\u0642\u064a\u0627: $3", "sw": "Mwaka $1: $2 / Afrika: $3"}],
  [/^This week \((.+)\): (.+) \/ Africa: (.+)$/, 'week', {"fr": "Cette semaine ($1) : $2 / Afrique : $3", "pt": "Esta semana ($1): $2 / \u00c1frica: $3", "es": "Esta semana ($1): $2 / \u00c1frica: $3", "ar": "\u0647\u0630\u0627 \u0627\u0644\u0623\u0633\u0628\u0648\u0639 ($1): $2 / \u0623\u0641\u0631\u064a\u0642\u064a\u0627: $3", "sw": "Wiki hii ($1): $2 / Afrika: $3"}],
  [/^([A-Z][a-z]+) (\d{4}): (.+) \/ Africa: (.+)$/, 'month', {}],
  [/^Africa-linked \((.+) on map\)$/, 'afmap', {"fr": "Li\u00e9s \u00e0 l'Afrique ($1 sur la carte)", "pt": "Ligados a \u00c1frica ($1 no mapa)", "es": "Vinculados a \u00c1frica ($1 en el mapa)", "ar": "\u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627 ($1 \u0639\u0644\u0649 \u0627\u0644\u062e\u0631\u064a\u0637\u0629)", "sw": "Zinazohusiana na Afrika ($1 kwenye ramani)"}],
  [/^RCC: (.+)$/, 'rcc', {"fr": "CCR : $1", "pt": "CCR: $1", "es": "CCR: $1", "ar": "\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a: $1", "sw": "RCC: $1"}],
  [/^Total records: (.+)$/, 'tot', {"fr": "Total des enregistrements : $1", "pt": "Total de registos: $1", "es": "Total de registros: $1", "ar": "\u0625\u062c\u0645\u0627\u0644\u064a \u0627\u0644\u0633\u062c\u0644\u0627\u062a: $1", "sw": "Jumla ya rekodi: $1"}],
  [/^High priority: (.+)$/, 'hi', {"fr": "Priorit\u00e9 \u00e9lev\u00e9e : $1", "pt": "Prioridade alta: $1", "es": "Prioridad alta: $1", "ar": "\u0623\u0648\u0644\u0648\u064a\u0629 \u0639\u0627\u0644\u064a\u0629: $1", "sw": "Kipaumbele cha juu: $1"}],
  [/^Moderate priority: (.+)$/, 'mod', {"fr": "Priorit\u00e9 mod\u00e9r\u00e9e : $1", "pt": "Prioridade moderada: $1", "es": "Prioridad moderada: $1", "ar": "\u0623\u0648\u0644\u0648\u064a\u0629 \u0645\u062a\u0648\u0633\u0637\u0629: $1", "sw": "Kipaumbele cha wastani: $1"}],
  [/^(.+): (\S+) records$/, 'recs', {"fr": "$1 : $2 enregistrements", "pt": "$1: $2 registos", "es": "$1: $2 registros", "ar": "$1: $2 \u0633\u062c\u0644\u0627\u064b", "sw": "$1: rekodi $2"}],
  [/^Top 12 diseases with a specific label\. (\S+) of (\S+) records are not shown here \(disease or application unspecified, or outside the top 12\)\.$/, 'top12', {"fr": "12 principales maladies avec un libell\u00e9 sp\u00e9cifique. $1 sur $2 enregistrements ne sont pas affich\u00e9s ici (maladie ou application non sp\u00e9cifi\u00e9e, ou hors du top 12).", "pt": "12 principais doen\u00e7as com etiqueta espec\u00edfica. $1 de $2 registos n\u00e3o s\u00e3o apresentados aqui (doen\u00e7a ou aplica\u00e7\u00e3o n\u00e3o especificada, ou fora do top 12).", "es": "12 principales enfermedades con etiqueta espec\u00edfica. $1 de $2 registros no se muestran aqu\u00ed (enfermedad o aplicaci\u00f3n no especificada, o fuera del top 12).", "ar": "\u0623\u0647\u0645 12 \u0645\u0631\u0636\u0627\u064b \u0628\u062a\u0633\u0645\u064a\u0629 \u0645\u062d\u062f\u062f\u0629. $1 \u0645\u0646 $2 \u0633\u062c\u0644\u0627\u064b \u063a\u064a\u0631 \u0645\u0639\u0631\u0648\u0636\u0629 \u0647\u0646\u0627 (\u0645\u0631\u0636 \u0623\u0648 \u062a\u0637\u0628\u064a\u0642 \u063a\u064a\u0631 \u0645\u062d\u062f\u062f\u060c \u0623\u0648 \u062e\u0627\u0631\u062c \u0623\u0639\u0644\u0649 12).", "sw": "Magonjwa 12 makuu yenye lebo mahususi. Rekodi $1 kati ya $2 hazionyeshwi hapa (ugonjwa au matumizi hayajabainishwa, au nje ya 12 bora)."}]
  ];
  var lang = 'en', rec = new WeakMap(), pending = false;
  function has(o,k){ return Object.prototype.hasOwnProperty.call(o,k); }
  function tr1(s){
    var D = L[lang]; if (!D) return s;
    var m = s.match(/^(\s*)([\s\S]*?)(\s*)$/), core = m[2];
    if (!core) return s;
    if (has(D,core)) return m[1]+D[core]+m[3];
    for (var i=0;i<PR.length;i++){
      var p = PR[i], mm = p[0].exec(core);
      if (!mm) continue;
      if (p[1] === 'month'){ var mon = (MO[lang]||{})[mm[1]] || mm[1]; return m[1]+mon+' '+mm[2]+MS[lang]+mm[3]+MT[lang]+mm[4]+m[3]; }
      if (p[2][lang] !== undefined) return m[1]+p[2][lang].replace(/\$(\d)/g, function(_,i){ var g = mm[+i]; return (g === undefined) ? '' : (has(D,g) ? D[g] : g); })+m[3];
    }
    var parts = core.replace(/([.!?])\s+/g,'$1\u0001').split('\u0001');
    if (parts.length > 1){
      var hit = false;
      var out = parts.map(function(q){ if (has(D,q)){ hit = true; return D[q]; } return q; });
      if (hit) return m[1]+out.join(' ')+m[3];
    }
    if (/;|\s\|\s/.test(core) && core.length < 400){
      var sp = core.split(/(\s*;\s*|\s+\|\s+)/), h2 = false;
      var o2 = sp.map(function(q,ix){ if (ix%2===1) return q; if (has(D,q)){ h2 = true; return D[q]; } return q; });
      if (h2) return m[1]+o2.join('')+m[3];
    }
    return s;
  }
  function visit(n){
    var v = n.nodeValue, r = rec.get(n), t;
    if (r && v === r.t){
      t = (lang !== 'en') ? tr1(r.o) : r.o;
      if (t !== v){ n.nodeValue = t; r.t = t; }
      return;
    }
    t = (lang !== 'en') ? tr1(v) : v;
    rec.set(n,{o:v,t:t});
    if (t !== v) n.nodeValue = t;
  }
  function walk(){
    pending = false;
    if (!document.body) return;
    var w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, null), n, list = [];
    while ((n = w.nextNode())){
      var p = n.parentNode;
      if (!p || /^(SCRIPT|STYLE|TEXTAREA)$/.test(p.nodeName)) continue;
      if (p.closest && p.closest('.no-tr')) continue;
      list.push(n);
    }
    for (var i=0;i<list.length;i++) visit(list[i]);
  }
  function schedule(){ if (!pending){ pending = true; (window.requestAnimationFrame || setTimeout)(walk); } }
  function setLang(l){
    lang = (L[l] ? l : 'en');
    try { document.documentElement.lang = lang; document.documentElement.dir = 'ltr'; } catch(e){}
    var el = document.getElementById('lang-draft'); if (el) el.textContent = NOTE[lang] || '';
    schedule();
  }
  new MutationObserver(schedule).observe(document.documentElement, {childList:true, subtree:true, characterData:true});
  if (window.jQuery){
    jQuery(document).on('shiny:inputchanged', function(e){ if (e.name === 'ui_lang') setLang(e.value); });
    jQuery(document).on('shiny:connected', function(){ var c = jQuery('input[name=ui_lang]:checked').val(); if (c) setLang(c); });
  }
})();
)---"

ui <- dashboardPage(
  title = "PREIS AI Tracker",
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      style="font-weight:700; font-size:15px;",
      "PREIS AI Tracker"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",              tabName="overview",      icon=icon("chart-line")),
      menuItem("AI x Disease x Country",   tabName="disease_intel", icon=icon("virus")),       # [C3] NOUVEAU
      menuItem("Methods & Applications",tabName="methods",       icon=icon("microchip")),
      menuItem("Focus Africa",          tabName="africa",        icon=icon("globe-africa")),
      menuItem("Priority Intelligence", tabName="priority_intel",icon=icon("triangle-exclamation")),
      menuItem("All Records",           tabName="records",       icon=icon("table")),
      menuItem("Downloads",             tabName="downloads",     icon=icon("download")),
      menuItem("PREIS AI Atlas",        tabName="registry",      icon=icon("clipboard-list")),   # [REGISTRY]
      menuItem("Methods and limits",    tabName="limits",        icon=icon("circle-info"))
    ),
    br(),
    tags$style(HTML(".no-tr label{color:#ddd; font-weight:600;} .no-tr .shiny-options-group{margin-left:10px;} #lang-draft{padding:0 2px 6px 2px; font-size:11px; color:#f0ad4e;} html[lang='ar'] .content-wrapper, html[lang='ar'] .sidebar-menu {direction:rtl; text-align:right;}")),
    tags$div(class="no-tr", style="padding:0 10px;",
             radioButtons("ui_lang", label=NULL, choices=c("English"="en", "Fran\u00e7ais"="fr", "Portugu\u00eas"="pt", "Espa\u00f1ol"="es", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629"="ar", "Kiswahili"="sw"), selected="en", inline=TRUE),
             tags$div(id="lang-draft")),
    tags$div(style="padding:0 10px; font-size:11px; color:#aaa; text-transform:uppercase; letter-spacing:1px; margin-bottom:6px;",
             "Filters"),
    sliderInput("year_range","Year",min=min_year,max=max_year,
                value=c(max(min_year,2015L),max_year),sep=""),
    selectInput("sel_alert",    "Alert level",
                choices=c("All",sort(unique(safe_label_vec(ai_static$alert_level)))),
                selected="All"),
    selectInput("sel_disease",  "Disease",                                                 # [C2] NOUVEAU
                choices=c("All",sort(unique(safe_label_vec(ai_static$disease_primary)))),
                selected="All"),
    selectInput("sel_method",   "AI method",
                choices=c("All",sort(unique(safe_label_vec(ai_static$method_family_primary)))),
                selected="All"),
    selectInput("sel_app",      "Application",
                choices=c("All",sort(unique(safe_label_vec(ai_static$application_type_primary)))),
                selected="All"),
    selectInput("sel_rcc",      "RCC",
                choices=c("All",sort(unique(safe_label_vec(ai_static$rcc)))),
                selected="All"),
    selectInput("sel_country",  "Country",
                choices=c("All",sort(unique(safe_label_vec(ai_static$country)))),
                selected="All"),
    selectInput("sel_signal",   "Signal profile",
                choices=c("All",sort(unique(safe_label_vec(ai_static$signal_profile)))),
                selected="All"),
    selectInput("sel_lang",     "Language of publication",                                 # [LANG]
                choices=c("All",sort(unique(safe_label_vec(ai_static$language)))),
                selected="All")
  ),
  
  dashboardBody(
    tags$head(tags$script(HTML(PREIS_I18N_JS))),
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo,
      .skin-blue .main-header .navbar {
        background: linear-gradient(90deg,#0B6B3A 0%,#14532D 60%,#8B1E2D 100%) !important;
      }
      .skin-blue .main-sidebar { background-color:#0D2B1D !important; }
      .skin-blue .sidebar-menu>li.active>a { border-left-color:#D4A017 !important; background:#14532D !important; }
      .box.box-danger>.box-header, .box.box-primary>.box-header { background:#0B6B3A !important; color:#fff !important; }
      .box.box-danger, .box.box-primary { border-top-color:#D4A017 !important; }
      .box.box-warning { border-top-color:#D4A017 !important; }
      #map_africa { border-radius:8px; border:1px solid #cfd8d3; }
      .leaflet-container { font-family:inherit; }
      .info.legend { border-radius:8px !important; box-shadow:0 2px 10px rgba(0,0,0,.25) !important; padding:10px 12px !important; font-size:13px; }
      .info.legend i { width:16px !important; height:16px !important; border-radius:3px; opacity:1 !important; }
      .content-wrapper, .right-side { background-color:#f2f4f7 !important; }
      .box { border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.07) !important; }
      .small-box h3 { font-size:26px; } .dataTables_wrapper, table.dataTable {font-size:15px;} .box-title{font-size:19px;} .info.legend{font-size:15px !important;} .small-box p{font-size:16px;}
      .preis-header { padding:10px 0 4px; }
      .preis-title { font-size:22px; font-weight:700; margin-bottom:4px; }
      .preis-sub { font-size:13px; color:#555; }
      .badge-row { display:flex; flex-wrap:wrap; gap:10px; margin-top:10px; }
      .badge-item { color:white; padding:10px 16px; border-radius:8px;
                    font-weight:700; font-size:13px; white-space:nowrap; }
      .b-year  { background:#c0392b; }
      .b-month { background:#e67e22; }
      .b-week  { background:#f39c12; color:#333; }
      .matrix-cell { text-align:center; font-size:12px; }
    "))),
    
    # Header
    fluidRow(
      box(width=12, status="primary", solidHeader=FALSE,
          div(class="preis-header",
              div(class="preis-title",
                  "PREIS AI Tracker — Dashboard"),
              div(class="preis-sub",
                  "Monitoring AI applications for public health surveillance in Africa | ",
                  tags$b("Source: "), basename(data_file), " | ",
                  tags$b("Data generated: "),
                  tryCatch({ gd <- suppressWarnings(max(as.Date(ai_static$generated_date), na.rm=TRUE))
                             if (is.finite(gd)) as.character(gd) else "unknown" },
                           error=function(e) "unknown")),
              # [2026-10-06] Mention obligatoire : resultats automatiques, non valides humainement
              div(style="margin-top:8px; padding:8px 12px; background:#fff8e1; border-left:4px solid #f39c12; font-size:12px; color:#5d4a00;",
                  tags$b("Automated classification, not yet validated by human review. "),
                  "Disease, method, application and Africa-relevance labels and scores are produced by keyword rules and may contain errors. ",
                  "Records outside the infectious-disease/epidemic scope are excluded from this view. ",
                  "Do not use as a basis for operational decisions without expert verification."),
              uiOutput("pub_badges")
          )
      )
    ),
    
    # Value boxes
    fluidRow(
      valueBoxOutput("vb_total",     width=3),
      valueBoxOutput("vb_critical",  width=3),
      valueBoxOutput("vb_africa",    width=3),
      valueBoxOutput("vb_disease",   width=3)   # [C4] remplace vb_predictive
    ),
    
    # Tabs
    tabItems(
      
      ## ── OVERVIEW ──────────────────────────────────────────
      tabItem(tabName="overview",
              fluidRow(
                box(width=6, title="Priority distribution",    status="primary",  solidHeader=TRUE, plotOutput("plot_priority",      height=390)),
                box(width=6, title="Alert level distribution", status="danger",   solidHeader=TRUE, plotOutput("plot_alert_level",   height=390))
              ),
              fluidRow(
                box(width=6, title="AI methods",               status="success",  solidHeader=TRUE, plotOutput("plot_methods",       height=416)),
                box(width=6, title="AI applications",          status="success",  solidHeader=TRUE, plotOutput("plot_applications",  height=416))
              ),
              fluidRow(
                box(width=6, title="Africa relevance",         status="warning",  solidHeader=TRUE, plotOutput("plot_africa_rel",    height=364)),
                box(width=6, title="Novelty",                  status="warning",  solidHeader=TRUE, plotOutput("plot_novelty",       height=364))
              )
      ),
      
      ## ── [C3] IA × MALADIE × PAYS — NOUVEL ONGLET ─────────
      tabItem(tabName="disease_intel",
              fluidRow(
                box(width=12, status="danger", solidHeader=TRUE,
                    title="Diseases under AI surveillance — top priority",
                    plotOutput("plot_disease_priority", height=468))
              ),
              fluidRow(
                box(width=6, status="primary", solidHeader=TRUE,
                    title="AI method x Disease matrix",
                    DTOutput("table_method_disease")),
                box(width=6, status="success", solidHeader=TRUE,
                    title="Application x Disease matrix",
                    DTOutput("table_app_disease"))
              ),
              fluidRow(
                box(width=6, status="warning", solidHeader=TRUE,
                    title="Top countries per disease (Africa-linked)",
                    plotOutput("plot_disease_country", height=468)),
                box(width=6, status="info", solidHeader=TRUE,
                    title="Disease trend over time",
                    plotOutput("plot_disease_trend", height=468))
              ),
              fluidRow(
                box(width=12, status="primary", solidHeader=TRUE,
                    title="Disease intelligence records",
                    DTOutput("table_disease_records"))
              )
      ),
      
      ## ── METHODS & APPLICATIONS ────────────────────────────
      tabItem(tabName="methods",
              fluidRow(
                box(width=6, title="Methods by priority",     status="warning", solidHeader=TRUE, plotOutput("plot_methods_priority",  height=520)),
                box(width=6, title="Applications by priority",status="warning", solidHeader=TRUE, plotOutput("plot_app_priority",      height=520))
              ),
              fluidRow(
                box(width=12, title="Methods × Applications", status="info",    solidHeader=TRUE, DTOutput("table_method_app"))
              )
      ),
      
      ## ── FOCUS AFRICA ──────────────────────────────────────
      tabItem(tabName="africa",
              fluidRow(
                box(width=12, status="danger", solidHeader=TRUE,
                    title="Africa-linked AI records — geographic distribution",
                    checkboxInput("show_names", "Show country names on the map", value=TRUE),
                    leafletOutput("map_africa", height="calc(100vh - 190px)"))
              ),
              fluidRow(
                box(width=4, title="By RCC",               status="danger",   solidHeader=TRUE, plotOutput("plot_rcc",        height=390)),
                box(width=4, title="Top countries",        status="danger",   solidHeader=TRUE, plotOutput("plot_country",    height=390)),
                box(width=4, title="Geography status",     status="danger",   solidHeader=TRUE, plotOutput("plot_geo_status", height=390))
              ),
              # [C5] Évolution temporelle
              fluidRow(
                box(width=12, title="Africa-linked records — trend over time", status="warning", solidHeader=TRUE,
                    plotOutput("plot_africa_trend", height=364))
              )
      ),
      
      ## ── PRIORITY INTELLIGENCE — [C8] nettoyé ──────────────
      tabItem(tabName="priority_intel",
              fluidRow(
                box(width=4, status="danger",  solidHeader=TRUE,
                    title="Critical alerts",
                    DTOutput("table_critical_small")),
                box(width=8, status="primary", solidHeader=TRUE,
                    title="Priority score distribution by disease",
                    plotOutput("plot_priority_disease", height=416))
              ),
              fluidRow(
                box(width=12, title="High priority intelligence records", status="primary", solidHeader=TRUE,
                    DTOutput("table_high_priority"))
              )
      ),
      
      ## ── ALL RECORDS ───────────────────────────────────────
      tabItem(tabName="records",
              fluidRow(
                box(width=12, title="All intelligence records", status="primary", solidHeader=TRUE,
                    DTOutput("table_records"))
              )
      ),
      
      ## ── DOWNLOADS ─────────────────────────────────────────
      tabItem(tabName="downloads",
              fluidRow(
                box(width=4, title="Filtered dataset",       status="success", solidHeader=TRUE,
                    p("All records matching current filters."),
                    downloadButton("dl_filtered", "Download CSV")),
                box(width=4, title="Africa-linked dataset",  status="success", solidHeader=TRUE,
                    p("Records with African geographic signal."),
                    downloadButton("dl_africa", "Download CSV")),
                box(width=4, title="High priority alerts",   status="danger",  solidHeader=TRUE,
                    p("High-priority records (top priority tier)."),
                    downloadButton("dl_high", "Download CSV"))
              ),
              fluidRow(
                box(width=12, status="info", solidHeader=TRUE,
                    title="Dataset information",
                    tableOutput("tbl_data_info"))
              )
      )
      ,
      registry_ui(),   # [REGISTRY]
      ## [C22] Methods and limits (28 Oct 2026)
      tabItem(tabName="limits",
              fluidRow(
                box(width=12, status="warning", solidHeader=TRUE, title="Methods and limits",
                    tags$p(tags$b("Status: research prototype."), " Automatic screening of public bibliographic metadata (PubMed, Europe PMC, bioRxiv, medRxiv, arXiv). Titles, authors, journal, year and links are shown; abstracts are not reproduced. Follow the link to read the source."),
                    tags$ul(
                      tags$li("All labels (AI method, application, disease, country, public-health scope, priority) are produced automatically by rules and have NOT been validated for operational decisions."),
                      tags$li("Pilot validation: two samples of 50 articles, one reviewer, stratified and unweighted; agreement for AI as study method 43/50 in each sample. Scope and geography are the weakest dimensions. No result should be read as a prevalence."),
                      tags$li("Preprints are the majority of records and are not peer reviewed. Europe PMC was unavailable from 16 to 30 October 2026 and was reloaded on 31 October 2026; records added by the source between runs can be missed until the next run."),
                      tags$li("A study country is assigned to only a small share of records; country counts are lower bounds, not the distribution of research."),
                      tags$li("Associations are not causal and predictions are not certainties. Alerts are prepared in test mode; test emails are sent only to the author's own addresses. An outbreak mode (active for the 2026 Bundibugyo virus disease outbreak) widens routing of filovirus articles to the response team."),
                      tags$li("This tool is not an official product of, and is not endorsed by, any institution. The views are those of the author(s).")
                    ),
                    tags$p("Last data load: ", textOutput("limits_updated", inline=TRUE))
                )
              )
      )
    )
  )
)

## ------------------------------------------------------------
## SERVER
## ------------------------------------------------------------
server <- function(input, output, session) {
  
  ## [LANG 20261115] libelles des graphiques (images): suivent la langue choisie
  PLOT_L <- list(
    fr = c("Records"="Enregistrements", "Year"="Ann\u00e9e", "Africa-linked records"="Enregistrements li\u00e9s \u00e0 l'Afrique", "Disease"="Maladie", "Mean priority score (0-10)"="Score de priorit\u00e9 moyen (0-10)", "Alert level"="Niveau d'alerte", "Top 20 named diseases. "="20 principales maladies nomm\u00e9es. ", " records with no specific disease (Unspecified) are not shown."=" enregistrements sans maladie pr\u00e9cise (non sp\u00e9cifi\u00e9e) ne sont pas affich\u00e9s.", "Top 3 countries per disease (Africa-linked)"="3 principaux pays par maladie (li\u00e9s \u00e0 l'Afrique)", "Top RCC per disease (no country data available)"="Principaux CCR par maladie (aucune donn\u00e9e de pays disponible)", "Loess trend line"="Ligne de tendance Loess"),
    pt = c("Records"="Registos", "Year"="Ano", "Africa-linked records"="Registos ligados a \u00c1frica", "Disease"="Doen\u00e7a", "Mean priority score (0-10)"="Pontua\u00e7\u00e3o m\u00e9dia de prioridade (0-10)", "Alert level"="N\u00edvel de alerta", "Top 20 named diseases. "="20 principais doen\u00e7as nomeadas. ", " records with no specific disease (Unspecified) are not shown."=" registos sem doen\u00e7a espec\u00edfica (n\u00e3o especificada) n\u00e3o s\u00e3o apresentados.", "Top 3 countries per disease (Africa-linked)"="3 principais pa\u00edses por doen\u00e7a (ligados a \u00c1frica)", "Top RCC per disease (no country data available)"="Principais CCR por doen\u00e7a (sem dados de pa\u00eds)", "Loess trend line"="Linha de tend\u00eancia Loess"),
    es = c("Records"="Registros", "Year"="A\u00f1o", "Africa-linked records"="Registros vinculados a \u00c1frica", "Disease"="Enfermedad", "Mean priority score (0-10)"="Puntuaci\u00f3n media de prioridad (0-10)", "Alert level"="Nivel de alerta", "Top 20 named diseases. "="20 principales enfermedades nombradas. ", " records with no specific disease (Unspecified) are not shown."=" registros sin enfermedad espec\u00edfica (no especificada) no se muestran.", "Top 3 countries per disease (Africa-linked)"="3 principales pa\u00edses por enfermedad (vinculados a \u00c1frica)", "Top RCC per disease (no country data available)"="Principales CCR por enfermedad (sin datos de pa\u00eds)", "Loess trend line"="L\u00ednea de tendencia Loess"),
    ar = c("Records"="\u0627\u0644\u0633\u062c\u0644\u0627\u062a", "Year"="\u0627\u0644\u0633\u0646\u0629", "Africa-linked records"="\u0633\u062c\u0644\u0627\u062a \u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627", "Disease"="\u0627\u0644\u0645\u0631\u0636", "Mean priority score (0-10)"="\u0645\u062a\u0648\u0633\u0637 \u062f\u0631\u062c\u0629 \u0627\u0644\u0623\u0648\u0644\u0648\u064a\u0629 (0-10)", "Alert level"="\u0645\u0633\u062a\u0648\u0649 \u0627\u0644\u062a\u0646\u0628\u064a\u0647", "Top 20 named diseases. "="\u0623\u0647\u0645 20 \u0645\u0631\u0636\u0627\u064b \u0645\u0633\u0645\u0651\u0649. ", " records with no specific disease (Unspecified) are not shown."=" \u0633\u062c\u0644\u0627\u064b \u0628\u0644\u0627 \u0645\u0631\u0636 \u0645\u062d\u062f\u062f (\u063a\u064a\u0631 \u0645\u062d\u062f\u062f) \u063a\u064a\u0631 \u0645\u0639\u0631\u0648\u0636\u0629.", "Top 3 countries per disease (Africa-linked)"="\u0623\u0647\u0645 3 \u0628\u0644\u062f\u0627\u0646 \u0644\u0643\u0644 \u0645\u0631\u0636 (\u0627\u0644\u0645\u0631\u062a\u0628\u0637\u0629 \u0628\u0623\u0641\u0631\u064a\u0642\u064a\u0627)", "Top RCC per disease (no country data available)"="\u0623\u0647\u0645 \u0627\u0644\u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0644\u0643\u0644 \u0645\u0631\u0636 (\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u0628\u0644\u062f)", "Loess trend line"="\u062e\u0637 \u0627\u062a\u062c\u0627\u0647 \u0644\u0648\u064a\u0633"),
    sw = c("Records"="Rekodi", "Year"="Mwaka", "Africa-linked records"="Rekodi zinazohusiana na Afrika", "Disease"="Ugonjwa", "Mean priority score (0-10)"="Wastani wa alama za kipaumbele (0-10)", "Alert level"="Kiwango cha tahadhari", "Top 20 named diseases. "="Magonjwa 20 yaliyotajwa zaidi. ", " records with no specific disease (Unspecified) are not shown."=" rekodi zisizo na ugonjwa mahususi (haijabainishwa) hazionyeshwi.", "Top 3 countries per disease (Africa-linked)"="Nchi 3 kuu kwa kila ugonjwa (zinazohusiana na Afrika)", "Top RCC per disease (no country data available)"="RCC kuu kwa kila ugonjwa (hakuna data ya nchi)", "Loess trend line"="Mstari wa mwelekeo wa Loess")
  )
  tx <- function(s) { l <- input$ui_lang; if (!is.null(l) && !(identical(l, "ar") && !PREIS_RAGG) && l %in% names(PLOT_L) && s %in% names(PLOT_L[[l]])) unname(PLOT_L[[l]][[s]]) else s }
  tvf <- function() { l <- input$ui_lang; function(x) preis_tv(x, l) }
  
  # [C7] Auto-refresh toutes les heures si le fichier change
  ai_data <- reactiveFileReader(
    intervalMillis = 3600000,   # 1 heure
    session = session,
    filePath = data_file,
    readFunc = load_ai_data
  )
  
  # [C2] filtered_data avec TOUS les filtres incluant disease
  filtered_data <- reactive({
    x <- ai_data()
    x <- x %>% filter(
      is.na(year_num) | (year_num >= input$year_range[1] & year_num <= input$year_range[2])
    )
    if (!is.null(input$sel_alert)   && input$sel_alert   != "All")
      x <- x %>% filter(alert_level            == input$sel_alert)
    if (!is.null(input$sel_disease) && input$sel_disease != "All")   # [C2]
      x <- x %>% filter(disease_primary        == input$sel_disease)
    if (!is.null(input$sel_method)  && input$sel_method  != "All")
      x <- x %>% filter(method_family_primary  == input$sel_method)
    if (!is.null(input$sel_app)     && input$sel_app     != "All")
      x <- x %>% filter(application_type_primary == input$sel_app)
    if (!is.null(input$sel_rcc)     && input$sel_rcc     != "All")
      x <- x %>% filter(rcc                    == input$sel_rcc)
    if (!is.null(input$sel_country) && input$sel_country != "All")
      x <- x %>% filter(country                == input$sel_country)
    if (!is.null(input$sel_signal)  && input$sel_signal  != "All")
      x <- x %>% filter(signal_profile         == input$sel_signal)
    if (!is.null(input$sel_lang)    && input$sel_lang    != "All")   # [LANG] s applique a tous les onglets
      x <- x %>% filter(language               == input$sel_lang)
    x
  })
  
  registry_server(input, output, session, reg_dir, filtered_data)   # [REGISTRY] (after filtered_data)

  # Badges nouvelles publications
  output$pub_badges <- renderUI({
    x       <- filtered_data()
    n_yr    <- sum(x$is_new_this_year,  na.rm=TRUE)
    n_mo    <- sum(x$is_new_this_month, na.rm=TRUE)
    n_wk    <- sum(x$is_new_this_week,  na.rm=TRUE)
    n_yr_af <- sum(x$is_new_this_year  & x$africa_relevance_score > 0, na.rm=TRUE)
    n_mo_af <- sum(x$is_new_this_month & x$africa_relevance_score > 0, na.rm=TRUE)
    n_wk_af <- sum(x$is_new_this_week  & x$africa_relevance_score > 0, na.rm=TRUE)
    pct     <- function(a, b) ifelse(b > 0, paste0(round(a/b*100), "%"), "0%")
    ew_lbl  <- paste0("EpiWeek W",
                      formatC(lubridate::epiweek(Sys.Date()), width=2, flag="0"),
                      "/", lubridate::epiyear(Sys.Date()))
    div(class="badge-row",
      div(class="badge-item b-year",
          paste0("Year ", lubridate::year(Sys.Date()), ": ",
                 scales::comma(n_yr), " / Africa: ",
                 scales::comma(n_yr_af), " (", pct(n_yr_af, n_yr), ")")),
      div(class="badge-item b-month",
          paste0(format(Sys.Date(), "%B %Y"), ": ",
                 scales::comma(n_mo), " / Africa: ",
                 scales::comma(n_mo_af), " (", pct(n_mo_af, n_mo), ")")),
      div(class="badge-item b-week",
          paste0("This week (", format(Sys.Date()-7L, "%d %b"), "-", format(Sys.Date(), "%d %b"), "): ",
                 scales::comma(n_wk), " / Africa: ",
                 scales::comma(n_wk_af), " (", pct(n_wk_af, n_wk), ")")))
  })

  
  # Value boxes
  output$vb_total <- renderValueBox({
    valueBox(scales::comma(nrow(filtered_data())),
             "AI records", icon=icon("database"), color="blue")
  })
  output$vb_critical <- renderValueBox({
    n <- sum(filtered_data()$alert_level %in% c("Critical","High"), na.rm=TRUE)
    valueBox(scales::comma(n),
             "High-priority records", icon=icon("triangle-exclamation"), color="red")
  })
  output$vb_africa <- renderValueBox({
    d <- filtered_data()
    n <- sum(d$africa_relevance_score > 0, na.rm=TRUE)
    n_map <- sum(d$africa_relevance_score > 0 & !is.na(d$country) & d$country != "Unspecified", na.rm=TRUE)
    valueBox(scales::comma(n),
             paste0("Africa-linked (", scales::comma(n_map), " on map)"), icon=icon("globe-africa"), color="green")
  })
  output$vb_disease <- renderValueBox({                              # [C4]
    n <- n_distinct(filtered_data()$disease_primary[
      filtered_data()$disease_primary != "Non spécifié"])
    valueBox(scales::comma(n),
             "Diseases under surveillance", icon=icon("virus"), color="purple")
  })
  
  ## ── OVERVIEW plots ────────────────────────────────────────
  output$plot_priority <- renderPlot({
    filtered_data() %>%
      count(priority_level, sort=TRUE) %>%
      ggplot(aes(reorder(priority_level,n), n, fill=priority_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=priority_colors, na.value="grey70") +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_alert_level <- renderPlot({
    filtered_data() %>%
      count(alert_level, sort=TRUE) %>%
      ggplot(aes(reorder(alert_level,n), n, fill=alert_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=alert_colors, na.value="grey70") +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_methods <- renderPlot({
    filtered_data() %>%
      filter(method_family_primary != "Unspecified") %>%
      count(method_family_primary, sort=TRUE) %>% slice_head(n=10) %>%
      ggplot(aes(reorder(method_family_primary,n), n)) +
      geom_col(fill="#2980B9", show.legend=FALSE) +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_applications <- renderPlot({
    filtered_data() %>%
      filter(application_type_primary != "Unspecified") %>%
      count(application_type_primary, sort=TRUE) %>% slice_head(n=10) %>%
      ggplot(aes(reorder(application_type_primary,n), n)) +
      geom_col(fill="#27AE60", show.legend=FALSE) +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_africa_rel <- renderPlot({
    filtered_data() %>%
      count(africa_relevance_level, sort=TRUE) %>%
      ggplot(aes(reorder(africa_relevance_level,n), n, fill=africa_relevance_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=africa_rel_colors, na.value="grey70") +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_novelty <- renderPlot({
    filtered_data() %>%
      count(novelty_level, sort=TRUE) %>%
      ggplot(aes(reorder(novelty_level,n), n, fill=novelty_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=novelty_colors, na.value="grey70") +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  ## ── [C3] IA × MALADIE × PAYS plots ───────────────────────
  
  output$plot_disease_priority <- renderPlot({
    filtered_data() %>%
      filter(disease_primary != "Non spécifié") -> dd_all
    n_unspec <- sum(dd_all$disease_primary %in% c("Unspecified","Non spécifié"))
    dd_all %>%
      filter(!disease_primary %in% c("Unspecified","Non spécifié")) %>%
      count(disease_primary, alert_level) %>%
      group_by(disease_primary) %>%
      mutate(total=sum(n)) %>% ungroup() %>%
      filter(total >= sort(unique(total), decreasing=TRUE)[min(20, n_distinct(disease_primary))]) %>%
      ggplot(aes(reorder(disease_primary, total), n, fill=alert_level)) +
      geom_col() +
      scale_fill_manual(values=alert_colors, na.value="grey70", name=tx("Alert level"), labels=tvf()) +
      scale_x_discrete(labels=tvf()) + coord_flip() +
      labs(x=NULL, y=tx("Records"), title=NULL,
           caption=paste0(tx("Top 20 named diseases. "), format(n_unspec, big.mark=","),
                          tx(" records with no specific disease (Unspecified) are not shown."))) +
      theme_minimal(base_size=20) +
      theme(legend.position="bottom", plot.caption=element_text(hjust=0, colour="#555555"))
  })
  
  output$table_method_disease <- renderDT({
    filtered_data() %>%
      filter(disease_primary != "Non spécifié",
             method_family_primary != "Unspecified") %>%
      count(method_family_primary, disease_primary) %>%
      pivot_wider(names_from=disease_primary, values_from=n, values_fill=0) %>%
      rename(`AI Method`=method_family_primary) %>%
      datatable(rownames=FALSE, options=list(pageLength=10, scrollX=TRUE),
                caption="Rows = AI method | Columns = disease")
  })
  
  output$table_app_disease <- renderDT({
    filtered_data() %>%
      filter(disease_primary != "Non spécifié",
             application_type_primary != "Unspecified") %>%
      count(application_type_primary, disease_primary) %>%
      pivot_wider(names_from=disease_primary, values_from=n, values_fill=0) %>%
      rename(Application=application_type_primary) %>%
      datatable(rownames=FALSE, options=list(pageLength=10, scrollX=TRUE),
                caption="Rows = Application | Columns = disease")
  })
  
  output$plot_disease_country <- renderPlot({
    d <- filtered_data() %>%
      filter(disease_primary != "Non specifié",
             disease_primary != "Non spécifié",
             !is.na(country), country != "Unspecified")
    
    if (nrow(d) == 0) {
      # Fallback : utiliser africa_relevance_score > 0 sans filtre country strict
      d <- filtered_data() %>%
        filter(disease_primary != "Non specifié",
               disease_primary != "Non spécifié",
               africa_relevance_score > 0)
    }
    
    if (nrow(d) == 0) {
      plot.new(); title(preis_tv("No data with country information", input$ui_lang)); return(invisible())
    }
    
    # Grouper par pays si disponible, sinon par RCC
    if (sum(!is.na(d$country) & d$country != "Unspecified") > 10) {
      d %>%
        filter(country != "Unspecified") %>%
        count(disease_primary, country, sort=TRUE) %>%
        group_by(disease_primary) %>%
        slice_max(n, n=3, with_ties=FALSE) %>% ungroup() %>%
        slice_max(n, n=40, with_ties=FALSE) %>%
        mutate(label=paste0(disease_primary, " — ", country)) %>%
        ggplot(aes(reorder(label, n), n, fill=disease_primary)) +
        geom_col(show.legend=FALSE) +
        scale_x_discrete(labels=tvf()) + coord_flip() +
        labs(x=NULL, y=tx("Records"),
             subtitle=tx("Top 3 countries per disease (Africa-linked)")) +
        theme_minimal(base_size=18)
    } else {
      # Fallback RCC si pas de country
      d %>%
        filter(!is.na(rcc), rcc != "Unspecified") %>%
        count(disease_primary, rcc, sort=TRUE) %>%
        group_by(disease_primary) %>%
        slice_max(n, n=3, with_ties=FALSE) %>% ungroup() %>%
        slice_max(n, n=40, with_ties=FALSE) %>%
        mutate(label=paste0(disease_primary, " — ", rcc)) %>%
        ggplot(aes(reorder(label, n), n, fill=disease_primary)) +
        geom_col(show.legend=FALSE) +
        scale_x_discrete(labels=tvf()) + coord_flip() +
        labs(x=NULL, y=tx("Records"),
             subtitle=tx("Top RCC per disease (no country data available)")) +
        theme_minimal(base_size=18)
    }
  })
  
  output$plot_disease_trend <- renderPlot({
    d <- filtered_data() %>%
      filter(disease_primary != "Non spécifié", !is.na(year_num),
             year_num >= 2010) %>%
      count(year_num, disease_primary)
    top_dis <- d %>% group_by(disease_primary) %>%
      summarise(tot=sum(n)) %>% slice_max(tot, n=8) %>% pull(disease_primary)
    d %>% filter(disease_primary %in% top_dis) %>%
      ggplot(aes(year_num, n, color=disease_primary, group=disease_primary)) +
      geom_line(linewidth=0.9) + geom_point(size=3) +
      scale_color_discrete(labels=tvf()) +
      labs(x=tx("Year"), y=tx("Records"), color=tx("Disease")) +
      theme_minimal(base_size=19) +
      theme(legend.position="bottom",
            legend.text=element_text(size=16))
  })
  
  output$table_disease_records <- renderDT({             # [C4]
    filtered_data() %>%
      filter(disease_primary != "Non spécifié") %>%
      arrange(desc(priority_score)) %>%
      select(disease_primary, disease_confidence, disease_mentioned, alert_level, title, year, country, countries_mentioned, rcc,
             method_family_primary, application_type_primary,
             priority_score, africa_relevance_level,
             novelty_level, url) %>%
      mutate(url=if_else(url!="",
                         paste0('<a href="',url,'" target="_blank">link</a>'), "")) %>%
      datatable(rownames=FALSE, escape=FALSE,
                options=list(pageLength=12, scrollX=TRUE))
  })
  
  ## ── METHODS plots ─────────────────────────────────────────
  output$plot_methods_priority <- renderPlot({
    z <- filtered_data() %>%
      filter(method_family_primary != "Unspecified") %>%
      count(method_family_primary, priority_level)
    top <- z %>% group_by(method_family_primary) %>%
      summarise(n=sum(n)) %>% slice_max(n, n=10, with_ties=FALSE)
    z %>% semi_join(top, by="method_family_primary") %>%
      ggplot(aes(method_family_primary, n, fill=priority_level)) +
      geom_col() +
      scale_fill_manual(values=priority_colors, na.value="grey70", labels=tvf(), name=preis_tv("Priority level", input$ui_lang)) +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_app_priority <- renderPlot({
    z <- filtered_data() %>%
      filter(application_type_primary != "Unspecified") %>%
      count(application_type_primary, priority_level)
    top <- z %>% group_by(application_type_primary) %>%
      summarise(n=sum(n)) %>% slice_max(n, n=10, with_ties=FALSE)
    z %>% semi_join(top, by="application_type_primary") %>%
      ggplot(aes(application_type_primary, n, fill=priority_level)) +
      geom_col() +
      scale_fill_manual(values=priority_colors, na.value="grey70", labels=tvf(), name=preis_tv("Priority level", input$ui_lang)) +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$table_method_app <- renderDT({
    filtered_data() %>%
      count(method_family_primary, application_type_primary, sort=TRUE) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=12, scrollX=TRUE))
  })
  
  ## ── AFRICA plots ──────────────────────────────────────────
  output$plot_rcc <- renderPlot({
    filtered_data() %>%
      filter(rcc != "Unspecified") %>%
      count(rcc, sort=TRUE) %>%
      ggplot(aes(reorder(rcc,n), n, fill=rcc)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=rcc_colors, na.value="grey70") +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_country <- renderPlot({
    filtered_data() %>%
      filter(country != "Unspecified") %>%
      count(country, sort=TRUE) %>% slice_head(n=12) %>%
      ggplot(aes(reorder(country,n), n)) +
      geom_col(fill="#2980B9") +
      scale_x_discrete(labels=tvf()) + coord_flip() + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  output$plot_geo_status <- renderPlot({
    filtered_data() %>%
      mutate(geo = ifelse(country != "Unspecified",
                          "African geography detected",
                          "No geography detected")) %>%
      count(geo) %>%
      ggplot(aes(geo, n, fill=geo)) +
      geom_col(show.legend=FALSE) +
      scale_x_discrete(labels=tvf()) + labs(x=NULL, y=tx("Records")) + theme_minimal(base_size=19)
  })
  
  # [C5] Évolution temporelle Africa
  output$plot_africa_trend <- renderPlot({
    filtered_data() %>%
      filter(africa_relevance_score > 0, !is.na(year_num), year_num >= 2010) %>%
      count(year_num) %>%
      ggplot(aes(year_num, n)) +
      geom_line(color="#C0392B", linewidth=1.2) +
      geom_point(color="#C0392B", size=3) +
      geom_smooth(method="loess", se=TRUE, alpha=0.15, color="#7A1D2F") +
      labs(x=tx("Year"), y=tx("Africa-linked records"),
           caption=tx("Loess trend line")) +
      theme_minimal(base_size=19)
  })
  
  output$map_africa <- renderLeaflet({
    x <- filtered_data()
    
    # Normaliser clé pays
    norm_key <- function(v) {
      v <- tolower(iconv(as.character(v), to="ASCII//TRANSLIT"))
      gsub("[^a-z]", "", v)
    }
    
    # Agréger par pays
    md <- x %>%
      filter(!is.na(country), country != "Unspecified") %>%
      mutate(ck = norm_key(country),
             rcc = ifelse(is.na(rcc)|rcc=="Unspecified","Unspecified",rcc)) %>%
      group_by(country, ck, iso3, rcc) %>%
      summarise(n_records=n(),
                n_high=sum(alert_level %in% c("Critical","High"), na.rm=TRUE),
                n_moderate=sum(alert_level %in% c("Moderate","Medium"), na.rm=TRUE),
                .groups="drop")
    
    # Joindre coordonnées
    if (!is.null(country_points) && nrow(md) > 0) {
      pts <- country_points %>%
        mutate(ck = norm_key(country_join),
               iso3_up = toupper(trimws(iso3_join))) %>%
        select(ck, iso3_up, lon, lat) %>%
        distinct(ck, .keep_all=TRUE)
      
      md <- md %>%
        mutate(iso3_up = toupper(trimws(iso3))) %>%
        left_join(pts %>% select(ck, lon, lat), by="ck")
      
      # Fallback sur iso3
      miss <- is.na(md$lon)
      if (any(miss)) {
        pts_iso <- country_points %>%
          mutate(iso3_up=toupper(trimws(iso3_join))) %>%
          select(iso3_up, lon, lat) %>%
          distinct(iso3_up, .keep_all=TRUE)
        fb <- md[miss,] %>% left_join(pts_iso, by="iso3_up")
        fb <- md %>% select(iso3_up) %>% left_join(pts_iso, by="iso3_up")
        md$lon <- dplyr::coalesce(md$lon, fb$lon)
        md$lat <- dplyr::coalesce(md$lat, fb$lat)
      }
      md <- md %>% filter(!is.na(lon), !is.na(lat))
    } else {
      md <- md %>% mutate(lon=NA_real_, lat=NA_real_) %>% filter(FALSE)
    }
    
    # [20261012] Refonte cartographique : fond clair sans libelles (lisibilite du choroplethe), satellite en option
    m <- leaflet(options=leafletOptions(minZoom=3, maxZoom=8)) %>%
      fitBounds(lng1=-19, lat1=-35, lng2=52, lat2=37.5) %>%
      setMaxBounds(lng1=-30, lat1=-42, lng2=62, lat2=42) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group="Light map") %>%   # sans cle API (CartoDB exige desormais une cle)
      addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
      addLayersControl(baseGroups=c("Light map","Satellite"), position="topright",
                       options=layersControlOptions(collapsed=TRUE)) %>%
      addScaleBar(position="bottomleft")
    
    if (!is.null(africa_map)) {
      mp_pl <- sf::st_drop_geometry(africa_map)
      mp_gm <- sf::st_geometry(africa_map)
      cc <- names(mp_pl)[tolower(names(mp_pl)) %in%
                           c("country","name","admin","country_name","sovereignt")][1]
      if (!is.na(cc)) {
        # Normaliser les noms pour la jointure (GeoJSON utilise country_key lowercase)
        mp_pl$country_join     <- clean_txt(mp_pl[[cc]])
        mp_pl$country_join_key <- tolower(iconv(mp_pl$country_join,
                                                to="ASCII//TRANSLIT"))
        mp_pl$country_join_key <- gsub("[^a-z]", "", mp_pl$country_join_key)
        
        poly_c <- x %>%
          filter(country != "Unspecified") %>%
          mutate(
            country_key_norm = tolower(iconv(country, to="ASCII//TRANSLIT")),
            country_key_norm = gsub("[^a-z]", "", country_key_norm)
          ) %>%
          count(country, country_key_norm, rcc, name="n") %>%
          rename(rcc_fill=rcc)
        
        # Jointure sur clé normalisée
        rcc_fallback_vec <- if ("rcc" %in% names(mp_pl)) mp_pl$rcc else NA_character_
        mp_pl <- mp_pl %>%
          left_join(poly_c, by=c("country_join_key"="country_key_norm")) %>%
          mutate(
            n        = ifelse(is.na(n), 0L, as.integer(n)),
            rcc_fill = case_when(
              !is.na(rcc_fill) & rcc_fill != "" ~ rcc_fill,
              # Fallback : RCC depuis GeoJSON si disponible
              !is.na(rcc_fallback_vec) & rcc_fallback_vec != "" ~ rcc_fallback_vec,
              TRUE ~ "Unspecified"
            )
          )
        # [20261010] Noms de pays affiches sur la carte
        ctr <- tryCatch({
          suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(
            sf::st_sf(mp_pl, geometry=mp_gm)))))
        }, error=function(e) NULL)
        message("DIAG rcc_fill Nord: ", paste(capture.output(print(mp_pl[grepl("morocco|algeria|libya|egypt|tunisia", tolower(mp_pl$country_join_key)), c("country_join","rcc_fill","n")])), collapse=" | "))
        poly <- sf::st_sf(mp_pl, geometry=mp_gm)
        m <- m %>% addPolygons(
          data=poly,
          fillColor=~unname(rcc_colors[ifelse(
            rcc_fill %in% names(rcc_colors), rcc_fill, "Unspecified")]),
          color=~ifelse(n > 0, "#263238", "#FFFFFF"), weight=~ifelse(n > 0, 2, 1), opacity=1, fillOpacity=0.85,   # [20261015] couleurs pleines = legende RCC ; bordure sombre = pays avec enregistrements
          highlightOptions=highlightOptions(weight=2.5, color="#263238", bringToFront=TRUE),
          label=~paste0(country_join, " - ", scales::comma(n), " records"),
          popup=~paste0(
            "<b>", country_join, "</b><br/>",
            "RCC: ", rcc_fill, "<br/>",
            "Records: ", scales::comma(n)
          )
        )
        # [20261012] Etiquettes hierarchisees par niveau de zoom (grands pays -> noms ; petits -> code ISO3 puis nom)
        m_before_labels <- m   # [20261014] si les etiquettes echouent, la carte s'affiche quand meme
        m <- tryCatch({
        if ((is.null(input$show_names) || isTRUE(input$show_names)) && !is.null(ctr) && nrow(ctr) == nrow(poly)) {
          short_nm <- c("Democratic Republic of the Congo"="DR Congo", "Republic of the Congo"="Congo",
            "Kingdom of eSwatini"="Eswatini", "Republic of Cabo Verde"="Cabo Verde",
            "Central African Republic"="C.A.R.", "S\u00e3o Tom\u00e9 and Principe"="S\u00e3o Tom\u00e9",
            "The Gambia"="Gambia", "Somaliland"="")
          iso_nm <- c(Zimbabwe="ZWE", Zambia="ZMB", Uganda="UGA", Tunisia="TUN", Togo="TGO", Tanzania="TZA",
            "Kingdom of eSwatini"="SWZ", "South Sudan"="SSD", Sudan="SDN", "South Africa"="ZAF", Somalia="SOM",
            "Sierra Leone"="SLE", Senegal="SEN", "S\u00e3o Tom\u00e9 and Principe"="STP", Rwanda="RWA", Nigeria="NGA",
            Niger="NER", Namibia="NAM", Mozambique="MOZ", Morocco="MAR", "Western Sahara"="ESH", Mauritania="MRT",
            Mali="MLI", Malawi="MWI", Madagascar="MDG", Libya="LBY", Liberia="LBR", Lesotho="LSO", Kenya="KEN",
            "Guinea-Bissau"="GNB", Guinea="GIN", Ghana="GHA", "The Gambia"="GMB", Gabon="GAB", Ethiopia="ETH",
            Eritrea="ERI", "Equatorial Guinea"="GNQ", Egypt="EGY", Djibouti="DJI", "C\u00f4te d'Ivoire"="CIV",
            "Democratic Republic of the Congo"="COD", "Republic of the Congo"="COG", Comoros="COM", Chad="TCD",
            "Central African Republic"="CAF", "Republic of Cabo Verde"="CPV", Cameroon="CMR", Burundi="BDI",
            "Burkina Faso"="BFA", Botswana="BWA", Benin="BEN", Angola="AGO", Algeria="DZA")
          nm  <- as.character(poly$country_join)
          disp <- ifelse(nm %in% names(short_nm), unname(short_nm[nm]), nm)
          iso  <- unname(iso_nm[nm]); iso[is.na(iso)] <- ""
          # [20261014] surface approchee par boite englobante : st_area() exige le paquet lwgeom (absent sur shinyapps.io) quand s2 est desactive
          ar <- vapply(sf::st_geometry(poly), function(g) { b <- sf::st_bbox(g); as.numeric((b[3]-b[1])*(b[4]-b[2])) }, numeric(1))
          ar[is.na(ar)] <- 0
          big <- ar >= sort(ar, decreasing=TRUE)[min(20, length(ar))]
          keep <- nzchar(disp)
          lab_style <- function(sz) list("font-size"=sz, "font-weight"="600", "color"="#1F2A30",
            "letter-spacing"="0.2px", "white-space"="nowrap",
            "text-shadow"="-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff, 0 0 4px #fff")
          lo <- function(sz) labelOptions(noHide=TRUE, direction="center", textOnly=TRUE, style=lab_style(sz))
          m <- m %>%
            addLabelOnlyMarkers(lng=ctr[big & keep,1], lat=ctr[big & keep,2], label=disp[big & keep],
                                group="lbl_big", labelOptions=lo("12px")) %>%
            addLabelOnlyMarkers(lng=ctr[!big & keep & nzchar(iso),1], lat=ctr[!big & keep & nzchar(iso),2],
                                label=iso[!big & keep & nzchar(iso)], group="lbl_iso", labelOptions=lo("9.5px")) %>%
            addLabelOnlyMarkers(lng=ctr[!big & keep,1], lat=ctr[!big & keep,2], label=disp[!big & keep],
                                group="lbl_small", labelOptions=lo("10.5px")) %>%
            groupOptions("lbl_big",   zoomLevels=3:8) %>%
            groupOptions("lbl_iso",   zoomLevels=4:4) %>%
            groupOptions("lbl_small", zoomLevels=5:8)
        }
        m
        }, error=function(e) { message("LABELS ERR: ", conditionMessage(e)); m_before_labels })
      }
    }
    
    if (nrow(md) > 0)
      m <- m %>% addCircleMarkers(
        data=md, lng=~lon, lat=~lat,
        radius=~pmax(8, pmin(32, sqrt(n_records)*3.4)),
        color="#FFFFFF", weight=3, fillColor="#0D2B1D", fillOpacity=0.95,
        label=~paste0(country, ": ", scales::comma(n_records), " records"),
        popup=~paste0(
          "<b>", country, "</b><br/>",
          "RCC: ", rcc, "<br/>",
          "Total records: ", scales::comma(n_records), "<br/>",
          "High priority: ", n_high, "<br/>",
          "Moderate priority: ", n_moderate
        )
      )
    
    m %>% addLegend("bottomright",
                    colors=unname(rcc_colors),
                    labels=names(rcc_colors),
                    title="Regional Coordinating Centre (fill)",
                    opacity=1) %>%
      addControl(html="<div style='background:rgba(255,255,255,.9);padding:5px 8px;border-radius:4px;font-size:11px;color:#263238'><span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#263238;margin-right:5px'></span>Circle size = number of records<br/><span style='display:inline-block;width:10px;height:10px;border:2px solid #263238;margin-right:5px;vertical-align:middle'></span>Dark outline = country with records<br/><span style='color:#607D8B'>Only countries validated from title/dominant text are counted; Africa-linked records without a named country are not mapped</span></div>",
                 position="bottomright")
  })
  
  ## ── PRIORITY INTELLIGENCE ─────────────────────────────────
  output$plot_priority_disease <- renderPlot({         # [C4]
    filtered_data() %>%
      filter(disease_primary != "Non spécifié",
             priority_score > 0) %>%
      group_by(disease_primary) %>%
      summarise(mean_score=mean(priority_score, na.rm=TRUE),
                n=n(), .groups="drop") %>%
      filter(n >= 3) %>%
      slice_max(mean_score, n=15) %>%
      ggplot(aes(reorder(disease_primary, mean_score), mean_score)) +
      geom_col(fill="#C0392B") +
      geom_text(aes(label=paste0("n=",n)), hjust=-0.1, size=6) +
      scale_x_discrete(labels=tvf()) + coord_flip() +
      labs(x=NULL, y=tx("Mean priority score (0-10)")) +
      theme_minimal(base_size=19) +
      expand_limits(y=11)
  })
  
  output$table_critical_small <- renderDT({
    filtered_data() %>%
      filter(alert_level %in% c("Critical","High")) %>%
      arrange(desc(priority_score)) %>%
      select(disease_primary, title, country, priority_score) %>%
      slice_head(n=10) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=5, dom="t"),
                caption="Top 10 high-priority records")
  })
  
  output$table_high_priority <- renderDT({              # [C4] disease + alert_level
    filtered_data() %>%
      filter(alert_level %in% c("Critical","High")) %>%
      arrange(desc(priority_score)) %>%
      select(alert_level, disease_primary, disease_confidence, disease_mentioned, title, year, country, countries_mentioned, rcc,
             method_family_primary, application_type_primary,
             priority_score, novelty_level, africa_relevance_level,
             tracker_recommendation, url) %>%
      mutate(url=if_else(url!="",
                         paste0('<a href="',url,'" target="_blank">link</a>'),"")) %>%
      datatable(rownames=FALSE, escape=FALSE,
                options=list(pageLength=15, scrollX=TRUE))
  })
  
  ## ── ALL RECORDS ───────────────────────────────────────────
  output$table_records <- renderDT({                    # [C4]
    filtered_data() %>%
      arrange(desc(priority_score)) %>%
      select(alert_level, disease_primary, disease_confidence, disease_mentioned, title, year, country, countries_mentioned, rcc,
             method_family_primary, application_type_primary,
             priority_score, novelty_level, africa_relevance_level,
             signal_profile, url) %>%
      mutate(url=if_else(url!="",
                         paste0('<a href="',url,'" target="_blank">link</a>'),"")) %>%
      datatable(rownames=FALSE, escape=FALSE,
                options=list(pageLength=15, scrollX=TRUE))
  })
  
  ## ── DOWNLOADS ─────────────────────────────────────────────
  output$dl_filtered <- downloadHandler(
    filename=function() paste0("PREIS_AI_filtered_",Sys.Date(),".csv"),
    content=function(f) write_csv(filtered_data(), f, na="")
  )
  output$dl_africa <- downloadHandler(
    filename=function() paste0("PREIS_AI_africa_",Sys.Date(),".csv"),
    content=function(f) write_csv(
      filtered_data() %>% filter(africa_relevance_score > 0), f, na="")
  )
  output$dl_high <- downloadHandler(
    filename=function() paste0("PREIS_AI_high_priority_",Sys.Date(),".csv"),
    content=function(f) write_csv(
      filtered_data() %>% filter(alert_level %in% c("Critical","High")), f, na="")
  )
  
  output$limits_updated <- renderText({ format(file.mtime(data_file), "%Y-%m-%d %H:%M") })
  
  output$tbl_data_info <- renderTable({
    tibble(
      Item  = c("Source file","Total records","Last loaded","Diseases","Countries","AI methods"),
      Value = c(
        basename(data_file),
        scales::comma(nrow(ai_data())),
        as.character(Sys.time()),
        as.character(n_distinct(ai_data()$disease_primary[ai_data()$disease_primary!="Non spécifié"])),
        as.character(n_distinct(ai_data()$country[ai_data()$country!="Unspecified"])),
        as.character(n_distinct(ai_data()$method_family_primary[ai_data()$method_family_primary!="Unspecified"]))
      )
    )
  })
}

shinyApp(ui, server)
