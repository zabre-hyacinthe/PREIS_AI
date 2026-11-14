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
project_dir <- "D:/PREIS_AI_CLEAN"
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
      
      disease_primary          = pick_first(., "disease_primary", "Non sp\u00e9cifi\u00e9"),
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
  High="#8B0000", Moderate="#F39C12",
  Low="#2980B9", Unspecified="#95A5A6"
)
alert_colors <- c(
  High="#8B0000", Moderate="#F39C12",
  Low="#2980B9", Unspecified="#95A5A6"
)
africa_rel_colors <- c(
  High="#C0392B", Moderate="#F39C12", Low="#2980B9",
  None="#BDC3C7", Unspecified="#95A5A6"
)
novelty_colors <- c(
  High="#8E44AD", Moderate="#16A085", Low="#7F8C8D",
  Unspecified="#BDC3C7"
)

## ------------------------------------------------------------
## CARTE
## ------------------------------------------------------------
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
                selected="All")
  ),
  
  dashboardBody(
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
      .small-box h3 { font-size:26px; }
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
                box(width=6, title="Priority distribution",    status="primary",  solidHeader=TRUE, plotOutput("plot_priority",      height=300)),
                box(width=6, title="Alert level distribution", status="danger",   solidHeader=TRUE, plotOutput("plot_alert_level",   height=300))
              ),
              fluidRow(
                box(width=6, title="AI methods",               status="success",  solidHeader=TRUE, plotOutput("plot_methods",       height=320)),
                box(width=6, title="AI applications",          status="success",  solidHeader=TRUE, plotOutput("plot_applications",  height=320))
              ),
              fluidRow(
                box(width=6, title="Africa relevance",         status="warning",  solidHeader=TRUE, plotOutput("plot_africa_rel",    height=280)),
                box(width=6, title="Novelty",                  status="warning",  solidHeader=TRUE, plotOutput("plot_novelty",       height=280))
              )
      ),
      
      ## ── [C3] IA × MALADIE × PAYS — NOUVEL ONGLET ─────────
      tabItem(tabName="disease_intel",
              fluidRow(
                box(width=12, status="danger", solidHeader=TRUE,
                    title="Diseases under AI surveillance — top priority",
                    plotOutput("plot_disease_priority", height=360))
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
                    plotOutput("plot_disease_country", height=360)),
                box(width=6, status="info", solidHeader=TRUE,
                    title="Disease trend over time",
                    plotOutput("plot_disease_trend", height=360))
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
                box(width=6, title="Methods by priority",     status="warning", solidHeader=TRUE, plotOutput("plot_methods_priority",  height=400)),
                box(width=6, title="Applications by priority",status="warning", solidHeader=TRUE, plotOutput("plot_app_priority",      height=400))
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
                box(width=4, title="By RCC",               status="danger",   solidHeader=TRUE, plotOutput("plot_rcc",        height=300)),
                box(width=4, title="Top countries",        status="danger",   solidHeader=TRUE, plotOutput("plot_country",    height=300)),
                box(width=4, title="Geography status",     status="danger",   solidHeader=TRUE, plotOutput("plot_geo_status", height=300))
              ),
              # [C5] Évolution temporelle
              fluidRow(
                box(width=12, title="Africa-linked records — trend over time", status="warning", solidHeader=TRUE,
                    plotOutput("plot_africa_trend", height=280))
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
                    plotOutput("plot_priority_disease", height=320))
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
    n <- sum(filtered_data()$alert_level == "High", na.rm=TRUE)
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
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_alert_level <- renderPlot({
    filtered_data() %>%
      count(alert_level, sort=TRUE) %>%
      ggplot(aes(reorder(alert_level,n), n, fill=alert_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=alert_colors, na.value="grey70") +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_methods <- renderPlot({
    filtered_data() %>%
      filter(method_family_primary != "Unspecified") %>%
      count(method_family_primary, sort=TRUE) %>% slice_head(n=10) %>%
      ggplot(aes(reorder(method_family_primary,n), n)) +
      geom_col(fill="#2980B9", show.legend=FALSE) +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_applications <- renderPlot({
    filtered_data() %>%
      filter(application_type_primary != "Unspecified") %>%
      count(application_type_primary, sort=TRUE) %>% slice_head(n=10) %>%
      ggplot(aes(reorder(application_type_primary,n), n)) +
      geom_col(fill="#27AE60", show.legend=FALSE) +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_africa_rel <- renderPlot({
    filtered_data() %>%
      count(africa_relevance_level, sort=TRUE) %>%
      ggplot(aes(reorder(africa_relevance_level,n), n, fill=africa_relevance_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=africa_rel_colors, na.value="grey70") +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_novelty <- renderPlot({
    filtered_data() %>%
      count(novelty_level, sort=TRUE) %>%
      ggplot(aes(reorder(novelty_level,n), n, fill=novelty_level)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=novelty_colors, na.value="grey70") +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
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
      scale_fill_manual(values=alert_colors, na.value="grey70", name="Alert level") +
      coord_flip() +
      labs(x=NULL, y="Records", title=NULL,
           caption=paste0("Top 20 named diseases. ", format(n_unspec, big.mark=","),
                          " records with no specific disease (Unspecified) are not shown.")) +
      theme_minimal(base_size=13) +
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
      plot.new(); title("No data with country information"); return(invisible())
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
        coord_flip() +
        labs(x=NULL, y="Records",
             subtitle="Top 3 countries per disease (Africa-linked)") +
        theme_minimal(base_size=11)
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
        coord_flip() +
        labs(x=NULL, y="Records",
             subtitle="Top RCC per disease (no country data available)") +
        theme_minimal(base_size=11)
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
      geom_line(linewidth=0.9) + geom_point(size=2) +
      labs(x="Year", y="Records", color="Disease") +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom",
            legend.text=element_text(size=10))
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
      scale_fill_manual(values=priority_colors, na.value="grey70") +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
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
      scale_fill_manual(values=priority_colors, na.value="grey70") +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
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
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_country <- renderPlot({
    filtered_data() %>%
      filter(country != "Unspecified") %>%
      count(country, sort=TRUE) %>% slice_head(n=12) %>%
      ggplot(aes(reorder(country,n), n)) +
      geom_col(fill="#2980B9") +
      coord_flip() + labs(x=NULL, y="Records") + theme_minimal(base_size=12)
  })
  
  output$plot_geo_status <- renderPlot({
    filtered_data() %>%
      mutate(geo = ifelse(country != "Unspecified",
                          "African geography detected",
                          "No geography detected")) %>%
      count(geo) %>%
      ggplot(aes(geo, n, fill=geo)) +
      geom_col(show.legend=FALSE) +
      labs(x=NULL, y="Records") + theme_minimal(base_size=12)
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
      labs(x="Year", y="Africa-linked records",
           caption="Loess trend line") +
      theme_minimal(base_size=12)
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
                n_high=sum(alert_level=="High", na.rm=TRUE),
                n_moderate=sum(alert_level=="Moderate", na.rm=TRUE),
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
      geom_text(aes(label=paste0("n=",n)), hjust=-0.1, size=3) +
      coord_flip() +
      labs(x=NULL, y="Mean priority score (0-10)") +
      theme_minimal(base_size=12) +
      expand_limits(y=11)
  })
  
  output$table_critical_small <- renderDT({
    filtered_data() %>%
      filter(alert_level == "High") %>%
      arrange(desc(priority_score)) %>%
      select(disease_primary, title, country, priority_score) %>%
      slice_head(n=10) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=5, dom="t"),
                caption="Top 10 high-priority records")
  })
  
  output$table_high_priority <- renderDT({              # [C4] disease + alert_level
    filtered_data() %>%
      filter(alert_level == "High") %>%
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
      filtered_data() %>% filter(alert_level == "High"), f, na="")
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
