# ============================================================
# 70b_send_email_direct.R — PREIS-AI SITREP v6 DEFINITIF
# Africa CDC — AI Surveillance Intelligence
# Corrections v6 :
#   [F1] Charger .Renviron si SMTP absent (Rscript sans --vanilla)
#   [F2] Utiliser .data[[col]] dans mutate pour éviter "not found"
#   [F3] Toutes colonnes référencées via .data[[]]
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(tibble); library(blastula); library(lubridate)
  library(purrr); library(scales)
})

PROJECT_ROOT <- "D:/PREIS_AI_CLEAN"

# [FIX 2026-09-27] Garde-fou reel : sans cette variable d'environnement
# explicitement positionnee a TRUE, AUCUN email reel n'est envoye -- le
# script journalise uniquement ce qu'il AURAIT envoye (mode preview).
# Activer un envoi reel :  Sys.setenv(PREIS_AI_EMAIL_SEND_NOW="TRUE")  avant
# de sourcer/lancer ce script (ou set la variable d'environnement systeme).
SEND_NOW <- toupper(Sys.getenv("PREIS_AI_EMAIL_SEND_NOW", "FALSE")) %in% c("TRUE","1","YES")

# [F1] Charger .Renviron si SMTP_PASS absent
if (Sys.getenv("SMTP_PASS") == "") {
  rv <- file.path(Sys.getenv("USERPROFILE"),
                  "OneDrive", "Documents", ".Renviron")
  if (file.exists(rv)) { readRenviron(rv); message(".Renviron chargé") }
}

# ── Logging ──────────────────────────────────────────────────
log_file <- file.path(PROJECT_ROOT, "logs", "ai_email_cycle.log")
dir.create(dirname(log_file), recursive=TRUE, showWarnings=FALSE)
log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ",
                 paste0(..., collapse=""))
  cat(line, "\n")
  tryCatch(cat(line, "\n", file=log_file, append=TRUE),
           error=function(e) invisible(NULL))
}
log_msg("START PREIS-AI SITREP v6")
log_msg("SEND_NOW = ", SEND_NOW,
        if (!SEND_NOW) " (mode preview -- aucun email reel ne sera envoye)" else " (envoi REEL active)")

# ── Helpers ───────────────────────────────────────────────────
num  <- function(x, d=0) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[is.na(v)] <- d; v
}
cln  <- function(x, d="") {
  x <- trimws(as.character(x))
  x[x %in% c("","NA","NULL","N/A","null","NaN","None")] <- d; x
}
cln_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("","NA","NULL","N/A","null","NaN","None")] <- NA_character_; x
}
scope_r <- function(x) {
  x <- cln(x, "All"); x[toupper(x) == "ALL"] <- "All"; x
}

# EpiWeek ISO Lun-Dim
ew     <- epiweek(Sys.Date())
eyr    <- epiyear(Sys.Date())
ews    <- floor_date(Sys.Date(), "week", week_start=1)
ewe    <- ews + 6L
ew_lbl <- sprintf("EpiWeek W%02d/%d (%s\u2013%s)",
                  ew, eyr, format(ews, "%d %b"), format(ewe, "%d %b %Y"))

# ── Dictionnaire 55 pays africains ───────────────────────────
africa_dict <- list(
  "Algeria"       = c("Algeria","Algérie","DZA"),
  "Egypt"         = c("Egypt","Egypte","Égypte","EGY"),
  "Libya"         = c("Libya","Libye","LBY"),
  "Morocco"       = c("Morocco","Maroc","MAR"),
  "Tunisia"       = c("Tunisia","Tunisie","TUN"),
  "Sudan"         = c("Sudan","Soudan","SDN"),
  "South Sudan"   = c("South Sudan","Soudan du Sud","SSD"),
  "Mauritania"    = c("Mauritania","Mauritanie","MRT"),
  "Nigeria"       = c("Nigeria","Nigerian","NGA"),
  "Ghana"         = c("Ghana","Ghanaian","GHA"),
  "Senegal"       = c("Senegal","Sénégal","Senegalese","SEN"),
  "Mali"          = c("Mali","Malian","MLI"),
  "Burkina Faso"  = c("Burkina Faso","Burkinabe","BFA"),
  "Niger"         = c("Niger","Nigerien","NER"),
  "Guinea"        = c("Guinea","Guinée","GIN"),
  "Guinea-Bissau" = c("Guinea-Bissau","GNB"),
  "Sierra Leone"  = c("Sierra Leone","SLE"),
  "Liberia"       = c("Liberia","LBR"),
  "Cote d'Ivoire" = c("Cote d'Ivoire","Ivory Coast","CIV"),
  "Togo"          = c("Togo","Togolese","TGO"),
  "Benin"         = c("Benin","Bénin","BEN"),
  "Gambia"        = c("Gambia","GMB","The Gambia"),
  "Cape Verde"    = c("Cape Verde","Cabo Verde","CPV"),
  "Cameroon"      = c("Cameroon","Cameroun","CMR"),
  "DR Congo"      = c("Democratic Republic of Congo","DRC","DR Congo","COD"),
  "Congo"         = c("Republic of Congo","Congo-Brazzaville","COG"),
  "CAR"           = c("Central African Republic","CAR","RCA"),
  "Chad"          = c("Chad","Tchad","TCD"),
  "Gabon"         = c("Gabon","GAB"),
  "Ethiopia"      = c("Ethiopia","Ethiopie","Ethiopian","ETH"),
  "Kenya"         = c("Kenya","Kenyan","KEN"),
  "Tanzania"      = c("Tanzania","Tanzanian","TZA"),
  "Uganda"        = c("Uganda","Ugandan","UGA"),
  "Rwanda"        = c("Rwanda","RWA"),
  "Burundi"       = c("Burundi","BDI"),
  "Somalia"       = c("Somalia","Somali","SOM"),
  "Djibouti"      = c("Djibouti","DJI"),
  "Eritrea"       = c("Eritrea","ERI"),
  "Comoros"       = c("Comoros","Comores","COM"),
  "Seychelles"    = c("Seychelles","SYC"),
  "Mauritius"     = c("Mauritius","Maurice","MUS"),
  "Madagascar"    = c("Madagascar","Malagasy","MDG"),
  "South Africa"  = c("South Africa","Afrique du Sud","South African","ZAF","RSA"),
  "Zimbabwe"      = c("Zimbabwe","ZWE"),
  "Zambia"        = c("Zambia","Zambian","ZMB"),
  "Mozambique"    = c("Mozambique","MOZ"),
  "Malawi"        = c("Malawi","MWI"),
  "Angola"        = c("Angola","AGO"),
  "Botswana"      = c("Botswana","BWA"),
  "Namibia"       = c("Namibia","NAM"),
  "Lesotho"       = c("Lesotho","LSO"),
  "Eswatini"      = c("Eswatini","Swaziland","SWZ"),
  "West Africa"   = c("West Africa","Western Africa","West African"),
  "East Africa"   = c("East Africa","Eastern Africa","East African"),
  "Central Africa"= c("Central Africa","Afrique Centrale"),
  "Southern Africa"=c("Southern Africa","Afrique Australe"),
  "North Africa"  = c("North Africa","Northern Africa"),
  "Africa"        = c("African region","pan-African","pan African",
                      "across Africa","African continent")
)

rcc_map <- c(
  "Algeria"="Northern","Egypt"="Northern","Libya"="Northern",
  "Morocco"="Northern","Tunisia"="Northern","Mauritania"="Northern",
  "Nigeria"="Western","Ghana"="Western","Senegal"="Western",
  "Mali"="Western","Burkina Faso"="Western","Niger"="Western",
  "Guinea"="Western","Guinea-Bissau"="Western","Sierra Leone"="Western",
  "Liberia"="Western","Cote d'Ivoire"="Western","Togo"="Western",
  "Benin"="Western","Gambia"="Western","Cape Verde"="Western",
  "Cameroon"="Central","DR Congo"="Central","Congo"="Central",
  "CAR"="Central","Chad"="Central","Gabon"="Central",
  "Ethiopia"="Eastern","Kenya"="Eastern","Tanzania"="Eastern",
  "Uganda"="Eastern","Rwanda"="Eastern","Burundi"="Eastern",
  "Somalia"="Eastern","Djibouti"="Eastern","Eritrea"="Eastern",
  "South Sudan"="Eastern","Comoros"="Eastern","Seychelles"="Eastern",
  "Sudan"="Eastern",
  "South Africa"="Southern","Zimbabwe"="Southern","Zambia"="Southern",
  "Mozambique"="Southern","Malawi"="Southern","Angola"="Southern",
  "Botswana"="Southern","Namibia"="Southern","Lesotho"="Southern",
  "Eswatini"="Southern","Madagascar"="Southern","Mauritius"="Southern",
  "West Africa"="Western","East Africa"="Eastern",
  "Central Africa"="Central","Southern Africa"="Southern",
  "North Africa"="Northern","Africa"="All"
)

extract_country <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  for (nm in names(africa_dict)) {
    for (pat in africa_dict[[nm]]) {
      if (str_detect(text, regex(pat, ignore_case=TRUE)))
        return(nm)
    }
  }
  NA_character_
}

# ── Chemins ───────────────────────────────────────────────────
p_alerts <- file.path(PROJECT_ROOT, "dashboard/data/dashboard/alerts_ai.csv")
p_dash   <- file.path(PROJECT_ROOT,
                      "data/dashboard/ai_intelligence_dashboard_dataset.csv")
p_recip  <- file.path(PROJECT_ROOT, "config/alert_recipients_ai.csv")
p_sent   <- file.path(PROJECT_ROOT,
                      "dashboard/data/dashboard/sent_log_ai.csv")
p_email  <- file.path(PROJECT_ROOT, "R/AI/60_email.R")

for (p in c(p_alerts, p_dash, p_recip, p_email))
  if (!file.exists(p)) stop("Fichier manquant: ", p)

source(p_email)

# ── Chargement ────────────────────────────────────────────────
alerts_raw <- read_csv(p_alerts, show_col_types=FALSE,
                       col_types=cols(.default=col_character()))
log_msg("alerts_ai: ", nrow(alerts_raw), " lignes | cols: ",
        ncol(alerts_raw))

# Vérifier colonnes disponibles
has_col <- function(col) col %in% names(alerts_raw)

# Geo depuis dashboard dataset
geo_tbl <- read_csv(p_dash, show_col_types=FALSE,
                    col_types=cols(.default=col_character())) %>%
  filter(!is.na(country), trimws(country) != "") %>%
  mutate(doi_j = tolower(trimws(coalesce(doi, "")))) %>%
  select(doi_j, country, iso3, rcc) %>%
  filter(doi_j != "") %>%
  distinct(doi_j, .keep_all=TRUE)
log_msg("geo_tbl: ", nrow(geo_tbl), " lignes")

recip <- read_csv(p_recip, show_col_types=FALSE,
                  col_types=cols(.default=col_character())) %>%
  mutate(
    name  = cln(coalesce(name, email), "Recipient"),
    email = tolower(trimws(coalesce(email, ""))),
    rcc   = scope_r(coalesce(rcc, "All"))
  ) %>%
  filter(email != "") %>%
  distinct(email, .keep_all=TRUE)
log_msg("Destinataires: ", nrow(recip))

# ── [F2] Harmoniser alertes avec .data[[]] ────────────────────
alerts <- alerts_raw %>%
  mutate(
    # Clé geo
    doi_j = tolower(trimws(coalesce(
      if (has_col("doi")) .data[["doi"]] else NA_character_, ""))),
    
    # Scores — toujours via .data[[]]
    priority_score = num(
      if (has_col("priority_score")) .data[["priority_score"]] else 0),
    africa_relevance_score = num(
      if (has_col("africa_relevance_score")) .data[["africa_relevance_score"]] else 0),
    novelty_score = num(
      if (has_col("novelty_score")) .data[["novelty_score"]] else 0),
    
    # Textes — [F3] tous via .data[[]]
    title = cln(
      if (has_col("title")) .data[["title"]] else "Untitled", "Untitled"),
    # [FIX 2026-09-27] alert_level n'existe plus dans alerts_ai.csv depuis
    # juillet -> alias sur priority_level reel (High/Moderate/Low)
    alert_level = cln(
      if (has_col("priority_level")) .data[["priority_level"]]
      else if (has_col("alert_level")) .data[["alert_level"]]
      else "Low", "Low"),
    africa_relevance_level = cln(
      if (has_col("africa_relevance_level")) .data[["africa_relevance_level"]] else ""),
    ai_method = cln(
      if (has_col("method_family_primary")) .data[["method_family_primary"]] else ""),
    application = cln(
      if (has_col("application_type_primary")) .data[["application_type_primary"]] else ""),
    url = cln_na(
      if (has_col("url")) .data[["url"]] else NA_character_),
    
    # Maladie
    disease = {
      d <- if (has_col("disease_primary")) .data[["disease_primary"]]
      else NA_character_
      d <- cln_na(d)
      ifelse(d %in% c("Non sp\u00e9cifi\u00e9","Non specifi\u00e9",
                      "Unspecified",""), NA_character_, d)
    },
    
    send_alert = tolower(cln(
      if (has_col("send_alert")) .data[["send_alert"]] else "false",
      "false")) %in% c("true","1","yes")
  )

# ── Jointure geo doi ──────────────────────────────────────────
alerts <- alerts %>%
  left_join(geo_tbl, by="doi_j") %>%
  mutate(
    country = ifelse(is.na(country)|country=="", "Unspecified", country),
    rcc     = scope_r(ifelse(is.na(rcc)|rcc=="", "All", rcc))
  ) %>%
  select(-doi_j)

n_doi <- sum(alerts$country != "Unspecified")
log_msg("Geo doi: ", n_doi, " matches")

# ── Fallback : extraction depuis titre ────────────────────────
miss_idx <- which(alerts$country == "Unspecified")
if (length(miss_idx) > 0) {
  log_msg("Extraction pays depuis titre pour ", length(miss_idx), " articles...")
  texts <- alerts$title[miss_idx]
  extracted <- sapply(texts, extract_country, USE.NAMES=FALSE)
  alerts$country[miss_idx] <- ifelse(!is.na(extracted), extracted, "Unspecified")
  alerts$rcc[miss_idx] <- ifelse(
    !is.na(extracted),
    sapply(extracted, function(x) {
      r <- rcc_map[x]; if (is.na(r)) "All" else unname(r)
    }),
    "All"
  )
  log_msg("Geo titre: ", sum(!is.na(extracted)), " pays extraits")
}

n_geo <- sum(alerts$country != "Unspecified")
log_msg("Geo totale: ", n_geo, "/", nrow(alerts),
        " (", round(n_geo/nrow(alerts)*100,1), "%)")

# ── Filtrage Africa strict ────────────────────────────────────
africa_rcc   <- c("Northern","Western","Central","Eastern","Southern","All")
africa_alerts <- alerts %>%
  filter(africa_relevance_score > 0,
         alert_level %in% c("High","Moderate"),
         rcc %in% africa_rcc | country == "Unspecified") %>%
  arrange(desc(priority_score), desc(africa_relevance_score))

africa_email <- africa_alerts %>%
  filter(alert_level == "High")

log_msg("Africa alertes: ", nrow(africa_alerts),
        " | Email digest (HIGH): ", nrow(africa_email))

# ── Sent log ──────────────────────────────────────────────────
write_sl <- function(p, new) {
  nc <- new %>% mutate(across(everything(), as.character))
  if (file.exists(p)) {
    old <- read_csv(p, show_col_types=FALSE,
                    col_types=cols(.default=col_character()))
    write_csv(bind_rows(old, nc), p)
  } else {
    write_csv(nc, p)
  }
}

# ── Construction SITREP ───────────────────────────────────────
build_sitrep <- function(df_email, df_stats, rcc_lbl, is_global=FALSE) {
  # [FIX 2026-09-27] 3 paliers reels (High/Moderate/Low) ; df_email ne
  # contient plus que le palier High (voir africa_email ci-dessus).
  n_high     <- sum(df_email$alert_level=="High", na.rm=TRUE)
  n_moderate <- sum(df_stats$alert_level=="Moderate", na.rm=TRUE)
  
  dis <- df_stats %>% filter(!is.na(disease)) %>%
    count(disease, sort=TRUE) %>% slice_head(n=8)
  met <- df_stats %>%
    filter(ai_method!="",!is.na(ai_method),
           !ai_method %in% c("Unspecified","NA")) %>%
    count(ai_method, sort=TRUE) %>% slice_head(n=5)
  app <- df_stats %>%
    filter(application!="",!is.na(application),
           !application %in% c("Unspecified","NA")) %>%
    count(application, sort=TRUE) %>% slice_head(n=4)
  ctr <- df_stats %>%
    filter(country!="Unspecified",!is.na(country)) %>%
    count(country, sort=TRUE) %>% slice_head(n=6)
  top <- df_email %>%
    arrange(desc(priority_score)) %>% slice_head(n=12)
  
  region_row <- if (!is_global)
    paste0("| Region | **", rcc_lbl, "** |\n") else ""
  
  s_exec <- paste0(
    "## Executive Summary\n\n| Indicator | Value |\n|:---|---:|\n",
    "| AI articles monitored | **", comma(nrow(alerts_raw)), "** |\n",
    "| Africa-linked | **", comma(nrow(africa_alerts)), "** |\n",
    region_row,
    "| HIGH | **", n_high, "** |\n",
    "| MODERATE | **", n_moderate, "** |\n\n"
  )
  
  mk_tbl <- function(d, cv, cn, hdr, lbl) {
    if (nrow(d)==0) return("")
    rows <- paste(paste0("| ",d[[cv]]," | ",d[[cn]]," |"), collapse="\n")
    paste0("## ",hdr,"\n\n| ",lbl," | Articles |\n|:---|---:|\n",rows,"\n\n")
  }
  
  s_dis <- mk_tbl(dis,"disease",    "n","Diseases under AI surveillance","Disease")
  s_met <- mk_tbl(met,"ai_method",  "n","AI methods deployed","AI Method")
  s_app <- mk_tbl(app,"application","n","Surveillance applications","Application")
  
  s_ctr <- if (nrow(ctr)>0)
    paste0("## Top countries \u2014 ",rcc_lbl,"\n\n",
           paste(paste0("- **",ctr$country,"** (",ctr$n," articles)"),
                 collapse="\n"),"\n\n") else ""
  
  s_top <- if (nrow(top)>0) {
    items <- map_chr(seq_len(nrow(top)), function(i) {
      a    <- top[i,]
      badge<- paste0("[", toupper(a$alert_level), "]")
      ctr_ <- if (a$country!="Unspecified") a$country else "Africa"
      dis_ <- if (!is.na(a$disease)) paste0("**",a$disease,"**") else "\u2014"
      mth_ <- if (a$ai_method!=""&&!is.na(a$ai_method)&&
                  a$ai_method!="Unspecified") a$ai_method else "\u2014"
      lnk  <- if (!is.na(a$url))
        paste0("\n  [Read article](",a$url,")") else ""
      paste0(badge," **",ctr_,"** \u2014 ",dis_,"\n",
             "> ",str_trunc(a$title,200),"\n",
             "> Method: ",mth_," | Priority: **",round(a$priority_score,1),
             "/10** | Africa: **",a$africa_relevance_level,"**",lnk)
    })
    paste0("## Priority alerts \u2014 ",rcc_lbl,"\n\n",
           paste(items,collapse="\n\n---\n\n"),"\n\n")
  } else paste0("## Status\n\n_No high-priority alerts for **",
                rcc_lbl,"** this period._\n\n")
  
  s_act <- paste0("## Recommended actions\n\n",
                  if(n_high>0) paste0("- **",n_high," HIGH**: immediate review\n") else "",
                  if(n_moderate>0) paste0("- **",n_moderate," MODERATE**: review within 48h\n") else "",
                  if(n_high==0&&n_moderate==0) "- No immediate action required\n" else "",
                  "- [PREIS-AI Dashboard]",
                  "(https://zrhyacinthepreis26.shinyapps.io/PREIS_AI_Dashboard)\n\n")
  
  paste0(
    "# PREIS-AI Intelligence SITREP\n",
    "**Africa CDC \u2014 AI Surveillance Intelligence Observatory**\n\n",
    "**",format(Sys.Date(),"%d %B %Y")," | ",ew_lbl,"**\n\n",
    if(!is_global) paste0("> Regional report: **",rcc_lbl,"**\n\n")
    else "> Global Africa report\n\n",
    "---\n\n",s_exec,"---\n\n",
    s_dis,s_met,s_app,s_ctr,
    "---\n\n",s_top,"---\n\n",s_act,
    "---\n_Automated report \u2014 PREIS-AI Tracker \u2014 Africa CDC_\n",
    "_Do not reply to this email._\n"
  )
}

# ── Envoi par RCC ─────────────────────────────────────────────
sent_rows <- list()
n_sent    <- 0L

for (i in seq_len(nrow(recip))) {
  rec_email <- recip$email[i]
  rec_name  <- recip$name[i]
  rec_rcc   <- recip$rcc[i]
  is_global <- toupper(rec_rcc) == "ALL"
  
  if (is_global) {
    df_e <- africa_email; df_s <- africa_alerts
    lbl  <- "All Africa Regions"
  } else {
    df_s <- africa_alerts %>%
      filter(str_detect(tolower(rcc),     fixed(tolower(rec_rcc))) |
               str_detect(tolower(country), fixed(tolower(rec_rcc))))
    df_e <- df_s %>% filter(alert_level == "High")
    lbl  <- rec_rcc
    if (nrow(df_s)==0) {
      log_msg("Aucune alerte RCC [",rec_rcc,"] \u2014 skip")
      next
    }
  }
  
  n_h <- sum(df_e$alert_level=="High", na.rm=TRUE)
  n_m <- sum(df_s$alert_level=="Moderate", na.rm=TRUE)
  log_msg("Envoi [",lbl,"] -> ",rec_name," <",rec_email,
          "> HIGH=",n_h," MODERATE=",n_m)
  
  body <- build_sitrep(df_e, df_s, lbl, is_global)
  subj <- paste0(
    if(n_h>0)"[HIGH] " else if(n_m>0)"[MODERATE] " else "[INFO] ",
    "PREIS-AI SITREP | ",ew_lbl,
    if(!is_global) paste0(" | ",lbl) else " | All Regions",
    " | HIGH:",n_h," MODERATE:",n_m)
  
  if (SEND_NOW) {
    tryCatch({
      em <- ai_compose_email(body)
      ai_smtp_send(email_obj=em, to=rec_email, subject=subj, verbose=FALSE)
      log_msg("OK: ",rec_name," <",rec_email,">")
      n_sent <- n_sent + 1L
      sent_rows[[length(sent_rows)+1]] <- tibble(
        date_sent=as.character(Sys.Date()),
        datetime_sent=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
        recipient_name=rec_name, recipient_email=rec_email,
        recipient_rcc=lbl, scope="AI", subject=subj,
        n_alerts=as.character(nrow(df_e)),
        status="sent", mode="automatic")
    }, error=function(e) log_msg("ERREUR [",rec_email,"]: ",e$message))
  } else {
    # [FIX 2026-09-27] Mode preview par defaut : AUCUN envoi reel.
    log_msg("PREVIEW (aucun envoi reel, SEND_NOW=FALSE) -> ",
            rec_name," <",rec_email,"> subject: ",subj)
    sent_rows[[length(sent_rows)+1]] <- tibble(
      date_sent=as.character(Sys.Date()),
      datetime_sent=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
      recipient_name=rec_name, recipient_email=rec_email,
      recipient_rcc=lbl, scope="AI", subject=subj,
      n_alerts=as.character(nrow(df_e)),
      status="preview_only", mode="dry_run")
  }
  
  Sys.sleep(1.5)
}

# ── Sent log + résumé ─────────────────────────────────────────
if (length(sent_rows)>0) write_sl(p_sent, bind_rows(sent_rows))
log_msg("Emails envoyes: ", n_sent, " / ", nrow(recip))
log_msg("END PREIS-AI SITREP v6")
