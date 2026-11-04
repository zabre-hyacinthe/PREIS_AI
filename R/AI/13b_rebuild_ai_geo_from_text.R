suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

rebuild_ai_geo_from_text <- function(project_dir = "D:/PREIS_AI") {
  
  in_file  <- file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final.csv")
  out_file <- file.path(project_dir, "data", "enriched", "ai", "ai_tracker_final_geo.csv")
  qa_file  <- file.path(project_dir, "data", "outputs", "ai", "ai_geo_rebuild_qc.csv")
  
  if (!file.exists(in_file)) stop("Input file not found: ", in_file)
  
  df <- readr::read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  if (nrow(df) == 0) stop("Input file is empty: ", in_file)
  
  norm_txt <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x |>
      str_to_lower() |>
      str_replace_all("[[:punct:]]+", " ") |>
      str_replace_all("\\s+", " ") |>
      str_trim()
  }
  
  clean_blank <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    str_trim(x)
  }
  
  geo_ref <- tibble::tribble(
    ~country, ~iso3, ~rcc, ~pattern,
    
    "Algeria","DZA","Northern","\\balgeria\\b|\\balgerian\\b",
    "Angola","AGO","Southern","\\bangola\\b|\\bangolan\\b",
    "Benin","BEN","Western","\\bbenin\\b|\\bbeninese\\b",
    "Botswana","BWA","Southern","\\bbotswana\\b|\\bmotswana\\b|\\bbatswana\\b",
    "Burkina Faso","BFA","Western","\\bburkina faso\\b|\\bburkinabe\\b|\\bburkinab[eé]\\b",
    "Burundi","BDI","Eastern","\\bburundi\\b|\\bburundian\\b",
    "Cabo Verde","CPV","Western","\\bcabo verde\\b|\\bcape verde\\b|\\bcape verdean\\b|\\bcabo verdean\\b",
    "Cameroon","CMR","Central","\\bcameroon\\b|\\bcameroun\\b|\\bcameroonian\\b",
    "Central African Republic","CAF","Central","\\bcentral african republic\\b|\\bcar\\b|\\bcentrafrican\\b|\\bcentral african\\b",
    "Chad","TCD","Central","\\bchad\\b|\\bchadian\\b",
    "Comoros","COM","Eastern","\\bcomoros\\b|\\bcomorian\\b",
    "Democratic Republic of the Congo","COD","Central","\\bdemocratic republic of the congo\\b|\\bdr congo\\b|\\bdrc\\b|\\bcongo kinshasa\\b|\\bdrc\\b|\\bcongolese\\b",
    "Republic of the Congo","COG","Central","\\brepublic of the congo\\b|\\bcongo brazzaville\\b|\\bbrazzaville\\b",
    "Cote d'Ivoire","CIV","Western","\\bcote d ivoire\\b|\\bcote d'ivoire\\b|\\bcôte d ivoire\\b|\\bcôte d'ivoire\\b|\\bivory coast\\b|\\bivoirian\\b",
    "Djibouti","DJI","Eastern","\\bdjibouti\\b|\\bdjiboutian\\b",
    "Egypt","EGY","Northern","\\begypt\\b|\\begyptian\\b",
    "Equatorial Guinea","GNQ","Central","\\bequatorial guinea\\b|\\bequatoguinean\\b",
    "Eritrea","ERI","Eastern","\\beritrea\\b|\\beritrean\\b",
    "Eswatini","SWZ","Southern","\\beswatini\\b|\\bswaziland\\b|\\bswazi\\b",
    "Ethiopia","ETH","Eastern","\\bethiopia\\b|\\bethiopian\\b",
    "Gabon","GAB","Central","\\bgabon\\b|\\bgabonese\\b",
    "Gambia","GMB","Western","\\bgambia\\b|\\bgambian\\b",
    "Ghana","GHA","Western","\\bghana\\b|\\bghanaian\\b",
    "Guinea","GIN","Western","\\bguinea\\b|\\bguinean\\b",
    "Guinea-Bissau","GNB","Western","\\bguinea bissau\\b|\\bguinea-bissau\\b|\\bbissau guinean\\b",
    "Kenya","KEN","Eastern","\\bkenya\\b|\\bkenyan\\b",
    "Lesotho","LSO","Southern","\\blesotho\\b|\\bbasotho\\b|\\bmosotho\\b",
    "Liberia","LBR","Western","\\bliberia\\b|\\bliberian\\b",
    "Libya","LBY","Northern","\\blibya\\b|\\blibyan\\b",
    "Madagascar","MDG","Eastern","\\bmadagascar\\b|\\bmalagasy\\b",
    "Malawi","MWI","Eastern","\\bmalawi\\b|\\bmalawian\\b",
    "Mali","MLI","Western","\\bmali\\b|\\bmalian\\b",
    "Mauritania","MRT","Western","\\bmauritania\\b|\\bmauritanian\\b",
    "Mauritius","MUS","Eastern","\\bmauritius\\b|\\bmauritian\\b",
    "Morocco","MAR","Northern","\\bmorocco\\b|\\bmoroccan\\b",
    "Mozambique","MOZ","Southern","\\bmozambique\\b|\\bmozambican\\b",
    "Namibia","NAM","Southern","\\bnamibia\\b|\\bnamibian\\b",
    "Niger","NER","Western","\\bniger\\b|\\bnigerien\\b",
    "Nigeria","NGA","Western","\\bnigeria\\b|\\bnigerian\\b",
    "Rwanda","RWA","Eastern","\\brwanda\\b|\\brwandan\\b",
    "Sao Tome and Principe","STP","Central","\\bsao tome and principe\\b|\\bsao tome and principe\\b|\\bsao tome and principe\\b|\\bsão tomé and príncipe\\b|\\bsao tome\\b",
    "Senegal","SEN","Western","\\bsenegal\\b|\\bsenegalese\\b",
    "Seychelles","SYC","Eastern","\\bseychelles\\b|\\bseychellois\\b",
    "Sierra Leone","SLE","Western","\\bsierra leone\\b|\\bsierra leonean\\b",
    "Somalia","SOM","Eastern","\\bsomalia\\b|\\bsomali\\b",
    "South Africa","ZAF","Southern","\\bsouth africa\\b|\\bsouth african\\b",
    "South Sudan","SSD","Eastern","\\bsouth sudan\\b|\\bsouth sudanese\\b",
    "Sudan","SDN","Eastern","\\bsudan\\b|\\bsudanese\\b",
    "Tanzania","TZA","Eastern","\\btanzania\\b|\\btanzanian\\b|\\bunited republic of tanzania\\b",
    "Togo","TGO","Western","\\btogo\\b|\\btogolese\\b",
    "Tunisia","TUN","Northern","\\btunisia\\b|\\btunisian\\b",
    "Uganda","UGA","Eastern","\\buganda\\b|\\bugandan\\b",
    "Zambia","ZMB","Eastern","\\bzambia\\b|\\bzambian\\b",
    "Zimbabwe","ZWE","Southern","\\bzimbabwe\\b|\\bzimbabwean\\b"
  )
  
  iso_map <- geo_ref %>% select(country, iso3, rcc)
  
  get_first_match_country <- function(txt) {
    hits <- geo_ref$country[str_detect(txt, regex(geo_ref$pattern, ignore_case = TRUE))]
    if (length(hits) == 0) return(NA_character_)
    hits[1]
  }
  
  country_txt0  <- clean_blank(if ("country_txt"  %in% names(df)) df$country_txt  else "")
  iso3_txt0     <- clean_blank(if ("iso3_txt"     %in% names(df)) df$iso3_txt     else "")
  rcc_txt0      <- clean_blank(if ("rcc_txt"      %in% names(df)) df$rcc_txt      else "")
  country_norm0 <- clean_blank(if ("country_norm" %in% names(df)) df$country_norm else "")
  iso3_norm0    <- clean_blank(if ("iso3_norm"    %in% names(df)) df$iso3_norm    else "")
  rcc_norm0     <- clean_blank(if ("rcc_norm"     %in% names(df)) df$rcc_norm     else "")
  geo_unit0     <- clean_blank(if ("geo_unit"     %in% names(df)) df$geo_unit     else "")
  
  title0    <- clean_blank(if ("title"    %in% names(df)) df$title    else "")
  abstract0 <- clean_blank(if ("abstract" %in% names(df)) df$abstract else "")
  text_all0 <- clean_blank(if ("text_all" %in% names(df)) df$text_all else "")
  authors0  <- clean_blank(if ("authors"  %in% names(df)) df$authors  else "")
  journal0  <- clean_blank(if ("journal"  %in% names(df)) df$journal  else "")
  
  text_norm <- norm_txt(paste(
    country_txt0, iso3_txt0, rcc_txt0,
    country_norm0, iso3_norm0, rcc_norm0,
    geo_unit0, title0, abstract0, text_all0, authors0, journal0
  ))
  
  harmonize_country <- function(x) {
    x2 <- norm_txt(x)
    dplyr::case_when(
      x2 == "" ~ NA_character_,
      str_detect(x2, "south africa|south african") ~ "South Africa",
      str_detect(x2, "^nigeria$|nigerian") ~ "Nigeria",
      str_detect(x2, "^kenya$|kenyan") ~ "Kenya",
      str_detect(x2, "ethiopia|ethiopian") ~ "Ethiopia",
      str_detect(x2, "egypt|egyptian") ~ "Egypt",
      str_detect(x2, "ghana|ghanaian") ~ "Ghana",
      str_detect(x2, "tanzania|tanzanian|united republic of tanzania") ~ "Tanzania",
      str_detect(x2, "uganda|ugandan") ~ "Uganda",
      str_detect(x2, "rwanda|rwandan") ~ "Rwanda",
      str_detect(x2, "senegal|senegalese") ~ "Senegal",
      str_detect(x2, "morocco|moroccan") ~ "Morocco",
      str_detect(x2, "algeria|algerian") ~ "Algeria",
      str_detect(x2, "cameroon|cameroun|cameroonian") ~ "Cameroon",
      str_detect(x2, "madagascar|malagasy") ~ "Madagascar",
      str_detect(x2, "malawi|malawian") ~ "Malawi",
      str_detect(x2, "botswana|motswana|batswana") ~ "Botswana",
      str_detect(x2, "burkina faso|burkinabe|burkinabé") ~ "Burkina Faso",
      str_detect(x2, "^guinea$|guinean") ~ "Guinea",
      str_detect(x2, "democratic republic of the congo|dr congo|drc|congo kinshasa") ~ "Democratic Republic of the Congo",
      str_detect(x2, "republic of the congo|congo brazzaville") ~ "Republic of the Congo",
      str_detect(x2, "cabo verde|cape verde") ~ "Cabo Verde",
      str_detect(x2, "cote d ivoire|cote d'ivoire|côte d ivoire|côte d'ivoire|ivory coast") ~ "Cote d'Ivoire",
      TRUE ~ x
    )
  }
  
  country_from_fields <- ifelse(country_txt0 != "", country_txt0, NA_character_)
  country_from_fields <- ifelse(is.na(country_from_fields) | country_from_fields == "", country_norm0, country_from_fields)
  country_from_fields <- vapply(country_from_fields, harmonize_country, character(1))
  
  country_from_text <- vapply(text_norm, get_first_match_country, character(1))
  
  iso_lookup <- setNames(iso_map$country, iso_map$iso3)
  iso3_guess <- toupper(ifelse(iso3_txt0 != "", iso3_txt0, iso3_norm0))
  country_from_iso3 <- unname(iso_lookup[iso3_guess])
  country_from_iso3[is.na(country_from_iso3)] <- NA_character_
  
  final_country <- dplyr::coalesce(
    na_if(country_from_fields, ""),
    na_if(country_from_iso3, ""),
    na_if(country_from_text, "")
  )
  
  df_out <- df %>%
    mutate(country = final_country) %>%
    left_join(iso_map, by = "country", suffix = c("", "_map")) %>%
    mutate(
      # [FIX P1] iso3_map / rcc_map = valeurs déduites du pays via le gazetteer.
      # L'ancien code retombait directement sur iso3/rcc (colonnes vides) ->
      # iso3 et rcc restaient "Unspecified" même quand le pays était détecté.
      iso3 = dplyr::coalesce(
        na_if(toupper(clean_blank(iso3_txt0)), ""),
        na_if(toupper(clean_blank(iso3_norm0)), ""),
        if ("iso3_map" %in% names(cur_data())) iso3_map else NA_character_,
        iso3
      ),
      rcc = dplyr::coalesce(
        na_if(clean_blank(rcc_txt0), ""),
        na_if(clean_blank(rcc_norm0), ""),
        if ("rcc_map" %in% names(cur_data())) rcc_map else NA_character_,
        rcc
      )
    ) %>%
    mutate(
      iso3 = if_else(is.na(iso3) | iso3 == "", "Unspecified", toupper(iso3)),
      rcc = case_when(
        is.na(rcc) | rcc == "" ~ "Unspecified",
        str_detect(norm_txt(rcc), "north") ~ "Northern",
        str_detect(norm_txt(rcc), "west") ~ "Western",
        str_detect(norm_txt(rcc), "east") ~ "Eastern",
        str_detect(norm_txt(rcc), "central") ~ "Central",
        str_detect(norm_txt(rcc), "south") ~ "Southern",
        TRUE ~ rcc
      ),
      country = if_else(is.na(country) | country == "", "Unspecified", country)
    )
  
  qa <- tibble(
    metric = c(
      "n_records",
      "n_country_detected",
      "n_iso3_detected",
      "n_rcc_detected",
      "n_distinct_countries",
      "n_distinct_rcc"
    ),
    value = c(
      nrow(df_out),
      sum(df_out$country != "Unspecified", na.rm = TRUE),
      sum(df_out$iso3 != "Unspecified", na.rm = TRUE),
      sum(df_out$rcc != "Unspecified", na.rm = TRUE),
      dplyr::n_distinct(df_out$country[df_out$country != "Unspecified"]),
      dplyr::n_distinct(df_out$rcc[df_out$rcc != "Unspecified"])
    )
  )
  
  # [FIX P1] Retirer les colonnes techniques de jointure
  df_out <- df_out %>% dplyr::select(-dplyr::any_of(c("iso3_map", "rcc_map", "country_map")))

  readr::write_csv(df_out, out_file, na = "")
  readr::write_csv(qa, qa_file, na = "")
  
  invisible(df_out)
}