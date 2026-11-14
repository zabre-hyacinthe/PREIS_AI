## ============================================================
## PREIS AI Atlas - Shiny module v3 (tab "PREIS AI Atlas")
## Question of the module: which AI application, on what (disease), where (RCC, country) and when (year)?
## All sidebar filters apply (year, alert level, disease, method, application, RCC, country, signal profile),
## because the tab is computed from filtered_data(). Reads CSV in registry_data/ only. Writes nothing.
## NOT YET RUN IN R when written. ASCII only (accents as \u escapes).
## ============================================================
REG_FORM_URL <- "https://forms.gle/LSUZpjn4W6pKHpry9"
REG_UNSPEC <- c("Unspecified", "Non sp\u00e9cifi\u00e9", "Non specifie", "")

reg_read <- function(reg_dir, f) {
  p <- file.path(reg_dir, f)
  if (!file.exists(p)) return(NULL)
  tryCatch(suppressWarnings(readr::read_csv(p, show_col_types = FALSE, progress = FALSE, na = c("", "NA"))),
           error = function(e) NULL)
}
reg_link <- function(url, text) {
  ifelse(is.na(url) | url == "", htmltools::htmlEscape(text),
         paste0('<a href="', url, '" target="_blank" rel="noopener">', htmltools::htmlEscape(text), "</a>"))
}
reg_key <- function(doi, url) {
  d <- tolower(trimws(as.character(doi))); d[is.na(d) | d %in% c("", "na")] <- NA_character_
  u <- tolower(trimws(as.character(url))); u <- sub("^https?://", "", u); u <- sub("^www\\.", "", u); u <- sub("/+$", "", u)
  u[is.na(u) | u %in% c("", "na")] <- NA_character_
  dplyr::coalesce(d, u)
}
reg_norm <- function(v) {
  v <- gsub("[^a-z]", "", tolower(iconv(as.character(v), to = "ASCII//TRANSLIT")))
  alias <- c(gambia = "thegambia", eswatini = "kingdomofeswatini", swaziland = "kingdomofeswatini",
             caboverde = "republicofcaboverde", congo = "republicofthecongo", drcongo = "democraticrepublicofthecongo",
             drc = "democraticrepublicofthecongo", ivorycoast = "cotedivoire", saotome = "saotomeandprincipe")
  hit <- v %in% names(alias); v[hit] <- unname(alias[v[hit]]); v
}
reg_theme <- function() ggplot2::theme_minimal(base_size = 19) +
  ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(face = "bold", size = 20),
                 axis.text = ggplot2::element_text(color = "#1F2A30"), panel.grid.minor = ggplot2::element_blank())
reg_pal <- c("#7A1D2F", "#1E7F67", "#D4A017", "#1F5F6B", "#C8622B", "#6A4C93", "#2F5D8A", "#C0392B", "#8C9A96", "#4A7C59")

registry_ui <- function() {
  tabItem(tabName = "registry",
    tags$style(HTML("
      #registry .small-box h3 { font-size: 46px; font-weight: 800; } #registry .small-box p { font-size: 19px; }
      #registry .box-title { font-size: 24px; font-weight: 700; }
      #registry table.dataTable td, #registry table.dataTable th { font-size: 17px; }
      #registry .reg-q { font-size: 20px; line-height: 1.45; } #registry .reg-note { font-size: 16px; color: #444; }
      #registry .reg-obj { font-size: 19px; line-height: 1.4; margin: 0; }
      #registry .reg-obj .box-body { padding: 8px 14px; }
      #registry .leaflet-container { font-size: 15px; }
      .reg-pill { display:inline-block; padding:3px 11px; border-radius:11px; color:#fff; font-size:15px; font-weight:700; }")),
    fluidRow(box(width = 12, status = "warning", solidHeader = TRUE, title = "PREIS AI Atlas: which AI, on what, where, at what stage?",
      div(class = "reg-obj",
        tags$b("Objective: "), "map every AI application used for epidemic surveillance in Africa: ",
        tags$b("WHAT"), " it does, ", tags$b("ON WHAT"), " disease, ", tags$b("WHERE"), " (RCC, country), ", tags$b("WHEN"), " and at what stage. ",
        "Literature signals and known platforms; filters on the left apply. ",
        tags$b("Candidates are keyword signals, not verified systems; registry entries: 0."))),
      uiOutput("reg_asof")),
    fluidRow(
      valueBoxOutput("reg_vb_n",      width = 3),
      valueBoxOutput("reg_vb_plat",   width = 3),
      valueBoxOutput("reg_vb_cand",   width = 3),
      valueBoxOutput("reg_vb_reg",    width = 3)),
    fluidRow(box(width = 12, title = "THE MATRIX: which AI application, on which disease? (number of records)", status = "primary", solidHeader = TRUE,
          plotOutput("reg_plot_heat", height = 720),
          div(class = "reg-note", textOutput("reg_heat_note")))),
    fluidRow(
      box(width = 6, title = "WHAT: which AI applications?", status = "primary", solidHeader = TRUE,
          plotOutput("reg_plot_app", height = 400)),
      box(width = 6, title = "ON WHAT: which diseases?", status = "primary", solidHeader = TRUE,
          plotOutput("reg_plot_dis", height = 400))),
    fluidRow(box(width = 12, title = "KNOWN AI SURVEILLANCE PLATFORMS (documented at source, not validated)", status = "success", solidHeader = TRUE,
      div(class = "reg-note", "Each line cites its public source. Stage is quoted from that source. Documented = stated by the owner or implementer; it is not verified by the registry and says nothing about performance. Country, RCC and disease filters apply when they match."),
      DT::DTOutput("reg_tbl_plat"))),
    fluidRow(box(width = 12, title = "WHERE: Regional Coordinating Centres (fill) and Member States (outline)", status = "danger", solidHeader = TRUE,
          checkboxInput("reg_show_names", "Show country names on the map", value = TRUE),
          leafletOutput("reg_map", height = 620),
          div(class = "reg-note", "Fill = RCC. Dark outline and circle = Member State with at least one record. Click a country for its applications. Only records with a validated study country are placed; the others are counted in the totals but not on the map."))),
    fluidRow(
      box(width = 6, title = "WHERE: records by RCC", status = "danger", solidHeader = TRUE, plotOutput("reg_plot_rcc", height = 340)),
      box(width = 6, title = "WHERE: top Member States", status = "danger", solidHeader = TRUE, plotOutput("reg_plot_cty", height = 340))),
    fluidRow(box(width = 12, title = "WHEN: records by year of publication and application", status = "warning", solidHeader = TRUE,
          plotOutput("reg_plot_year", height = 420),
          div(class = "reg-note", "The current year is partial. Publication year is not the year of deployment."))),
    fluidRow(box(width = 12, title = "Candidate systems found automatically (stage signal from the wording only)", status = "success", solidHeader = TRUE,
      div(class = "reg-note", "Strength reflects the wording of the title and abstract (for example 'deployed', 'pilot'), not a verified stage. Each candidate must be checked by two independent validators."),
      DT::DTOutput("reg_tbl_cand"))),
    fluidRow(box(width = 12, title = "Leads: applications read by one reader (preliminary, not validated)", status = "success", solidHeader = TRUE,
      div(class = "reg-note", "None is confirmed as routine use. A lead is a reason to ask the team, not a finding. Year, country and disease filters apply."),
      DT::DTOutput("reg_tbl_leads"))),
    fluidRow(box(width = 12, title = "Registry status and declaration", status = "danger", solidHeader = TRUE,
      div(class = "reg-q",
        tags$p("Statuses: ", tags$b("declared"), " (the owner says so), ", tags$b("documented"), " (a public document shows it), ",
               tags$b("verified"), " (two independent validators agree). Entries now: declared 0, documented 0, verified 0."),
        tags$p("Phase 1 (done): automatic candidates from the literature. Phase 1b (planned): grey literature. Phase 2 (planned): declarations by RCCs, ministries and partners, then cross-validation."),
        tags$p(tags$a(href = REG_FORM_URL, target = "_blank", rel = "noopener", tags$b("Declare an AI system (about 5 minutes)")),
               " - recorded as declared, not verified. Nothing is published with a name or country without consent.")))))
}

registry_server <- function(input, output, session, reg_dir, filtered_data) {
  cand  <- reactive({ reg_read(reg_dir, "candidates_latest.csv") })
  leads <- reactive({ reg_read(reg_dir, "registry_leads.csv") })
  plat  <- reactive({ reg_read(reg_dir, "registry_platforms.csv") })
  summ  <- reactive({ reg_read(reg_dir, "registry_summary.csv") })

  ## records in the registry perimeter, after ALL sidebar filters
  base <- reactive({
    x <- filtered_data()
    if ("ai_method_evidence" %in% names(x)) x <- x[!is.na(x$ai_method_evidence) & x$ai_method_evidence == "explicit", , drop = FALSE]
    if ("ph_scope_tier" %in% names(x)) x <- x[!is.na(x$ph_scope_tier) & x$ph_scope_tier == "Core", , drop = FALSE]
    af <- (x$africa_relevance_level %in% c("High", "Moderate")) | (!is.na(x$country) & !(x$country %in% REG_UNSPEC))
    x[af, , drop = FALSE]
  })
  cand_in_view <- reactive({
    x <- base(); cd <- cand(); if (is.null(cd) || nrow(x) == 0) return(NULL)
    cd$k <- reg_key(cd$doi, cd$url); cd <- cd[!is.na(cd$k) & !duplicated(cd$k), c("k", "signal", "signal_strength", "matched_phrase"), drop = FALSE]
    x$k <- reg_key(x$doi, x$url)
    m <- merge(x, cd, by = "k")
    m[order(match(m$signal_strength, c("strong", "medium", "weak")), -suppressWarnings(as.numeric(m$year_num))), , drop = FALSE]
  })
  app_levels <- sort(unique(as.character(ai_static$application_type_primary)))
  app_cols <- setNames(rep(reg_pal, length.out = length(app_levels)), app_levels)

  output$reg_asof <- renderUI({
    s <- summ(); d <- if (!is.null(s)) s$value[s$key == "analysis_closed"] else character(0)
    f <- file.path(reg_dir, "candidates_latest.csv")
    div(class = "reg-note", paste0("Tracker data in view: ", nrow(base()), " records after filters. Candidates file updated: ",
        if (file.exists(f)) format(file.mtime(f), "%Y-%m-%d") else "n/a", ". Reference analysis closed: ", if (length(d)) d[1] else "n/a", "."))
  })

  output$reg_vb_n <- renderValueBox(valueBox(format(nrow(base()), big.mark = ","), "Africa-linked records, explicit AI (after filters)", icon = icon("globe-africa"), color = "yellow"))
  output$reg_vb_plat <- renderValueBox({ p <- plat(); valueBox(if (is.null(p)) 0 else sum(p$relevance == "core"), "Known AI platforms in Africa (documented, core)", icon = icon("server"), color = "teal") })
  output$reg_vb_cand <- renderValueBox({ m <- cand_in_view(); valueBox(if (is.null(m)) 0 else nrow(m), "Candidates in view (not verified)", icon = icon("magnifying-glass"), color = "purple") })
  output$reg_vb_strong <- renderValueBox({ m <- cand_in_view(); valueBox(if (is.null(m)) 0 else sum(m$signal_strength == "strong"), "Strong wording signals", icon = icon("flag"), color = "green") })
  output$reg_vb_reg <- renderValueBox(valueBox(0, "Registry entries (declared, documented, verified)", icon = icon("clipboard-check"), color = "red"))

  need_rows <- function(x) validate(need(nrow(x) > 0, "No record matches the current filters. Widen the filters in the left bar."))

  output$reg_plot_app <- renderPlot({
    x <- base(); need_rows(x)
    d <- as.data.frame(table(application = x$application_type_primary), stringsAsFactors = FALSE)
    d <- d[order(d$Freq), ]; d$application <- factor(d$application, levels = d$application)
    ggplot(d, aes(application, Freq, fill = application)) + geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(aes(label = Freq), hjust = -0.2, size = 5, fontface = "bold") +
      scale_fill_manual(values = app_cols) + scale_x_discrete(labels = function(v) stringr::str_wrap(v, 28)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + coord_flip() + labs(x = NULL, y = "Records") + reg_theme()
  })

  output$reg_plot_dis <- renderPlot({
    x <- base(); need_rows(x); x <- x[!(x$disease_primary %in% REG_UNSPEC), , drop = FALSE]; need_rows(x)
    d <- as.data.frame(table(disease = x$disease_primary), stringsAsFactors = FALSE)
    d <- d[order(-d$Freq), ][seq_len(min(12, nrow(d))), ]; d <- d[order(d$Freq), ]; d$disease <- factor(d$disease, levels = d$disease)
    ggplot(d, aes(disease, Freq)) + geom_col(fill = "#7A1D2F", width = 0.7) +
      geom_text(aes(label = Freq), hjust = -0.2, size = 5, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + coord_flip() + labs(x = NULL, y = "Records") + reg_theme()
  })

  heat_data <- reactive({
    x <- base(); x <- x[!(x$disease_primary %in% REG_UNSPEC) & !(x$application_type_primary %in% REG_UNSPEC), , drop = FALSE]
    if (nrow(x) == 0) return(x)
    top <- names(sort(table(x$disease_primary), decreasing = TRUE))[seq_len(min(12, length(unique(x$disease_primary))))]
    x[x$disease_primary %in% top, , drop = FALSE]
  })
  output$reg_plot_heat <- renderPlot({
    x <- heat_data(); need_rows(x)
    d <- as.data.frame(table(disease = x$disease_primary, application = x$application_type_primary), stringsAsFactors = FALSE)
    d <- d[d$Freq > 0, ]
    ord <- names(sort(table(x$disease_primary), decreasing = FALSE)); d$disease <- factor(d$disease, levels = ord)
    ggplot(d, aes(application, disease, fill = Freq)) + geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = Freq, color = Freq > max(Freq) * 0.55), size = 5.5, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#1F2A30")) +
      scale_fill_gradient(low = "#F6E6EA", high = "#7A1D2F", name = "Records") +
      scale_x_discrete(labels = function(v) stringr::str_wrap(v, 16), position = "top") +
      labs(x = NULL, y = NULL) + reg_theme() + theme(legend.position = "right", axis.text.x = element_text(size = 13))
  })
  output$reg_heat_note <- renderText({
    x <- base(); y <- heat_data()
    paste0("Top 12 diseases with a specific label. ", nrow(x) - nrow(y), " of ", nrow(x), " records are not shown here (disease or application unspecified, or outside the top 12).")
  })

  output$reg_plot_rcc <- renderPlot({
    x <- base(); x <- x[!(x$rcc %in% REG_UNSPEC), , drop = FALSE]; need_rows(x)
    d <- as.data.frame(table(rcc = x$rcc), stringsAsFactors = FALSE); d <- d[order(d$Freq), ]; d$rcc <- factor(d$rcc, levels = d$rcc)
    ggplot(d, aes(rcc, Freq, fill = rcc)) + geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(aes(label = Freq), hjust = -0.2, size = 5, fontface = "bold") +
      scale_fill_manual(values = rcc_colors, na.value = "grey70") + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() + labs(x = NULL, y = "Records with a study country") + reg_theme()
  })
  output$reg_plot_cty <- renderPlot({
    x <- base(); x <- x[!(x$country %in% REG_UNSPEC), , drop = FALSE]; need_rows(x)
    d <- as.data.frame(table(country = x$country, rcc = x$rcc), stringsAsFactors = FALSE); d <- d[d$Freq > 0, ]
    d <- d[order(-d$Freq), ][seq_len(min(12, nrow(d))), ]; d <- d[order(d$Freq), ]; d$country <- factor(d$country, levels = unique(d$country))
    ggplot(d, aes(country, Freq, fill = rcc)) + geom_col(width = 0.7) + geom_text(aes(label = Freq), hjust = -0.2, size = 5, fontface = "bold") +
      scale_fill_manual(values = rcc_colors, na.value = "grey70") + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() + labs(x = NULL, y = "Records") + reg_theme()
  })

  output$reg_plot_year <- renderPlot({
    x <- base(); x <- x[!is.na(x$year_num) & x$year_num >= 2015, , drop = FALSE]; need_rows(x)
    d <- as.data.frame(table(year = x$year_num, application = x$application_type_primary), stringsAsFactors = FALSE)
    d$year <- as.integer(as.character(d$year)); d <- d[d$Freq > 0, ]
    ggplot(d, aes(year, Freq, fill = application)) + geom_col(width = 0.75) +
      scale_fill_manual(values = app_cols) + scale_x_continuous(breaks = seq(2015, max(d$year), 1)) +
      guides(fill = guide_legend(nrow = 3)) + labs(x = "Year of publication", y = "Records") + reg_theme()
  })

  output$reg_map <- renderLeaflet({
    x <- base(); show_names <- is.null(input$reg_show_names) || isTRUE(input$reg_show_names)
    m <- leaflet(options = leafletOptions(minZoom = 3, maxZoom = 8)) %>%
      fitBounds(lng1 = -19, lat1 = -35, lng2 = 52, lat2 = 37.5) %>% setMaxBounds(lng1 = -30, lat1 = -42, lng2 = 62, lat2 = 42) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Light map") %>% addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Light map", "Satellite"), position = "topright", options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft")
    if (is.null(africa_map)) return(m)
    mp <- sf::st_drop_geometry(africa_map); gm <- sf::st_geometry(africa_map)
    cc <- names(mp)[tolower(names(mp)) %in% c("country", "name", "admin", "country_name", "sovereignt")][1]
    if (is.na(cc)) return(m)
    mp$country_join <- clean_txt(mp[[cc]]); mp$ck <- reg_norm(mp$country_join)
    xc <- x[!(x$country %in% REG_UNSPEC), , drop = FALSE]
    if (nrow(xc) > 0) {
      xc$ck <- reg_norm(xc$country)
      cnt <- as.data.frame(table(ck = xc$ck), stringsAsFactors = FALSE); names(cnt)[2] <- "n"
      tt <- as.data.frame(table(ck = xc$ck, app = xc$application_type_primary), stringsAsFactors = FALSE); tt <- tt[tt$Freq > 0, ]
      tt <- tt[order(tt$ck, -tt$Freq), ]
      txt <- vapply(split(tt, tt$ck), function(g) paste0(utils::head(paste0(g$app, ": ", g$Freq), 4), collapse = "<br/>"), character(1))
      mp$n <- cnt$n[match(mp$ck, cnt$ck)]; mp$apps <- unname(txt[mp$ck])
    } else { mp$n <- NA_integer_; mp$apps <- NA_character_ }
    mp$n <- ifelse(is.na(mp$n), 0L, as.integer(mp$n)); mp$apps <- ifelse(is.na(mp$apps), "", mp$apps)
    rcc_fb <- if ("rcc" %in% names(mp)) mp$rcc else rep("Unspecified", nrow(mp))
    mp$rcc_fill <- ifelse(!is.na(rcc_fb) & rcc_fb %in% names(rcc_colors), rcc_fb, "Unspecified")
    poly <- sf::st_sf(mp, geometry = gm)
    m <- m %>% addPolygons(data = poly,
      fillColor = ~unname(rcc_colors[rcc_fill]), color = ~ifelse(n > 0, "#263238", "#FFFFFF"), weight = ~ifelse(n > 0, 2.5, 1),
      opacity = 1, fillOpacity = 0.85, highlightOptions = highlightOptions(weight = 3, color = "#263238", bringToFront = TRUE),
      label = ~paste0(country_join, " - ", n, " record(s)"),
      popup = ~paste0("<b>", country_join, "</b><br/>RCC: ", rcc_fill, "<br/>Records: ", n, ifelse(apps != "", paste0("<hr style='margin:4px 0'/>", apps), "")))
    ctr <- tryCatch(suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(gm))), error = function(e) NULL)
    if (!is.null(ctr) && nrow(ctr) == nrow(poly)) {
      has <- poly$n > 0
      if (any(has)) m <- m %>% addCircleMarkers(lng = ctr[has, 1], lat = ctr[has, 2], radius = pmax(9, pmin(32, sqrt(poly$n[has]) * 6)),
        color = "#FFFFFF", weight = 3, fillColor = "#0D2B1D", fillOpacity = 0.95, label = paste0(poly$country_join[has], ": ", poly$n[has], " record(s)"))
      if (any(has)) m <- m %>% addLabelOnlyMarkers(lng = ctr[has, 1], lat = ctr[has, 2], label = as.character(poly$n[has]),
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, style = list("color" = "#FFFFFF", "font-weight" = "700", "font-size" = "12px")))
      if (show_names) {
        short_nm <- c("Democratic Republic of the Congo" = "DR Congo", "Republic of the Congo" = "Congo", "Kingdom of eSwatini" = "Eswatini",
                      "Republic of Cabo Verde" = "Cabo Verde", "Central African Republic" = "C.A.R.", "The Gambia" = "Gambia", "Somaliland" = "")
        nm <- as.character(poly$country_join); disp <- ifelse(nm %in% names(short_nm), unname(short_nm[nm]), nm)
        ar <- vapply(sf::st_geometry(poly), function(g) { b <- sf::st_bbox(g); as.numeric((b[3] - b[1]) * (b[4] - b[2])) }, numeric(1)); ar[is.na(ar)] <- 0
        big <- ar >= sort(ar, decreasing = TRUE)[min(20, length(ar))]; keep <- nzchar(disp) & !(poly$n > 0)
        sty <- function(sz) list("font-size" = sz, "font-weight" = "600", "color" = "#1F2A30", "white-space" = "nowrap",
          "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff, 0 0 4px #fff")
        lo <- function(sz) labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, style = sty(sz))
        m <- tryCatch(m %>%
          addLabelOnlyMarkers(lng = ctr[big & keep, 1], lat = ctr[big & keep, 2], label = disp[big & keep], group = "reg_lbl_big", labelOptions = lo("12px")) %>%
          addLabelOnlyMarkers(lng = ctr[!big & keep, 1], lat = ctr[!big & keep, 2], label = disp[!big & keep], group = "reg_lbl_small", labelOptions = lo("10.5px")) %>%
          groupOptions("reg_lbl_big", zoomLevels = 3:8) %>% groupOptions("reg_lbl_small", zoomLevels = 5:8), error = function(e) m)
      }
    }
    m %>% addLegend("bottomright", colors = unname(rcc_colors), labels = names(rcc_colors), title = "Regional Coordinating Centre (fill)", opacity = 1)
  })

  output$reg_tbl_cand <- DT::renderDT({
    m <- cand_in_view(); validate(need(!is.null(m) && nrow(m) > 0, "No candidate matches the current filters."))
    link <- ifelse(!is.na(m$doi) & m$doi != "", paste0("https://doi.org/", m$doi), m$url)
    col <- c(strong = "#1E7F67", medium = "#C8622B", weak = "#8C9A96")[m$signal_strength]
    out <- data.frame(Strength = paste0('<span class="reg-pill" style="background:', col, '">', m$signal_strength, "</span>"),
      Title = reg_link(link, m$title), Year = m$year_num, Country = m$country, RCC = m$rcc, Disease = m$disease_primary,
      Application = m$application_type_primary, `Stage signal` = m$signal, Wording = m$matched_phrase, check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE, order = list()))
  })

  output$reg_tbl_plat <- DT::renderDT({
    p <- plat(); validate(need(!is.null(p) && nrow(p) > 0, "Platforms file not found."))
    if (!is.null(input$sel_country) && input$sel_country != "All") p <- p[p$country_scope == input$sel_country | p$relevance == "adjacent", , drop = FALSE]
    validate(need(nrow(p) > 0, "No platform matches the current filters."))
    col <- c(core = "#1E7F67", adjacent = "#8C9A96")[p$relevance]
    out <- data.frame(Platform = reg_link(p$source_url, p$name), Country = p$country_scope, RCC = p$rcc, Function = p[["function"]],
      `AI as stated` = p$ai_method_stated, `Stage as stated` = p$stage_stated, Owner = p$owner_or_implementer,
      Scope = paste0('<span class="reg-pill" style="background:', col, '">', p$relevance, "</span>"), Caveat = p$caveat,
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, escape = FALSE, options = list(pageLength = 12, scrollX = TRUE, order = list()))
  })

  output$reg_tbl_leads <- DT::renderDT({
    l <- leads(); validate(need(!is.null(l), "Leads file not found."))
    yr <- input$year_range; if (!is.null(yr)) l <- l[is.na(l$year) | (l$year >= yr[1] & l$year <= yr[2]), , drop = FALSE]
    if (!is.null(input$sel_country) && input$sel_country != "All") l <- l[l$country == input$sel_country, , drop = FALSE]
    if (!is.null(input$sel_disease) && input$sel_disease != "All") l <- l[l$disease == input$sel_disease, , drop = FALSE]
    validate(need(nrow(l) > 0, "No lead matches the current filters."))
    out <- data.frame(Country = l$country, Disease = l$disease, Year = l$year, Lead = reg_link(l$url, l$description_en),
                      Reading = gsub("_", " ", l$stage), stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, escape = FALSE, options = list(pageLength = 11, scrollX = TRUE))
  })
}
