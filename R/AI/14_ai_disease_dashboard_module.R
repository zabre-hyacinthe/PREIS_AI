# ============================================================
# PREIS-AI — DISEASE DASHBOARD MODULE
# File: D:/PREIS_AI/R/AI/14_ai_disease_dashboard_module.R
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(DT)
  library(stringr)
  library(tidyr)
})

safe_read_csv <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(tibble::tibble())
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) tibble::tibble()
  )
}

normalize_text <- function(x, default = "") {
  x <- as.character(x)
  x[is.na(x)] <- default
  trimws(x)
}

load_ai_disease_dashboard_data <- function(project_dir = "D:/PREIS_AI") {
  data_dir <- file.path(project_dir, "data", "dashboard")
  
  list(
    dataset = safe_read_csv(file.path(data_dir, "ai_intelligence_dashboard_dataset.csv")),
    qc = safe_read_csv(file.path(data_dir, "ai_disease_component_qc.csv")),
    disease_summary = safe_read_csv(file.path(data_dir, "ai_disease_summary.csv")),
    disease_method = safe_read_csv(file.path(data_dir, "ai_disease_method_summary.csv")),
    disease_application = safe_read_csv(file.path(data_dir, "ai_disease_application_summary.csv")),
    disease_rcc = safe_read_csv(file.path(data_dir, "ai_disease_rcc_summary.csv")),
    disease_year = safe_read_csv(file.path(data_dir, "ai_disease_year_summary.csv"))
  )
}

ai_disease_filter_ui <- function(id = "ai_disease") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        selectInput(
          ns("disease_primary"),
          "Disease",
          choices = NULL,
          multiple = TRUE
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("disease_group"),
          "Disease group",
          choices = NULL,
          multiple = TRUE
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("pathogen_class"),
          "Pathogen class",
          choices = NULL,
          multiple = TRUE
        )
      )
    )
  )
}

ai_disease_outputs_ui <- function(id = "ai_disease") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        plotOutput(ns("top_diseases_plot"), height = "320px")
      ),
      column(
        width = 6,
        plotOutput(ns("disease_by_rcc_plot"), height = "320px")
      )
    ),
    fluidRow(
      column(
        width = 6,
        plotOutput(ns("disease_by_method_plot"), height = "320px")
      ),
      column(
        width = 6,
        plotOutput(ns("disease_trend_plot"), height = "320px")
      )
    ),
    fluidRow(
      column(
        width = 12,
        DTOutput(ns("disease_table"))
      )
    )
  )
}

ai_disease_module_server <- function(id = "ai_disease",
                                     project_dir = "D:/PREIS_AI",
                                     base_filtered_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    disease_data <- reactive({
      load_ai_disease_dashboard_data(project_dir = project_dir)
    })
    
    observe({
      dsb <- disease_data()$dataset
      if (nrow(dsb) == 0) return(NULL)
      
      if (!"disease_primary" %in% names(dsb)) dsb$disease_primary <- "Unspecified"
      if (!"disease_group" %in% names(dsb)) dsb$disease_group <- "Unspecified"
      if (!"pathogen_class" %in% names(dsb)) dsb$pathogen_class <- "Unspecified"
      
      updateSelectInput(
        session, "disease_primary",
        choices = sort(unique(normalize_text(dsb$disease_primary, "Unspecified"))),
        selected = NULL
      )
      
      updateSelectInput(
        session, "disease_group",
        choices = sort(unique(normalize_text(dsb$disease_group, "Unspecified"))),
        selected = NULL
      )
      
      updateSelectInput(
        session, "pathogen_class",
        choices = sort(unique(normalize_text(dsb$pathogen_class, "Unspecified"))),
        selected = NULL
      )
    })
    
    filtered_data <- reactive({
      df <- if (is.null(base_filtered_data)) {
        disease_data()$dataset
      } else {
        base_filtered_data()
      }
      
      if (nrow(df) == 0) return(df)
      
      if (!"disease_primary" %in% names(df)) df$disease_primary <- "Unspecified"
      if (!"disease_group" %in% names(df)) df$disease_group <- "Unspecified"
      if (!"pathogen_class" %in% names(df)) df$pathogen_class <- "Unspecified"
      
      df <- df %>%
        mutate(
          disease_primary = normalize_text(disease_primary, "Unspecified"),
          disease_group = normalize_text(disease_group, "Unspecified"),
          pathogen_class = normalize_text(pathogen_class, "Unspecified"),
          rcc = if ("rcc" %in% names(.)) normalize_text(rcc, "Unspecified") else "Unspecified",
          method_family_primary = if ("method_family_primary" %in% names(.)) normalize_text(method_family_primary, "Unspecified") else "Unspecified",
          application_type_primary = if ("application_type_primary" %in% names(.)) normalize_text(application_type_primary, "Unspecified") else "Unspecified",
          year = if ("year" %in% names(.)) suppressWarnings(as.integer(year)) else NA_integer_
        )
      
      if (!is.null(input$disease_primary) && length(input$disease_primary) > 0) {
        df <- df %>% filter(disease_primary %in% input$disease_primary)
      }
      
      if (!is.null(input$disease_group) && length(input$disease_group) > 0) {
        df <- df %>% filter(disease_group %in% input$disease_group)
      }
      
      if (!is.null(input$pathogen_class) && length(input$pathogen_class) > 0) {
        df <- df %>% filter(pathogen_class %in% input$pathogen_class)
      }
      
      df
    })
    
    output$top_diseases_plot <- renderPlot({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No data available"))
      
      plot_df <- df %>%
        count(disease_primary, sort = TRUE) %>%
        slice_head(n = 10)
      
      ggplot(plot_df, aes(x = reorder(disease_primary, n), y = n)) +
        geom_col() +
        coord_flip() +
        labs(
          title = "Top diseases",
          x = NULL,
          y = "Number of records"
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$disease_by_rcc_plot <- renderPlot({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No data available"))
      
      plot_df <- df %>%
        count(disease_primary, rcc, sort = TRUE) %>%
        group_by(disease_primary) %>%
        mutate(total = sum(n)) %>%
        ungroup() %>%
        arrange(desc(total)) %>%
        filter(disease_primary %in% unique(disease_primary)[1:min(8, n_distinct(disease_primary))])
      
      ggplot(plot_df, aes(x = disease_primary, y = n, fill = rcc)) +
        geom_col(position = "stack") +
        coord_flip() +
        labs(
          title = "Disease by RCC",
          x = NULL,
          y = "Number of records"
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$disease_by_method_plot <- renderPlot({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No data available"))
      
      plot_df <- df %>%
        count(disease_primary, method_family_primary, sort = TRUE) %>%
        filter(method_family_primary != "Unspecified") %>%
        group_by(disease_primary) %>%
        mutate(total = sum(n)) %>%
        ungroup() %>%
        arrange(desc(total)) %>%
        filter(disease_primary %in% unique(disease_primary)[1:min(8, n_distinct(disease_primary))])
      
      ggplot(plot_df, aes(x = disease_primary, y = n, fill = method_family_primary)) +
        geom_col(position = "stack") +
        coord_flip() +
        labs(
          title = "Disease by AI method",
          x = NULL,
          y = "Number of records"
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$disease_trend_plot <- renderPlot({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No data available"))
      
      plot_df <- df %>%
        filter(!is.na(year)) %>%
        count(year, disease_primary, sort = TRUE) %>%
        group_by(disease_primary) %>%
        mutate(total = sum(n)) %>%
        ungroup() %>%
        arrange(desc(total)) %>%
        filter(disease_primary %in% unique(disease_primary)[1:min(6, n_distinct(disease_primary))])
      
      ggplot(plot_df, aes(x = year, y = n, color = disease_primary)) +
        geom_line(linewidth = 1) +
        geom_point() +
        labs(
          title = "Disease trend over time",
          x = "Year",
          y = "Number of records",
          color = "Disease"
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$disease_table <- renderDT({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No data available"))
      
      keep_cols <- intersect(
        c(
          "title",
          "disease_primary",
          "disease_group",
          "pathogen_class",
          "country",
          "rcc",
          "year",
          "method_family_primary",
          "application_type_primary",
          "priority_level"
        ),
        names(df)
      )
      
      DT::datatable(
        df[, keep_cols, drop = FALSE],
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    return(list(
      filtered_data = filtered_data
    ))
  })
}