# MFA Shiny application - install in inst/shiny/app.R
library(shiny)
library(MFA)
library(DT)
library(DBI)

purposes <- c("Creel Census", "Discretionary Survey", "Fish Community", "General Survey", "Limnology", "Management Evaluation", "Population Estimate", "Population Reduction", "Recruitment Evaluation", "Recruitment v Evaluation", "Research Project", "Special Study", "Species Evaluation", "Status & Trends", "Stocking Evaluation")
gears <- c("ANGLING", "BACKSHK", "BOOMSHK", "Envelop", "EXPGNET", "GLGNET", "LMFYKE", "LIMNO", "MNWTRAP", "OTHER", "PhotoPt", "PLNKNET", "SEINE", "SETHKS", "SMFYKE", "SRGNET", "STRMSHK", "TOXSAM", "TRAPNET", "TRAWL", "VGNET")
analyses <- c("Catch summary"="catchSummary", "CPUE by gear"="CPUE_byGear", "Species detections by year"="detectionByYear", "Length frequency"="lengthFreqByEffort", "Relative stock density (RSD)"="RSD", "Age-length summary"="ageLengthSummary")
# Concise, app-oriented descriptions.
analysis_help <- list(
  catchSummary = list(description = "Summarizes catch by survey and species, including total catch, lengths, and fish meeting the applicable legal minimum size.", details = "Input: FISH_query(QueryType = 'Catch'). Output: one row per survey and species. Average length is weighted; the underlying function notes possible differences from SFRs due to data issues."),
  CPUE_byGear = list(description = "Calculates catch per unit effort by survey, gear type, and species.", details = "Input: catch and effort data from FISH_query(QueryType = 'Catch'). Uses primary effort when available, otherwise alternate effort. Seconds are converted to minutes. Missing effort quantities may cause records to be excluded. Output: table or figure."),
  detectionByYear = list(description = "Shows whether each species was detected in each year, along with annual survey and effort counts.", details = "Input: FISH_query(QueryType = 'Catch'). Detection requires at least one fish caught. Years come from survey start dates. Combining multiple waterbodies merges their detection histories and generates a warning."),
  lengthFreqByEffort = list(description = "Summarizes fish length frequencies by survey, sampling module, species, and inch group.", details = "Input: FISH_query(QueryType = 'Efforts') and an active FISHub connection. Counts use unmarked fish from inch-group catch records to avoid double counting. Scale-envelope records are not included. Output: table or figure."),
  RSD = list(description = "Calculates PSD and relative stock density using Gabelhouse length categories.", details = "Input: length-frequency data. PSD and RSD-P, RSD-M, and RSD-T express the percentage of stock-length fish at or above Quality, Preferred, Memorable, and Trophy lengths. Only species with reference lengths are included."),
  ageLengthSummary = list(description = "Summarizes fish length at age by survey and species.", details = "Input: FISH_query(QueryType = 'Efforts') and an active FISHub connection. Reports simple and, when length-frequency data permit, catch-weighted mean lengths. Multiple age readings for a fish are resolved by the most frequent age. Output: table, figure, or raw data.")
)

# Retrieve the source Rd file during development, falling back to installed help.
get_function_rd <- function(function_name) {
  pkg_root <- tryCatch(getNamespaceInfo(asNamespace("MFA"), "path"),
                       error = function(e) "")
  candidates <- unique(file.path(
    c(pkg_root, getwd(), file.path(getwd(), ".."),
      file.path(getwd(), "..", ".."), file.path(getwd(), "..", "..", "..")),
    "man", paste0(function_name, ".Rd")
  ))
  local_file <- candidates[file.exists(candidates)]
  if (length(local_file)) return(tools::parse_Rd(local_file[[1]]))
  topic <- utils::help(function_name, package = "MFA")
  if (length(topic)) return(utils:::.getHelpFile(topic))
  NULL
}

# Render Rd markup as HTML, rather than showing the underlying markup or
# terminal formatting codes. Only documentation from the MFA package is used.
get_function_help_html <- function(function_name) {
  tryCatch({
    rd <- get_function_rd(function_name)
    if (is.null(rd)) return(tags$p("Function documentation not found. Run devtools::document() to generate man/*.Rd files."))
    html <- paste(capture.output(tools::Rd2HTML(rd, package = "MFA")), collapse = "\n")
    # Rd2HTML produces a complete HTML document; embed only its body.
    body <- sub("(?s)^.*?<body[^>]*>", "", html, perl = TRUE)
    body <- sub("(?s)</body>.*$", "", body, perl = TRUE)
    if (identical(body, html)) stop("HTML body could not be extracted")
    div(class = "mfa-rd-html", HTML(body))
  }, error = function(e) tags$p("Unable to display MFA help: ", conditionMessage(e)))
}

help_panel <- function(function_name) {
  info <- analysis_help[[function_name]]
  tagList(
    tags$p(class = "mfa-help-summary", info$description),
    tags$p(class = "mfa-help-summary", info$details)
  )
}

parse_csv <- function(x, numeric = FALSE) {
  x <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  x <- x[nzchar(x)]
  if (!length(x)) return(NULL)
  if (numeric) {
    out <- suppressWarnings(as.numeric(x))
    if (anyNA(out)) stop("Enter comma-separated numeric values only.")
    return(out)
  }
  x
}

ui <- navbarPage(
  title = "MFA | Fisheries Data",
  id = "page",
  
  tags$head(tags$style(HTML("
    :root { --mfa-teal:#277C78; --mfa-light:#61C8AF; --mfa-navy:#142D3E; --mfa-green:#0D8252; --mfa-border:#dce7e7; }
    .mfa-warning-item { padding: 13px 0; border-bottom: 1px solid #cbdada; }
    .mfa-warning-item:first-child { padding-top: 0; }
    .mfa-warning-item:last-child { border-bottom: 0; padding-bottom: 0; }
    body { background:#f5f8f8; color:#203640; font-family:'Segoe UI',Arial,sans-serif; }
    .navbar-default { background:var(--mfa-navy); border:0; border-radius:0; box-shadow:0 2px 8px rgba(20,45,62,.14); }
    .navbar-default .navbar-brand { color:#fff !important; font-weight:700; letter-spacing:.3px; }
    .navbar-default .navbar-nav > li > a { color:#d5e8e8 !important; font-weight:600; }
    .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover { color:var(--mfa-navy) !important; background:var(--mfa-light) !important; }
    .navbar-default .navbar-nav > li > a:hover { color:#fff !important; background:#24495a !important; }
    .container-fluid { max-width:1680px; }
    .well { background:#fff; border:1px solid var(--mfa-border); border-radius:12px; box-shadow:0 2px 12px rgba(20,45,62,.045); padding:20px; }
    .main-panel { background:#fff; border:1px solid var(--mfa-border); border-radius:12px; padding:22px 26px; min-height:280px; box-shadow:0 2px 12px rgba(20,45,62,.045); }
    h2,h3,h4 { color:var(--mfa-navy); font-weight:700; }
    h4 { font-size:17px; margin-top:12px; margin-bottom:14px; }
    .control-label { color:#314c58; font-weight:600; font-size:13px; }
    .form-control { border:1px solid #cbdada; border-radius:7px; box-shadow:none; }
    .form-control:focus { border-color:var(--mfa-teal); box-shadow:0 0 0 3px rgba(39,124,120,.14); }
    .btn { border-radius:7px; font-weight:600; transition:background-color .15s,box-shadow .15s; }
    .btn-primary, .btn-primary:focus { background:var(--mfa-teal); border-color:var(--mfa-teal); color:#fff; }
    .btn-primary:hover, .btn-primary:active { background:var(--mfa-navy); border-color:var(--mfa-navy); }
    .btn-default { background:#fff; color:var(--mfa-teal); border-color:#a9c8c5; }
    .btn-default:hover { background:#eaf5f2; color:var(--mfa-navy); border-color:var(--mfa-teal); }
    .btn:focus, a:focus { outline:2px solid var(--mfa-light); outline-offset:2px; }
    a { color:var(--mfa-teal); } a:hover { color:var(--mfa-navy); }
    hr { border-top-color:var(--mfa-border); margin:20px 0; }
    .mfa-survey-chip { display:inline-block; padding:5px 9px; border:1px solid #b5d5d0; border-radius:7px; background:#eff8f5; color:var(--mfa-teal); text-decoration:none !important; font-weight:600; }
    .mfa-survey-chip:hover { background:var(--mfa-navy); color:#fff; border-color:var(--mfa-navy); }
    .mfa-survey-chip:focus { outline:2px solid var(--mfa-light); outline-offset:2px; }
    .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select { border:1px solid #cbdada; border-radius:6px; }
    table.dataTable thead th { background:#eaf4f2; color:var(--mfa-navy); border-bottom:2px solid var(--mfa-teal) !important; }
    table.dataTable tbody tr.selected { background-color:#d9eee8 !important; }
    .pagination > .active > a, .pagination > .active > a:hover { background:var(--mfa-teal); border-color:var(--mfa-teal); }
    .shiny-notification { border-left:4px solid var(--mfa-teal); border-radius:8px; box-shadow:0 5px 20px rgba(20,45,62,.15); }
    .shiny-notification-message { background:#edf8f4; color:var(--mfa-navy); }
    .shiny-notification-error { border-left-color:#b63b3b; }
    .modal-header { background:var(--mfa-navy); color:#fff; border-radius:6px 6px 0 0; }
    .modal-title { color:#fff; }
    .mfa-intro { max-width:900px; margin:28px auto; background:#fff; border:1px solid var(--mfa-border); border-radius:14px; padding:32px 38px; box-shadow:0 2px 14px rgba(20,45,62,.05); }
    .mfa-intro h2 { margin-top:0; }
    .mfa-intro h4 { border-left:4px solid var(--mfa-teal); padding-left:12px; margin-top:24px; }
    .mfa-intro-lead { color:#4c6670; font-size:16px; line-height:1.6; }
    .mfa-export .btn { margin:0 5px 8px 0; }
    .mfa-help-summary { color:#425d66; font-size:13px; line-height:1.55; margin:8px 0 12px; }
    .mfa-rd-html { max-height:440px; overflow:auto; overflow-wrap:anywhere; padding:12px; margin-top:10px; background:#f5f8f8; border:1px solid var(--mfa-border); border-radius:8px; font-size:12px; }
    .mfa-rd-html pre { white-space:pre-wrap; overflow-wrap:anywhere; }
    .mfa-rd-html h2, .mfa-rd-html h3 { font-size:15px; }
    #reset_query { margin-top:8px; }
    #run_analysis, #run_analysis:focus { background:var(--mfa-green); border-color:var(--mfa-green); color:white; font-size:15px; padding:11px; }
    #run_analysis:hover { background:#08633e; border-color:#08633e; }
    .mfa-note { font-size:12px; color:#4c6670; line-height:1.5; }
    @media(max-width:767px) { .main-panel { padding:16px; } .mfa-intro { margin:14px; padding:20px; } }
  "))),
  tabPanel("Read Me", value = "readme",
           fluidPage(
             div(class = "mfa-intro",
                 h2("Michigan Fisheries Analysis (MFA) Data Explorer"),
                 p(class = "mfa-intro-lead", textOutput("mfa_version", inline = TRUE)),
                 p(class = "mfa-intro-lead", "Use this app to query FISHub surveys, build a selection of surveys across multiple queries and waterbodies, and run MFA analyses."),
                 h4("1. Browse surveys"),
                 p("Choose filters on Browse Surveys and click Run query. Leave filters blank to search broadly. The results table can be sorted and filtered."),
                 h4("2. Build your survey selection"),
                 p("Select one or more rows and click Add selected surveys, or choose Add all matching surveys. Repeat with different waterbodies or filters; previously added surveys remain in your selection. The left panel lists selected Survey IDs, lets you remove surveys, and provides survey CSV exports."),
                 p("Reset query clears the filters and current query results, but keeps your selected surveys. Clear selection removes surveys from the analysis set. Database and data-quality warnings appear with the relevant query or analysis."),
                 h4("3. View and export Catch/Effort data"),
                 p("Open Catch and Effort Data to select gear types and species. These filters apply to all analyses and to effort/catch data displayed or exported on that tab."),
                 h4("4. Run analyses"),
                 p("Gear and species filters on Catch and Effort Data are shared by every analysis. Clear them to use all gear types and species."),
                 p("Open Analyses, choose a method, and click Run analysis. Analyses use all surveys in your accumulated selection, regardless of the most recent query. Download analysis tables as CSV or figures as PNG."),
                 h4("Notes and Warnings"),
                 p("MFA and this application are still in development. Results may contain errors. Please review outputs carefully and notify Lucas Nathan (nathanl@michigan.gov) if you identify a potential error."),
                 p("FISHub access requires the configured SQL Server connection and appropriate permissions."),
                 p("Large queries may take time or exceed SQL Server query limits.")
                 
             )
           )
  ),
  tabPanel("Browse Surveys", value = "browse",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h4("Survey filters"),
                          textInput("mdnrid", "MDNRID(s)", placeholder = "Comma-separated"),
                          textInput("water", "Waterbody name(s)", placeholder = "Exact names, comma-separated"),
                          selectInput("fmu", "FMU", c("All"="", "LS", "NLM", "CLM", "SLM", "NLH", "SLH", "LE"), multiple = TRUE),
                          selectInput("water_type", "Water type", c("GL", "IL", "ISR"), multiple = TRUE),
                          textInput("survey_id", "Survey ID(s)", placeholder = "Comma-separated"),
                          selectInput("purpose", "Survey purpose", purposes, multiple = TRUE),
                          textInput("year", "Year(s)", placeholder = "e.g., 2024, 2025"),
                          selectInput("gear", "Gear type", gears, multiple = TRUE),
                          selectizeInput("species", "Species", choices = NULL, multiple = TRUE,
                                         options = list(placeholder = "All species; type to search")),
                          actionButton("query", "Run query", class = "btn-primary", width = "100%"),
                          actionButton("reset_query", "Reset query", width = "100%"),
                          br(), br(),
                          h4("Surveys selected"),
                          uiOutput("basket_summary"),
                          actionButton("clear_selection", "Clear selection"),
                          br(), br(),
                          hr(),
                          h4("Export selected surveys"),
                          p("Download survey-level data for the surveys in your selection."),
                          uiOutput("export_ui"),
             ),
             mainPanel(width = 9, class = "main-panel",
                       h3("Explore FISHub surveys"),
                       p("Query surveys and add matching rows to your selection. Use Catch and Effort Data to filter and inspect effort or catch records."),
                       uiOutput("query_summary"),
                       DTOutput("survey_table"),
                       br(),
                       uiOutput("add_survey_buttons"),
                       
             )
           )
  ),
  tabPanel("Catch and Effort Data", value = "view_data",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h4("Selected surveys"),
                          uiOutput("data_selected_summary"),
                          hr(),
                          h4("Filters for data and analyses"),
                          selectInput("effort_gear_filter", "Gear type", choices = c("All gear types" = ""), multiple = TRUE),
                          selectizeInput("effort_species_filter", "Species", choices = NULL, multiple = TRUE,
                                         options = list(placeholder = "All species in selected surveys; type to search")),
                          p(class = "mfa-note", "These filters apply to every analysis and to effort/catch data viewed or exported here. Species filtering excludes efforts without catches of the selected species; consider this when interpreting CPUE and other effort-based results."),
                          actionButton("clear_effort_filters", "Clear filters", width = "100%"),
                          hr(),
                          selectInput("data_type", "Data to view", c("Effort data" = "Efforts", "Catch data" = "Catch")),
                          actionButton("load_data", "Load data", class = "btn-primary", width = "100%"),
                          br(), br(),
                          uiOutput("data_export_ui")
             ),
             mainPanel(width = 9, class = "main-panel",
                       h3("Survey effort and catch data"),
                       p("Choose a data type and load records for the selected surveys. Large queries may take time."),
                       uiOutput("data_summary"),
                       DTOutput("data_table")
             )
           )
  ),
  tabPanel("Analyses", value = "analyses",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h4("Selected surveys"),
                          uiOutput("selected_summary"),
                          hr(),
                          h4("Analysis options"),
                          uiOutput("active_effort_filters"),
                          selectInput("analysis", "Analysis", analyses),
                          uiOutput("analysis_help"),
                          actionButton("show_function_help", "Full MFA function help", icon = icon("circle-help"), width = "100%"),
                          conditionalPanel("input.analysis == 'CPUE_byGear' || input.analysis == 'lengthFreqByEffort' || input.analysis == 'ageLengthSummary'",
                                           selectInput("output_type", "Output", c("Table", "Figure", "RawData"))
                          ),
                          conditionalPanel("input.analysis == 'catchSummary'",
                                           checkboxInput("custom_legal", "Specify a minimum legal size", FALSE),
                                           conditionalPanel("input.custom_legal",
                                                            textInput("legal_species", "Species", placeholder = "e.g., Walleye"),
                                                            numericInput("legal_size", "Minimum size (inches)", value = NA, min = 0, step = 0.5),
                                                            p(class = "mfa-note", "Only minimum size limits are supported. More complex legal-size summaries must be completed manually outside MFA.")
                                           )
                          ),
                          actionButton("run_analysis", "Run analysis", class = "btn-primary", width = "100%"),
                          br(), br(),
                          uiOutput("analysis_download_ui"),
             ),
             mainPanel(width = 9, class = "main-panel",
                       uiOutput("analysis_results")
             )
           )
  )
)

server <- function(input, output, session) {
  con <- reactiveVal(NULL)
  connection_error <- reactiveVal(NULL)
  observeEvent(TRUE, {
    tryCatch(con(MFA::FISHub_connect()), error = function(e) connection_error(conditionMessage(e)))
  }, once = TRUE)
  session$onSessionEnded(function() {
    db <- isolate(con())
    if (!is.null(db) && DBI::dbIsValid(db)) DBI::dbDisconnect(db)
  })
  
  
  output$mfa_version <- renderText({
    paste0("MFA package version: ", as.character(utils::packageVersion("MFA")))
  })
  
  observeEvent(input$show_function_help, {
    function_name <- input$analysis
    req(function_name)
    showModal(modalDialog(
      title = paste0("MFA function help: ", function_name, "()"),
      div(style = "max-height: 70vh; overflow-y: auto;", get_function_help_html(function_name)),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$analysis_help <- renderUI({
    req(input$analysis)
    tagList(tags$h5("About this analysis"), help_panel(input$analysis))
  })
  
  queried <- reactiveVal(NULL)
  selected_ids <- reactiveVal(NULL)
  selection_purpose <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)
  analysis_error <- reactiveVal("")
  analysis_kind <- reactiveVal(NULL)
  viewed_data <- reactiveVal(NULL)
  viewed_type <- reactiveVal(NULL)
  
  # All species for survey discovery; species actually recorded in the
  # selected surveys for effort/catch filtering. Read directly from FISHub
  # instead of running a full catch summary just to populate a dropdown.
  observeEvent(con(), {
    db <- con()
    req(db)
    tryCatch({
      all_species <- dplyr::tbl(db, "SpeciesStrain") %>%
        dplyr::distinct(Species) %>% dplyr::collect()
      all_species <- sort(unique(stats::na.omit(trimws(all_species$Species))))
      updateSelectizeInput(session, "species", choices = all_species,
                           selected = isolate(input$species), server = TRUE)
    }, error = function(e) {
      showNotification(paste("Could not load species list:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })
  
  observeEvent(list(con(), selected_ids()), {
    db <- con()
    ids <- selected_ids()
    if (is.null(db) || !length(ids)) {
      updateSelectizeInput(session, "effort_species_filter", choices = character(0),
                           selected = character(0), server = TRUE)
      return()
    }
    tryCatch({
      # Identify species recorded in any of the catch sources for the
      # selected surveys, including inch-group and scale-envelope records.
      modules <- dplyr::tbl(db, "SurveyEffort") %>%
        dplyr::filter(SurveyId %in% !!ids) %>%
        dplyr::select(ModuleId) %>% dplyr::distinct() %>%
        dplyr::inner_join(dplyr::tbl(db, "ModuleData") %>%
                            dplyr::select(ModuleId, ModuleDataId), by = "ModuleId") %>%
        dplyr::select(ModuleDataId) %>% dplyr::distinct()
      catch_tables <- c("ModuleDataCatchBySpecies", "ModuleDataCatchSampleByInchGroup",
                        "ModuleDataScaleEnvelope")
      strain_ids <- unique(unlist(lapply(catch_tables, function(table_name) {
        dplyr::tbl(db, table_name) %>%
          dplyr::select(ModuleDataId, SpeciesStrainId) %>%
          dplyr::semi_join(modules, by = "ModuleDataId") %>%
          dplyr::distinct(SpeciesStrainId) %>% dplyr::collect() %>%
          dplyr::pull(SpeciesStrainId)
      }), use.names = FALSE))
      selected_species <- if (length(strain_ids)) {
        dplyr::tbl(db, "SpeciesStrain") %>%
          dplyr::filter(SpeciesStrainId %in% !!strain_ids) %>%
          dplyr::distinct(Species) %>% dplyr::collect() %>% dplyr::pull(Species)
      } else character(0)
      selected_species <- sort(unique(stats::na.omit(trimws(selected_species))))
      current <- isolate(input$effort_species_filter)
      updateSelectizeInput(session, "effort_species_filter",
                           choices = selected_species,
                           selected = intersect(current, selected_species), server = TRUE)
    }, error = function(e) {
      showNotification(paste("Could not load species for selected surveys:",
                             conditionMessage(e)), type = "error", duration = NULL)
    })
  }, ignoreInit = FALSE)
  
  # Keep the filter controls populated from known FISHub gear types and
  # species present in the current selection, without requiring a catch query.
  gear_choices <- c("ANGLING", "BACKSHK", "BOOMSHK", "Envelop", "EXPGNET", "GLGNET",
                    "LMFYKE", "LIMNO", "MNWTRAP", "OTHER", "PhotoPt", "PLNKNET",
                    "SEINE", "SETHKS", "SMFYKE", "SRGNET", "STRMSHK", "TOXSAM",
                    "TRAPNET", "TRAWL", "VGNET")
  observe({
    updateSelectInput(session, "effort_gear_filter", choices = gear_choices,
                      selected = isolate(input$effort_gear_filter))
  })
  observeEvent(input$clear_effort_filters, {
    updateSelectInput(session, "effort_gear_filter", selected = character(0))
    updateSelectizeInput(session, "effort_species_filter", selected = character(0))
  })
  active_filters <- reactive({
    list(gear = if (length(input$effort_gear_filter)) input$effort_gear_filter else NULL,
         species = if (length(input$effort_species_filter)) trimws(input$effort_species_filter[nzchar(trimws(input$effort_species_filter))]) else NULL)
  })
  output$active_effort_filters <- renderUI({
    f <- active_filters()
    if (!length(f$gear) && !length(f$species))
      return(p(class = "mfa-note", "Gear and species: all (change on Catch and Effort Data)."))
    tagList(
      p(class = "mfa-note", strong("Active data filters")),
      if (length(f$gear)) p(class = "mfa-note", "Gear: ", paste(f$gear, collapse = ", ")),
      if (length(f$species)) p(class = "mfa-note", "Species: ", paste(f$species, collapse = ", ")),
      p(class = "mfa-note", "Change filters on Catch and Effort Data.")
    )
  })
  
  # Capture messages for temporary notifications and warnings for modal dialogs.
  capture_mfa <- function(expr) {
    notes <- character()
    result <- withCallingHandlers(expr,
                                  warning = function(w) { notes <<- c(notes, paste("WARNING:", conditionMessage(w))); invokeRestart("muffleWarning") },
                                  message = function(m) { notes <<- c(notes, conditionMessage(m)); invokeRestart("muffleMessage") }
    )
    list(data = result,
         notes = paste(unique(notes[!startsWith(notes, "WARNING:")]), collapse = "\n\n"),
         warnings = unique(sub("^WARNING: ", "", notes[startsWith(notes, "WARNING:")])))
  }
  
  show_warnings <- function(warnings, title = "MFA data warnings") {
    warnings <- unique(warnings[nzchar(warnings)])
    if (!length(warnings)) return(invisible(NULL))
    showModal(modalDialog(
      title = title,
      tagList(
        tags$p(tags$strong("Development notice: "), "MFA and this application are still in development. Be alert for potential errors and notify the MFA team if you find any."),
        tags$hr(),
        tags$div(class = "mfa-warning-list",
                 lapply(seq_along(warnings), function(i) tags$div(
                   class = "mfa-warning-item",
                   tags$p(style = "white-space: pre-wrap; margin: 0;", warnings[[i]])
                 ))
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "m"
    ))
  }
  
  show_messages <- function(messages) {
    messages <- unique(trimws(messages[nzchar(trimws(messages))]))
    for (msg in messages) {
      showNotification(msg, type = "message", duration = 5, closeButton = TRUE)
    }
  }
  
  survey_links <- function(ids) {
    tags$div(style = "display: flex; flex-wrap: wrap; gap: 6px; max-height: 220px; overflow-y: auto;",
             lapply(ids, function(id) {
               tags$a(
                 as.character(id), " \u00d7",
                 href = "#",
                 title = paste("Remove survey", id),
                 class = "mfa-survey-chip",
                 onclick = sprintf("Shiny.setInputValue('remove_survey_id', '%s', {priority: 'event'}); return false;", id)
               )
             })
    )
  }
  
  observeEvent(input$query, {
    req(con())
    tryCatch({
      args <- list(con = con(), QueryType = "Survey",
                   MDNRID = parse_csv(input$mdnrid), WaterBodyName = parse_csv(input$water),
                   FMU = if (length(input$fmu)) input$fmu else NULL,
                   WaterTypeAbbr = if (length(input$water_type)) input$water_type else NULL,
                   SurveyId = parse_csv(input$survey_id, TRUE),
                   SurveyPurpose = if (length(input$purpose)) input$purpose else NULL,
                   Year = parse_csv(input$year, TRUE),
                   GearType = if (length(input$gear)) input$gear else NULL,
                   Species = if (length(input$species)) input$species else NULL)
      result <- capture_mfa(do.call(MFA::FISH_query, args))
      queried(result$data)
      show_messages(result$notes)
      show_warnings(result$warnings, "Survey query warnings")
    }, error = function(e) {
      queried(NULL)
      showNotification(paste("Query failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })
  
  output$query_summary <- renderUI({
    dat <- queried()
    if (is.null(dat)) return(p("Run a query to see matching records."))
    tags$p(tags$strong(format(nrow(dat), big.mark = ",")), " rows; ",
           tags$strong(length(unique(dat$SurveyId))), " unique surveys")
  })
  output$survey_table <- renderDT({
    req(queried())
    datatable(queried(), selection = list(mode = "multiple", selected = NULL),
              filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  }, server = TRUE)
  output$add_survey_buttons <- renderUI({
    dat <- queried()
    if (is.null(dat) || !nrow(dat)) return(NULL)
    tagList(
      actionButton("use_selected", "Add selected surveys", class = "btn-primary"),
      actionButton("use_all", "Add all matching surveys")
    )
  })
  
  # Selection persists across separate queries, including different waterbodies.
  add_selection <- function(ids) {
    ids <- unique(ids[!is.na(ids)])
    if (!length(ids)) return(showNotification("No surveys selected.", type = "warning"))
    selected_ids(sort(unique(c(isolate(selected_ids()), ids))))
    selection_purpose(NULL) # Mixed survey purposes must not inherit a query-specific filter.
    analysis_result(NULL)
    analysis_kind(NULL)
    analysis_error("")
    showNotification(paste(length(ids), "survey IDs added to selection."), type = "message")
  }
  observeEvent(input$use_selected, {
    req(queried())
    rows <- input$survey_table_rows_selected
    if (!length(rows)) return(showNotification("Select one or more rows first.", type = "warning"))
    add_selection(queried()$SurveyId[rows])
  })
  observeEvent(input$use_all, {
    req(queried())
    add_selection(queried()$SurveyId)
  })
  observeEvent(input$remove_survey_id, {
    ids <- selected_ids()
    if (!length(ids)) return()
    selected_ids(setdiff(ids, suppressWarnings(as.numeric(input$remove_survey_id))))
    analysis_result(NULL)
    analysis_kind(NULL)
  })
  observeEvent(input$clear_selection, {
    selected_ids(NULL)
    selection_purpose(NULL)
    analysis_result(NULL)
    analysis_kind(NULL)
    analysis_error("")
  })
  output$basket_summary <- renderUI({
    ids <- selected_ids()
    if (!length(ids)) return(p("No surveys added yet."))
    tagList(p(strong(length(ids)), " surveys selected (click a number to remove):"),
            survey_links(ids))
  })
  output$data_selected_summary <- renderUI({
    ids <- selected_ids()
    if (!length(ids)) return(p("No surveys selected. Add surveys on Browse Surveys."))
    tagList(p(strong(length(ids)), " selected surveys:"), survey_links(ids))
  })
  output$selected_summary <- renderUI({
    ids <- selected_ids()
    if (!length(ids)) p("No surveys selected. Return to Browse Surveys to add surveys.")
    else tagList(p(strong(length(ids)), " selected surveys:"), survey_links(ids))
  })
  observeEvent(selected_ids(), {
    viewed_data(NULL)
    viewed_type(NULL)
  }, ignoreInit = TRUE)
  observeEvent(input$reset_query, {
    updateTextInput(session, "mdnrid", value = "")
    updateTextInput(session, "water", value = "")
    updateTextInput(session, "survey_id", value = "")
    updateTextInput(session, "year", value = "")
    updateSelectizeInput(session, "species", selected = character(0))
    for (id in c("fmu", "water_type", "purpose", "gear"))
      updateSelectInput(session, id, selected = character(0))
    queried(NULL)
  })
  
  # Cache keys include filters so a change cannot reuse an unfiltered query.
  cache <- new.env(parent = emptyenv())
  fetch_data <- function(db, type, ids, purpose, gear = NULL, species = NULL, legal_sizes = NULL) {
    key <- paste(type, paste(sort(ids), collapse = ","), purpose %||% "",
                 paste(sort(gear), collapse = ","), paste(sort(species), collapse = ","),
                 paste(names(legal_sizes), legal_sizes, collapse = ","), sep = "|")
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache))
    args <- list(con = db, QueryType = type, SurveyId = ids, SurveyPurpose = purpose,
                 GearType = gear, Species = species, Special_Legal_Sizes = legal_sizes)
    val <- capture_mfa(do.call(MFA::FISH_query, args))
    assign(key, val, envir = cache)
    val
  }
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # A new filter selection invalidates previously viewed or analyzed results.
  observeEvent(active_filters(), {
    viewed_data(NULL)
    viewed_type(NULL)
    analysis_result(NULL)
    analysis_kind(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$load_data, {
    req(con())
    ids <- selected_ids()
    if (!length(ids)) {
      showNotification("Select surveys on Browse Surveys first.", type = "message", duration = 5)
      return()
    }
    viewed_data(NULL)
    viewed_type(NULL)
    withProgress(message = "Loading survey data", value = 0, {
      tryCatch({
        type <- input$data_type
        filters <- active_filters()
        result <- fetch_data(con(), type, ids, NULL, filters$gear, filters$species)
        incProgress(1)
        viewed_data(result$data)
        viewed_type(type)
        show_messages(result$notes)
        show_warnings(result$warnings, paste(type, "data warnings"))
      }, error = function(e) showNotification(paste("Data query failed:", conditionMessage(e)), type = "error", duration = NULL))
    })
  })
  output$data_summary <- renderUI({
    dat <- viewed_data()
    if (is.null(dat)) return(NULL)
    p(strong(format(nrow(dat), big.mark = ",")), " ", viewed_type(), " records")
  })
  output$data_table <- renderDT({
    req(viewed_data())
    datatable(viewed_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  }, server = TRUE)
  output$data_export_ui <- renderUI({
    if (is.null(viewed_data())) return(NULL)
    downloadButton("export_viewed_data", paste("Download", viewed_type(), "CSV"))
  })
  output$export_viewed_data <- downloadHandler(
    filename = function() paste0("MFA_", viewed_type(), "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(viewed_data())
      write.csv(viewed_data(), file, row.names = FALSE, na = "")
    }
  )
  
  observeEvent(input$analysis, {
    choices <- switch(input$analysis,
                      "CPUE_byGear" = c("Table", "Figure"),
                      "lengthFreqByEffort" = c("Table", "Figure"),
                      "ageLengthSummary" = c("Table", "Figure", "RawData"),
                      c("Table"))
    updateSelectInput(session, "output_type", choices = choices, selected = "Table")
  })
  
  observeEvent(list(input$custom_legal, input$legal_species, input$legal_size), {
    analysis_result(NULL)
    analysis_kind(NULL)
    analysis_error("")
  }, ignoreInit = TRUE)
  
  observeEvent(input$run_analysis, {
    req(con(), selected_ids())
    analysis_result(NULL)
    analysis_kind(NULL)
    analysis_error("")
    withProgress(message = "Running MFA analysis", value = 0, {
      tryCatch({
        ids <- selected_ids()
        purpose <- selection_purpose()
        method <- input$analysis
        filters <- active_filters()
        needs_effort <- method %in% c("lengthFreqByEffort", "RSD", "ageLengthSummary")
        db <- con()
        legal_override <- NULL
        if (identical(method, "catchSummary") && isTRUE(input$custom_legal)) {
          species <- trimws(input$legal_species)
          size <- input$legal_size
          if (!nzchar(species)) stop("Enter a species for the minimum legal size override.")
          if (is.null(size) || length(size) != 1 || !is.finite(size) || size < 0)
            stop("Enter a valid minimum legal size in inches.")
          legal_override <- stats::setNames(as.numeric(size), species)
        }
        # A legal-size override changes N_legal in FISH_query's catch output,
        # so it must be included in the query cache key and the query rerun.
        source <- fetch_data(db, if (needs_effort) "Efforts" else "Catch", ids, purpose,
                             filters$gear, filters$species, legal_sizes = legal_override)
        if (!is.null(legal_override)) {
          # FISH_query matches overrides to the exact FISHub Species name.
          # Do not silently present default-size results if the name differs.
          matched <- any(!is.na(source$data$Species) &
                           source$data$Species == names(legal_override))
          if (!matched) {
            stop(paste0("The legal-size species '", names(legal_override),
                        "' was not found in the returned catch data. Check the exact species name and active species filter."))
          }
        }
        incProgress(.5)
        if (!nrow(source$data)) stop("No matching data were returned for these surveys.")
        output_type <- if (method %in% c("CPUE_byGear", "lengthFreqByEffort", "ageLengthSummary")) input$output_type else "Table"
        result <- capture_mfa(switch(method,
                                     "catchSummary" = MFA::catchSummary(source$data),
                                     "CPUE_byGear" = MFA::CPUE_byGear(source$data, OutputType = output_type),
                                     "detectionByYear" = MFA::detectionByYear(source$data),
                                     "lengthFreqByEffort" = MFA::lengthFreqByEffort(db, source$data, OutputType = output_type),
                                     "RSD" = MFA::RSD(MFA::lengthFreqByEffort(db, source$data, OutputType = "Table")),
                                     "ageLengthSummary" = MFA::ageLengthSummary(db, source$data, OutputType = output_type)
        ))
        analysis_result(result$data)
        analysis_kind(if (inherits(result$data, "ggplot")) "figure" else "table")
        show_messages(c(source$notes, result$notes))
        show_warnings(c(source$warnings, result$warnings), "Analysis warnings")
        incProgress(.5)
      }, error = function(e) analysis_error(conditionMessage(e)))
    })
    if (nzchar(analysis_error())) {
      showNotification(paste("Analysis failed:", analysis_error()), type = "error", duration = NULL)
    } else if (!is.null(analysis_result())) {
      showNotification("Analysis complete", type = "message", duration = 4, closeButton = TRUE)
    }
  })
  output$analysis_results <- renderUI({
    if (identical(analysis_kind(), "figure") && !is.null(analysis_result())) {
      plotOutput("analysis_plot", height = "520px")
    } else if (identical(analysis_kind(), "table") && !is.null(analysis_result())) {
      DTOutput("analysis_table")
    } else {
      NULL
    }
  })
  output$analysis_plot <- renderPlot({
    req(identical(analysis_kind(), "figure"), analysis_result())
    print(analysis_result())
  })
  output$analysis_table <- renderDT({
    req(identical(analysis_kind(), "table"), analysis_result())
    datatable(analysis_result(), options = list(pageLength = 15, scrollX = TRUE), filter = "top")
  })
  output$export_ui <- renderUI({
    if (!length(selected_ids())) return(p("Add surveys to enable exports."))
    tags$div(class = "mfa-export",
             downloadButton("export_survey", "Survey CSV")
    )
  })
  make_export <- function(type) {
    downloadHandler(
      filename = function() paste0("MFA_", type, "_", Sys.Date(), ".csv"),
      content = function(file) {
        ids <- isolate(selected_ids())
        req(length(ids), con())
        result <- capture_mfa(MFA::FISH_query(con(), QueryType = type, SurveyId = ids))
        show_warnings(result$warnings, paste(type, "export warnings"))
        show_messages(result$notes)
        write.csv(result$data, file, row.names = FALSE, na = "")
      }
    )
  }
  output$export_survey <- make_export("Survey")
  output$analysis_download_ui <- renderUI({
    kind <- analysis_kind()
    if (is.null(analysis_result())) return(NULL)
    if (identical(kind, "figure")) downloadButton("download_plot", "Download figure PNG")
    else if (identical(kind, "table")) downloadButton("download_analysis", "Download analysis CSV")
    else NULL
  })
  output$download_analysis <- downloadHandler(
    filename = function() paste0("MFA_", input$analysis, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(identical(analysis_kind(), "table"), analysis_result())
      write.csv(analysis_result(), file, row.names = FALSE, na = "")
    }
  )
  output$download_plot <- downloadHandler(
    filename = function() paste0("MFA_", input$analysis, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(identical(analysis_kind(), "figure"), analysis_result())
      ggplot2::ggsave(file, plot = analysis_result(), width = 12, height = 7, dpi = 180)
    }
  )
}

shinyApp(ui, server)
