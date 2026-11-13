# MFA Shiny application - install in inst/shiny/app.R
suppressPackageStartupMessages({
  library(shiny)
  library(MFA)
  library(DT)
  library(DBI)
})

# Preflight check: runs while app.R is being loaded, BEFORE Shiny starts
# serving the app or opens its browser window.
message("Testing connection")
.mfa_preflight_connection <- tryCatch(
  MFA::FISHub_connect(),
  error = function(e) {
    stop(
      "Cannot connect to FISHub. Connect to the State of Michigan network ",
      "or establish a VPN connection, then try again.\n",
      "Connection details: ", conditionMessage(e),
      call. = FALSE
    )
  }
)
if (!isTRUE(tryCatch(DBI::dbIsValid(.mfa_preflight_connection),
                     error = function(e) FALSE))) {
  stop("FISHub connection test failed. Connect to the State of Michigan network ",
       "or establish a VPN connection, then try again.", call. = FALSE)
}
DBI::dbDisconnect(.mfa_preflight_connection)
rm(.mfa_preflight_connection)
message("FISHub connection established")

categories <- c("Creel Census", "Discretionary Survey", "Fish Community", "General Survey", "Limnology", "Management Evaluation", "Population Estimate", "Population Reduction", "Recruitment Evaluation", "Recruitment v Evaluation", "Research Project", "Special Study", "Species Evaluation", "Status & Trends", "Stocking Evaluation")
purposes <- c("Creel Survey", "Fish Community", "Fish Kill Investigation", "General Survey", "Habitat Evaluation", "Limnology", "Population Estimate", "Population Reduction", "Population Reduction Evaluation", "Recruitment Evaluation", "Regulations Evaluation", "Research Project", "Research Section Study", "Special Study", "Species Evaluation", "Status & Trends", "Stocking Evaluation")
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

# Convert Rd documentation to a readable, responsive help page.
# Rd2txt avoids the layout tables in Rd2HTML; we then render each section
# separately so prose is not displayed as an indented console transcript.
get_function_help_html <- function(function_name) {
  tryCatch({
    rd <- get_function_rd(function_name)
    if (is.null(rd)) {
      return(tags$p("Function documentation not found. Run devtools::document() to generate man/*.Rd files."))
    }
    
    lines <- local({
      old <- options(width = 76)
      on.exit(options(old), add = TRUE)
      capture.output(tools::Rd2txt(rd, package = "MFA"))
    })
    lines <- gsub(".\\x08", "", lines, perl = TRUE)
    lines <- sub("[[:space:]]+$", "", lines)
    
    # Section names come from Rd2txt, rather than hardcoding one function.
    section_names <- c("Description", "Usage", "Arguments", "Details",
                       "Value", "Note", "Notes", "References", "See Also",
                       "Examples", "Author", "Authors", "Format", "Source")
    clean <- trimws(lines)
    is_heading <- clean %in% paste0(section_names, ":")
    starts <- which(is_heading)
    if (!length(starts)) stop("No documentation sections found")
    
    # Reflow text paragraphs. Rd2txt inserts fixed-width line breaks and
    # indentation that should not carry over to HTML prose.
    render_prose <- function(x) {
      x <- trimws(x)
      groups <- split(seq_along(x), cumsum(!nzchar(x)))
      parts <- lapply(groups, function(indices) {
        text <- x[indices]
        text <- text[nzchar(text)]
        if (!length(text)) return(NULL)
        paragraph <- paste(text, collapse = " ")
        # Rd2txt leaves LaTeX equations as literal text. Render them with
        # MathJax instead of displaying the backslash commands.
        if (grepl("\\\\(frac|times|sum|sqrt|left|right|geq|leq)", paragraph)) {
          return(tags$div(
            class = "mfa-help-equation",
            HTML(paste0("\\[", paragraph, "\\]"))
          ))
        }
        tags$p(paragraph)
      })
      tagList(parts)
    }
    
    render_arguments <- function(x) {
      # The first line of an Rd2txt argument starts at the left margin,
      # followed by indented continuation lines.
      x <- x[nzchar(trimws(x))]
      if (!length(x)) return(NULL)
      first <- grepl("^[^[:space:]].*?:[[:space:]]", x)
      groups <- cumsum(first)
      if (!any(first)) return(render_prose(x))
      tagList(lapply(split(x, groups), function(group) {
        line <- trimws(group[1])
        name <- sub(":.*$", "", line)
        description <- sub("^[^:]+:[[:space:]]*", "", line)
        rest <- if (length(group) > 1) trimws(group[-1]) else character()
        tags$div(class = "mfa-help-argument",
                 tags$code(class = "mfa-help-argname", name),
                 tags$div(class = "mfa-help-argdesc",
                          paste(c(description, rest[nzchar(rest)]), collapse = " ")))
      }))
    }
    
    sections <- lapply(seq_along(starts), function(i) {
      start <- starts[i]
      end <- if (i < length(starts)) starts[i + 1] - 1 else length(lines)
      heading <- sub(":$", "", clean[start])
      content <- if (end > start) lines[(start + 1):end] else character()
      content <- content[!is.na(content)]
      
      rendered <- if (heading == "Arguments") {
        render_arguments(content)
      } else if (heading %in% c("Usage", "Examples")) {
        # Keep code line breaks; remove only the common leading indentation.
        nonempty <- content[nzchar(trimws(content))]
        if (length(nonempty)) {
          indent <- min(nchar(sub("^(\\s*).*", "\\1", nonempty, perl = TRUE)))
          content <- substring(content, indent + 1)
        }
        tags$pre(class = "mfa-help-code", paste(content, collapse = "\n"))
      } else {
        render_prose(content)
      }
      tags$section(class = "mfa-help-section",
                   tags$h4(heading), rendered)
    })
    tags$div(class = "mfa-help-document", sections)
  }, error = function(e) {
    tags$p("Unable to display MFA help: ", conditionMessage(e))
  })
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
  
  header=tags$head(tags$style(HTML("
    :root { --dnr-primary1:#5d7f36; --dnr-graylight:#ceced8; --dnr-primary2:#314a24; --dnr-accent1:#903e1f; --dnr-graydark:#45454c; }
    .mfa-connection-success { color:#0D8252; font-weight:700; font-size:16px; }
    .mfa-connection-failure { color:#903e1f; font-weight:700; font-size:15px; }
    .mfa-warning-item { padding: 13px 0; border-bottom: 1px solid #cbdada; }
    .mfa-warning-item:first-child { padding-top: 0; }
    .mfa-warning-item:last-child { border-bottom: 0; padding-bottom: 0; }
    body { background:#f5f8f8; color:#203640; font-family:'Segoe UI',Arial,sans-serif; }
    .navbar-default { background:var(--dnr-primary2); border:0; border-radius:0; box-shadow:0 2px 8px rgba(20,45,62,.14); }
    .navbar-default .navbar-brand { color:#fff !important; font-weight:700; letter-spacing:.3px; }
    .navbar-default .navbar-nav > li > a { color:#d5e8e8 !important; font-weight:600; }
    .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover { color:var(--dnr-primary2) !important; background:var(--dnr-graylight) !important; }
    .navbar-default .navbar-nav > li > a:hover { color:#fff !important; background:#24495a !important; }
    .container-fluid { max-width:1680px; }
    .well { background:#fff; border:1px solid var(--dnr-graydark); border-radius:12px; box-shadow:0 2px 12px rgba(20,45,62,.045); padding:20px; }
    .main-panel { background:#fff; border:1px solid var(--dnr-graydark); border-radius:12px; padding:22px 26px; min-height:280px; box-shadow:0 2px 12px rgba(20,45,62,.045); }
    h2,h3,h4 { color:var(--dnr-primary2); font-weight:700; }
    h4 { font-size:17px; margin-top:12px; margin-bottom:14px; }
    .control-label { color:#314c58; font-weight:600; font-size:13px; }
    .form-control { border:1px solid #cbdada; border-radius:7px; box-shadow:none; }
    .form-control:focus { border-color:var(--dnr-primary1); box-shadow:0 0 0 3px rgba(39,124,120,.14); }
    .btn { border-radius:7px; font-weight:600; transition:background-color .15s,box-shadow .15s; }
    .btn-primary, .btn-primary:focus { background:var(--dnr-primary1); border-color:var(--dnr-primary1); color:#fff; }
    .btn-primary:hover, .btn-primary:active { background:var(--dnr-primary2); border-color:var(--dnr-primary2); }
    .btn-default { background:#fff; color:var(--dnr-primary1); border-color:#a9c8c5; }
    .btn-default:hover { background:#eaf5f2; color:var(--dnr-primary2); border-color:var(--dnr-primary1); }
    .btn:focus, a:focus { outline:2px solid var(--dnr-graylight); outline-offset:2px; }
    a { color:var(--dnr-primary1); } a:hover { color:var(--dnr-primary2); }
    hr { border-top-color:var(--dnr-graydark); margin:20px 0; }
    .mfa-survey-chip { display:inline-block; padding:5px 9px; border:1px solid #b5d5d0; border-radius:7px; background:#eff8f5; color:var(--dnr-primary1); text-decoration:none !important; font-weight:600; }
    .mfa-survey-chip:hover { background:var(--dnr-primary2); color:#fff; border-color:var(--dnr-primary2); }
    .mfa-survey-chip:focus { outline:2px solid var(--dnr-graylight); outline-offset:2px; }
    .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select { border:1px solid #cbdada; border-radius:6px; }
    table.dataTable thead th { background:#eaf4f2; color:var(--dnr-primary1); border-bottom:2px solid var(--dnr-primary1) !important; }
    table.dataTable tbody tr.selected { background-color:#d9eee8 !important; }
    .pagination > .active > a, .pagination > .active > a:hover { background:var(--dnr-primary1); border-color:var(--dnr-primary1); }
    .shiny-notification { border-left:4px solid var(--dnr-primary1); border-radius:8px; box-shadow:0 5px 20px rgba(20,45,62,.15); }
    .shiny-notification-message { background:#edf8f4; color:var(--dnr-primary2); }
    .shiny-notification-error { border-left-color:#b63b3b; }
    .modal-header { background:var(--dnr-primary2); color:#fff; border-radius:6px 6px 0 0; }
    .modal-title { color:#fff; }
    .mfa-intro { max-width:900px; margin:28px auto; background:#fff; border:1px solid var(--dnr-graydark); border-radius:14px; padding:32px 38px; box-shadow:0 2px 14px rgba(20,45,62,.05); }
    .mfa-intro h2 { margin-top:0; }
    .mfa-intro h4 { border-left:4px solid var(--dnr-primary1); padding-left:12px; margin-top:24px; }
    .mfa-intro-lead { color:#4c6670; font-size:16px; line-height:1.6; }
    .mfa-export .btn { margin:0 5px 8px 0; }
    .mfa-help-summary { color:#425d66; font-size:13px; line-height:1.55; margin:8px 0 12px; }
    /* Responsive function documentation: normal prose and code-only blocks. */
    .mfa-help-document { width:100%; min-width:0; max-width:100%; font-family:'Segoe UI',Arial,sans-serif; font-size:14px; line-height:1.6; overflow-wrap:anywhere; }
    .mfa-help-section { margin:0 0 22px; min-width:0; }
    .mfa-help-section h4 { margin:0 0 9px; padding-bottom:5px; border-bottom:1px solid #d9e2df; color:var(--dnr-primary2); font-size:16px; }
    .mfa-help-section p { margin:0 0 10px; }
    .mfa-help-equation { max-width:100%; padding:12px 4px; margin:10px 0 16px; text-align:center; overflow-x:auto; overflow-y:hidden; }
    .mfa-help-equation .MathJax { max-width:100%; }
    .mfa-help-equation mjx-container { max-width:100%; }
    .mfa-help-argument { margin:0 0 12px; display:grid; grid-template-columns:minmax(110px, 25%) minmax(0, 1fr); gap:12px; align-items:start; }
    .mfa-help-argname { font-weight:600; overflow-wrap:anywhere; white-space:normal; }
    .mfa-help-argdesc { min-width:0; overflow-wrap:anywhere; }
    .mfa-help-code { display:block; width:100%; max-width:100%; min-width:0; box-sizing:border-box; margin:0; padding:12px 14px; background:#f5f8f8; border:1px solid #d9e2df; border-radius:7px; color:#203640; font-family:Consolas,'Courier New',monospace; font-size:12px; line-height:1.55; white-space:pre-wrap !important; overflow-wrap:anywhere; word-break:normal; overflow-x:visible; }
    @media(max-width:600px) { .mfa-help-argument { display:block; } .mfa-help-argname { display:block; margin-bottom:3px; } }
    #reset_query { margin-top:8px; }
    /* High-priority actions and CSV exports share the accent color. */
    #run_analysis, #query, #load_data,
    #export_survey, #export_viewed_data, #download_analysis,
    #run_analysis:focus, #query:focus, #load_data:focus,
    #export_survey:focus, #export_viewed_data:focus, #download_analysis:focus {
      background:var(--dnr-accent1); border-color:var(--dnr-accent1); color:#fff;
    }
    #run_analysis:hover, #query:hover, #load_data:hover,
    #export_survey:hover, #export_viewed_data:hover, #download_analysis:hover {
      background:#702e17; border-color:#702e17; color:#fff;
    }
    #run_analysis { font-size:15px; padding:11px; }
    .mfa-note { font-size:12px; color:#4c6670; line-height:1.5; }
    @media(max-width:767px) { .main-panel { padding:16px; } .mfa-intro { margin:14px; padding:20px; } }
  ")),
                   # MathJax must typeset equations after the dynamically generated help modal is visible.
                   # Supports both MathJax 2 (used by many Shiny installations) and MathJax 3.
                   tags$script(HTML("
    $(document).on('shown.bs.modal', '.modal', function () {
      var modal = this;
      if (!modal.querySelector('.mfa-help-equation')) return;
      function typeset() {
        if (window.MathJax && MathJax.Hub && MathJax.Hub.Queue) {
          MathJax.Hub.Queue(['Typeset', MathJax.Hub, modal]);
        } else if (window.MathJax && MathJax.typesetPromise) {
          MathJax.typesetPromise([modal]);
        }
      }
      typeset();
      // Shiny may finish binding the dynamic content after the modal becomes visible.
      setTimeout(typeset, 250);
    });
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
                          selectInput("fmu", "FMU", c("All"="", "LS", "NLM", "CLM", "SLM", "NLH", "SLH", "LE"), multiple = TRUE),
                          selectizeInput("water", "Waterbody name(s)", choices = NULL, multiple = TRUE,
                                         options = list(placeholder = "Search or type partial waterbody names",
                                                        create = TRUE, persist = FALSE, plugins = list("remove_button"))),
                          selectizeInput("county", "County", choices = NULL, multiple = TRUE,
                                         options = list(placeholder = "All counties; type to search")),
                          textInput("mdnrid", "MDNRID(s)", placeholder = "Comma-separated"),
                          selectInput("water_type", "Water type", c("GL", "IL", "ISR"), multiple = TRUE),
                          selectizeInput("site_name", "Site name(s)", choices = NULL, multiple = TRUE,
                                         options = list(placeholder = "Search or type partial site names",
                                                        create = TRUE, persist = FALSE, plugins = list("remove_button"))),
                          textInput("survey_id", "Survey ID(s)", placeholder = "Comma-separated"),
                          selectInput("category", "Survey category", categories, multiple = TRUE),
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
                          selectInput("data_type", "Data to view", c("Effort data" = "Efforts", "Effort summary" = "EffortSummary", "Catch data" = "Catch")),
                          conditionalPanel("input.data_type == 'EffortSummary'",
                                           checkboxInput("aggregate_effort", "Aggregate across surveys", FALSE),
                                           p(class = "mfa-note", "When checked, effort is summarized across surveys by gear type and measurement unit.")),
                          actionButton("load_data", "Load data", class = "btn-primary", width = "100%"),
                          br(), br(),
                          uiOutput("data_export_ui")
             ),
             mainPanel(width = 9, class = "main-panel",
                       h3("Survey effort and catch data"),
                       p("Choose effort records, catch records, or an effort summary for the selected surveys. Large queries may take time."),
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
                          actionButton("show_function_help", "Full MFA function help", icon = icon("question-circle"), width = "100%"),
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
  network_message <- paste(
    "Cannot connect to FISHub. Connect to the State of Michigan network",
    "or establish a VPN connection, then try again."
  )
  connect_fishub <- function() {
    db <- con()
    if (!is.null(db) && isTRUE(tryCatch(DBI::dbIsValid(db), error = function(e) FALSE)))
      return(TRUE)
    con(NULL)
    tryCatch({
      db <- MFA::FISHub_connect()
      if (!DBI::dbIsValid(db)) stop("Connection is not valid.")
      con(db)
      connection_error(NULL)
      TRUE
    }, error = function(e) {
      connection_error(conditionMessage(e))
      FALSE
    })
  }
  # Large, persistent connection warning for later actions. Users can
  # connect to VPN and retry without restarting the application.
  notify_connection <- function() {
    showModal(modalDialog(
      title = "FISHub connection unavailable",
      tags$p(network_message),
      if (!is.null(isolate(connection_error())) && nzchar(isolate(connection_error())))
        tags$p(class = "mfa-note", "Connection details: ",
               isolate(connection_error())),
      easyClose = TRUE,
      footer = modalButton("Close"), size = "m"
    ))
  }
  
  # The database preflight runs before the Shiny window opens. Once the
  # session starts, connect for this session and show only the development notice.
  observeEvent(TRUE, {
    if (!connect_fishub()) {
      notify_connection()
      return()
    }
    showModal(modalDialog(
      title = "MFA development notice",
      tags$h4("Development notice"),
      tags$p("MFA and this Shiny application are still in development. ",
             "Results may contain bugs or data errors. Review outputs carefully ",
             "and report suspected problems to Lucas Nathan (nathanl@michigan.gov)."),
      tags$hr(),
      tags$p("Select 'I acknowledge' to continue using the app."),
      easyClose = FALSE,
      footer = actionButton("acknowledge_development", "I acknowledge",
                            class = "btn-primary"),
      size = "m"
    ))
  }, once = TRUE)
  observeEvent(input$acknowledge_development, {
    removeModal()
  })
  session$onSessionEnded(function() {
    db <- isolate(con())
    if (!is.null(db) && DBI::dbIsValid(db)) DBI::dbDisconnect(db)
  })
  
  
  output$connection_status <- renderUI({
    db <- con()
    if (is.null(db)) {
      tags$p(class = "mfa-note", style = "color:#903e1f; font-weight:600;",
             "FISHub is not connected. Connect to the network or VPN before querying.")
    } else {
      NULL
    }
  })
  
  output$mfa_version <- renderText({
    paste0("MFA package version: ", as.character(utils::packageVersion("MFA")))
  })
  
  observeEvent(input$show_function_help, {
    function_name <- input$analysis
    req(function_name)
    showModal(modalDialog(
      title = paste0("MFA function help: ", function_name, "()"),
      shiny::withMathJax(get_function_help_html(function_name)),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$analysis_help <- renderUI({
    req(input$analysis)
    tagList(tags$h5("About this analysis"), help_panel(input$analysis))
  })
  
  queried <- reactiveVal(NULL)
  selected_ids <- reactiveVal(NULL)
  selection_category <- reactiveVal(NULL)
  selection_purpose <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)
  analysis_error <- reactiveVal("")
  analysis_kind <- reactiveVal(NULL)
  viewed_data <- reactiveVal(NULL)
  viewed_type <- reactiveVal(NULL)
  
  pending_browse_species <- reactiveVal(NULL)
  
  # Lightweight active-waterbody metadata, collected once per connection.
  # Names are suggestions only; typed partial names are also valid filters.
  waterbody_lookup <- reactive({
    db <- con()
    req(db)
    dplyr::tbl(db, "WaterBody") %>%
      dplyr::filter(IsActive == TRUE) %>%
      dplyr::left_join(
        dplyr::tbl(db, "EntityOrganizationalUnit") %>%
          dplyr::select(EntityOrganizationalUnitId, UnitReportId),
        by = "EntityOrganizationalUnitId"
      ) %>%
      dplyr::mutate(FMU = dplyr::case_when(
        UnitReportId == "NH  " ~ "NLH",
        UnitReportId == "SM  " ~ "SLM",
        UnitReportId == "SH  " ~ "SLH",
        UnitReportId == "ER  " ~ "LE",
        UnitReportId == "CM  " ~ "CLM",
        UnitReportId == "NM  " ~ "NLM",
        UnitReportId == "ES  " ~ "LS",
        UnitReportId == "WS  " ~ "LS",
        TRUE ~ UnitReportId
      )) %>%
      dplyr::select(MDNRID, WaterBodyName, FMU) %>%
      dplyr::distinct() %>%
      dplyr::collect()
  })

  observeEvent(list(waterbody_lookup(), input$fmu), {
    tryCatch({
      wb <- waterbody_lookup()
      if (length(input$fmu)) {
        wb <- wb[!is.na(wb$FMU) & wb$FMU %in% input$fmu, , drop = FALSE]
      }
      names <- sort(unique(wb$WaterBodyName[!is.na(wb$WaterBodyName) &
                                                nzchar(trimws(wb$WaterBodyName))]))
      # Preserve typed entries even when they aren't in the suggestion list.
      current <- isolate(input$water)
      updateSelectizeInput(session, "water", choices = unique(c(names, current)),
                           selected = current, server = TRUE)
    }, error = function(e) {
      showNotification(paste("Could not load waterbody names:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })

  # SiteName is available in FISH_query's Survey output. Use that supported
  # interface rather than assuming which underlying FISHub table stores it.
  # Populate when a waterbody name or MDNRID is specified; avoid a broad
  # unfiltered survey query just to fill a dropdown.
  observeEvent(list(input$water, input$mdnrid, input$fmu), {
    db <- con()
    req(db)
    water <- input$water
    mdnrids <- tryCatch(parse_csv(input$mdnrid, numeric = TRUE),
                        error = function(e) NULL)
    if (!length(water) && !length(mdnrids)) {
      updateSelectizeInput(session, "site_name",
                           choices = isolate(input$site_name),
                           selected = isolate(input$site_name), server = TRUE)
      return()
    }
    tryCatch({
      survey_args <- list(con = db, QueryType = "Survey",
                          WaterBodyName = if (length(water)) water else NULL,
                          MDNRID = mdnrids,
                          FMU = if (length(input$fmu)) input$fmu else NULL)
      survey_data <- suppressMessages(suppressWarnings(
        do.call(MFA::FISH_query, survey_args)
      ))
      if (!"SiteName" %in% names(survey_data))
        stop("SiteName was not found in the FISH_query Survey output.")
      sites <- sort(unique(as.character(survey_data$SiteName)))
      sites <- sites[!is.na(sites) & nzchar(trimws(sites))]
      current <- isolate(input$site_name)
      updateSelectizeInput(session, "site_name", choices = unique(c(sites, current)),
                           selected = current, server = TRUE)
    }, error = function(e) {
      showNotification(paste("Could not load site name suggestions:", conditionMessage(e)),
                       type = "warning", duration = 8)
    })
  })

  # Populate county choices from the County lookup table.
  observeEvent(con(), {
    db <- con()
    req(db)
    tryCatch({
      counties <- dplyr::tbl(db, "County") %>%
        dplyr::distinct(CountyName) %>%
        dplyr::collect() %>%
        dplyr::pull(CountyName)
      counties <- sort(unique(counties[!is.na(counties) & nzchar(trimws(counties))]))
      updateSelectizeInput(session, "county", choices = counties,
                           selected = isolate(input$county), server = TRUE)
    }, error = function(e) {
      showNotification(paste("Could not load counties:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })

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
      pending <- isolate(pending_browse_species())
      # When adding surveys, carry forward the Browse Surveys species filter.
      # Only retain species that actually occur in the accumulated selection.
      wanted <- if (is.null(pending)) current else pending
      updateSelectizeInput(session, "effort_species_filter",
                           choices = selected_species,
                           selected = intersect(wanted, selected_species), server = TRUE)
      pending_browse_species(NULL)
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
    if (!connect_fishub()) {
      notify_connection()
      return()
    }
    tryCatch({
      mdnrids <- parse_csv(input$mdnrid, numeric = TRUE)
      # FISH_query already accepts MDNRID. Resolve county to active MDNRIDs
      # so this filter does not require a new CountyName argument in MFA.
      if (length(input$county)) {
        county_ids <- dplyr::tbl(con(), "WaterBody") %>%
          dplyr::filter(IsActive == TRUE) %>%
          dplyr::inner_join(
            dplyr::tbl(con(), "County") %>%
              dplyr::filter(CountyName %in% !!input$county) %>%
              dplyr::select(CountyId),
            by = "CountyId"
          ) %>%
          dplyr::distinct(MDNRID) %>%
          dplyr::collect() %>%
          dplyr::pull(MDNRID)
        mdnrids <- if (length(mdnrids)) intersect(mdnrids, county_ids) else county_ids
        if (!length(mdnrids)) stop("No waterbodies match the selected county and MDNRID filters.")
      }
      args <- list(con = con(), QueryType = "Survey",
                   MDNRID = mdnrids,
                   WaterBodyName = if (length(input$water)) input$water else NULL,
                   SiteName = if (length(input$site_name)) input$site_name else NULL,
                   FMU = if (length(input$fmu)) input$fmu else NULL,
                   WaterTypeAbbr = if (length(input$water_type)) input$water_type else NULL,
                   SurveyId = parse_csv(input$survey_id, TRUE),
                   SurveyCategory = if (length(input$category)) input$category else NULL,
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
    # Apply discovery filters to subsequent effort/catch viewing and analyses.
    browse_gears <- isolate(input$gear)
    browse_species <- isolate(input$species)
    pending_browse_species(if (length(browse_species)) browse_species else character(0))
    updateSelectInput(session, "effort_gear_filter",
                      selected = if (length(browse_gears)) browse_gears else character(0))
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
    updateSelectizeInput(session, "water", selected = character(0))
    updateSelectizeInput(session, "county", selected = character(0))
    updateTextInput(session, "mdnrid", value = "")
    updateSelectizeInput(session, "site_name", selected = character(0))
    updateTextInput(session, "survey_id", value = "")
    updateTextInput(session, "year", value = "")
    updateSelectizeInput(session, "species", selected = character(0))
    for (id in c("fmu", "water_type", "category", "purpose", "gear"))
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
  
  # Aggregation changes require reloading the displayed summary.
  observeEvent(list(input$data_type, input$aggregate_effort), {
    viewed_data(NULL)
    viewed_type(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$load_data, {
    if (!connect_fishub()) {
      notify_connection()
      return()
    }
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
        source_type <- if (identical(type, "EffortSummary")) "Efforts" else type
        source <- fetch_data(con(), source_type, ids, NULL, filters$gear, filters$species)
        if (identical(type, "EffortSummary")) {
          summary <- capture_mfa(MFA::effortSummary(
            source$data, Aggregate = isTRUE(input$aggregate_effort)
          ))
          displayed <- summary$data
          label <- "Effort summary"
          notes <- c(source$notes, summary$notes)
          warnings <- c(source$warnings, summary$warnings)
        } else {
          displayed <- source$data
          label <- type
          notes <- source$notes
          warnings <- source$warnings
        }
        incProgress(1)
        viewed_data(displayed)
        viewed_type(label)
        show_messages(notes)
        show_warnings(warnings, paste(label, "data warnings"))
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
    filename = function() paste0("MFA_", gsub(" ", "_", viewed_type()), "_", Sys.Date(), ".csv"),
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
  
  # Effort-based MFA functions query additional tables by module/survey,
  # so the species filter must also be applied to their final outputs.
  filter_analysis_species <- function(result, species) {
    if (!length(species)) return(result)
    if (inherits(result, "ggplot")) {
      if (!is.data.frame(result$data) || !"Species" %in% names(result$data))
        stop("Cannot apply the species filter to this analysis figure.")
      result$data <- result$data[!is.na(result$data$Species) &
                                   result$data$Species %in% species, , drop = FALSE]
      if (!nrow(result$data)) stop("No results for the selected species.")
      return(result)
    }
    if (!is.data.frame(result) || !"Species" %in% names(result))
      stop("Cannot apply the species filter: analysis output has no Species column.")
    result <- result[!is.na(result$Species) & result$Species %in% species, , drop = FALSE]
    if (!nrow(result)) stop("No results for the selected species.")
    result
  }

  observeEvent(input$run_analysis, {
    if (!length(selected_ids())) {
      showNotification("No surveys loaded. Go to Browse Surveys, run a query, and add at least one survey before running an analysis.",
                       type = "warning", duration = 8)
      return()
    }
    if (!connect_fishub()) {
      notify_connection()
      return()
    }
    analysis_result(NULL)
    analysis_kind(NULL)
    analysis_error("")
    withProgress(message = "Running MFA analysis", value = 0, {
      tryCatch({
        ids <- selected_ids()
        category <- selection_category()
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
        source <- fetch_data(db, if (needs_effort) "Efforts" else "Catch",
                             ids = ids, purpose = purpose,
                             gear = filters$gear, species = filters$species,
                             legal_sizes = legal_override)
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
                                     "RSD" = {
                                       length_data <- MFA::lengthFreqByEffort(db, source$data, OutputType = "Table")
                                       if (length(filters$species)) {
                                         length_data <- filter_analysis_species(length_data, filters$species)
                                       }
                                       MFA::RSD(length_data)
                                     },
                                     "ageLengthSummary" = MFA::ageLengthSummary(db, source$data, OutputType = output_type)
        ))
        # These functions can return species beyond the FISH_query effort filter.
        if (length(filters$species) && method %in% c("lengthFreqByEffort", "RSD", "ageLengthSummary")) {
          result$data <- filter_analysis_species(result$data, filters$species)
        }
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
