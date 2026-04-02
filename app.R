# =====================================================
# WBL Parenthood Pillar — Analytics and Simulation Tool
# Women, Business and the Law (World Bank)
# Author: Aieshwarya Davis
# =====================================================

# ---- Load Required Libraries ----
library(shiny)
library(plotly)
library(dplyr)
library(readxl)
library(ggplot2)
library(tibble)
library(tidyr)

# ---- Load Input Data ----

# WBL Reform Hero sheet — country-level parenthood scores and changes
reform_data <- read_excel("data/WBL Simulator workbook.xlsx", sheet = "Reform Hero")

# GDP per capita (current USD) filtered to 2010 onwards
gdp_data <- read_excel("data/GDP per capita data.xlsx") %>%
  filter(Year >= 2010,
         Indicator == "GDP per capita (current US$)") %>%
  select(Economy, Year, GDP_per_capita = Value)

# Female labor force participation rate filtered to 2010 onwards
labor_data <- read_excel("data/Labor force participation.xlsx") %>%
  filter(Year >= 2010,
         grepl("Labor force participation rate, female", Indicator)) %>%
  select(Economy, Year, Labor_participation = Value)

# =====================================================
# HELPER FUNCTIONS
# =====================================================

# ---- Percentile Label Generator ----
# Generates performance level labels based on the number of distinct ranks
# Handles edge cases for 1-5 ranks explicitly; generalizes beyond 5
generate_percentile_labels <- function(max_rank) {
  if (max_rank == 1) {
    return("100th percentile")
  } else if (max_rank == 2) {
    return(c("Upper 50th percentile", "Lower 50th percentile"))
  } else if (max_rank == 3) {
    return(c("Upper 33rd percentile", "Middle 33rd percentile", "Lower 33rd percentile"))
  } else if (max_rank == 4) {
    return(c("Upper 25th percentile", "Upper-middle 25th percentile",
             "Lower-middle 25th percentile", "Lower 25th percentile"))
  } else if (max_rank == 5) {
    return(c("Top 20th percentile", "Upper 20th percentile", "Middle 20th percentile",
             "Lower 20th percentile", "Bottom 20th percentile"))
  } else {
    # For more than 5 ranks, compute percentage share and label accordingly
    ordinals <- c("1st", "2nd", "3rd", paste0(4:max_rank, "th"))
    percentage <- round(100/max_rank, 0)
    labels <- character(max_rank)
    for(i in 1:max_rank) {
      if(i == 1) {
        labels[i] <- paste0("Top ", percentage, "th percentile")
      } else if(i == max_rank) {
        labels[i] <- paste0("Bottom ", percentage, "th percentile")
      } else {
        labels[i] <- paste0(ordinals[i], " ", percentage, "th percentile")
      }
    }
    return(labels)
  }
}

# ---- Percentile Label Lookup ----
# Returns the label for a specific rank given the total number of ranks
get_percentile_label <- function(rank, max_rank) {
  labels <- generate_percentile_labels(max_rank)
  return(labels[rank])
}

# =====================================================
# USER INTERFACE
# =====================================================

ui <- fluidPage(
  
  # ---- Google Fonts and Font Awesome Icons ----
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Andes:wght@300;400;500;700&family=Open+Sans:wght@300;400;500;600;700&display=swap');
      @import url('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css');

      /* ---- Base body styling ---- */
      body {
        font-family: 'Open Sans', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
        background-color: #f7f7f7;
        color: #212529;
      }

      /* ---- World Bank color palette as CSS variables ---- */
      :root {
        --wb-blue: #0071BC;
        --wb-dark-blue: #003A5D;
        --wb-light-blue: #009FDA;
        --wb-navy: #002244;
        --wb-teal: #00A0B0;
        --wb-orange: #F05023;
        --wb-red: #D12800;
        --wb-yellow: #FFD700;
        --wb-green: #7CB342;
        --wb-purple: #872175;
        --wb-gray: #6C757D;
        --wb-light-gray: #F8F9FA;
      }

      /* ---- Main title banner ---- */
      .main-title-section {
        background: linear-gradient(135deg, var(--wb-dark-blue), var(--wb-blue));
        color: white;
        text-align: center;
        padding: 30px 20px;
        margin: -15px -15px 20px -15px;
        border-radius: 0;
        font-family: 'Andes', 'Open Sans', sans-serif;
        position: relative;
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }

      /* ---- Title icon and layout ---- */
      .title-with-icon {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 20px;
        margin-bottom: 10px;
      }

      .title-icon {
        font-size: 3rem;
        color: var(--wb-yellow);
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        animation: pulse 3s infinite;
      }

      /* ---- Pulse animation for title icon ---- */
      @keyframes pulse {
        0% { transform: scale(1); }
        50% { transform: scale(1.08); }
        100% { transform: scale(1); }
      }

      /* ---- Title text sizing ---- */
      .main-title-section h1 {
        font-size: 2.4em;
        margin: 0;
        font-weight: 400;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        font-family: 'Andes', sans-serif;
      }
      .main-title-section h2 {
        font-size: 1.5em;
        margin: 10px 0 8px 0;
        opacity: 0.95;
        font-weight: 500;
        color: var(--wb-yellow);
        font-family: 'Open Sans', sans-serif;
      }
      .main-title-section p {
        font-size: 0.95em;
        margin: 5px 0 0 0;
        opacity: 0.85;
        font-style: italic;
        font-family: 'Open Sans', sans-serif;
      }
      .main-title-section .catchy-phrase {
        font-size: 1.1em;
        margin: 12px 0 0 0;
        opacity: 0.9;
        font-weight: 300;
        font-family: 'Open Sans', sans-serif;
        color: var(--wb-light-blue);
      }

      /* ---- Decorative corner icons in title banner ---- */
      .title-decoration {
        position: absolute;
        top: 15px;
        right: 25px;
        font-size: 1.8rem;
        color: rgba(255, 215, 0, 0.4);
        transform: rotate(15deg);
      }
      .title-decoration-left {
        position: absolute;
        top: 20px;
        left: 25px;
        font-size: 1.5rem;
        color: rgba(255, 215, 0, 0.4);
        transform: rotate(-15deg);
      }

      /* ---- Sidebar panel styling ---- */
      .sidebar-container {
        background: linear-gradient(135deg, var(--wb-navy), var(--wb-dark-blue));
        color: white;
        padding: 25px 20px;
        border-radius: 12px;
        margin-bottom: 25px;
        font-family: 'Open Sans', sans-serif;
        box-shadow: 0 6px 12px rgba(0,0,0,0.15);
        border: 1px solid rgba(255,255,255,0.1);
      }

      .sidebar-container h4 {
        color: var(--wb-yellow);
        font-family: 'Andes', sans-serif;
        margin-top: 0;
        margin-bottom: 20px;
        font-weight: 500;
        font-size: 1.3em;
        border-bottom: 2px solid var(--wb-yellow);
        padding-bottom: 8px;
      }

      .sidebar-container .form-group {
        margin-bottom: 25px;
      }

      .sidebar-container label {
        color: white;
        font-family: 'Open Sans', sans-serif;
        font-weight: 600;
        font-size: 0.95em;
        margin-bottom: 8px;
        display: block;
      }

      /* ---- Country filter box within sidebar ---- */
      .filter-countries-container {
        background: rgba(255,255,255,0.1);
        border-radius: 10px;
        padding: 15px;
        margin-top: 10px;
        border: 1px solid rgba(255,255,255,0.2);
      }

      .filter-countries-title {
        color: var(--wb-yellow);
        font-weight: 600;
        margin-bottom: 15px;
        font-size: 1.1em;
        text-align: center;
      }

      /* ---- Select All / Clear All buttons ---- */
      .filter-buttons {
        display: flex;
        gap: 8px;
        margin-bottom: 15px;
        justify-content: center;
      }

      .filter-buttons button {
        border-radius: 8px !important;
        font-size: 12px !important;
        padding: 8px 16px !important;
        border: none !important;
        font-family: 'Open Sans', sans-serif !important;
        font-weight: 600 !important;
        transition: all 0.3s ease !important;
        cursor: pointer !important;
      }

      .select-all-btn {
        background-color: var(--wb-green) !important;
        color: white !important;
      }
      .select-all-btn:hover {
        background-color: #689F2E !important;
        transform: translateY(-2px) !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      }

      .clear-all-btn {
        background-color: var(--wb-orange) !important;
        color: white !important;
      }
      .clear-all-btn:hover {
        background-color: #D63E1A !important;
        transform: translateY(-2px) !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      }

      /* ---- Scrollable multi-select country list ---- */
      .scrollable-select {
        max-height: 300px;
        overflow-y: auto;
        border: 2px solid rgba(255,255,255,0.3);
        border-radius: 8px;
        background: rgba(255,255,255,0.05);
        padding: 3px;
      }

      /* Custom scrollbar styling */
      .scrollable-select::-webkit-scrollbar { width: 12px; }
      .scrollable-select::-webkit-scrollbar-track {
        background: rgba(255,255,255,0.1);
        border-radius: 6px;
        margin: 2px;
      }
      .scrollable-select::-webkit-scrollbar-thumb {
        background: linear-gradient(45deg, var(--wb-yellow), var(--wb-light-blue));
        border-radius: 6px;
        border: 2px solid rgba(255,255,255,0.1);
      }
      .scrollable-select::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(45deg, var(--wb-light-blue), var(--wb-yellow));
        border: 2px solid rgba(255,255,255,0.2);
      }

      /* Select element inside scrollable container */
      .scrollable-select select {
        border: none !important;
        background: transparent !important;
        color: white !important;
        font-family: 'Open Sans', sans-serif !important;
        font-size: 13px !important;
        width: 100% !important;
        height: auto !important;
        padding: 4px 8px !important;
        outline: none !important;
        box-shadow: none !important;
        overflow: visible !important;
        resize: none !important;
      }

      .scrollable-select select option {
        background: var(--wb-dark-blue) !important;
        color: white !important;
        padding: 10px 12px !important;
        border-bottom: 1px solid rgba(255,255,255,0.1) !important;
        font-weight: 400 !important;
        line-height: 1.4 !important;
      }

      .scrollable-select select option:checked,
      .scrollable-select select option:hover {
        background: linear-gradient(135deg, var(--wb-blue), var(--wb-light-blue)) !important;
        color: var(--wb-yellow) !important;
        font-weight: 600 !important;
      }

      /* ---- Main content box styling ---- */
      .content-box {
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        font-family: 'Open Sans', sans-serif;
        border-radius: 12px !important;
        background: white;
        border: 1px solid #e0e0e0;
        margin-bottom: 25px;
        overflow: hidden;
      }

      .content-box-header {
        border-top-left-radius: 12px !important;
        border-top-right-radius: 12px !important;
        background: linear-gradient(135deg, var(--wb-blue), var(--wb-light-blue));
        border-bottom: 2px solid var(--wb-dark-blue);
        padding: 18px 25px;
        font-weight: 600;
        color: white;
        font-family: 'Andes', sans-serif;
        font-size: 1.1em;
      }

      .content-box-body {
        border-bottom-left-radius: 12px !important;
        border-bottom-right-radius: 12px !important;
        padding: 25px;
      }

      /* ---- Country status card ---- */
      .status-box {
        background: linear-gradient(135deg, var(--wb-blue) 0%, var(--wb-dark-blue) 100%);
        color: white;
        padding: 25px;
        border-radius: 12px !important;
        margin-bottom: 25px;
        font-family: 'Open Sans', sans-serif;
        box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      }

      /* ---- Key metric display ---- */
      .metric {
        font-size: 28px;
        font-weight: 600;
        font-family: 'Andes', sans-serif;
        color: var(--wb-yellow);
      }

      /* ---- Scrollable chart container ---- */
      .scrollable-chart {
        height: 450px;
        overflow-x: auto;
        overflow-y: hidden;
        border: 2px solid var(--wb-light-blue);
        border-radius: 12px !important;
        background: var(--wb-light-gray);
      }

      .chart-container {
        min-width: 800px;
        height: 430px;
      }

      /* ---- Form input styling ---- */
      .form-control, .selectize-input, select {
        border-radius: 8px !important;
        border: 2px solid var(--wb-light-blue) !important;
        font-family: 'Open Sans', sans-serif !important;
        transition: all 0.3s ease !important;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: var(--wb-blue) !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 113, 188, 0.25) !important;
      }

      /* ---- Radio button label styling ---- */
      .radio input[type='radio'] { margin-right: 8px; }
      .radio label {
        font-weight: 500 !important;
        color: white !important;
      }

      /* ---- Correlation analysis box ---- */
      .correlation-box {
        background: linear-gradient(135deg, #C4553C 0%, #A8472F 100%);
        color: white;
        padding: 25px;
        border-radius: 12px !important;
        margin-bottom: 25px;
        font-family: 'Open Sans', sans-serif;
        box-shadow: 0 6px 12px rgba(0,0,0,0.15);
        border: 2px solid rgba(255,255,255,0.1);
      }

      .correlation-metric {
        font-size: 18px;
        font-weight: 600;
        font-family: 'Andes', sans-serif;
        margin: 8px 0;
        color: #FFE5B4;
      }

      .correlation-interpretation {
        font-size: 14px;
        font-style: italic;
        opacity: 0.95;
        margin-top: 12px;
        line-height: 1.4;
        color: rgba(255,255,255,0.95);
      }

      .correlation-coefficient {
        font-size: 12px;
        margin-top: 8px;
        opacity: 0.8;
        font-family: 'Open Sans', sans-serif;
        color: rgba(255,255,255,0.8);
      }

      /* ---- Tab navigation styling ---- */
      .nav-tabs {
        border-bottom: 3px solid var(--wb-blue);
        margin-bottom: 25px;
      }
      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0 !important;
        font-weight: 600 !important;
        font-family: 'Open Sans', sans-serif !important;
        color: var(--wb-dark-blue) !important;
        background-color: var(--wb-light-gray) !important;
        border: 2px solid var(--wb-light-blue) !important;
        border-bottom: none !important;
        margin-right: 5px !important;
        transition: all 0.3s ease !important;
      }
      .nav-tabs > li > a:hover {
        background-color: var(--wb-blue) !important;
        color: white !important;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: var(--wb-blue) !important;
        color: var(--wb-yellow) !important;
        border-color: var(--wb-blue) !important;
        border-bottom: 3px solid var(--wb-blue) !important;
      }

      /* ---- Plotly tooltip styling ---- */
      .plotly .hoverlayer .hovertext {
        font-family: 'Open Sans', sans-serif !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        border-radius: 8px !important;
        box-shadow: 0 6px 12px rgba(0,0,0,0.3) !important;
        border: 2px solid var(--wb-blue) !important;
      }

      /* ---- Verbatim text output panels ---- */
      pre {
        border-radius: 8px !important;
        background-color: var(--wb-light-gray) !important;
        border: 2px solid var(--wb-light-blue) !important;
        font-family: 'Open Sans', monospace !important;
        padding: 20px !important;
        line-height: 1.6 !important;
      }

      /* ---- Responsive adjustments for small screens ---- */
      @media (max-width: 768px) {
        .main-title-section h1 { font-size: 2em; }
        .main-title-section h2 { font-size: 1.3em; }
        .title-with-icon { flex-direction: column; gap: 10px; }
        .title-icon { font-size: 2.5rem; }
        .title-decoration, .title-decoration-left { display: none; }
        .filter-buttons { flex-direction: column; }
        .scrollable-select { max-height: 200px; }
      }

      /* ---- Horizontal section divider ---- */
      .section-divider {
        height: 3px;
        background: linear-gradient(90deg, var(--wb-blue), var(--wb-light-blue), var(--wb-blue));
        margin: 30px 0;
        border-radius: 2px;
      }

      /* ---- Percentile note box within sidebar ---- */
      .percentile-note {
        background: rgba(255,255,255,0.1);
        border-left: 4px solid var(--wb-yellow);
        padding: 8px 12px;
        margin: 10px 0;
        font-size: 11px;
        border-radius: 4px;
        color: rgba(255,255,255,0.9);
        font-style: italic;
      }
    "))
  ),
  
  # ---- Title Banner ----
  div(class = "main-title-section",
      div(class = "title-decoration", HTML("<i class='fas fa-chart-line'></i>")),
      div(class = "title-decoration-left", HTML("<i class='fas fa-balance-scale'></i>")),
      div(class = "title-with-icon",
          div(class = "title-icon", HTML("<i class='fas fa-female'></i>")),
          h1("Women, Business and the Law Analysis and Simulation Tool")
      ),
      h2("Parenthood Pillar"),
      p("Developed by Aieshwarya Davis"),
      div(class = "catchy-phrase", "Unlocking Economic Potential Through Legal Reform")
  ),
  
  # ---- Tab Layout ----
  tabsetPanel(
    id = "main_tabs",
    
    # ---- Tab 1: Country Comparison and Analytical Insights ----
    tabPanel(
      title = "Country Comparison & Analytical Insights",
      value = "comparison_tab",
      
      fluidRow(
        # Sidebar: country selection, comparison type, view type, country filter
        column(3,
               div(class = "sidebar-container",
                   h4(HTML("<i class='fas fa-cog'></i> Analysis Configuration")),
                   
                   # Country dropdown
                   selectInput("selected_country", "Select Country:",
                               choices = sort(unique(reform_data$Economy)),
                               selected = "India"),
                   
                   # Comparison group type
                   h4(HTML("<i class='fas fa-layer-group'></i> Comparison Type"), style = "margin-top: 25px;"),
                   radioButtons("comparison_type", "",
                                choices = list(
                                  "Peer Group (Region + Income)" = "peer",
                                  "Regional Comparison" = "region",
                                  "Income Group Comparison" = "income"
                                ),
                                selected = "peer"),
                   
                   # Top performers vs top reformers toggle
                   h4(HTML("<i class='fas fa-eye'></i> View Type"), style = "margin-top: 25px;"),
                   radioButtons("view_type", "",
                                choices = list("Top Performers" = "performers",
                                               "Top Reformers" = "reformers"),
                                selected = "performers"),
                   
                   # Scrollable country filter with select/clear all
                   div(class = "filter-countries-container",
                       div(class = "filter-countries-title",
                           HTML("<i class='fas fa-filter'></i> Filter Countries")),
                       div(class = "percentile-note",
                           HTML("<b>Note:</b> Percentiles represent performance levels, not country distributions. Countries with identical scores share the same percentile category.")),
                       div(class = "filter-buttons",
                           actionButton("select_all", "Select All", class = "select-all-btn"),
                           actionButton("deselect_all", "Clear All", class = "clear-all-btn")
                       ),
                       div(class = "scrollable-select",
                           selectInput("selected_countries",
                                       label = NULL,
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE,
                                       selectize = FALSE,
                                       size = 12)
                       )
                   )
               )
        ),
        
        # Main panel: status card and comparison bar chart
        column(9,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-info-circle'></i> Selected Country Status")),
                   div(class = "content-box-body",
                       div(class = "status-box",
                           uiOutput("country_status_card")
                       )
                   )
               ),
               
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-chart-bar'></i> "),
                       textOutput("chart_title", inline = TRUE)),
                   div(class = "content-box-body",
                       div(class = "scrollable-chart",
                           div(class = "chart-container",
                               plotlyOutput("comparison_chart", height = "430px")
                           )
                       ),
                       tags$p(style = "margin-top: 15px; color: #666; font-style: italic; font-family: 'Open Sans', sans-serif; text-align: center;",
                              HTML("Countries ordered by performance level &bull; Continuous blue line shows average &bull; Use scroll & zoom to explore"))
                   )
               )
        )
      ),
      
      div(class = "section-divider"),
      
      # GDP and labor force participation trend charts
      fluidRow(
        column(6,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-chart-line'></i> GDP per Capita Analysis (2010-2024)")),
                   div(class = "content-box-body",
                       div(style = "font-size: 12px; color: #666; font-style: italic; margin-top: -10px; margin-bottom: 15px; text-align: center;",
                           "Association between WBL Parenthood performance levels and GDP per capita"),
                       div(style = "text-align: center; margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6;",
                           radioButtons("gdp_chart_type",
                                        label = div(style = "font-weight: 600; color: #003A5D; margin-bottom: 8px;", "Chart View:"),
                                        choices = list(
                                          "Average GDP by Performance Level Over Time" = "trends",
                                          "GDP Growth Patterns by Performance Level" = "correlation"
                                        ),
                                        selected = "trends",
                                        inline = TRUE)
                       ),
                       plotlyOutput("gdp_trend_chart", height = "400px")
                   )
               )
        ),
        
        column(6,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-users'></i> Female Labor Force Participation Analysis (2010-2024)")),
                   div(class = "content-box-body",
                       div(style = "font-size: 12px; color: #666; font-style: italic; margin-top: -10px; margin-bottom: 15px; text-align: center;",
                           "Association between WBL Parenthood performance levels and female labor force participation"),
                       div(style = "text-align: center; margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6;",
                           radioButtons("labor_chart_type",
                                        label = div(style = "font-weight: 600; color: #003A5D; margin-bottom: 8px;", "Chart View:"),
                                        choices = list(
                                          "Average FLFP by Performance Level Over Time" = "trends",
                                          "FLFP Growth Patterns by Performance Level" = "correlation"
                                        ),
                                        selected = "trends",
                                        inline = TRUE)
                       ),
                       plotlyOutput("labor_trend_chart", height = "400px")
                   )
               )
        )
      ),
      
      # Statistical correlation panel
      fluidRow(
        column(12,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-calculator'></i> Statistical Correlation Analysis")),
                   div(class = "content-box-body",
                       div(class = "correlation-box",
                           uiOutput("enhanced_correlation_analysis")
                       )
                   )
               )
        )
      ),
      
      # Summary and context narrative panels
      fluidRow(
        column(6,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-list-ul'></i> Comparison Group Summary")),
                   div(class = "content-box-body",
                       verbatimTextOutput("peer_summary")
                   )
               )
        ),
        column(6,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-lightbulb'></i> Context & Insights")),
                   div(class = "content-box-body",
                       verbatimTextOutput("regional_context")
                   )
               )
        )
      )
    ),
    
    # ---- Tab 2: Policy Simulation (Placeholder) ----
    tabPanel(
      title = "Policy Simulation",
      value = "simulation_tab",
      
      fluidRow(
        column(3,
               div(class = "sidebar-container",
                   h4(HTML("<i class='fas fa-sliders-h'></i> Simulation Controls")),
                   div(style = "color: #FFD700; font-style: italic; line-height: 1.5;",
                       "Configuration options will be available here once the simulation features are implemented.")
               )
        ),
        
        column(9,
               div(style = "padding: 50px 30px; text-align: center;",
                   h2("Policy Simulation Tool", style = "color: #003A5D; margin-bottom: 20px; font-family: 'Andes', sans-serif;"),
                   p("Advanced simulation capabilities for policy impact analysis will be available here.",
                     style = "font-size: 1.2em; color: #6c757d; margin-bottom: 30px; font-family: 'Open Sans', sans-serif;"),
                   div(style = "background: linear-gradient(135deg, #f8f9fa, #e9ecef); padding: 30px; border-radius: 12px; border: 2px solid #009FDA;",
                       h4("Coming Soon Features:", style = "color: #003A5D; margin-bottom: 20px; font-family: 'Andes', sans-serif;"),
                       tags$ul(style = "text-align: left; color: #6c757d; max-width: 450px; margin: 0 auto; font-family: 'Open Sans', sans-serif; line-height: 1.8;",
                               tags$li("Policy impact modeling"),
                               tags$li("Economic outcome predictions"),
                               tags$li("Scenario analysis"),
                               tags$li("Comparative policy recommendations")
                       )
                   )
               )
        )
      )
    ),
    
    # ---- Tab 3: About ----
    tabPanel(
      title = "About",
      value = "about_tab",
      fluidRow(
        column(12,
               div(class = "content-box",
                   div(class = "content-box-header",
                       HTML("<i class='fas fa-info-circle'></i> About this tool")),
                   div(class = "content-box-body",
                       h3("Introduction", style = "color: #003A5D; font-family: 'Andes', sans-serif;"),
                       p("This proof-of-concept devised by Aieshwarya Davis turns Women, Business and the Law (WBL) – Parenthood data into an interactive analytics tool for policy dialogue. It lets users: (i) compare any country against region/income peers on 2024 performance or reform progress (2010–2024), (ii) see how outcomes move—via GDP per capita and female labor force participation trends—and (iii) view a compact correlation summary that links legal performance to economic indicators. The goal is to help WBL teams and counterparts prioritize reforms, communicate gaps, and track progress with clear, reproducible visuals.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       p("(Prototype data inputs: WBL parenthood scores and change, GDP per capita, and female labor participation, 2010–latest available.)", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h3("How to use the tool", style = "color: #003A5D; font-family: 'Andes', sans-serif; margin-top: 30px;"),
                       h4("Choose a country", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("Use the dropdown in 'Analysis Configuration.' The status card shows its 2010 score, 2024 score, and change.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h4("Set the comparison frame", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("Select Peer Group (Region + Income), Region, or Income Group to define the benchmark pool.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h4("Pick the lens", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       tags$ul(style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;",
                               tags$li(tags$b("Top Performers:"), " categorizes countries by 2024 Parenthood score performance levels."),
                               tags$li(tags$b("Top Reformers:"), " categorizes countries by score change performance levels (2010–latest).")
                       ),
                       
                       h4("Filter the countries shown", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("Use the scrollable multi-select in the sidebar to include/exclude countries in the bar chart. 'Select All' and 'Clear All' speed up exploration.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h4("Read the bar chart", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       tags$ul(style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;",
                               tags$li("Bars are ordered by performance level; percentile labels appear above each bar."),
                               tags$li("The horizontal blue line marks the group average (updates with your filter)."),
                               tags$li("Color highlights call out your selected country, top performers, and median performers.")
                       ),
                       
                       h4("Explore outcome trends", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("GDP per Capita Analysis and Female Labor Participation Analysis show:", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       tags$ul(style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;",
                               tags$li("the selected country (orange line) and"),
                               tags$li("performance level average series for context."),
                               tags$li("Toggle between trends over time and growth pattern analysis by performance level.")
                       ),
                       p("Hover to see values and period growth/change.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h4("Scan the correlation box", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("A compact summary shows the association between WBL performance levels and outcomes, plus performance-level growth/change snippets. Interpretation tip: a positive correlation means better performance levels are associated with better outcomes.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h4("Use the narrative panels", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("Comparison Group Summary: quick stats for the current view.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       p("Context & Insights: gaps to leaders/average to inform talking points.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h4("Understanding Performance Percentiles", style = "color: #0071BC; font-family: 'Open Sans', sans-serif;"),
                       p("Performance percentiles represent performance level categories, not country distribution percentiles. Countries with identical WBL scores are grouped into the same percentile category. For example, if there are 3 distinct performance levels among compared countries, they are labeled as 'Upper 33rd percentile', 'Middle 33rd percentile', and 'Lower 33rd percentile' regardless of how many countries are in each group.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;"),
                       
                       h3("Policy Simulation (Preview)", style = "color: #003A5D; font-family: 'Andes', sans-serif; margin-top: 30px;"),
                       p("The second tab outlines the forthcoming simulation controls for scenario testing (impact of legal changes on outcomes), which can be built using the LTGM and the existing Child Care Simulator from the ILO.", style = "font-family: 'Open Sans', sans-serif; line-height: 1.6;")
                   )
               )
        )
      )
    )
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ---- Sign formatting helper ----
  # Formats numeric change values with explicit +/- sign to one decimal place
  format_change <- function(x) {
    sprintf("%+0.1f", x)
  }
  
  # ---- Compound Annual Growth Rate (CAGR) helper ----
  # Returns NA for invalid inputs (missing values, zero start, zero time span)
  # Vectorized to handle column-level operations
  calculate_cagr <- function(start_value, end_value, years) {
    start_value <- as.numeric(start_value)
    end_value   <- as.numeric(end_value)
    years       <- as.numeric(years)
    
    result <- ifelse(
      is.na(start_value) | is.na(end_value) | start_value <= 0 | years <= 0,
      NA,
      ((end_value / start_value)^(1/years) - 1) * 100
    )
    return(result)
  }
  
  # ---- Selected country data ----
  # Filters reform_data to the currently selected country
  selected_country_data <- reactive({
    reform_data %>% filter(Economy == input$selected_country)
  })
  
  # ---- Comparison country pool ----
  # Builds the full peer group based on comparison type and view type
  # Assigns dense ranks and percentile labels to all countries in the group
  comparison_countries <- reactive({
    country_info <- selected_country_data()
    if(nrow(country_info) == 0) return(data.frame())
    
    # Filter to peer group based on comparison type selection
    if(input$comparison_type == "peer") {
      all_comparison <- reform_data %>%
        filter(Region == country_info$Region,
               `Income Group` == country_info$`Income Group`)
    } else if(input$comparison_type == "region") {
      all_comparison <- reform_data %>%
        filter(Region == country_info$Region)
    } else {
      all_comparison <- reform_data %>%
        filter(`Income Group` == country_info$`Income Group`)
    }
    
    # Rank by 2024 score (performers) or score change (reformers)
    if(input$view_type == "performers") {
      all_comparison <- all_comparison %>%
        arrange(desc(`2024 PARENTHOOD SCORE`), desc(`Difference in score`)) %>%
        mutate(Rank = dense_rank(desc(`2024 PARENTHOOD SCORE`)))
    } else {
      all_comparison <- all_comparison %>%
        arrange(desc(`Difference in score`), desc(`2024 PARENTHOOD SCORE`)) %>%
        mutate(Rank = dense_rank(desc(`Difference in score`)))
    }
    
    # Compute percentile labels and highlight categories
    max_rank    <- max(all_comparison$Rank, na.rm = TRUE)
    median_rank <- median(all_comparison$Rank, na.rm = TRUE)
    
    all_comparison <- all_comparison %>%
      mutate(
        Max_Rank        = max_rank,
        Percentile_Label = get_percentile_label(Rank, max_rank),
        Highlight = case_when(
          Economy == input$selected_country ~ "Selected Country",
          Rank == 1                          ~ "Top Performers",
          Rank == median_rank                ~ "Median Performers",
          TRUE                               ~ "Other Countries"
        )
      )
    
    return(all_comparison)
  })
  
  # ---- Filtered rank data for trend charts ----
  # Returns rank and percentile label for currently selected countries only
  filtered_ranks_data <- reactive({
    filtered_countries <- input$selected_countries
    if(is.null(filtered_countries) || length(filtered_countries) == 0) return(NULL)
    
    comparison_data <- comparison_countries()
    
    rank_info <- comparison_data %>%
      filter(Economy %in% filtered_countries) %>%
      select(Economy, Rank, Percentile_Label)
    
    return(rank_info)
  })
  
  # ---- GDP trend data builder ----
  # Joins GDP data to filtered countries via rank info
  # Computes performance-level averages and CAGR alongside selected country series
  gdp_trend_data <- reactive({
    filtered_countries <- input$selected_countries
    selected_country   <- input$selected_country
    
    if(is.null(filtered_countries) || length(filtered_countries) == 0) return(NULL)
    
    rank_info <- filtered_ranks_data()
    if(is.null(rank_info)) return(NULL)
    
    # Join GDP data to filtered countries
    gdp_individual <- gdp_data %>%
      inner_join(rank_info, by = "Economy") %>%
      select(Country = Economy, Year, GDP_per_capita, Rank, Percentile_Label) %>%
      filter(!is.na(GDP_per_capita))
    
    if(nrow(gdp_individual) == 0) return(NULL)
    
    # Country count per rank for labeling
    rank_counts <- gdp_individual %>%
      group_by(Rank, Percentile_Label) %>%
      summarise(Country_Count = n_distinct(Country), .groups = "drop")
    
    # Performance-level average time series with CAGR
    gdp_rank_averages <- gdp_individual %>%
      group_by(Rank, Year) %>%
      summarise(GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE), .groups = "drop") %>%
      left_join(rank_counts, by = "Rank") %>%
      group_by(Rank) %>%
      arrange(Year) %>%
      summarise(
        Percentile_Label = first(Percentile_Label),
        Country_Count    = first(Country_Count),
        First_Year       = min(Year),
        Last_Year        = max(Year),
        First_Value      = GDP_per_capita[Year == min(Year)][1],
        Last_Value       = GDP_per_capita[Year == max(Year)][1],
        .groups = "drop"
      ) %>%
      mutate(
        CAGR    = calculate_cagr(First_Value, Last_Value, Last_Year - First_Year),
        Country = paste0(Percentile_Label, " Avg"),
        Type    = "Performance Level Average"
      ) %>%
      left_join(
        gdp_individual %>%
          group_by(Rank, Year) %>%
          summarise(GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE), .groups = "drop"),
        by = "Rank"
      )
    
    # Selected country individual series with CAGR
    gdp_selected <- gdp_individual %>%
      filter(Country == selected_country) %>%
      arrange(Year) %>%
      group_by(Country) %>%
      mutate(
        First_Year  = min(Year),
        Last_Year   = max(Year),
        First_Value = GDP_per_capita[Year == min(Year)][1],
        Last_Value  = GDP_per_capita[Year == max(Year)][1],
        CAGR          = calculate_cagr(First_Value, Last_Value, Last_Year - First_Year),
        Type          = "Selected Country",
        Country_Count = 1
      ) %>%
      ungroup()
    
    bind_rows(gdp_rank_averages, gdp_selected)
  })
  
  # ---- Labor trend data builder ----
  # Same structure as gdp_trend_data but for female labor force participation
  labor_trend_data <- reactive({
    filtered_countries <- input$selected_countries
    selected_country   <- input$selected_country
    
    if(is.null(filtered_countries) || length(filtered_countries) == 0) return(NULL)
    
    rank_info <- filtered_ranks_data()
    if(is.null(rank_info)) return(NULL)
    
    # Join labor data to filtered countries
    labor_individual <- labor_data %>%
      inner_join(rank_info, by = "Economy") %>%
      select(Country = Economy, Year, Labor_participation, Rank, Percentile_Label) %>%
      filter(!is.na(Labor_participation))
    
    if(nrow(labor_individual) == 0) return(NULL)
    
    # Country count per rank
    rank_counts <- labor_individual %>%
      group_by(Rank, Percentile_Label) %>%
      summarise(Country_Count = n_distinct(Country), .groups = "drop")
    
    # Performance-level average time series
    labor_rank_averages <- labor_individual %>%
      group_by(Rank, Year) %>%
      summarise(Labor_participation = mean(Labor_participation, na.rm = TRUE), .groups = "drop") %>%
      left_join(rank_counts, by = "Rank") %>%
      mutate(Country = paste0(Percentile_Label, " Avg"),
             Type    = "Performance Level Average")
    
    # Selected country individual series
    labor_selected <- labor_individual %>%
      filter(Country == selected_country) %>%
      mutate(Type          = "Selected Country",
             Country_Count = 1)
    
    bind_rows(labor_rank_averages, labor_selected)
  })
  
  # ---- Correlation analysis reactive ----
  # Computes Spearman correlations between WBL percentile scores and economic outcomes
  # Uses percentile scores where higher = better WBL performance (intuitive direction)
  # Returns correlation coefficients, ranges, growth summaries, and interpretive text
  enhanced_correlation_analysis <- reactive({
    
    gdp_data   <- gdp_trend_data()
    labor_data <- labor_trend_data()
    
    # Default results structure
    results <- list(
      gdp_correlation       = NA,
      gdp_interpretation    = "No data available",
      gdp_rank_changes      = "No data",
      gdp_correlation_range = "No data",
      labor_correlation       = NA,
      labor_interpretation    = "No data available",
      labor_rank_changes      = "No data",
      labor_correlation_range = "No data",
      overall_interpretation  = "Insufficient data for analysis"
    )
    
    # ---- GDP correlation ----
    if(!is.null(gdp_data)) {
      gdp_rank_data <- gdp_data %>%
        filter(Type == "Performance Level Average") %>%
        mutate(
          Max_Rank         = max(Rank, na.rm = TRUE),
          # Convert rank to percentile score: 100 = best performer, lower = worse
          Percentile_Score = ((Max_Rank - Rank + 1) / Max_Rank) * 100
        )
      
      if(nrow(gdp_rank_data) >= 3 && length(unique(gdp_rank_data$Percentile_Score)) >= 3) {
        
        # Compute per-year Spearman correlations between percentile score and GDP
        yearly_correlations <- gdp_rank_data %>%
          group_by(Year) %>%
          filter(n() >= 3) %>%
          summarise(
            yearly_corr = cor(Percentile_Score, GDP_per_capita, method = "spearman", use = "complete.obs"),
            .groups = "drop"
          ) %>%
          filter(!is.na(yearly_corr))
        
        if(nrow(yearly_correlations) >= 2) {
          mean_corr <- mean(yearly_correlations$yearly_corr, na.rm = TRUE)
          min_corr  <- min(yearly_correlations$yearly_corr, na.rm = TRUE)
          max_corr  <- max(yearly_correlations$yearly_corr, na.rm = TRUE)
          results$gdp_correlation       <- mean_corr
          results$gdp_correlation_range <- paste0("Range: ", round(min_corr, 3), " to ", round(max_corr, 3))
        } else {
          # Fall back to single-point correlation if insufficient yearly observations
          overall_corr <- cor(gdp_rank_data$Percentile_Score, gdp_rank_data$GDP_per_capita,
                              method = "spearman", use = "complete.obs")
          results$gdp_correlation       <- overall_corr
          results$gdp_correlation_range <- "Single timepoint analysis"
        }
        
        # CAGR by performance level for growth summary
        cagr_data <- gdp_rank_data %>%
          filter(!is.na(CAGR)) %>%
          distinct(Rank, CAGR, Country_Count, Percentile_Label, Percentile_Score)
        
        if(nrow(cagr_data) >= 3) {
          cagr_corr <- cor(cagr_data$Percentile_Score, cagr_data$CAGR, method = "spearman", use = "complete.obs")
          cagr_text <- paste(
            sapply(1:nrow(cagr_data), function(i) {
              paste0(cagr_data$Percentile_Label[i], " (", cagr_data$Country_Count[i], " countries): ",
                     format_change(cagr_data$CAGR[i]), "% CAGR")
            }),
            collapse = " | "
          )
          results$gdp_rank_changes <- paste0("Growth rates: ", cagr_text,
                                             " | Percentile-CAGR correlation: ", round(cagr_corr, 3))
        }
        
        # Interpret correlation direction and strength
        if(!is.na(results$gdp_correlation)) {
          strength <- if(abs(results$gdp_correlation) >= 0.7) "Strong" else if(abs(results$gdp_correlation) >= 0.4) "Moderate" else "Weak"
          
          if(results$gdp_correlation > 0) {
            results$gdp_interpretation <- paste0(strength, " positive relationship: better WBL Parenthood performance leads to higher GDP per capita")
          } else {
            results$gdp_interpretation <- paste0(strength, " negative relationship: better WBL Parenthood performance leads to lower GDP per capita (counterintuitive)")
          }
        }
      }
    }
    
    # ---- Labor force participation correlation ----
    if(!is.null(labor_data)) {
      labor_rank_data <- labor_data %>%
        filter(Type == "Performance Level Average") %>%
        mutate(
          Max_Rank         = max(Rank, na.rm = TRUE),
          Percentile_Score = ((Max_Rank - Rank + 1) / Max_Rank) * 100
        )
      
      if(nrow(labor_rank_data) >= 3 && length(unique(labor_rank_data$Percentile_Score)) >= 3) {
        
        # Per-year Spearman correlations
        yearly_correlations <- labor_rank_data %>%
          group_by(Year) %>%
          filter(n() >= 3) %>%
          summarise(
            yearly_corr = cor(Percentile_Score, Labor_participation, method = "spearman", use = "complete.obs"),
            .groups = "drop"
          ) %>%
          filter(!is.na(yearly_corr))
        
        if(nrow(yearly_correlations) >= 2) {
          mean_corr <- mean(yearly_correlations$yearly_corr, na.rm = TRUE)
          min_corr  <- min(yearly_correlations$yearly_corr, na.rm = TRUE)
          max_corr  <- max(yearly_correlations$yearly_corr, na.rm = TRUE)
          results$labor_correlation       <- mean_corr
          results$labor_correlation_range <- paste0("Range: ", round(min_corr, 3), " to ", round(max_corr, 3))
        } else {
          overall_corr <- cor(labor_rank_data$Percentile_Score, labor_rank_data$Labor_participation,
                              method = "spearman", use = "complete.obs")
          results$labor_correlation       <- overall_corr
          results$labor_correlation_range <- "Single timepoint analysis"
        }
        
        # Total change by performance level (2010 to 2024)
        labor_changes <- labor_rank_data %>%
          group_by(Rank) %>%
          arrange(Year) %>%
          summarise(
            Percentile_Label = first(Percentile_Label),
            Value_2010       = Labor_participation[Year == 2010][1],
            Value_2024       = Labor_participation[Year == 2024][1],
            Total_Change     = ifelse(!is.na(Value_2010) & !is.na(Value_2024), Value_2024 - Value_2010, NA),
            Country_Count    = first(Country_Count),
            .groups = "drop"
          ) %>%
          arrange(Rank) %>%
          filter(!is.na(Total_Change))
        
        if(nrow(labor_changes) > 0) {
          change_text <- paste(
            sapply(1:nrow(labor_changes), function(i) {
              paste0(labor_changes$Percentile_Label[i], " (", labor_changes$Country_Count[i], " countries): ",
                     format_change(labor_changes$Total_Change[i]), " pp")
            }),
            collapse = " | "
          )
          results$labor_rank_changes <- change_text
        }
        
        # Interpret correlation direction and strength
        if(!is.na(results$labor_correlation)) {
          strength <- if(abs(results$labor_correlation) >= 0.7) "Strong" else if(abs(results$labor_correlation) >= 0.4) "Moderate" else "Weak"
          
          if(results$labor_correlation > 0) {
            results$labor_interpretation <- paste0(strength, " positive relationship: better WBL Parenthood performance leads to higher female labor force participation")
          } else {
            results$labor_interpretation <- paste0(strength, " negative relationship: better WBL Parenthood performance leads to lower female labor force participation (counterintuitive)")
          }
        }
      }
    }
    
    # ---- Overall interpretation ----
    # Synthesizes GDP and labor correlations into a single policy-facing statement
    if(!is.na(results$gdp_correlation) || !is.na(results$labor_correlation)) {
      if(!is.na(results$gdp_correlation) && !is.na(results$labor_correlation)) {
        if(results$gdp_correlation > 0 && results$labor_correlation > 0) {
          if(abs(results$gdp_correlation) >= 0.4 && abs(results$labor_correlation) >= 0.4) {
            results$overall_interpretation <- "Strong evidence: Both economic indicators show positive relationships with better WBL Parenthood performance, consistent with the policy hypothesis that better legal frameworks support economic outcomes."
          } else if(abs(results$gdp_correlation) >= 0.4 || abs(results$labor_correlation) >= 0.4) {
            results$overall_interpretation <- "Moderate evidence: One indicator shows strong positive relationship with WBL performance, with supportive evidence from the other."
          } else {
            results$overall_interpretation <- "Weak but consistent evidence: Both indicators show the expected positive direction but with weak statistical associations."
          }
        } else if(results$gdp_correlation > 0 || results$labor_correlation > 0) {
          results$overall_interpretation <- "Mixed evidence: Economic outcomes show varied relationships with WBL Parenthood performance — one positive, one negative or inconclusive."
        } else {
          results$overall_interpretation <- "Counterintuitive evidence: Both indicators suggest negative relationships with WBL performance, requiring further investigation."
        }
      } else if(!is.na(results$gdp_correlation)) {
        results$overall_interpretation <- if(results$gdp_correlation > 0.3) {
          "GDP evidence supports policy hypothesis: Better WBL Parenthood performance shows positive statistical association with economic prosperity."
        } else if(results$gdp_correlation < -0.3) {
          "Counterintuitive GDP evidence: Better WBL performance associated with lower GDP — may indicate measurement or contextual factors."
        } else {
          "Limited GDP evidence: Weak statistical relationship between WBL Parenthood performance and economic outcomes."
        }
      } else if(!is.na(results$labor_correlation)) {
        results$overall_interpretation <- if(results$labor_correlation > 0.3) {
          "Labor evidence supports policy hypothesis: Better WBL Parenthood performance shows positive statistical association with women's workforce participation."
        } else if(results$labor_correlation < -0.3) {
          "Counterintuitive labor evidence: Better WBL performance associated with lower female participation — may indicate cultural or contextual factors."
        } else {
          "Limited labor evidence: Weak statistical relationship between WBL Parenthood performance and women's participation patterns."
        }
      }
    }
    
    return(results)
  })
  
  # ---- Update country filter choices ----
  # Rebuilds the scrollable multi-select whenever comparison group changes
  # Labels each country with its percentile level for context
  observe({
    comparison_data <- comparison_countries()
    
    comparison_data_sorted <- comparison_data %>%
      arrange(Rank) %>%
      mutate(label = paste0(Economy, " (", Percentile_Label, ")"))
    
    choices_list <- setNames(comparison_data_sorted$Economy, comparison_data_sorted$label)
    
    updateSelectInput(session, "selected_countries",
                      choices  = choices_list,
                      selected = comparison_data_sorted$Economy)
  })
  
  # ---- Select All button handler ----
  observeEvent(input$select_all, {
    comparison_data <- comparison_countries()
    updateSelectInput(session, "selected_countries", selected = comparison_data$Economy)
  })
  
  # ---- Clear All button handler ----
  observeEvent(input$deselect_all, {
    updateSelectInput(session, "selected_countries", selected = character(0))
  })
  
  # ---- Filtered comparison country set ----
  # Subsets the full comparison pool to only the countries currently selected in the filter
  filtered_comparison_countries <- reactive({
    comparison_data <- comparison_countries()
    
    if(is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      return(comparison_data[0,])
    }
    
    comparison_data %>%
      filter(Economy %in% input$selected_countries)
  })
  
  # ---- Selected country rank information ----
  # Extracts rank, total group size, and percentile label for the selected country
  selected_country_rank <- reactive({
    comparison_data <- comparison_countries()
    selected_info   <- comparison_data %>% filter(Economy == input$selected_country)
    
    if(nrow(selected_info) > 0) {
      list(
        rank             = selected_info$Rank[1],
        total            = nrow(comparison_data),
        max_rank         = max(comparison_data$Rank, na.rm = TRUE),
        percentile_label = selected_info$Percentile_Label[1]
      )
    } else {
      list(rank = NA, total = 0, max_rank = 0, percentile_label = "Unknown")
    }
  })
  
  # ---- Country status card ----
  # Renders the header card showing 2010 score, 2024 score, change, and performance level
  output$country_status_card <- renderUI({
    country_info <- selected_country_data()
    rank_info    <- selected_country_rank()
    
    comparison_desc <- switch(input$comparison_type,
                              "peer"   = paste0(country_info$Region, " (", country_info$`Income Group`, ")"),
                              "region" = country_info$Region,
                              "income" = country_info$`Income Group`
    )
    
    performance_text <- if(!is.na(rank_info$rank)) {
      paste0(rank_info$percentile_label, " (", rank_info$total, " countries)")
    } else {
      "Performance level: Not available"
    }
    
    score_change_formatted <- format_change(country_info$`Difference in score`)
    
    div(style = "font-family: 'Open Sans', sans-serif;",
        h3(paste0(country_info$Economy, " - ", performance_text),
           style = "margin-top: 0; font-family: 'Andes', sans-serif; color: #FFD700;"),
        p(paste("Comparison Group:", comparison_desc),
          style = "font-family: 'Open Sans', sans-serif; margin-bottom: 8px;"),
        p(paste("Region:", country_info$Region, "| Income Group:", country_info$`Income Group`),
          style = "font-family: 'Open Sans', sans-serif; margin-bottom: 20px;"),
        div(style = "display: flex; justify-content: space-between; margin-top: 15px;",
            div(
              div(class = "metric", country_info$`2024 PARENTHOOD SCORE`),
              div("2024 WBL Parenthood Score",
                  style = "font-family: 'Open Sans', sans-serif; font-size: 14px; margin-top: 5px;")
            ),
            div(
              div(class = "metric", score_change_formatted),
              div("Score Change (2010-2024)",
                  style = "font-family: 'Open Sans', sans-serif; font-size: 14px; margin-top: 5px;")
            ),
            div(
              div(class = "metric", country_info$`2010 PARENTHOOD SCORE`),
              div("2010 WBL Parenthood Score",
                  style = "font-family: 'Open Sans', sans-serif; font-size: 14px; margin-top: 5px;")
            )
        )
    )
  })
  
  # ---- Bar chart title ----
  # Dynamic title reflecting view type and comparison group
  output$chart_title <- renderText({
    country_info <- selected_country_data()
    
    view_text <- if(input$view_type == "performers") {
      "Countries by 2024 WBL Parenthood Pillar Performance Levels"
    } else {
      "Countries by Reform Progress Performance Levels (2010-2024)"
    }
    
    comparison_text <- switch(input$comparison_type,
                              "peer"   = paste0("Peer Group: ", country_info$Region, " (", country_info$`Income Group`, ")"),
                              "region" = paste0("Regional: ", country_info$Region),
                              "income" = paste0("Income Group: ", country_info$`Income Group`)
    )
    
    paste0(view_text, " - ", comparison_text, " - Filtered View")
  })
  
  # ---- Comparison bar chart ----
  # Bars colored by performance level; selected country highlighted in WB orange
  # Horizontal line marks the filtered group average
  # Performance level legend rendered as annotation below chart
  output$comparison_chart <- renderPlotly({
    chart_data <- filtered_comparison_countries()
    
    if(nrow(chart_data) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "No countries selected. Please select countries from the filter in the sidebar.",
                                   font = list(family = "Open Sans, sans-serif", size = 16)),
                      font = list(family = "Open Sans, sans-serif")))
    }
    
    # Set y variable and average based on view type
    if(input$view_type == "performers") {
      y_var     <- chart_data$`2024 PARENTHOOD SCORE`
      y_title   <- "2024 WBL Parenthood Score"
      avg_value <- mean(chart_data$`2024 PARENTHOOD SCORE`, na.rm = TRUE)
    } else {
      y_var     <- chart_data$`Difference in score`
      y_title   <- "Score Change (2010-2024)"
      avg_value <- mean(chart_data$`Difference in score`, na.rm = TRUE)
    }
    
    # Map performance levels to WB color palette
    unique_levels      <- chart_data %>% distinct(Rank, Percentile_Label) %>% arrange(Rank)
    level_colors       <- c("#003A5D", "#0071BC", "#009FDA", "#00A0B0", "#7CB342", "#872175")
    level_color_mapping <- setNames(level_colors[1:nrow(unique_levels)], unique_levels$Percentile_Label)
    
    # Selected country gets WB orange regardless of performance level
    chart_data <- chart_data %>%
      mutate(
        Bar_Color = case_when(
          Economy == input$selected_country ~ "#F05023",
          TRUE                              ~ level_color_mapping[Percentile_Label]
        )
      )
    
    # Order bars by rank then score within rank
    chart_data <- chart_data %>%
      arrange(Rank, desc(y_var)) %>%
      mutate(Display_Order  = row_number(),
             Country_Ordered = factor(Economy, levels = Economy))
    
    p <- plot_ly()
    
    # Add bars with hover tooltips
    p <- p %>%
      add_trace(
        data = chart_data,
        x    = ~Country_Ordered,
        y    = ~y_var,
        marker = list(color = chart_data$Bar_Color,
                      line  = list(color = "#002244", width = 1)),
        type = "bar",
        hovertemplate = paste0(
          "<b style='color: #ffffff; font-family: Open Sans, sans-serif;'>%{x}</b><br>",
          "<span style='color: #ecf0f1; font-family: Open Sans, sans-serif;'>", y_title, ": <b style='color: #ffffff;'>%{y}</b></span><br>",
          "<span style='color: #ecf0f1; font-family: Open Sans, sans-serif;'>Performance Level: <b style='color: #ffffff;'>", chart_data$Percentile_Label, "</b></span>",
          "<extra></extra>"
        ),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          title          = "",
          showgrid       = FALSE,
          showticklabels = TRUE,
          tickfont       = list(size = 11, family = "Open Sans, sans-serif", color = "#333333"),
          tickangle      = 45,
          categoryorder  = "array",
          categoryarray  = chart_data$Economy
        ),
        yaxis = list(
          title      = y_title,
          showgrid   = TRUE,
          titlefont  = list(family = "Open Sans, sans-serif", size = 12),
          tickfont   = list(family = "Open Sans, sans-serif", size = 10)
        ),
        showlegend   = FALSE,
        margin       = list(l = 70, r = 50, t = 60, b = 160),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font         = list(family = "Open Sans, sans-serif"),
        
        # Horizontal average line
        shapes = list(
          list(
            type  = "line",
            x0    = 0, x1 = 1, xref = "paper",
            y0    = avg_value, y1 = avg_value, yref = "y",
            line  = list(color = "#003A5D", width = 3)
          )
        )
      ) %>%
      # Performance level color key as annotation below chart
      add_annotations(
        x     = 0.02, y = -0.32,
        xref  = "paper", yref = "paper",
        text  = paste0(
          "<b style='font-family: Open Sans, sans-serif; font-size: 13px;'>Performance Levels:</b> ",
          paste(sapply(1:nrow(unique_levels), function(i) {
            paste0("<span style='color: ", level_colors[i], "; font-size: 14px;'>●</span> ", unique_levels$Percentile_Label[i])
          }), collapse = "  "),
          "  <span style='color: #F05023; font-size: 14px;'>●</span> Selected Country  <span style='color: #003A5D; font-size: 14px;'>—</span> Group Average"
        ),
        showarrow = FALSE,
        bgcolor   = "rgba(255,255,255,0.95)",
        bordercolor = "#dee2e6",
        borderwidth = 1, borderpad = 8,
        font      = list(size = 12, family = "Open Sans, sans-serif"),
        xanchor   = "left", yanchor = "top"
      ) %>%
      # Average line label
      add_annotations(
        x    = 0.02, xref = "paper",
        y    = avg_value + max(y_var) * 0.02,
        text = paste0("Average: ", round(avg_value, 1)),
        showarrow = FALSE,
        font      = list(size = 10, color = "#003A5D", family = "Open Sans, sans-serif", weight = "bold"),
        bgcolor   = "rgba(255,255,255,0.9)",
        bordercolor = "#003A5D", borderwidth = 1, borderpad = 3,
        yref    = "y", xanchor = "left"
      ) %>%
      config(
        displayModeBar            = TRUE,
        displaylogo               = FALSE,
        modeBarButtonsToRemove    = c("select2d", "lasso2d"),
        scrollZoom                = TRUE,
        doubleClick               = "reset"
      )
    
    return(p)
  })
  
  # ---- GDP trend chart ----
  # Two modes controlled by radio button:
  # "trends"      — line chart of GDP per capita over time by performance level
  # "correlation" — bar chart of Spearman correlation between time and GDP by performance level
  output$gdp_trend_chart <- renderPlotly({
    
    if(input$gdp_chart_type == "correlation") {
      
      filtered_countries <- input$selected_countries
      if(is.null(filtered_countries) || length(filtered_countries) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No GDP data available for correlation analysis",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      rank_info <- filtered_ranks_data()
      if(is.null(rank_info)) return(NULL)
      
      # Join GDP to filtered countries and compute per-level correlations
      gdp_individual <- gdp_data %>%
        inner_join(rank_info, by = "Economy") %>%
        select(Country = Economy, Year, GDP_per_capita, Rank, Percentile_Label) %>%
        filter(!is.na(GDP_per_capita))
      
      if(nrow(gdp_individual) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "Insufficient data for correlation analysis",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      # Spearman correlation between year and GDP within each performance level
      rank_correlations <- gdp_individual %>%
        group_by(Rank, Percentile_Label) %>%
        filter(n() >= 6) %>%
        summarise(
          correlation     = cor(Year, GDP_per_capita, method = "spearman", use = "complete.obs"),
          country_count   = n_distinct(Country),
          avg_gdp         = mean(GDP_per_capita, na.rm = TRUE),
          years_span      = max(Year) - min(Year) + 1,
          .groups = "drop"
        ) %>%
        filter(!is.na(correlation)) %>%
        arrange(Rank) %>%
        mutate(Percentile_Label = factor(Percentile_Label, levels = Percentile_Label))
      
      if(nrow(rank_correlations) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "Insufficient data points for performance level correlation analysis",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      rank_colors <- c("#003A5D", "#0071BC", "#009FDA", "#00A0B0", "#7CB342", "#872175", "#F05023", "#D12800")
      bar_colors  <- rank_colors[1:nrow(rank_correlations)]
      
      p <- plot_ly(
        data = rank_correlations,
        x    = ~Percentile_Label,
        y    = ~correlation,
        type = "bar",
        marker = list(color = bar_colors, line = list(color = "#002244", width = 1)),
        hovertemplate = paste0(
          "<b style='color: #ffffff; font-family: Open Sans, sans-serif;'>%{x}</b><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>GDP Time-Growth Correlation: %{y:.3f}</span><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Countries: ", rank_correlations$country_count, "</span><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Avg GDP: $", sprintf("%s", format(round(rank_correlations$avg_gdp), big.mark = ",")), "</span><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Data span: ", rank_correlations$years_span, " years</span>",
          "<extra></extra>"
        ),
        hoverlabel = list(bgcolor = "#003A5D", bordercolor = "#003A5D",
                          font = list(family = "Open Sans, sans-serif", size = 13, color = "white")),
        showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(
            title         = "WBL Parenthood Performance Level",
            titlefont     = list(family = "Open Sans, sans-serif"),
            tickfont      = list(family = "Open Sans, sans-serif"),
            tickangle     = 45,
            categoryorder = "array",
            categoryarray = levels(rank_correlations$Percentile_Label)
          ),
          yaxis = list(
            title     = "GDP Time-Growth Correlation (ρ)",
            titlefont = list(family = "Open Sans, sans-serif"),
            tickfont  = list(family = "Open Sans, sans-serif"),
            range     = c(-1, 1)
          ),
          margin        = list(l = 80, r = 50, t = 50, b = 150),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          font          = list(family = "Open Sans, sans-serif"),
          
          # Zero reference line
          shapes = list(
            list(type = "line",
                 x0 = -0.5, x1 = length(rank_correlations$Percentile_Label) - 0.5,
                 y0 = 0, y1 = 0,
                 line = list(color = "#6C757D", width = 2, dash = "dash"))
          ),
          
          # Interpretation note
          annotations = list(
            list(
              x = 0.02, y = -0.45, xref = "paper", yref = "paper",
              text = "<b>What this shows:</b> How strongly GDP grows over time for countries in each performance level.<br>Higher values = stronger growth patterns within that performance group.",
              showarrow = FALSE,
              bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#dee2e6",
              borderwidth = 1, borderpad = 8,
              font = list(size = 11, family = "Open Sans, sans-serif", color = "#495057"),
              xanchor = "left", yanchor = "top"
            )
          )
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d"), scrollZoom = TRUE)
      
      return(p)
      
    } else {
      
      # Trends view — line chart of GDP over time by performance level
      trend_data <- gdp_trend_data()
      
      if(is.null(trend_data) || nrow(trend_data) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No GDP data available for selected countries",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      # Build color mapping from performance levels
      comparison_data     <- comparison_countries()
      unique_levels       <- comparison_data %>% distinct(Rank, Percentile_Label) %>% arrange(Rank)
      level_colors        <- c("#003A5D", "#0071BC", "#009FDA", "#00A0B0", "#7CB342", "#872175")
      level_color_mapping <- setNames(level_colors[1:nrow(unique_levels)], unique_levels$Percentile_Label)
      
      # Color lookup: selected country = WB orange, others = performance level color
      get_color <- function(country_name, type) {
        if(type == "Selected Country") return("#F05023")
        for(label in names(level_color_mapping)) {
          if(grepl(label, country_name, fixed = TRUE)) return(level_color_mapping[label])
        }
        return("#003A5D")
      }
      
      p <- plot_ly()
      
      # Add one line per country or performance level average
      for(country in unique(trend_data$Country)) {
        country_data <- trend_data %>% filter(Country == country)
        country_type <- unique(country_data$Type)
        if(nrow(country_data) < 2) next
        
        cagr_value <- unique(country_data$CAGR)[1]
        cagr_text  <- if(!is.na(cagr_value)) format_change(cagr_value) else "N/A"
        
        line_color    <- get_color(country, country_type)
        country_count <- unique(country_data$Country_Count)[1]
        
        count_text <- if(country_type == "Selected Country") "" else
          paste0("<br><span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Countries in performance level: ", country_count, "</span>")
        
        p <- p %>%
          add_trace(
            data = country_data,
            x    = ~Year, y = ~GDP_per_capita,
            name = country, type = "scatter", mode = "lines+markers",
            line   = list(color = line_color, width = 2),
            marker = list(size = 4, color = line_color),
            hovertemplate = paste0(
              "<b style='color: #ffffff; font-family: Open Sans, sans-serif;'>%{fullData.name}</b><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Year: %{x}</span><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>GDP per capita: $%{y:,.0f}</span><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Type: ", country_type, "</span><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>CAGR: ", cagr_text, "%</span>",
              count_text, "<extra></extra>"
            ),
            hoverlabel = list(bgcolor = line_color, bordercolor = line_color,
                              font = list(family = "Open Sans, sans-serif", size = 13, color = "white")),
            showlegend = TRUE
          )
      }
      
      p <- p %>%
        layout(
          xaxis = list(title = "Year",
                       titlefont = list(family = "Open Sans, sans-serif"),
                       tickfont  = list(family = "Open Sans, sans-serif")),
          yaxis = list(title = "GDP per capita (USD)",
                       titlefont = list(family = "Open Sans, sans-serif"),
                       tickfont  = list(family = "Open Sans, sans-serif")),
          legend = list(orientation = "v", x = 1.02, y = 1,
                        font = list(family = "Open Sans, sans-serif", size = 10)),
          margin        = list(l = 60, r = 150, t = 50, b = 50),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          font          = list(family = "Open Sans, sans-serif")
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d"), scrollZoom = TRUE)
      
      return(p)
    }
  })
  
  # ---- Labor force participation trend chart ----
  # Same two-mode structure as GDP chart but for female labor force participation
  # "trends"      — line chart by performance level over time
  # "correlation" — bar chart of Spearman correlation between time and FLFP by level
  output$labor_trend_chart <- renderPlotly({
    
    if(input$labor_chart_type == "correlation") {
      
      filtered_countries <- input$selected_countries
      if(is.null(filtered_countries) || length(filtered_countries) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No labor data available for correlation analysis",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      rank_info <- filtered_ranks_data()
      if(is.null(rank_info)) return(NULL)
      
      # Join labor data and compute per-level correlations
      labor_individual <- labor_data %>%
        inner_join(rank_info, by = "Economy") %>%
        select(Country = Economy, Year, Labor_participation, Rank, Percentile_Label) %>%
        filter(!is.na(Labor_participation))
      
      if(nrow(labor_individual) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "Insufficient data for correlation analysis",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      # Spearman correlation between year and FLFP within each performance level
      rank_correlations <- labor_individual %>%
        group_by(Rank, Percentile_Label) %>%
        filter(n() >= 6) %>%
        summarise(
          correlation     = cor(Year, Labor_participation, method = "spearman", use = "complete.obs"),
          country_count   = n_distinct(Country),
          avg_participation = mean(Labor_participation, na.rm = TRUE),
          years_span      = max(Year) - min(Year) + 1,
          .groups = "drop"
        ) %>%
        filter(!is.na(correlation)) %>%
        arrange(Rank) %>%
        mutate(Percentile_Label = factor(Percentile_Label, levels = Percentile_Label))
      
      if(nrow(rank_correlations) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "Insufficient data points for performance level correlation analysis",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      # Green palette for labor chart to distinguish from blue GDP chart
      rank_colors <- c("#7CB342", "#689F2E", "#8BC34A", "#9CCC65", "#AED581", "#C5E1A5", "#DCEDC8", "#F1F8E9")
      bar_colors  <- rank_colors[1:nrow(rank_correlations)]
      
      p <- plot_ly(
        data = rank_correlations,
        x    = ~Percentile_Label,
        y    = ~correlation,
        type = "bar",
        marker = list(color = bar_colors, line = list(color = "#689F2E", width = 1)),
        hovertemplate = paste0(
          "<b style='color: #ffffff; font-family: Open Sans, sans-serif;'>%{x}</b><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>FLFP Time-Growth Correlation: %{y:.3f}</span><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Countries: ", rank_correlations$country_count, "</span><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Avg FLFP: ", round(rank_correlations$avg_participation, 1), "%</span><br>",
          "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Data span: ", rank_correlations$years_span, " years</span>",
          "<extra></extra>"
        ),
        hoverlabel = list(bgcolor = "#7CB342", bordercolor = "#7CB342",
                          font = list(family = "Open Sans, sans-serif", size = 13, color = "white")),
        showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(
            title         = "WBL Parenthood Performance Level",
            titlefont     = list(family = "Open Sans, sans-serif"),
            tickfont      = list(family = "Open Sans, sans-serif"),
            tickangle     = 45,
            categoryorder = "array",
            categoryarray = levels(rank_correlations$Percentile_Label)
          ),
          yaxis = list(
            title     = "FLFP Time-Growth Correlation (ρ)",
            titlefont = list(family = "Open Sans, sans-serif"),
            tickfont  = list(family = "Open Sans, sans-serif"),
            range     = c(-1, 1)
          ),
          margin        = list(l = 80, r = 50, t = 50, b = 150),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          font          = list(family = "Open Sans, sans-serif"),
          
          # Zero reference line
          shapes = list(
            list(type = "line",
                 x0 = -0.5, x1 = length(rank_correlations$Percentile_Label) - 0.5,
                 y0 = 0, y1 = 0,
                 line = list(color = "#6C757D", width = 2, dash = "dash"))
          ),
          
          # Interpretation note
          annotations = list(
            list(
              x = 0.02, y = -0.45, xref = "paper", yref = "paper",
              text = "<b>What this shows:</b> How strongly FLFP grows over time for countries in each performance level.<br>Higher values = stronger growth patterns within that performance group.",
              showarrow = FALSE,
              bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#dee2e6",
              borderwidth = 1, borderpad = 8,
              font = list(size = 11, family = "Open Sans, sans-serif", color = "#495057"),
              xanchor = "left", yanchor = "top"
            )
          )
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d"), scrollZoom = TRUE)
      
      return(p)
      
    } else {
      
      # Trends view — line chart of FLFP over time by performance level
      trend_data <- labor_trend_data()
      
      if(is.null(trend_data) || nrow(trend_data) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No labor data available for selected countries",
                                     font = list(family = "Open Sans, sans-serif", size = 14)),
                        font = list(family = "Open Sans, sans-serif")))
      }
      
      # Build color mapping from performance levels (same as GDP chart)
      comparison_data     <- comparison_countries()
      unique_levels       <- comparison_data %>% distinct(Rank, Percentile_Label) %>% arrange(Rank)
      level_colors        <- c("#003A5D", "#0071BC", "#009FDA", "#00A0B0", "#7CB342", "#872175")
      level_color_mapping <- setNames(level_colors[1:nrow(unique_levels)], unique_levels$Percentile_Label)
      
      # Color lookup: selected country = WB orange, others = performance level color
      get_color <- function(country_name, type) {
        if(type == "Selected Country") return("#F05023")
        for(label in names(level_color_mapping)) {
          if(grepl(label, country_name, fixed = TRUE)) return(level_color_mapping[label])
        }
        return("#003A5D")
      }
      
      p <- plot_ly()
      
      # Add one line per country or performance level average
      for(country in unique(trend_data$Country)) {
        country_data <- trend_data %>% filter(Country == country)
        country_type <- unique(country_data$Type)
        if(nrow(country_data) < 2) next
        
        # Calculate total change across available years
        first_year  <- min(country_data$Year)
        last_year   <- max(country_data$Year)
        first_value <- country_data$Labor_participation[country_data$Year == first_year][1]
        last_value  <- country_data$Labor_participation[country_data$Year == last_year][1]
        
        total_change <- if(!is.na(first_value) && !is.na(last_value)) {
          format_change(last_value - first_value)
        } else "N/A"
        
        line_color    <- get_color(country, country_type)
        country_count <- unique(country_data$Country_Count)[1]
        
        count_text <- if(country_type == "Selected Country") "" else
          paste0("<br><span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Countries in performance level: ", country_count, "</span>")
        
        p <- p %>%
          add_trace(
            data = country_data,
            x    = ~Year, y = ~Labor_participation,
            name = country, type = "scatter", mode = "lines+markers",
            line   = list(color = line_color, width = 2),
            marker = list(size = 4, color = line_color),
            hovertemplate = paste0(
              "<b style='color: #ffffff; font-family: Open Sans, sans-serif;'>%{fullData.name}</b><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Year: %{x}</span><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Female labor participation: %{y:.1f}%</span><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Type: ", country_type, "</span><br>",
              "<span style='color: #ffffff; font-family: Open Sans, sans-serif;'>Total change: ", total_change, " pp</span>",
              count_text, "<extra></extra>"
            ),
            hoverlabel = list(bgcolor = line_color, bordercolor = line_color,
                              font = list(family = "Open Sans, sans-serif", size = 13, color = "white")),
            showlegend = TRUE
          )
      }
      
      p <- p %>%
        layout(
          xaxis = list(title = "Year",
                       titlefont = list(family = "Open Sans, sans-serif"),
                       tickfont  = list(family = "Open Sans, sans-serif")),
          yaxis = list(title = "Female Labor Force Participation (%)",
                       titlefont = list(family = "Open Sans, sans-serif"),
                       tickfont  = list(family = "Open Sans, sans-serif")),
          legend = list(orientation = "v", x = 1.02, y = 1,
                        font = list(family = "Open Sans, sans-serif", size = 10)),
          margin        = list(l = 60, r = 150, t = 50, b = 50),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          font          = list(family = "Open Sans, sans-serif")
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d"), scrollZoom = TRUE)
      
      return(p)
    }
  })
  
  # ---- Correlation analysis UI output ----
  # Renders the correlation box with GDP and labor results side by side
  # Includes methodology note and overall policy interpretation
  output$enhanced_correlation_analysis <- renderUI({
    corr_data <- enhanced_correlation_analysis()
    
    div(style = "font-family: 'Open Sans', sans-serif;",
        h3("Statistical Correlation Analysis",
           style = "margin-top: 0; font-family: 'Andes', sans-serif; color: #FFE5B4;"),
        
        # Explanation of correlation direction
        div(style = "background: rgba(255,255,255,0.1); padding: 10px; border-radius: 6px; margin-bottom: 15px; font-size: 11px; color: rgba(255,255,255,0.9);",
            HTML("<b>Understanding the relationships:</b><br>"),
            HTML("• <b>Positive correlation:</b> Higher WBL Parenthood percentiles → Better economic outcomes<br>"),
            HTML("• <b>Negative correlation:</b> Higher WBL Parenthood percentiles → Worse economic outcomes (counterintuitive)<br>"),
            HTML("• Correlation coefficients measure the statistical strength of these relationships")
        ),
        
        # GDP and labor results side by side
        div(style = "display: flex; justify-content: space-between; margin-top: 15px;",
            div(style = "flex: 1; margin-right: 20px;",
                div(class = "correlation-metric", corr_data$gdp_interpretation),
                div(class = "correlation-coefficient",
                    paste0("Statistical evidence: ρ = ", ifelse(!is.na(corr_data$gdp_correlation), round(corr_data$gdp_correlation, 3), "N/A"))),
                div(class = "correlation-coefficient", corr_data$gdp_correlation_range),
                div(style = "font-size: 11px; margin-top: 8px; opacity: 0.8;",
                    strong("Growth Analysis: "), br(), corr_data$gdp_rank_changes)
            ),
            div(style = "flex: 1;",
                div(class = "correlation-metric", corr_data$labor_interpretation),
                div(class = "correlation-coefficient",
                    paste0("Statistical evidence: ρ = ", ifelse(!is.na(corr_data$labor_correlation), round(corr_data$labor_correlation, 3), "N/A"))),
                div(class = "correlation-coefficient", corr_data$labor_correlation_range),
                div(style = "font-size: 11px; margin-top: 8px; opacity: 0.8;",
                    strong("Level Changes (2010-2024): "), br(), corr_data$labor_rank_changes)
            )
        ),
        
        # Overall policy insight
        div(class = "correlation-interpretation",
            strong("Policy Insight: "), corr_data$overall_interpretation),
        
        # Methodology footnote
        div(style = "font-size: 11px; margin-top: 10px; opacity: 0.8;",
            strong("Methodology: "), "Analysis correlates percentile scores (higher = better WBL performance) with economic outcomes. Positive correlation coefficients indicate positive relationships between WBL performance and outcomes. Spearman correlation (ρ) addresses non-linearity. CAGR = Compound Annual Growth Rate. pp = percentage points.")
    )
  })
  
  # ---- Comparison group summary text ----
  # Outputs key statistics for the currently filtered comparison group
  # Changes focus metrics depending on performers vs reformers view
  output$peer_summary <- renderText({
    country_info    <- selected_country_data()
    comparison_data <- comparison_countries()
    filtered_data   <- filtered_comparison_countries()
    rank_info       <- selected_country_rank()
    
    if(rank_info$total == 0) return("No comparison data available.")
    if(nrow(filtered_data) == 0) return("No countries selected in filter.")
    
    avg_score  <- round(mean(filtered_data$`2024 PARENTHOOD SCORE`), 1)
    avg_change <- round(mean(filtered_data$`Difference in score`), 1)
    max_score  <- max(filtered_data$`2024 PARENTHOOD SCORE`)
    max_change <- max(filtered_data$`Difference in score`)
    
    top_performers_count <- sum(filtered_data$Rank == 1, na.rm = TRUE)
    
    comparison_type_text <- switch(input$comparison_type,
                                   "peer"   = "peer group",
                                   "region" = "regional",
                                   "income" = "income group")
    
    avg_change_formatted <- format_change(avg_change)
    max_change_formatted <- format_change(max_change)
    
    if(input$view_type == "performers") {
      paste0(
        "Showing ", nrow(filtered_data), " of ", rank_info$total, " countries in ", comparison_type_text, "\n",
        "Your performance level: ", rank_info$percentile_label, "\n",
        "Countries in top performance level (in view): ", top_performers_count, "\n\n",
        "Filtered Group Statistics (2024 Performance):\n",
        "Average 2024 Parenthood Score: ", avg_score, " (blue line on chart)\n",
        "Highest 2024 Parenthood Score: ", max_score, "\n",
        "Average score change (2010-2024): ", avg_change_formatted, "\n",
        "Largest improvement: ", max_change_formatted
      )
    } else {
      paste0(
        "Showing ", nrow(filtered_data), " of ", rank_info$total, " countries in ", comparison_type_text, "\n",
        "Your performance level: ", rank_info$percentile_label, "\n",
        "Countries in top reform performance level (in view): ", top_performers_count, "\n\n",
        "Filtered Group Statistics (Reform Progress):\n",
        "Average score change (2010-2024): ", avg_change_formatted, " (blue line on chart)\n",
        "Largest improvement: ", max_change_formatted, "\n",
        "Average 2024 Parenthood Score: ", avg_score, "\n",
        "Highest 2024 Parenthood Score: ", max_score
      )
    }
  })
  
  # ---- Context and insights text ----
  # Computes gaps to top performers and group average for talking point support
  # Changes focus metrics depending on performers vs reformers view
  output$regional_context <- renderText({
    country_info    <- selected_country_data()
    comparison_data <- comparison_countries()
    filtered_data   <- filtered_comparison_countries()
    rank_info       <- selected_country_rank()
    
    if(rank_info$total == 0) return("No context data available.")
    if(nrow(filtered_data) == 0) return("No countries selected in filter.")
    
    # Top performer(s) in the filtered view
    top_performers <- filtered_data %>% filter(Rank == 1) %>% pull(Economy)
    
    comparison_insights <- switch(input$comparison_type,
                                  "peer"   = "Within your peer group",
                                  "region" = paste0("Across ", country_info$Region),
                                  "income" = paste0("Among ", country_info$`Income Group`, " countries"))
    
    countries_with_lower_performance <- nrow(filtered_data %>% filter(Rank > rank_info$rank))
    
    leaders_text <- if(length(top_performers) == 1) {
      paste0("Top performer (in view): ", top_performers[1])
    } else if(length(top_performers) > 1) {
      paste0("Top performers (in view, tied): ",
             paste(head(top_performers, 3), collapse = ", "),
             if(length(top_performers) > 3) paste0(" and ", length(top_performers) - 3, " others") else "")
    } else {
      "No top performers in current view"
    }
    
    if(input$view_type == "performers") {
      gap_to_top          <- max(filtered_data$`2024 PARENTHOOD SCORE`) - country_info$`2024 PARENTHOOD SCORE`
      avg_value           <- mean(filtered_data$`2024 PARENTHOOD SCORE`)
      gap_to_avg          <- country_info$`2024 PARENTHOOD SCORE` - avg_value
      score_change_formatted <- format_change(country_info$`Difference in score`)
      gap_to_avg_formatted   <- format_change(gap_to_avg)
      
      paste0(
        comparison_insights, " (filtered view):\n\n",
        leaders_text, "\n\n",
        "Your position (2024 Performance Focus):\n",
        "Gap to top performers: ", round(gap_to_top, 1), " points\n",
        "Gap to filtered average: ", gap_to_avg_formatted, " points\n",
        "Your score change since 2010: ", score_change_formatted, " points\n",
        "Countries with lower performance levels (in view): ", countries_with_lower_performance, "\n\n",
        "Economic Trends: Performance level averages show patterns by achievement level"
      )
    } else {
      gap_to_top         <- max(filtered_data$`Difference in score`) - country_info$`Difference in score`
      avg_value          <- mean(filtered_data$`Difference in score`)
      gap_to_avg         <- country_info$`Difference in score` - avg_value
      gap_to_avg_formatted <- format_change(gap_to_avg)
      
      paste0(
        comparison_insights, " (filtered view):\n\n",
        leaders_text, "\n\n",
        "Your position (Reform Progress Focus):\n",
        "Gap to top reform performers: ", round(gap_to_top, 1), " points\n",
        "Gap to filtered average: ", gap_to_avg_formatted, " points\n",
        "Your current 2024 Parenthood Score: ", country_info$`2024 PARENTHOOD SCORE`, " points\n",
        "Countries with lower reform performance (in view): ", countries_with_lower_performance, "\n\n",
        "Economic Trends: Performance level averages show patterns by reform achievement"
      )
    }
  })
}

# ---- Run the application ----
shinyApp(ui = ui, server = server)