# R version 4.4.3 (2025-02-28 ucrt) -- "Trophy Case"
# Prerequisites:
# 1. Install required packages:
#    install.packages(c("shiny", "bslib", "shinyjs", "plotly", "prophet", "dplyr", "tidyr", 
#                       "lubridate", "DT", "readxl", "keras", "reticulate", "forecast", "RColorBrewer"), 
#                     repos = "https://cran.r-project.org")
# 2. If prophet fails: install.packages("rstan"); Sys.setenv("R_MAKEFLAGS" = "-j4"); install.packages("prophet")
# 3. If DT fails: install.packages("DT", repos = "https://cran.microsoft.com")
# 4. For LSTM, ensure Python with tensorflow is installed and configured with reticulate:
#    reticulate::conda_install("conda-forge", c("tensorflow", "scikit-learn"))
# 5. Ensure R version >= 3.5.0

library(shiny)
library(bslib)
library(shinyjs)
library(plotly)
library(prophet)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(readxl)
library(reticulate)
library(forecast)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    base_font = font_google("Roboto"),
    heading_font = font_google("Oswald"),
    primary = "#00C4CC"
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #0A0A1A 0%, #1A2A3A 100%);
        position: relative;
        min-height: 100vh;
        overflow-y: auto;
        color: #F0F0F0;
      }
      .flag-background {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: url('https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/Flag_of_Pakistan.svg/1024px-Flag_of_Pakistan.svg.png') no-repeat center;
        background-size: cover;
        opacity: 0.05;
        z-index: -1;
        animation: flagWave 7s infinite ease-in-out, flagFade 4s infinite alternate;
      }
      @keyframes flagWave {
        0% { transform: translateX(0) scale(1.1); }
        50% { transform: translateX(-10px) scale(1.15); }
        100% { transform: translateX(0) scale(1.1); }
      }
      @keyframes flagFade {
        0% { opacity: 0.05; }
        100% { opacity: 0.1; }
      }
      h1, h2 {
        font-size: 3.5em;
        font-weight: 700;
        margin: 40px 0;
        text-align: center;
        color: #FFFFFF;
        z-index: 5;
        padding: 15px 30px;
        background-color: rgba(0, 0, 0, 0.8);
        border-radius: 10px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5);
        border: 2px solid transparent;
        background-image: linear-gradient(rgba(0, 0, 0, 0.8), rgba(0, 0, 0, 0.8)), 
                          linear-gradient(90deg, #00C4CC, #006600);
        background-origin: border-box;
        background-clip: padding-box, border-box;
        letter-spacing: 1.5px;
        text-transform: uppercase;
      }
      h2 { font-size: 2.2em; }
      .btn-custom {
        font-size: 1.5em;
        padding: 15px 30px;
        margin: 15px;
        border-radius: 20px;
        font-weight: 600;
        box-shadow: 0 8px 15px rgba(0, 0, 0, 0.4);
        transition: all 0.4s ease;
        z-index: 5;
        border: 2px solid transparent;
      }
      .btn-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 12px 20px rgba(0, 0, 0, 0.6);
        border-color: #FFFFFF;
      }
      .btn-crime { background: linear-gradient(45deg, #4A90E2, #1E90FF); color: #FFF; }
      .btn-food { background: linear-gradient(45deg, #50C878, #3CB371); color: #FFF; }
      .btn-urban { background: linear-gradient(45deg, #FF9500, #FF4500); color: #FFF; }
      .btn-back { background: linear-gradient(45deg, #6B7280, #4B5563); color: #FFF; }
      .btn-nav { background: linear-gradient(45deg, #9CA3AF, #6B7280); color: #FFF; font-size: 1em; }
      .btn-overview { background: linear-gradient(45deg, #4ECDC4, #45B7D1); color: #FFF; }
      .btn-prediction { background: linear-gradient(45deg, #FF6B6B, #D4A5A5); color: #FFF; }
      .btn-tables { background: linear-gradient(45deg, #9B59B6, #8E44AD); color: #FFF; }
      .btn-predict { background: linear-gradient(45deg, #00C4CC, #006600); color: #FFF; font-size: 1.1em; }
      .btn-clear { background: linear-gradient(45deg, #D1D5DB, #6B7280); color: #FFF; font-size: 1.1em; }
      .btn-next { background: linear-gradient(45deg, #00C4CC, #006600); color: #FFF; font-size: 1.1em; }
      .button-container {
        display: flex;
        justify-content: center;
        margin: 50px 0;
        flex-wrap: wrap;
        z-index: 5;
      }
      .nav-buttons {
        position: fixed;
        right: 15px;
        top: 50%;
        transform: translateY(-50%);
        display: flex;
        flex-direction: column;
        gap: 8px;
        z-index: 10;
      }
      .footer {
        position: fixed;
        bottom: 20px;
        left: 0;
        right: 0;
        text-align: center;
        font-size: 18px;
        color: #A0A0A0;
        text-shadow: 0 0 8px rgba(255, 255, 255, 0.2);
        z-index: 5;
      }
      .plot-container {
        background-color: rgba(0, 0, 0, 0.7);
        padding: 15px;
        border-radius: 8px;
        margin: 15px;
        z-index: 5;
      }
      .tab-content {
        margin: 15px;
        z-index: 5;
      }
      .input-panel {
        padding: 25px;
        margin: 15px;
        z-index: 4000;
        display: flex;
        flex-direction: column;
        align-items: center;
        background-color: rgba(0, 0, 0, 0.85);
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5);
      }
      .input-container {
        z-index: 4001;
        margin-bottom: 12px;
      }
      .form-control {
        background-color: #FFFFFF !important;
        color: #000000 !important;
        border: 3px solid #00C4CC !important;
        z-index: 4001 !important;
        padding: 10px !important;
        font-size: 1.3em !important;
        width: 220px !important;
        border-radius: 6px !important;
        opacity: 1 !important;
      }
      .form-control:focus {
        outline: none !important;
        border-color: #FFFFFF !important;
        box-shadow: 0 0 10px #00C4CC !important;
      }
      .error-message {
        color: #FF5555;
        font-size: 1.1em;
        margin-top: 15px;
        text-align: center;
        z-index: 4000;
      }
      .loading-spinner {
        display: none;
        font-size: 1.3em;
        color: #00C4CC;
        text-align: center;
        margin-top: 15px;
        z-index: 4000;
      }
      .loading-spinner.active {
        display: block;
      }
      .stats-box {
        background-color: rgba(0, 0, 0, 0.85);
        border: 2px solid #00C4CC;
        border-radius: 8px;
        padding: 15px;
        margin: 8px;
        text-align: center;
        color: #FFFFFF;
        font-size: 1.1em;
        font-weight: 600;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5);
        z-index: 5;
        width: 220px;
        min-height: 120px;
        display: inline-block;
        overflow: hidden;
        word-wrap: break-word;
        white-space: normal;
      }
      .stats-container {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        margin: 15px;
        z-index: 5;
      }
      .faostat-input-container {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 15px;
        margin-bottom: 10px;
        background-color: rgba(0, 0, 0, 0.8);
        padding: 12px;
        border-radius: 6px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5);
      }
      .faostat-input-container .form-control {
        width: 280px !important;
        font-size: 1.1em !important;
      }
      .faostat-input-container .radio-inline {
        color: #FFFFFF;
        font-size: 1em;
        margin: 0 8px;
      }
      .faostat-input-container .radio-inline input[type='radio'] {
        accent-color: #00C4CC;
        transform: scale(1.1);
      }
      .js-irs-0 .irs {
        width: 80%;
        margin: 0 auto;
        display: flex;
        justify-content: center;
      }
      .js-irs-0 .irs-line {
        background: linear-gradient(to right, #00C4CC, #006600);
        height: 8px;
        border-radius: 4px;
      }
      .js-irs-0 .irs-bar {
        background: linear-gradient(to right, #00C4CC, #006600);
        height: 8px;
        border-radius: 4px;
      }
      .js-irs-0 .irs-handle {
        background-color: #FFFFFF;
        border: 3px solid #00C4CC;
        width: 18px;
        height: 18px;
        top: 20px;
        border-radius: 50%;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.5);
      }
      .js-irs-0 .irs-handle:hover {
        transform: scale(1.2);
        background-color: #00C4CC;
        border-color: #FFFFFF;
      }
      .js-irs-0 .irs-single, .js-irs-0 .irs-min, .js-irs-0 .irs-max {
        color: #FFFFFF;
        background-color: rgba(0, 0,0, 0.8);
        padding: 4px 8px;
        border-radius: 4px;
        font-size: 12px;
      }
      .js-irs-0 .irs-grid-text {
        color: #A0A0A0;
        font-size: 10px;
      }
      .dataTables_wrapper .dataTables_filter input {
        font-size: 1em !important;
        width: 150px !important;
        padding: 6px !important;
        background-color: #FFFFFF !important;
        color: #000000 !important;
        border: 2px solid #00C4CC !important;
        border-radius: 4px !important;
      }
      .dataTables_wrapper .dataTables_length select {
        font-size: 1em !important;
        width: 80px !important;
        padding: 6px !important;
        background-color: #FFFFFF !important;
        color: #000000 !important;
        border: 2px solid #00C4CC !important;
        border-radius: 4px !important;
      }
      .dataTables_wrapper .dataTables_filter label,
      .dataTables_wrapper .dataTables_length label {
        color: #FFFFFF;
        font-size: 1em;
      }
      .dataTables_wrapper .dataTables_paginate {
        margin-top: 10px;
        text-align: center;
      }
      .dataTables_wrapper .paginate_button {
        background-color: rgba(0, 0, 0, 0.6) !important;
        color: #FFFFFF !important;
        border: 1px solid #00C4CC !important;
        border-radius: 4px !important;
        padding: 5px 10px !important;
        margin: 0 2px !important;
        font-size: 0.9em !important;
      }
      .dataTables_wrapper .paginate_button.current {
        background-color: rgba(0, 0, 0, 0.8) !important;
        font-weight: bold;
      }
      .dataTables_wrapper .paginate_button.disabled {
        background-color: rgba(107, 114, 128, 0.6) !important;
        color: #A0A0A0 !important;
        cursor: not-allowed;
      }
    "))
  ),
  
  div(class = "flag-background"),
  
  uiOutput("page")
)

# Crime Dashboard UI
crime_dashboard_ui <- fluidPage(
  div(class = "flag-background"),
  
  h1("Crime Dashboard"),
  
  div(class = "button-container",
      actionButton("back", "Back to Main Dashboard", class = "btn btn-custom btn-back")
  ),
  
  div(class = "nav-buttons",
      actionButton("scroll_up", "↑", class = "btn btn-custom btn-nav"),
      actionButton("scroll_down", "↓", class = "btn btn-custom btn-nav")
  ),
  
  div(class = "button-container", style = "display: flex; justify-content: center; gap: 15px;",
      actionButton("overview_btn", "Crime Overview", class = "btn btn-custom btn-overview"),
      actionButton("prediction_btn", "Crime Predictions", class = "btn btn-custom btn-prediction"),
      actionButton("tables_btn", "Data Tables", class = "btn btn-custom btn-tables")
  ),
  
  uiOutput("crime_content_ui"),
  
  div(class = "footer",
      "Developed by Noor Rehman & Alisha Rubab Version 1.0")
)

# Urbanization Dashboard UI
urban_dashboard_ui <- fluidPage(
  div(class = "flag-background"),
  
  h1("Urbanization Dashboard"),
  
  div(class = "button-container",
      actionButton("back", "Back to Main Dashboard", class = "btn btn-custom btn-back")
  ),
  
  div(class = "nav-buttons",
      actionButton("scroll_up", "↑", class = "btn btn-custom btn-nav"),
      actionButton("scroll_down", "↓", class = "btn btn-custom btn-nav")
  ),
  
  div(class = "button-container", style = "display: flex; justify-content: center; gap: 15px;",
      actionButton("urban_overview_btn", "Urban Overview", class = "btn btn-custom btn-overview"),
      actionButton("urban_prediction_btn", "Population Predictions", class = "btn btn-custom btn-prediction"),
      actionButton("urban_tables_btn", "Data Tables", class = "btn btn-custom btn-tables")
  ),
  
  uiOutput("urban_content_ui"),
  
  div(class = "footer",
      "Developed by Noor Rehman & Alisha Rubab Version 1.0")
)

# Food Dashboard UI (Unchanged)
food_dashboard_ui <- fluidPage(
  div(class = "flag-background"),
  
  h1("Food Dashboard"),
  
  div(class = "button-container",
      actionButton("back_food", "Back to Main Dashboard", class = "btn btn-custom btn-back")
  ),
  
  div(class = "nav-buttons",
      actionButton("scroll_up_food", "↑", class = "btn btn-custom btn-nav"),
      actionButton("scroll_down_food", "↓", class = "btn btn-custom btn-nav")
  ),
  
  div(class = "button-container", style = "display: flex; justify-content: center; gap: 20px;",
      actionButton("food_overview_btn", "Food Overview", class = "btn btn-custom btn-overview"),
      actionButton("food_prediction_btn", "Food Predictions", class = "btn btn-custom btn-prediction"),
      actionButton("food_tables_btn", "Data Tables", class = "btn btn-custom btn-tables")
  ),
  
  uiOutput("food_content_ui"),
  
  div(class = "footer",
      "Developed by Noor Rehman & Alisha Rubab Version 1.0")
)

# Main Dashboard UI
main_dashboard_ui <- fluidPage(
  div(class = "flag-background"),
  
  h1("National Development Hub"),
  
  div(class = "button-container",
      actionButton("crime", "Crime Dashboard", class = "btn btn-custom btn-crime"),
      actionButton("food", "Food Dashboard", class = "btn btn-custom btn-food"),
      actionButton("urban", "Urbanization Dashboard", class = "btn btn-custom btn-urban")
  ),
  
  div(class = "footer",
      "Developed by Noor Rehman & Alisha Rubab Version 1.0")
)

# Server logic
server <- function(input, output, session) {
  # Reactive values to track state
  current_page <- reactiveVal("main")
  active_content <- reactiveVal(NULL)
  table_page_crime <- reactiveVal(1)
  table_page_urban <- reactiveVal(1)
  food_table_page <- reactiveVal(1)
  
  # Helper function to get data file paths dynamically
  get_data_path <- function(filename) {
    possible_paths <- c(
      file.path(getwd(), filename),
      file.path("data", filename),
      file.path("~", "Desktop", "BIG PROJECT", "Chunk 01", "STAGE 02 - CRIME", filename),
      file.path("~", "Desktop", "BIG PROJECT", "Chunk 01", filename)
    )
    for (path in possible_paths) {
      if (file.exists(path)) return(normalizePath(path))
    }
    return(NULL)
  }
  
  # --- Crime Dashboard Data ---
  crime_data <- reactive({
    message("Starting crime_data() at ", Sys.time())
    df <- tryCatch({
      file_path <- "C:\\Users\\digital\\OneDrive\\Desktop\\BIG PROJECT\\Chunk 01\\Crime_Data_2012_2020.csv"
      if (is.null(file_path)) stop("Crime CSV file not found.")
      message("Loading Crime CSV from: ", file_path)
      df <- read.csv(file_path, stringsAsFactors = FALSE)
      message("Crime CSV loaded. Columns: ", paste(colnames(df), collapse = ", "))
      required_cols <- c("Year", "Type.of.Crime", "Punjab", "Sindh", "KPK", "Balochistan", "Islamabad", "Railways", "G.B", "AJK")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
      }
      df$Year <- as.integer(df$Year)
      df <- df %>% 
        mutate(across(c(Punjab, Sindh, KPK, Balochistan, Islamabad, Railways, G.B, AJK), ~coalesce(as.numeric(.x), 0))) %>% 
        mutate(Total = rowSums(select(., Punjab, Sindh, KPK, Balochistan, Islamabad, Railways, G.B, AJK), na.rm = TRUE))
      message("Crime data cleaned: ", capture.output(head(df)))
      df
    }, error = function(e) {
      message("Error loading Crime CSV: ", e$message)
      message("Using fallback crime dataset.")
      years <- 2012:2020
      crimes <- c("Theft", "Robbery", "Assault", "Burglary", "Fraud")
      n <- length(years) * length(crimes)
      base_trend <- 100 + seq(0, 50, length.out = length(years))
      seasonal <- 20 * sin(2 * pi * seq_along(years) / length(years))
      data.frame(
        Year = rep(years, each = length(crimes)),
        Type.of.Crime = rep(crimes, times = length(years)),
        Punjab = pmax(0, round(runif(n, 50, 500))),
        Sindh = pmax(0, round(runif(n, 50, 500))),
        KPK = pmax(0, round(runif(n, 50, 500))),
        Balochistan = pmax(0, round(runif(n, 50, 500))),
        Islamabad = pmax(0, round(runif(n, 10, 100))),
        Railways = pmax(0, round(runif(n, 5, 50))),
        G.B = pmax(0, round(runif(n, 5, 50))),
        AJK = pmax(0, round(runif(n, 5, 50)))
      ) %>% 
        mutate(Total = rowSums(select(., Punjab, Sindh, KPK, Balochistan, Islamabad, Railways, G.B, AJK), na.rm = TRUE))
    })
    validate(
      need(nrow(df) > 0, "Crime data is empty."),
      need(all(c("Year", "Total", "Type.of.Crime") %in% colnames(df)), "Required columns missing in crime data.")
    )
    message("Crime data structure: ", capture.output(str(df)))
    df
  })
  
  # --- Urbanization Dashboard Data ---
  urban_data <- reactive({
    message("Starting urban_data() at ", Sys.time())
    df <- tryCatch({
      file_path <- "C:\\Users\\digital\\OneDrive\\Desktop\\BIG PROJECT\\Chunk 01\\province_cleaned_final.xlsx"
      if (is.null(file_path)) stop("Urbanization Excel file not found.")
      message("Loading Urbanization Excel from: ", file_path)
      df <- read_excel(file_path)
      message("Urbanization Excel loaded. Columns: ", paste(colnames(df), collapse = ", "))
      required_cols <- c("Year", "Islamabad", "Punjab", "Sindh", "KPK", "Balochistan", 
                         "Azad Kashmir", "Northern Areas", "Tribal Areas", "Total")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
      }
      df$Year <- as.integer(df$Year)
      df <- df %>% 
        mutate(across(c(Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`, Total), 
                      ~coalesce(as.numeric(.x), 0)))
      message("Urbanization data cleaned: ", capture.output(head(df)))
      df
    }, error = function(e) {
      message("Error loading Urbanization Excel: ", e$message)
      message("Using fallback urbanization dataset.")
      years <- 1981:2025
      n <- length(years)
      base_trend <- 1e6 + seq(0, 5e6, length.out = n)
      seasonal <- 1e5 * sin(2 * pi * seq_along(years) / n)
      data.frame(
        Year = years,
        Islamabad = pmax(0, round(base_trend * 0.05 + seasonal + rnorm(n, 0, 1e4))),
        Punjab = pmax(0, round(base_trend * 0.4 + seasonal + rnorm(n, 0, 5e4))),
        Sindh = pmax(0, round(base_trend * 0.3 + seasonal + rnorm(n, 0, 4e4))),
        KPK = pmax(0, round(base_trend * 0.15 + seasonal + rnorm(n, 0, 3e4))),
        Balochistan = pmax(0, round(base_trend * 0.05 + seasonal + rnorm(n, 0, 2e4))),
        `Azad Kashmir` = pmax(0, round(base_trend * 0.02 + seasonal + rnorm(n, 0, 1e4))),
        `Northern Areas` = pmax(0, round(base_trend * 0.02 + seasonal + rnorm(n, 0, 1e4))),
        `Tribal Areas` = pmax(0, round(base_trend * 0.01 + seasonal + rnorm(n, 0, 5e3)))
      ) %>% 
        mutate(Total = rowSums(select(., Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`), na.rm = TRUE))
    })
    validate(
      need(nrow(df) > 0, "Urbanization data is empty."),
      need(all(c("Year", "Total") %in% colnames(df)), "Required columns missing in urbanization data.")
    )
    message("Urbanization data structure: ", capture.output(str(df)))
    df
  })
  
  # Render crime content UI
  output$crime_content_ui <- renderUI({
    message("Rendering crime_content_ui at ", Sys.time())
    if (is.null(active_content())) {
      return(div())
    } else if (active_content() == "overview") {
      div(class = "tab-content",
          h2("Crime Overview"),
          div(class = "input-panel",
              div(class = "input-container",
                  sliderInput(
                    inputId = "crime_year_range",
                    label = "Select Year Range:",
                    min = min(crime_data()$Year),
                    max = max(crime_data()$Year),
                    value = c(min(crime_data()$Year), max(crime_data()$Year)),
                    step = 1,
                    sep = ""
                  ),
                  selectInput(
                    inputId = "crime_subplot_select",
                    label = "Select Subplot:",
                    choices = c("Total Crimes", "Punjab", "Sindh", "KPK", "Balochistan", "Islamabad", "Railways", "G.B", "AJK"),
                    selected = "Total Crimes"
                  )
              )
          ),
          div(class = "stats-container",
              div(class = "stats-box",
                  strong("Total Crimes"), br(),
                  textOutput("crime_total_crimes")
              ),
              div(class = "stats-box",
                  strong("Avg Crimes per Year"), br(),
                  textOutput("crime_avg_crimes")
              ),
              div(class = "stats-box",
                  strong("Peak Crime Year"), br(),
                  textOutput("crime_peak_year")
              ),
              div(class = "stats-box",
                  strong("Crime Growth Rate"), br(),
                  textOutput("crime_growth_rate")
              )
          ),
          div(class = "plot-container",
              plotlyOutput("crime_summary_subplots", height = "400px")),
          div(class = "plot-container",
              div(class = "faostat-input-container",
                  selectInput(
                    inputId = "crime_type_select",
                    label = "Select Crime Type:",
                    choices = c("All Types", unique(crime_data()$Type.of.Crime)),
                    selected = "All Types"
                  ),
                  radioButtons(
                    inputId = "crime_plot_type",
                    label = "Crime Plot Type:",
                    choices = c("Bar Chart" = "bar", "Box Plot" = "box", "Line Chart" = "line"),
                    selected = "bar",
                    inline = TRUE
                  )
              ),
              plotlyOutput("crime_type_plot", height = "365px")),
          div(class = "plot-container",
              plotlyOutput("crime_category_bar_animated", height = "400px")),
          div(class = "plot-container",
              plotlyOutput("crime_category_stacked_area", height = "500px")),
          div(class = "plot-container",
              plotlyOutput("crime_category_heatmap", height = "400px")),
          div(class = "plot-container",
              plotlyOutput("crime_category_pie", height = "450px"))
      )
    } else if (active_content() == "prediction") {
      div(class = "tab-content",
          h2("Crime Predictions"),
          div(class = "input-panel",
              div(class = "input-container",
                  numericInput("predict_year", "Enter Year (2012-2030):", 
                               value = NULL, min = 2012, max = 2030, step = 1)
              ),
              div(class = "button-container", style = "display: flex; gap: 10px;",
                  actionButton("predict", "Predict", class = "btn btn-custom btn-predict"),
                  actionButton("clear", "Clear", class = "btn btn-custom btn-clear")
              ),
              div(id = "loading-spinner", class = "loading-spinner", "Processing Prediction..."),
              div(id = "error-message", class = "error-message", textOutput("prediction_error"))
          ),
          div(class = "plot-container",
              plotlyOutput("crime_prediction_plot", height = "400px")),
          div(class = "plot-container",
              DTOutput("prediction_result_table", height = "auto")),
          div(class = "plot-container",
              verbatimTextOutput("performance_metrics"))
      )
    } else if (active_content() == "tables") {
      div(class = "tab-content",
          h2("Data Tables"),
          div(class = "plot-container",
              DTOutput("crime_data_table", height = "auto")),
          div(class = "button-container",
              actionButton("crime_next_page", "Next Page", class = "btn btn-custom btn-next")
          )
      )
    }
  })
  
  # Render urbanization content UI
  output$urban_content_ui <- renderUI({
    message("Rendering urban_content_ui at ", Sys.time())
    if (is.null(active_content())) {
      return(div())
    } else if (active_content() == "urban_overview") {
      div(class = "tab-content",
          h2("Urban Overview"),
          div(class = "input-panel",
              div(class = "input-container",
                  sliderInput(
                    inputId = "urban_year_range",
                    label = "Select Year Range:",
                    min = min(urban_data()$Year),
                    max = max(urban_data()$Year),
                    value = c(min(urban_data()$Year), max(urban_data()$Year)),
                    step = 1,
                    sep = ""
                  ),
                  selectInput(
                    inputId = "urban_subplot_select",
                    label = "Select Subplot:",
                    choices = c("Total Population", "Islamabad", "Punjab", "Sindh", "KPK", "Balochistan", 
                                "Azad Kashmir", "Northern Areas", "Tribal Areas"),
                    selected = "Total Population"
                  )
              )
          ),
          div(class = "stats-container",
              div(class = "stats-box",
                  strong("Total Population"), br(),
                  textOutput("urban_total_population")
              ),
              div(class = "stats-box",
                  strong("Avg Population per Year"), br(),
                  textOutput("urban_avg_population")
              ),
              div(class = "stats-box",
                  strong("Peak Population Year"), br(),
                  textOutput("urban_peak_year")
              ),
              div(class = "stats-box",
                  strong("Population Growth Rate"), br(),
                  textOutput("urban_growth_rate")
              )
          ),
          div(class = "plot-container",
              plotlyOutput("urban_summary_subplots", height = "400px")),
          div(class = "plot-container",
              div(class = "faostat-input-container",
                  selectInput(
                    inputId = "urban_region_select",
                    label = "Select Region:",
                    choices = c("All Regions", "Islamabad", "Punjab", "Sindh", "KPK", "Balochistan", 
                                "Azad Kashmir", "Northern Areas", "Tribal Areas"),
                    selected = "All Regions"
                  ),
                  radioButtons(
                    inputId = "urban_plot_type",
                    label = "Urban Plot Type:",
                    choices = c("Bar Chart" = "bar", "Box Plot" = "box", "Line Chart" = "line"),
                    selected = "bar",
                    inline = TRUE
                  )
              ),
              plotlyOutput("urban_region_plot", height = "365px")),
          div(class = "plot-container",
              plotlyOutput("urban_category_bar_animated", height = "400px")),
          div(class = "plot-container",
              plotlyOutput("urban_category_stacked_area", height = "500px")),
          div(class = "plot-container",
              plotlyOutput("urban_category_heatmap", height = "400px")),
          div(class = "plot-container",
              plotlyOutput("urban_category_pie", height = "450px"))
      )
    } else if (active_content() == "urban_prediction") {
      div(class = "tab-content",
          h2("Population Predictions"),
          div(class = "input-panel",
              div(class = "input-container",
                  numericInput("urban_predict_year", "Enter Year (1981-2030):", 
                               value = NULL, min = 1981, max = 2030, step = 1)
              ),
              div(class = "button-container", style = "display: flex; gap: 10px;",
                  actionButton("urban_predict", "Predict", class = "btn btn-custom btn-predict"),
                  actionButton("urban_clear", "Clear", class = "btn btn-custom btn-clear")
              ),
              div(id = "urban-loading-spinner", class = "loading-spinner", "Processing Prediction..."),
              div(id = "urban-error-message", class = "error-message", textOutput("urban_prediction_error"))
          ),
          div(class = "plot-container",
              plotlyOutput("urban_prediction_plot", height = "400px")),
          div(class = "plot-container",
              DTOutput("urban_prediction_result_table", height = "auto")),
          div(class = "plot-container",
              verbatimTextOutput("urban_performance_metrics"))
      )
    } else if (active_content() == "urban_tables") {
      div(class = "tab-content",
          h2("Data Tables"),
          div(class = "plot-container",
              DTOutput("urban_data_table", height = "auto")),
          div(class = "button-container",
              actionButton("urban_next_page", "Next Page", class = "btn btn-custom btn-next")
          )
      )
    }
  })
  
  # Render UI based on page
  output$page <- renderUI({
    if (current_page() == "main") {
      main_dashboard_ui
    } else if (current_page() == "crime") {
      crime_dashboard_ui
    } else if (current_page() == "food") {
      food_dashboard_ui
    } else if (current_page() == "urban") {
      urban_dashboard_ui
    }
  })
  
  # Navigation
  observeEvent(input$crime, {
    message("Navigating to Crime Dashboard at ", Sys.time())
    current_page("crime")
    active_content(NULL)
  })
  
  observeEvent(input$food, {
    message("Navigating to Food Dashboard at ", Sys.time())
    current_page("food")
    active_content(NULL)
  })
  
  observeEvent(input$urban, {
    message("Navigating to Urbanization Dashboard at ", Sys.time())
    current_page("urban")
    active_content(NULL)
  })
  
  observeEvent(input$back, {
    message("Navigating back to Main Dashboard at ", Sys.time())
    current_page("main")
    active_content(NULL)
  })
  
  observeEvent(input$back_food, {
    message("Navigating back to Main Dashboard from Food at ", Sys.time())
    current_page("main")
    active_content(NULL)
  })
  
  # Scroll navigation
  observeEvent(input$scroll_up, {
    shinyjs::runjs("window.scrollBy({ top: -window.innerHeight * 0.8, behavior: 'smooth' });")
  })
  
  observeEvent(input$scroll_down, {
    shinyjs::runjs("window.scrollBy({ top: window.innerHeight * 0.8, behavior: 'smooth' });")
  })
  
  observeEvent(input$scroll_up_food, {
    shinyjs::runjs("window.scrollBy({ top: -window.innerHeight, behavior: 'smooth' });")
  })
  
  observeEvent(input$scroll_down_food, {
    shinyjs::runjs("window.scrollBy({ top: window.innerHeight, behavior: 'smooth' });")
  })
  
  # --- Crime Dashboard Navigation ---
  observeEvent(input$overview_btn, {
    message("Crime Overview button clicked at ", Sys.time())
    active_content("overview")
  })
  
  observeEvent(input$prediction_btn, {
    message("Crime Prediction button clicked at ", Sys.time())
    active_content("prediction")
  })
  
  observeEvent(input$tables_btn, {
    message("Crime Tables button clicked at ", Sys.time())
    active_content("tables")
    table_page_crime(1)
  })
  
  # --- Urbanization Dashboard Navigation ---
  observeEvent(input$urban_overview_btn, {
    message("Urban Overview button clicked at ", Sys.time())
    active_content("urban_overview")
  })
  
  observeEvent(input$urban_prediction_btn, {
    message("Urban Prediction button clicked at ", Sys.time())
    active_content("urban_prediction")
  })
  
  observeEvent(input$urban_tables_btn, {
    message("Urban Tables button clicked at ", Sys.time())
    active_content("urban_tables")
    table_page_urban(1)
  })
  
  # --- Food Dashboard Navigation (Unchanged) ---
  observeEvent(input$food_overview_btn, {
    active_content("food_overview")
  })
  
  observeEvent(input$food_prediction_btn, {
    active_content("food_prediction")
  })
  
  observeEvent(input$food_tables_btn, {
    active_content("food_tables")
    food_table_page(1)
  })
  
  # --- Crime Dashboard Pagination ---
  observeEvent(input$crime_next_page, {
    df <- crime_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- table_page_crime()
    if (current < total_pages) {
      table_page_crime(current + 1)
    }
  })
  
  observe({
    df <- crime_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- table_page_crime()
    shinyjs::toggleState("crime_next_page", current < total_pages)
  })
  
  # --- Urbanization Dashboard Pagination ---
  observeEvent(input$urban_next_page, {
    df <- urban_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- table_page_urban()
    if (current < total_pages) {
      table_page_urban(current + 1)
    }
  })
  
  observe({
    df <- urban_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- table_page_urban()
    shinyjs::toggleState("urban_next_page", current < total_pages)
  })
  
  # --- Food Dashboard Pagination (Unchanged) ---
  observeEvent(input$food_next_page, {
    df <- food_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- food_table_page()
    if (current < total_pages) {
      food_table_page(current + 1)
    }
  })
  
  observe({
    df <- food_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- food_table_page()
    shinyjs::toggleState("food_next_page", current < total_pages)
  })
  
  
  
  # --- Urbanization Dashboard Models ---
  urban_models <- reactiveVal(NULL)
  urban_forecast_data <- reactiveVal(NULL)
  
  observe({
    message("Starting urban_models() at ", Sys.time())
    start_time <- Sys.time()
    df <- urban_data()
    
    yearly_pop <- df %>%
      select(Year, Total) %>%
      arrange(Year) %>%
      filter(!is.na(Total), Total > 0)
    
    validate(need(nrow(yearly_pop) >= 10, "Insufficient urbanization data for modeling."))
    
    # ARIMA Model
    arima_model <- tryCatch({
      message("Building ARIMA model...")
      ts_data <- ts(yearly_pop$Total, start = min(yearly_pop$Year), frequency = 1)
      model <- auto.arima(ts_data)
      fitted <- fitted(model)
      forecast <- forecast(model, h = 5)
      list(
        fitted = as.numeric(fitted),
        forecast = as.numeric(forecast$mean),
        lower = as.numeric(forecast$lower[,2]),  # 95% CI
        upper = as.numeric(forecast$upper[,2]),
        years = seq(max(yearly_pop$Year) + 1, max(yearly_pop$Year) + 5)
      )
    }, error = function(e) {
      message("ARIMA error: ", e$message)
      NULL
    })
    
    # LSTM Model (unchanged)
    lstm_model <- tryCatch({
      # ... existing LSTM code ...
    }, error = function(e) {
      message("LSTM error: ", e$message)
      NULL
    })
    
    if (!is.null(arima_model) || !is.null(lstm_model)) {
      urban_models(list(arima = arima_model, lstm = lstm_model))
      urban_forecast_data(list(
        arima = if (!is.null(arima_model)) {
          data.frame(
            Year = arima_model$years,
            Forecast = arima_model$forecast,
            Lower = arima_model$lower,
            Upper = arima_model$upper
          )
        } else NULL,
        lstm = if (!is.null(lstm_model)) {
          data.frame(
            Year = lstm_model$years,
            Forecast = lstm_model$forecast,
            Lower = lstm_model$lower,
            Upper = lstm_model$upper
          )
        } else NULL
      ))
      message("Urbanization models built.")
    }
    
    message("Urbanization models took: ", as.numeric(Sys.time() - start_time, units = "secs"), " seconds")
  })
  
  # Update the urban prediction plot to show ARIMA instead of Holt-Winters
  output$urban_prediction_plot <- renderPlotly({
    message("Rendering urban_prediction_plot at ", Sys.time())
    target_year <- input$urban_predict_year
    if (is.null(target_year) || is.na(target_year) || !is.numeric(target_year) || target_year < 1981 || target_year > 2030) {
      return(NULL)
    }
    
    models <- urban_models()
    forecast_data <- urban_forecast_data()
    historical_data <- urban_data()
    
    validate(
      need(!is.null(models) && !is.null(forecast_data), "Urban forecast models not available."),
      need(nrow(historical_data) > 0, "No historical population data for forecasting.")
    )
    
    fig <- plot_ly()
    
    # Historical Data
    fig <- fig %>% add_trace(
      x = historical_data$Year,
      y = historical_data$Total,
      type = "scatter",
      mode = "lines+markers",
      name = "Historical",
      line = list(color = "#4e79a7"),
      marker = list(color = "#4e79a7"),
      hovertemplate = "<b>Historical</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>"
    )
    
    # ARIMA Forecast
    if (!is.null(models$arima)) {
      fig <- fig %>% add_trace(
        x = forecast_data$arima$Year,
        y = forecast_data$arima$Forecast,
        type = "scatter",
        mode = "lines+markers",
        name = "ARIMA Forecast",
        line = list(color = "orange"),
        marker = list(color = "orange"),
        hovertemplate = "<b>ARIMA</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>"
      )
      
      # Confidence Intervals
      fig <- fig %>%
        add_trace(
          x = forecast_data$arima$Year,
          y = forecast_data$arima$Upper,
          type = "scatter",
          mode = "lines",
          line = list(width = 0, color = "rgba(255,165,0,0)"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          x = forecast_data$arima$Year,
          y = forecast_data$arima$Lower,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = "rgba(255,165,0,0.2)",
          line = list(width = 0, color = "rgba(255,165,0,0)"),
          name = "ARIMA 95% CI",
          hovertemplate = "<b>Confidence Interval</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>"
        )
    }
    
    # LSTM Forecast (unchanged)
    if (!is.null(models$lstm)) {
      # ... existing LSTM plotting code ...
    }
    
    fig %>% layout(
      title = list(
        text = paste("Population Forecast (up to", target_year, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = "Year",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      yaxis = list(
        title = "Population",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  # Update performance metrics to include ARIMA
  output$urban_performance_metrics <- renderText({
    message("Rendering urban performance_metrics at ", Sys.time())
    models <- urban_models()
    historical_data <- urban_data()
    
    if (is.null(models) || is.null(historical_data)) {
      return("Performance metrics not available.")
    }
    
    output <- "Model Performance Metrics:\n"
    
    if (!is.null(models$arima)) {
      actual <- historical_data$Total[(length(historical_data$Total) - length(models$arima$fitted) + 1):length(historical_data$Total)]
      predicted <- models$arima$fitted
      if (length(actual) == length(predicted)) {
        mape <- mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
        rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
        output <- paste(
          output,
          sprintf("ARIMA MAPE: %.2f%%\n", mape),
          sprintf("ARIMA RMSE: %.2f\n", rmse)
        )
      }
    }
    
    if (!is.null(models$lstm)) {
      output <- paste(
        output,
        sprintf("LSTM MAPE: %.2f%%\n", models$lstm$mape)
      )
    }
    
    output
  })
  
  # --- Crime Dashboard Outputs ---
  
  output$crime_total_crimes <- renderText({
    message("Rendering crime_total_crimes at ", Sys.time())
    df <- crime_data() %>% filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2])
    validate(need(nrow(df) > 0, "No crime data for selected years."))
    sum(df$Total, na.rm = TRUE) %>% format(big.mark = ",")
  })
  
  output$crime_avg_crimes <- renderText({
    message("Rendering crime_avg_crimes at ", Sys.time())
    df <- crime_data() %>% filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2])
    validate(need(nrow(df) > 0, "No crime data for selected years."))
    mean(df$Total, na.rm = TRUE) %>% round(0) %>% format(big.mark = ",")
  })
  
  output$crime_peak_year <- renderText({
    message("Rendering crime_peak_year at ", Sys.time())
    df <- crime_data() %>% filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2])
    validate(need(nrow(df) > 0, "No crime data for selected years."))
    df$Year[which.max(df$Total)]
  })
  
  output$crime_growth_rate <- renderText({
    message("Rendering crime_growth_rate at ", Sys.time())
    df <- crime_data() %>% filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2])
    validate(need(nrow(df) > 1, "Need at least two years for growth rate."))
    years <- sort(df$Year)
    values <- df$Total[order(df$Year)]
    if (length(values) < 2 || values[1] == 0) return("N/A")
    rate <- ((values[length(values)] / values[1]) - 1) * 100
    sprintf("%.2f%%", rate)
  })
  
  # --- Crime Dashboard Outputs ---
  
  output$crime_summary_subplots <- renderPlotly({
    message("Rendering crime_summary_subplots at ", Sys.time())
    plot_data <- crime_data() %>% 
      filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2])
    
    if (input$crime_subplot_select == "Total Crimes") {
      # Calculate total crimes by year
      plot_data <- plot_data %>%
        group_by(Year) %>%
        summarise(Total = sum(Total, na.rm = TRUE))
      
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Total,
        type = "scatter",
        mode = "lines+markers",
        name = "Total Crimes",
        line = list(color = "#4e79a7"),
        text = ~paste("Year:", Year, "<br>Total Crimes:", round(Total, 0)),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Total Crimes", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else {
      # For regional plots, sum by region
      plot_data <- plot_data %>%
        group_by(Year) %>%
        summarise(Value = sum(.data[[input$crime_subplot_select]], na.rm = TRUE))
      
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Value,
        type = "scatter",
        mode = "lines+markers",
        name = input$crime_subplot_select,
        line = list(color = "#f28e2b"),
        text = ~paste("Year:", Year, "<br>", input$crime_subplot_select, ":", round(Value, 0)),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = paste(input$crime_subplot_select, "Crimes"), gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    }
    
    fig %>% layout(
      title = list(text = paste("Trend in", input$crime_subplot_select), x = 0.5),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = FALSE
    )
  })
  
  output$crime_type_plot <- renderPlotly({
    message("Rendering crime_type_plot at ", Sys.time())
    data <- crime_data() %>%
      filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2])
    
    if (input$crime_type_select != "All Types") {
      data <- data %>% filter(Type.of.Crime == input$crime_type_select)
    }
    
    vivid_colors <- c(
      "#FF1F5B", "#00CD6C", "#009ADE", "#AF58BA", "#FFC107",
      "#F06292", "#4CAF50", "#1976D2", "#AB47BC", "#FF5722",
      "#E91E63", "#8BC34A", "#0288D1", "#673AB7", "#FF9800"
    )
    all_types <- unique(data$Type.of.Crime)
    colors <- rep(vivid_colors, length.out = length(all_types))
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% layout(
        title = "No Data Available",
        template = "plotly_dark",
        plot_bgcolor = "rgba(0,0,0,0.6)",
        paper_bgcolor = "rgba(0,0,0,0.6)"
      ))
    }
    
    if (input$crime_plot_type == "line") {
      fig <- plot_ly(
        data,
        x = ~Year,
        y = ~Total,
        color = ~Type.of.Crime,
        colors = colors,
        type = "scatter",
        mode = "lines+markers",
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Crime Count: %{y:.0f}<extra></extra>"
      )
    } else if (input$crime_plot_type == "bar") {
      fig <- plot_ly(
        data,
        x = ~Year,
        y = ~Total,
        color = ~Type.of.Crime,
        colors = colors,
        type = "bar",
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Crime Count: %{y:.0f}<extra></extra>"
      )
    } else if (input$crime_plot_type == "box") {
      fig <- plot_ly(
        data,
        y = ~Type.of.Crime,
        x = ~Total,
        color = ~Type.of.Crime,
        colors = colors,
        type = "box",
        orientation = "h",
        hovertemplate = "<b>%{fullData.name}</b><br>Crime Count: %{x:.0f}<extra></extra>"
      )
    }
    
    fig %>% layout(
      title = list(
        text = paste("Crime Statistics (", input$crime_type_select, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = ifelse(input$crime_plot_type == "box", "Crime Count", "Year"),
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white"),
        tickangle = 0
      ),
      yaxis = list(
        title = ifelse(input$crime_plot_type == "box", "Crime Type", "Crime Count"),
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(size = 10, color = "white")
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "x unified"
    )
  })
  
  output$crime_category_bar_animated <- renderPlotly({
    message("Rendering crime_category_bar_animated at ", Sys.time())
    plot_data <- crime_data() %>%
      filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2]) %>%
      group_by(Year, Type.of.Crime) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")
    
    all_types <- unique(plot_data$Type.of.Crime)
    all_years <- seq(min(plot_data$Year), max(plot_data$Year))
    plot_data <- plot_data %>%
      tidyr::complete(Year = all_years, Type.of.Crime = all_types, fill = list(Total = 0))
    
    vivid_colors <- c(
      "#FF1F5B", "#00CD6C", "#009ADE", "#AF58BA", "#FFC107",
      "#F06292", "#4CAF50", "#1976D2", "#AB47BC", "#FF5722"
    )
    colors <- rep(vivid_colors, length.out = length(all_types))
    
    fig <- plot_ly(
      data = plot_data,
      x = ~Total,
      y = ~Type.of.Crime,
      type = "bar",
      color = ~Type.of.Crime,
      colors = colors,
      frame = ~Year,
      customdata = ~Year,
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Year: %{customdata}<br>",
        "Crime Count: %{x:.0f}<extra></extra>"
      ),
      hoverlabel = list(
        font = list(color = "black", size = 12),
        bgcolor = "white",
        bordercolor = "black"
      )
    ) %>%
      layout(
        title = list(
          text = paste("Crime Trends by Type (", input$crime_year_range[1], "-", input$crime_year_range[2], ")"),
          x = 0.5
        ),
        xaxis = list(
          title = "Crime Count",
          gridcolor = "rgba(255,255,255,0.2)",
          titlefont = list(color = "white"),
          tickfont = list(color = "white")
        ),
        yaxis = list(
          title = "Crime Type",
          autorange = "reversed",
          tickfont = list(size = 10, color = "white")
        ),
        template = "plotly_dark",
        font = list(color = "white", size = 12),
        plot_bgcolor = "rgba(0,0,0,0.6)",
        paper_bgcolor = "rgba(0,0,0,0.6)",
        showlegend = FALSE,
        hovermode = "closest"
      ) %>%
      animation_opts(
        frame = 300,
        transition = 300,
        easing = "cubic-in-out"
      )
    
    fig
  })
  
  output$crime_category_stacked_area <- renderPlotly({
    message("Rendering crime_category_stacked_area at ", Sys.time())
    df <- crime_data() %>%
      filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2]) %>%
      group_by(Year, Type.of.Crime) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Type.of.Crime, values_from = Total, values_fill = 0)
    
    types <- setdiff(colnames(df), "Year")
    colors <- brewer.pal(min(length(types), 9), "Pastel1")
    if (length(types) > 9) {
      colors <- c(colors, brewer.pal(min(length(types) - 9, 8), "Pastel2"))
    }
    
    fig <- plot_ly()
    for (i in seq_along(types)) {
      fig <- fig %>% add_trace(
        x = df$Year,
        y = df[[types[i]]],
        type = "scatter",
        mode = "lines",
        name = types[i],
        stackgroup = "one",
        line = list(width = 0),
        fill = "tonexty",
        fillcolor = colors[i],
        hovertemplate = "<b>%{data.name}</b><br>Year: %{x}<br>Crime Count: %{y:.0f}<extra></extra>",
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black")
      )
    }
    
    fig %>% layout(
      title = list(text = "Crime by Type Over Time", x = 0.5),
      xaxis = list(
        title = "Year",
        tickmode = "linear",
        range = c(min(df$Year) - 0.5, max(df$Year) + 0.5),
        gridcolor = "rgba(255,255,255,0.2)"
      ),
      yaxis = list(
        title = "Crime Count",
        gridcolor = "rgba(255,255,255,0.2)"
      ),
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.7,
        xanchor = "center",
        x = 0.5,
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.5)"
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  output$crime_category_heatmap <- renderPlotly({
    message("Rendering crime_category_heatmap at ", Sys.time())
    plot_data <- crime_data() %>%
      filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2]) %>%
      group_by(Year, Type.of.Crime) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")
    
    pivot_data <- plot_data %>%
      tidyr::pivot_wider(names_from = Year, values_from = Total, values_fill = 0) %>%
      arrange(Type.of.Crime)
    
    types <- pivot_data$Type.of.Crime
    years <- as.numeric(colnames(pivot_data)[-1])
    z_values <- as.matrix(pivot_data[, -1])
    
    fig <- plot_ly(
      x = years,
      y = types,
      z = z_values,
      type = "heatmap",
      colorscale = "Plasma",
      colorbar = list(title = "Crime Count"),
      text = matrix(paste("Type:", types, "<br>Year:", rep(years, each = length(types)),
                          "<br>Crime Count:", round(z_values, 0)), nrow = length(types)),
      hoverinfo = "text"
    )
    
    fig %>% layout(
      title = list(text = "Crime Heatmap by Type and Year", x = 0.5),
      xaxis = list(title = "Year", tickmode = "linear", range = c(input$crime_year_range[1] - 0.5, input$crime_year_range[2] + 0.5)),
      yaxis = list(title = "Crime Type", tickfont = list(size = 10), automargin = TRUE),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      hovermode = "closest"
    )
  })
  
  output$crime_category_pie <- renderPlotly({
    message("Rendering crime_category_pie at ", Sys.time())
    yearly_data <- crime_data() %>%
      filter(Year >= input$crime_year_range[1] & Year <= input$crime_year_range[2]) %>%
      group_by(Year, Type.of.Crime) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")
    
    avg_data <- yearly_data %>%
      group_by(Type.of.Crime) %>%
      summarise(Total = mean(Total, na.rm = TRUE), .groups = "drop")
    
    years <- sort(unique(yearly_data$Year))
    types <- unique(yearly_data$Type.of.Crime)
    
    colors <- brewer.pal(min(length(types), 8), "Set1")
    if (length(types) > 8) {
      colors <- c(colors, brewer.pal(min(length(types) - 8, 8), "Dark2"))
    }
    
    fig <- plot_ly()
    for (i in seq_along(years)) {
      year_data <- yearly_data %>% filter(Year == years[i])
      fig <- fig %>% add_trace(
        type = "pie",
        labels = year_data$Type.of.Crime,
        values = year_data$Total,
        textinfo = "percent+label",
        textposition = "auto",
        hovertemplate = "<b>%{label}</b><br>Crime Count: %{value:.0f}<br>Percentage: %{percent:.2%}<extra></extra>",
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black"),
        marker = list(colors = colors[1:length(year_data$Type.of.Crime)], line = list(color = "white", width = 1)),
        pull = rep(0.02, length(year_data$Type.of.Crime)),
        rotation = 90,
        sort = FALSE,
        visible = (years[i] == years[1])
      )
    }
    
    fig <- fig %>% add_trace(
      type = "pie",
      labels = avg_data$Type.of.Crime,
      values = avg_data$Total,
      textinfo = "percent+label",
      textposition = "auto",
      hovertemplate = "<b>%{label}</b><br>Crime Count: %{value:.0f}<br>Percentage: %{percent:.2%}<extra></extra>",
      hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black"),
      marker = list(colors = colors[1:length(avg_data$Type.of.Crime)], line = list(color = "white", width = 1)),
      pull = rep(0.02, length(avg_data$Type.of.Crime)),
      rotation = 90,
      sort = FALSE,
      visible = FALSE
    )
    
    buttons <- lapply(seq_along(years), function(i) {
      visible <- rep(FALSE, length(years) + 1)
      visible[i] <- TRUE
      list(
        label = as.character(years[i]),
        method = "update",
        args = list(list(visible = visible), list(title = paste("Crime Proportion by Type in", years[i])))
      )
    })
    buttons <- c(buttons, list(
      list(
        label = "Average",
        method = "update",
        args = list(list(visible = c(rep(FALSE, length(years)), TRUE)),
                    list(title = "Average Crime Proportion by Type"))
      )
    ))
    
    fig %>% layout(
      title = list(text = paste("Crime Proportion by Type in", years[1]), x = 0.5, y = 0.95),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      legend = list(orientation = "v", x = 1.1, xanchor = "left", y = 0.5, yanchor = "middle",
                    font = list(size = 10), bgcolor = "rgba(255,255,255,0.5)"),
      margin = list(t = 100, b = 50, l = 50, r = 150),
      hovermode = "closest",
      updatemenus = list(
        list(
          buttons = buttons,
          direction = "down",
          showactive = TRUE,
          x = 0.05,
          xanchor = "left",
          y = 1.1,
          yanchor = "top"
        )
      )
    )
  })
  
  # Reactive values for prediction results and models
  # Reactive values for prediction results and models
  crime_prediction_result <- reactiveVal(NULL)
  crime_models <- reactiveVal(NULL)
  
  # Crime Dashboard Prophet Model
  crime_prophet_model <- reactiveVal(NULL)
  crime_forecast_data <- reactiveVal(NULL)
  
  observe({
    message("Starting crime_prophet_model at ", Sys.time())
    start_time <- Sys.time()
    df <- crime_data()
    
    yearly_crime <- tryCatch({
      provinces <- c("Punjab", "Sindh", "KPK", "Balochistan", "Islamabad", "Railways", "G.B", "AJK")
      message("Preparing crime data for Prophet.")
      yearly_crime <- df %>%
        mutate(Total = rowSums(select(., all_of(provinces)), na.rm = TRUE)) %>%
        group_by(Year) %>%
        summarise(Total = sum(Total, na.rm = TRUE)) %>%
        rename(ds = Year, y = Total) %>%
        mutate(ds = as.Date(paste0(ds, "-01-01"))) %>%
        filter(!is.na(ds), !is.na(y), y >= 0)
      validate(need(nrow(yearly_crime) >= 2, "Insufficient data for Prophet model."))
      yearly_crime
    }, error = function(e) {
      message("Error preparing Prophet data: ", e$message)
      NULL
    })
    
    if (is.null(yearly_crime)) {
      message("Failed to prepare Prophet data.")
      return()
    }
    
    # Ensure crime counts are whole numbers
    yearly_crime <- yearly_crime %>% 
      mutate(y = round(y, 0))
    
    m <- tryCatch({
      model <- prophet(
        yearly_crime,
        changepoint.prior.scale = 0.5,
        n.changepoints = 25,
        yearly.seasonality = TRUE,
        seasonality.mode = "additive"
      )
      message("Prophet model built successfully.")
      model
    }, error = function(e) {
      message("Prophet error: ", e$message)
      NULL
    })
    
    if (!is.null(m)) {
      future <- make_future_dataframe(m, periods = 10, freq = "year")
      forecast <- predict(m, future) %>% 
        mutate(
          yhat = round(yhat, 0),
          yhat_lower = round(yhat_lower, 0),
          yhat_upper = round(yhat_upper, 0)
        )
      crime_prophet_model(list(model = m, data = yearly_crime))
      crime_forecast_data(forecast)
      crime_models(list(prophet = m))
      message("Crime forecast generated: ", capture.output(head(forecast)))
    }
    
    message("Crime model took: ", as.numeric(Sys.time() - start_time, units = "secs"), " seconds")
  })
  
  # Prediction event with Monte Carlo
  observeEvent(input$predict, {
    message("Crime Predict button clicked at ", Sys.time(), ": Year = ", input$predict_year)
    shinyjs::runjs("document.getElementById('loading-spinner').classList.add('active');")
    
    year <- input$predict_year
    
    if (is.null(year) || is.na(year) || !is.numeric(year) || year < 2012 || year > 2030) {
      output$prediction_error <- renderText("Invalid year. Please enter a year between 2012 and 2030.")
      crime_prediction_result(data.frame(Error = "Invalid year"))
    } else {
      tryCatch({
        df <- crime_data()
        model_data <- crime_prophet_model()
        
        if (is.null(model_data) || nrow(model_data$data) == 0) {
          output$prediction_error <- renderText("Error: No Prophet model or data available.")
          crime_prediction_result(data.frame(Error = "No model available"))
          return()
        }
        
        # Prepare yearly crime data for growth calculation
        yearly_crime <- df %>%
          mutate(Total = round(rowSums(select(., all_of(c("Punjab", "Sindh", "KPK", "Balochistan", "Islamabad", "Railways", "G.B", "AJK"))), na.rm = TRUE), 0)) %>%
          group_by(Year) %>%
          summarise(Total = sum(Total, na.rm = TRUE))
        
        # Calculate growth stats for Monte Carlo
        growth_stats <- list(
          Total = {
            growth <- diff(yearly_crime$Total) / lag(yearly_crime$Total)[-1]
            growth <- growth[is.finite(growth)]
            list(mean = mean(growth, na.rm = TRUE), sd = sd(growth, na.rm = TRUE))
          }
        )
        
        n_years <- year - max(yearly_crime$Year)
        if (n_years <= 0) {
          output$prediction_error <- renderText("Selected year must be after the last historical year.")
          crime_prediction_result(data.frame(Error = "Invalid forecast year"))
          return()
        }
        
        n_simulations <- 1000
        last_total <- tail(yearly_crime$Total, 1)
        
        # Monte Carlo simulation
        simulated_results <- matrix(NA, nrow = n_years, ncol = n_simulations)
        for (sim in 1:n_simulations) {
          current_total <- last_total
          future_totals <- numeric(n_years)
          for (year_offset in 1:n_years) {
            growth <- rnorm(1, mean = growth_stats$Total$mean, sd = growth_stats$Total$sd)
            current_total <- round(pmax(0, current_total * (1 + growth)), 0)  # Ensure non-negative
            future_totals[year_offset] <- current_total
          }
          simulated_results[, sim] <- future_totals
        }
        
        mean_forecast <- mean(simulated_results[n_years, ], na.rm = TRUE)
        lower_bound <- quantile(simulated_results[n_years, ], probs = 0.05, na.rm = TRUE)
        upper_bound <- quantile(simulated_results[n_years, ], probs = 0.95, na.rm = TRUE)
        
        forecast_year <- data.frame(
          Year = year,
          Prophet_Estimated = round(mean_forecast, 0),
          Prophet_Lower = round(lower_bound, 0),
          Prophet_Upper = round(upper_bound, 0)
        )
        
        if (!all(is.na(forecast_year[, -1]))) {
          crime_prediction_result(forecast_year)
          output$prediction_error <- renderText("")
        } else {
          output$prediction_error <- renderText("No prediction available for the selected year.")
          crime_prediction_result(data.frame(Error = "No prediction available"))
        }
        
      }, error = function(e) {
        message("Crime Prediction error: ", e$message)
        output$prediction_error <- renderText(paste("Prediction failed:", e$message))
        crime_prediction_result(data.frame(Error = "Prediction failed"))
      })
    }
    
    shinyjs::runjs("document.getElementById('loading-spinner').classList.remove('active');")
  })
  
  # Clear prediction
  observeEvent(input$clear, {
    message("Crime Clear button clicked at ", Sys.time())
    updateNumericInput(session, "predict_year", value = NULL)
    output$prediction_error <- renderText("")
    crime_prediction_result(NULL)
    crime_models(NULL)
    showNotification("Crime input cleared.", type = "message")
  })
  
  # Crime Prediction Plot
  output$crime_prediction_plot <- renderPlotly({
    message("Rendering crime_prediction_plot at ", Sys.time())
    target_year <- input$predict_year
    if (is.null(target_year) || is.na(target_year) || !is.numeric(target_year) || target_year < 2012 || target_year > 2030) {
      return(NULL)
    }
    
    df <- crime_data()
    
    validate(
      need(nrow(df) > 0, "No crime data available.")
    )
    
    # Prepare historical data
    yearly_crime <- df %>%
      mutate(Total = round(rowSums(select(., all_of(c("Punjab", "Sindh", "KPK", "Balochistan", "Islamabad", "Railways", "G.B", "AJK"))), na.rm = TRUE), 0)) %>%
      group_by(Year) %>%
      summarise(Total = sum(Total, na.rm = TRUE))
    
    # Monte Carlo simulation for plot
    growth_stats <- list(
      Total = {
        growth <- diff(yearly_crime$Total) / lag(yearly_crime$Total)[-1]
        growth <- growth[is.finite(growth)]
        list(mean = mean(growth, na.rm = TRUE), sd = sd(growth, na.rm = TRUE))
      }
    )
    
    n_years <- target_year - max(yearly_crime$Year)
    if (n_years <= 0) {
      return(NULL)
    }
    
    n_simulations <- 1000
    last_total <- tail(yearly_crime$Total, 1)
    
    simulated_results <- matrix(NA, nrow = n_years, ncol = n_simulations)
    for (sim in 1:n_simulations) {
      current_total <- last_total
      future_totals <- numeric(n_years)
      for (year_offset in 1:n_years) {
        growth <- rnorm(1, mean = growth_stats$Total$mean, sd = growth_stats$Total$sd)
        current_total <- round(pmax(0, current_total * (1 + growth)), 0)
        future_totals[year_offset] <- current_total
      }
      simulated_results[, sim] <- future_totals
    }
    
    future_years <- (max(yearly_crime$Year) + 1):(max(yearly_crime$Year) + n_years)
    mean_forecast <- rowMeans(simulated_results, na.rm = TRUE)
    lower_bound <- apply(simulated_results, 1, quantile, probs = 0.05, na.rm = TRUE)
    upper_bound <- apply(simulated_results, 1, quantile, probs = 0.95, na.rm = TRUE)
    
    # Create plot
    fig <- plot_ly() %>%
      add_trace(
        x = yearly_crime$Year,
        y = yearly_crime$Total,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical",
        line = list(color = "#4e79a7"),
        marker = list(color = "#4e79a7"),
        hovertemplate = "<b>Historical</b><br>Year: %{x}<br>Crime Count: %{y}<extra></extra>"
      ) %>%
      add_trace(
        x = future_years,
        y = mean_forecast,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(color = "orange"),
        marker = list(color = "orange"),
        hovertemplate = "<b>Forecast</b><br>Year: %{x}<br>Crime Count: %{y}<extra></extra>"
      )
    
    if (n_years > 1) {
      fig <- fig %>%
        add_trace(
          x = future_years,
          y = upper_bound,
          type = "scatter",
          mode = "lines",
          line = list(width = 0, color = "rgba(255,165,0,0)"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          x = future_years,
          y = lower_bound,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = "rgba(255,165,0,0.2)",
          line = list(width = 0, color = "rgba(255,165,0,0)"),
          name = "90% Confidence Interval",
          hovertemplate = "<b>Confidence Interval</b><br>Year: %{x}<br>Crime Count: %{y}<extra></extra>"
        )
    }
    
    fig %>% layout(
      title = list(
        text = paste("Crime Forecast (up to", target_year, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = "Year",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      yaxis = list(
        title = "Crime Count",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white"),
        tickformat = ","  # Whole numbers with commas
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  # Crime Prediction Result Table
  output$prediction_result_table <- renderDT({
    message("Rendering crime_prediction_result_table at ", Sys.time())
    result <- crime_prediction_result()
    validate(need(!is.null(result), "No prediction results available."))
    datatable(
      result,
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        paging = FALSE,
        info = FALSE,
        columnDefs = list(
          list(
            targets = 1:3,  # Prophet_Estimated, Prophet_Lower, Prophet_Upper
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && typeof data === 'number' ?",
              "    Math.round(data) : data;",
              "}"
            )
          )
        )
      ),
      class = "table-dark",
      style = "bootstrap5",
      rownames = FALSE,
      caption = "Crime Prediction Results"
    )
  })
  # Crime Data Table (paginated)
  output$crime_data_table <- renderDT({
    message("Crime data rows: ", nrow(crime_data())) # Debug
    datatable(
      crime_data(),
      options = list(
        scrollX = TRUE,
        responsive = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && typeof data === 'number' ?",
              "    parseFloat(data).toFixed(2) : data;",
              "}"
            )
          )
        ),
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE,
        dom = 'lfrtip'
      ),
      class = "display nowrap",
      style = "bootstrap"
    )
  })
  
  
  # --- Urbanization Dashboard Outputs ---
  
  output$urban_total_population <- renderText({
    message("Rendering urban_total_population at ", Sys.time())
    df <- urban_data() %>% filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2])
    validate(need(nrow(df) > 0, "No population data for selected years."))
    sum(df$Total, na.rm = TRUE) %>% format(big.mark = ",")
  })
  
  output$urban_avg_population <- renderText({
    message("Rendering urban_avg_population at ", Sys.time())
    df <- urban_data() %>% filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2])
    validate(need(nrow(df) > 0, "No population data for selected years."))
    mean(df$Total, na.rm = TRUE) %>% round(0) %>% format(big.mark = ",")
  })
  
  output$urban_peak_year <- renderText({
    message("Rendering urban_peak_year at ", Sys.time())
    df <- urban_data() %>% filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2])
    validate(need(nrow(df) > 0, "No population data for selected years."))
    df$Year[which.max(df$Total)]
  })
  
  output$urban_growth_rate <- renderText({
    message("Rendering urban_growth_rate at ", Sys.time())
    df <- urban_data() %>% filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2])
    validate(need(nrow(df) > 1, "Need at least two years for growth rate."))
    years <- sort(df$Year)
    values <- df$Total[order(df$Year)]
    if (length(values) < 2 || values[1] == 0) return("N/A")
    rate <- ((values[length(values)] / values[1]) - 1) * 100
    sprintf("%.2f%%", rate)
  })
  
  output$urban_summary_subplots <- renderPlotly({
    message("Rendering urban_summary_subplots at ", Sys.time())
    plot_data <- urban_data() %>% 
      filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2])
    
    if (input$urban_subplot_select == "Total Population") {
      # Calculate total population by year
      plot_data <- plot_data %>%
        group_by(Year) %>%
        summarise(Total = sum(Total, na.rm = TRUE))
      
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Total,
        type = "scatter",
        mode = "lines+markers",
        name = "Total Population",
        line = list(color = "#4e79a7"),
        text = ~paste("Year:", Year, "<br>Total Population:", round(Total, 0)),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Total Population", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else {
      # For other metrics, sum by year
      plot_data <- plot_data %>%
        group_by(Year) %>%
        summarise(Value = sum(.data[[input$urban_subplot_select]], na.rm = TRUE))
      
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Value,
        type = "scatter",
        mode = "lines+markers",
        name = input$urban_subplot_select,
        line = list(color = "#f28e2b"),
        text = ~paste("Year:", Year, "<br>", input$urban_subplot_select, ":", round(Value, 0)),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = paste(input$urban_subplot_select, "Population"), gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    }
    
    fig %>% layout(
      title = list(text = paste("Trend in", input$urban_subplot_select), x = 0.5),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = FALSE
    )
  })
  
  output$urban_region_plot <- renderPlotly({
    message("Rendering urban_region_plot at ", Sys.time())
    data <- urban_data() %>%
      filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2])
    
    if (input$urban_region_select != "All Regions") {
      data <- data %>% select(Year, all_of(input$urban_region_select))
      colnames(data)[2] <- "Value"
    } else {
      data <- data %>% select(Year, Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`)
    }
    
    vivid_colors <- c(
      "#FF1F5B", "#00CD6C", "#009ADE", "#AF58BA", "#FFC107",
      "#F06292", "#4CAF50", "#1976D2", "#AB47BC", "#FF5722"
    )
    all_regions <- colnames(data)[-1]
    colors <- rep(vivid_colors, length.out = length(all_regions))
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% layout(
        title = "No Data Available",
        template = "plotly_dark",
        plot_bgcolor = "rgba(0,0,0,0.6)",
        paper_bgcolor = "rgba(0,0,0,0.6)"
      ))
    }
    
    if (input$urban_plot_type == "line") {
      fig <- plot_ly()
      for (i in seq_along(all_regions)) {
        fig <- fig %>% add_trace(
          x = data$Year,
          y = data[[all_regions[i]]],
          type = "scatter",
          mode = "lines+markers",
          name = all_regions[i],
          line = list(color = colors[i]),
          hovertemplate = paste("<b>", all_regions[i], "</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>")
        )
      }
    } else if (input$urban_plot_type == "bar") {
      fig <- plot_ly()
      for (i in seq_along(all_regions)) {
        fig <- fig %>% add_trace(
          x = data$Year,
          y = data[[all_regions[i]]],
          type = "bar",
          name = all_regions[i],
          marker = list(color = colors[i]),
          hovertemplate = paste("<b>", all_regions[i], "</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>")
        )
      }
    } else if (input$urban_plot_type == "box") {
      fig <- plot_ly()
      for (i in seq_along(all_regions)) {
        fig <- fig %>% add_trace(
          y = all_regions[i],
          x = data[[all_regions[i]]],
          type = "box",
          name = all_regions[i],
          marker = list(color = colors[i]),
          orientation = "h",
          hovertemplate = paste("<b>", all_regions[i], "</b><br>Population: %{x:.0f}<extra></extra>")
        )
      }
    }
    
    fig %>% layout(
      title = list(
        text = paste("Population Statistics (", input$urban_region_select, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = ifelse(input$urban_plot_type == "box", "Population", "Year"),
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white"),
        tickangle = 0
      ),
      yaxis = list(
        title = ifelse(input$urban_plot_type == "box", "Region", "Population"),
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(size = 10, color = "white")
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "x unified"
    )
  })
  
  output$urban_category_bar_animated <- renderPlotly({
    message("Rendering urban_category_bar_animated at ", Sys.time())
    plot_data <- urban_data() %>%
      filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2]) %>%
      select(Year, Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`) %>%
      pivot_longer(cols = -Year, names_to = "Region", values_to = "Population")
    
    all_regions <- unique(plot_data$Region)
    all_years <- seq(min(plot_data$Year), max(plot_data$Year))
    plot_data <- plot_data %>%
      tidyr::complete(Year = all_years, Region = all_regions, fill = list(Population = 0))
    
    vivid_colors <- c(
      "#FF1F5B", "#00CD6C", "#009ADE", "#AF58BA", "#FFC107",
      "#F06292", "#4CAF50", "#1976D2", "#AB47BC", "#FF5722"
    )
    colors <- rep(vivid_colors, length.out = length(all_regions))
    
    fig <- plot_ly(
      data = plot_data,
      x = ~Population,
      y = ~Region,
      type = "bar",
      color = ~Region,
      colors = colors,
      frame = ~Year,
      customdata = ~Year,
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Year: %{customdata}<br>",
        "Population: %{x:.0f}<extra></extra>"
      ),
      hoverlabel = list(
        font = list(color = "black", size = 12),
        bgcolor = "white",
        bordercolor = "black"
      )
    ) %>%
      layout(
        title = list(
          text = paste("Population Trends by Region (", input$urban_year_range[1], "-", input$urban_year_range[2], ")"),
          x = 0.5
        ),
        xaxis = list(
          title = "Population",
          gridcolor = "rgba(255,255,255,0.2)",
          titlefont = list(color = "white"),
          tickfont = list(color = "white")
        ),
        yaxis = list(
          title = "Region",
          autorange = "reversed",
          tickfont = list(size = 10, color = "white")
        ),
        template = "plotly_dark",
        font = list(color = "white", size = 12),
        plot_bgcolor = "rgba(0,0,0,0.6)",
        paper_bgcolor = "rgba(0,0,0,0.6)",
        showlegend = FALSE,
        hovermode = "closest"
      ) %>%
      animation_opts(
        frame = 300,
        transition = 300,
        easing = "cubic-in-out"
      )
    
    fig
  })
  
  output$urban_category_stacked_area <- renderPlotly({
    message("Rendering urban_category_stacked_area at ", Sys.time())
    df <- urban_data() %>%
      filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2]) %>%
      select(Year, Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`)
    
    regions <- setdiff(colnames(df), "Year")
    colors <- brewer.pal(min(length(regions), 9), "Pastel1")
    if (length(regions) > 9) {
      colors <- c(colors, brewer.pal(min(length(regions) - 9, 8), "Pastel2"))
    }
    
    fig <- plot_ly()
    for (i in seq_along(regions)) {
      fig <- fig %>% add_trace(
        x = df$Year,
        y = df[[regions[i]]],
        type = "scatter",
        mode = "lines",
        name = regions[i],
        stackgroup = "one",
        line = list(width = 0),
        fill = "tonexty",
        fillcolor = colors[i],
        hovertemplate = "<b>%{data.name}</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>",
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black")
      )
    }
    
    fig %>% layout(
      title = list(text = "Population by Region Over Time", x = 0.5),
      xaxis = list(
        title = "Year",
        tickmode = "linear",
        range = c(min(df$Year) - 0.5, max(df$Year) + 0.5),
        gridcolor = "rgba(255,255,255,0.2)"
      ),
      yaxis = list(
        title = "Population",
        gridcolor = "rgba(255,255,255,0.2)"
      ),
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.7,
        xanchor = "center",
        x = 0.5,
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.5)"
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  output$urban_category_heatmap <- renderPlotly({
    message("Rendering urban_category_heatmap at ", Sys.time())
    plot_data <- urban_data() %>%
      filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2]) %>%
      select(Year, Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`) %>%
      pivot_longer(cols = -Year, names_to = "Region", values_to = "Population")
    
    pivot_data <- plot_data %>%
      tidyr::pivot_wider(names_from = Year, values_from = Population, values_fill = 0) %>%
      arrange(Region)
    
    regions <- pivot_data$Region
    years <- as.numeric(colnames(pivot_data)[-1])
    z_values <- as.matrix(pivot_data[, -1])
    
    fig <- plot_ly(
      x = years,
      y = regions,
      z = z_values,
      type = "heatmap",
      colorscale = "Plasma",
      colorbar = list(title = "Population"),
      text = matrix(paste("Region:", regions, "<br>Year:", rep(years, each = length(regions)),
                          "<br>Population:", round(z_values, 0)), nrow = length(regions)),
      hoverinfo = "text"
    )
    
    fig %>% layout(
      title = list(text = "Population Heatmap by Region and Year", x = 0.5),
      xaxis = list(title = "Year", tickmode = "linear", range = c(input$urban_year_range[1] - 0.5, input$urban_year_range[2] + 0.5)),
      yaxis = list(title = "Region", tickfont = list(size = 10), automargin = TRUE),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      hovermode = "closest"
    )
  })
  
  output$urban_category_pie <- renderPlotly({
    message("Rendering urban_category_pie at ", Sys.time())
    yearly_data <- urban_data() %>%
      filter(Year >= input$urban_year_range[1] & Year <= input$urban_year_range[2]) %>%
      select(Year, Islamabad, Punjab, Sindh, KPK, Balochistan, `Azad Kashmir`, `Northern Areas`, `Tribal Areas`) %>%
      pivot_longer(cols = -Year, names_to = "Region", values_to = "Population")
    
    avg_data <- yearly_data %>%
      group_by(Region) %>%
      summarise(Population = mean(Population, na.rm = TRUE), .groups = "drop")
    
    years <- sort(unique(yearly_data$Year))
    regions <- unique(yearly_data$Region)
    
    colors <- brewer.pal(min(length(regions), 8), "Set1")
    if (length(regions) > 8) {
      colors <- c(colors, brewer.pal(min(length(regions) - 8, 8), "Dark2"))
    }
    
    fig <- plot_ly()
    for (i in seq_along(years)) {
      year_data <- yearly_data %>% filter(Year == years[i])
      fig <- fig %>% add_trace(
        type = "pie",
        labels = year_data$Region,
        values = year_data$Population,
        textinfo = "percent+label",
        textposition = "auto",
        hovertemplate = "<b>%{label}</b><br>Population: %{value:.0f}<br>Percentage: %{percent:.2%}<extra></extra>",
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black"),
        marker = list(colors = colors[1:length(year_data$Region)], line = list(color = "white", width = 1)),
        pull = rep(0.02, length(year_data$Region)),
        rotation = 45,
        sort = FALSE,
        visible = (years[i] == years[1])
      )
    }
    
    fig <- fig %>% add_trace(
      type = "pie",
      labels = avg_data$Region,
      values = avg_data$Population,
      textinfo = "percent+label",
      textposition = "auto",
      hovertemplate = "<b>%{label}</b><br>Population: %{value:.0f}<br>Percentage: %{percent:.2%}<extra></extra>",
      hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black"),
      marker = list(colors = colors[1:length(avg_data$Region)], line = list(color = "white", width = 1)),
      pull = rep(0.02, length(avg_data$Region)),
      rotation = 45,
      sort = FALSE,
      visible = FALSE
    )
    
    buttons <- lapply(seq_along(years), function(i) {
      visible <- rep(FALSE, length(years) + 1)
      visible[i] <- TRUE
      list(
        label = as.character(years[i]),
        method = "update",
        args = list(list(visible = visible), list(title = paste("Population Proportion by Region in", years[i])))
      )
    })
    buttons <- c(buttons, list(
      list(
        label = "Average",
        method = "update",
        args = list(list(visible = c(rep(FALSE, length(years)), TRUE)),
                    list(title = "Average Population Proportion by Region"))
      )
    ))
    
    fig %>% layout(
      title = list(text = paste("Population Proportion by Region in", years[1]), x = 0.5),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      legend = list(orientation = "v", x = 1.1, xanchor = "left", y = 0.5, yanchor = "middle",
                    font = list(size = 10), bgcolor = "rgba(255,255,255,0.5)"),
      margin = list(t = 80, b = 50, l = 50, r = 150),
      hovermode = "closest",
      updatemenus = list(
        list(
          buttons = buttons,
          direction = "down",
          showactive = TRUE,
          x = 0.05,
          xanchor = "left",
          y = 1.1,
          yanchor = "top"
        )
      )
    )
  })
  
  output$urban_prediction_plot <- renderPlotly({
    message("Rendering urban_prediction_plot at ", Sys.time())
    target_year <- input$urban_predict_year
    if (is.null(target_year) || is.na(target_year) || !is.numeric(target_year) || target_year < 1981 || target_year > 2030) {
      return(NULL)
    }
    
    historical_data <- urban_data()
    
    validate(
      need(nrow(historical_data) > 0, "No historical population data for forecasting.")
    )
    
    # Calculate historical trend (linear regression for simplicity)
    trend_model <- lm(Total ~ Year, data = historical_data)
    historical_mean <- mean(historical_data$Total, na.rm = TRUE)
    historical_sd <- sd(historical_data$Total, na.rm = TRUE)
    
    # Generate future years up to target_year
    future_years <- seq(max(historical_data$Year) + 1, target_year, by = 1)
    n_simulations <- 1000  # Number of Monte Carlo simulations
    
    # Monte Carlo simulation: predict population with random variations
    set.seed(123)  # For reproducibility
    mc_forecasts <- matrix(NA, nrow = length(future_years), ncol = n_simulations)
    for (i in 1:n_simulations) {
      noise <- rnorm(length(future_years), mean = 0, sd = historical_sd * 0.1)  # 10% of historical SD
      mc_forecasts[, i] <- predict(trend_model, newdata = data.frame(Year = future_years)) + noise
    }
    
    # Calculate mean forecast and 95% confidence intervals
    forecast_data <- data.frame(
      Year = future_years,
      Forecast = rowMeans(mc_forecasts),
      Lower = apply(mc_forecasts, 1, quantile, probs = 0.025),
      Upper = apply(mc_forecasts, 1, quantile, probs = 0.975)
    )
    
    # Combine historical and forecast data for plotting
    plot_data <- rbind(
      data.frame(Year = historical_data$Year, Total = historical_data$Total, Type = "Historical"),
      data.frame(Year = forecast_data$Year, Total = forecast_data$Forecast, Type = "Forecast")
    )
    
    fig <- plot_ly()
    
    # Historical Data
    fig <- fig %>% add_trace(
      x = historical_data$Year,
      y = historical_data$Total,
      type = "scatter",
      mode = "lines+markers",
      name = "Historical",
      line = list(color = "#4e79a7"),
      marker = list(color = "#4e79a7"),
      hovertemplate = "<b>Historical</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>"
    )
    
    # Monte Carlo Forecast
    fig <- fig %>% add_trace(
      x = forecast_data$Year,
      y = forecast_data$Forecast,
      type = "scatter",
      mode = "lines+markers",
      name = "Monte Carlo Forecast",
      line = list(color = "#f28e2b"),
      marker = list(color = "#f28e2b"),
      hovertemplate = "<b>Monte Carlo</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>"
    )
    
    # Confidence Intervals
    fig <- fig %>%
      add_trace(
        x = forecast_data$Year,
        y = forecast_data$Upper,
        type = "scatter",
        mode = "lines",
        line = list(width = 0, color = "rgba(242,142,43,0)"),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      add_trace(
        x = forecast_data$Year,
        y = forecast_data$Lower,
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = "rgba(242,142,43,0.2)",
        line = list(width = 0, color = "rgba(242,142,43,0)"),
        name = "95% Confidence Interval",
        hovertemplate = "<b>Confidence Interval</b><br>Year: %{x}<br>Population: %{y:.0f}<extra></extra>"
      )
    
    fig %>% layout(
      title = list(
        text = paste("Population Forecast (up to", target_year, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = "Year",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      yaxis = list(
        title = "Population",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  urban_prediction_result <- reactiveVal(NULL)
  
  observeEvent(input$urban_predict, {
    message("Urban Predict button clicked at ", Sys.time(), ": Year = ", input$urban_predict_year)
    shinyjs::runjs("document.getElementById('urban-loading-spinner').classList.add('active');")
    
    year <- input$urban_predict_year
    
    if (is.null(year) || is.na(year) || !is.numeric(year) || year < 1981 || year > 2030) {
      output$urban_prediction_error <- renderText("Invalid year. Please enter a year between 1981 and 2030.")
      urban_prediction_result(NULL)
      shinyjs::runjs("document.getElementById('urban-loading-spinner').classList.remove('active');")
      return()
    }
    
    historical_data <- urban_data()
    
    if (is.null(historical_data) || nrow(historical_data) == 0) {
      output$urban_prediction_error <- renderText("Population historical data not available.")
      urban_prediction_result(NULL)
      shinyjs::runjs("document.getElementById('urban-loading-spinner').classList.remove('active');")
      return()
    }
    
    output$urban_prediction_error <- renderText("")
    
    # Monte Carlo simulation for the selected year
    trend_model <- lm(Total ~ Year, data = historical_data)
    historical_sd <- sd(historical_data$Total, na.rm = TRUE)
    n_simulations <- 1000
    
    set.seed(123)  # For reproducibility
    mc_forecasts <- numeric(n_simulations)
    predicted_trend <- predict(trend_model, newdata = data.frame(Year = year))
    noise <- rnorm(n_simulations, mean = 0, sd = historical_sd * 0.1)  # 10% of historical SD
    mc_forecasts <- predicted_trend + noise
    
    # Calculate mean forecast and 95% confidence intervals
    prediction <- data.frame(
      Year = year,
      Monte_Carlo_Prediction = round(mean(mc_forecasts), 0),
      Monte_Carlo_Lower_Bound = round(quantile(mc_forecasts, 0.025), 0),
      Monte_Carlo_Upper_Bound = round(quantile(mc_forecasts, 0.975), 0)
    )
    
    urban_prediction_result(prediction)
    shinyjs::runjs("document.getElementById('urban-loading-spinner').classList.remove('active');")
    message("Urban prediction completed for year ", year)
  })
  
  observeEvent(input$urban_clear, {
    message("Urban Clear button clicked at ", Sys.time())
    updateNumericInput(session, "urban_predict_year", value = NULL)
    output$urban_prediction_error <- renderText("")
    urban_prediction_result(NULL)
  })
  
  output$urban_prediction_result_table <- renderDT({
    message("Rendering urban prediction_result_table at ", Sys.time())
    result <- urban_prediction_result()
    if (is.null(result)) return(NULL)
    
    datatable(
      result,
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        paging = FALSE,
        info = FALSE,
        ordering = TRUE,
        responsive = TRUE,
        autoWidth = TRUE,
        dom = 't'
      ),
      class = "display compact",
      style = "bootstrap",
      rownames = FALSE,
      caption = "Population Prediction Results",
      fillContainer = TRUE
    ) %>% 
      formatStyle(
        columns = colnames(result),
        backgroundColor = "rgba(0, 0, 0, 0.8)",
        color = "white",
        fontSize = "14px"
      )
  })
  
  output$urban_performance_metrics <- renderText({
    message("Rendering urban performance_metrics at ", Sys.time())
    historical_data <- urban_data()
    
    if (is.null(historical_data) || nrow(historical_data) == 0) {
      return("Performance metrics not available.")
    }
    
    # Fit a linear model to historical data for performance evaluation
    trend_model <- lm(Total ~ Year, data = historical_data)
    predicted <- predict(trend_model, newdata = data.frame(Year = historical_data$Year))
    actual <- historical_data$Total
    
    # Calculate MAPE and RMSE
    mape <- mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    
    output <- paste(
      "Model Performance Metrics:\n",
      sprintf("Monte Carlo MAPE: %.2f%%\n", mape),
      sprintf("Monte Carlo RMSE: %.2f\n", rmse)
    )
    
    output
  })
  
  output$urban_data_table <- renderDT({
    message("Urban data rows: ", nrow(urban_data())) # Debug
    datatable(
      urban_data(),
      options = list(
        scrollX = TRUE,
        responsive = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && typeof data === 'number' ?",
              "    parseFloat(data).toFixed(2) : data;",
              "}"
            )
          )
        ),
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE,
        dom = 'lfrtip' # Default layout with length, filter, table, info, pagination
      ),
      class = "display nowrap",
      style = "bootstrap",
      rownames = FALSE
    )
  })
  
  # Load and prepare food data
  food_data <- reactive({
    df <- tryCatch({
      file_path <- "food_data.csv"
      message("Attempting to load food CSV from: ", file_path)
      df <- read.csv(file_path, stringsAsFactors = FALSE)
      message("Food CSV loaded successfully. Columns: ", paste(colnames(df), collapse = ", "))
      required_cols <- c("Year", "Per_Capita_kg", "Population", "Total_Cropped_Area", "Arable_Area", 
                         "Total_Irrigated_Area", "Water_Availibility", "Fertilizer_Consumption_Total")
      if (!all(required_cols %in% colnames(df))) {
        message("Missing required columns in food data: ", paste(required_cols[!required_cols %in% colnames(df)], collapse = ", "))
        stop("Error: Missing required columns in food data")
      }
      df
    }, error = function(e) {
      message("Error loading food CSV: ", e$message)
      message("Using fallback food dataset.")
      data.frame(
        Year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021),
        Per_Capita_kg = c(391.33, 402.58, 406.28, 407.26, 412.32, 420.92, 419.15),
        Population = c(210969298, 213524840, 216379655, 219731479, 223293280, 227196741, 231402117),
        Total_Cropped_Area = c(23.55, 23.13, 23.53, 22.65, 24.16, 24.00, 23.85),
        Arable_Area = c(30.99, 31.21, 31.16, 30.53, 30.93, 30.28, 30.23),
        Total_Irrigated_Area = c(18.60, 18.22, 18.63, 18.33, 19.34, 19.47, 19.42),
        Water_Availibility = c(133.00, 132.70, 133.40, 127.40, 130.00, 131.50, 130.76),
        Fertilizer_Consumption_Total = c(3699.2, 5039.9, 4762.5, 4613.9, 4548.3, 5008.0, 5001.1)
      )
    })
    message("Final food data structure: ", capture.output(str(df)))
    df
  })
  
  # Load and prepare FAOSTAT data with Category
  faostat_data <- reactive({
    df <- tryCatch({
      file_path <- "C:\\Users\\digital\\OneDrive\\Desktop\\BIG PROJECT\\Chunk 01\\FAOSTAT.csv"
      message("Attempting to load FAOSTAT CSV from: ", file_path)
      df <- read.csv(file_path, stringsAsFactors = FALSE)
      message("FAOSTAT CSV loaded successfully. Columns: ", paste(colnames(df), collapse = ", "))
      required_cols <- c("Year", "Item", "Value")
      if (!all(required_cols %in% colnames(df))) {
        message("Missing required columns in FAOSTAT data: ", paste(required_cols[!required_cols %in% colnames(df)], collapse = ", "))
        stop("Error: Missing required columns in FAOSTAT data")
      }
      df
    }, error = function(e) {
      message("Error loading FAOSTAT CSV: ", e$message)
      message("Using fallback FAOSTAT dataset.")
      years <- 2015:2021
      items <- c("Wheat and products", "Rice and products", "Maize and products", "Vegetables, other", "Bovine Meat", "Palm Oil")
      n <- length(years) * length(items)
      data.frame(
        Year = rep(years, each = length(items)),
        Item = rep(items, times = length(years)),
        Value = pmax(0, round(runif(n, 10, 100), 2))
      )
    })
    # Rename Value to Per_Capita_kg
    df <- df %>% rename(Per_Capita_kg = Value)
    # Add Category column
    item_to_category <- unlist(lapply(names(food_categories), function(cat) {
      setNames(rep(cat, length(food_categories[[cat]])), food_categories[[cat]])
    }))
    df$Category <- item_to_category[df$Item]
    # Set Category to Item if not found in food_categories
    df$Category[is.na(df$Category)] <- df$Item[is.na(df$Category)]
    message("Final FAOSTAT data structure: ", capture.output(str(df)))
    df
  })
  
  # Food categories for FAOSTAT data
  food_categories <- list(
    Cereals = c("Wheat and products", "Rice and products", "Maize and products", 
                "Barley and products", "Millet and products", "Sorghum and products", "Cereals, other"),
    Pulses = c("Beans", "Peas", "Pulses, Other and products"),
    Fruits = c("Oranges, Mandarines", "Lemons, Limes and products", "Citrus, Other", 
               "Bananas", "Apples and products", "Pineapples and products", "Dates", "Fruits, other"),
    Vegetables = c("Tomatoes and products", "Onions", "Vegetables, other"),
    Meat = c("Bovine Meat", "Mutton & Goat Meat", "Poultry Meat", "Meat, Other"),
    Oils = c("Soyabean Oil", "Groundnut Oil", "Sunflowerseed Oil", "Palm Oil", 
             "Sesameseed Oil", "Olive Oil", "Ricebran Oil", "Maize Germ Oil", "Oilcrops Oil, Other"),
    Other = c("Sugar non-centrifugal", "Sugar (Raw Equivalent)", "Sweeteners, Other", 
              "Coffee and products", "Cocoa Beans and products", "Tea (including mate)", 
              "Pepper", "Pimento", "Cloves", "Spices, Other", "Beverages, Fermented", 
              "Infant food", "Miscellaneous")
  )
  
  # Reactive value to store last valid prediction year
  last_valid_year <- reactiveVal(NULL)
  
  # Render food content UI
  output$food_content_ui <- renderUI({
    if (is.null(active_content())) {
      return(div())
    } else if (active_content() == "food_overview") {
      div(class = "tab-content",
          h2("Food Overview"),
          div(class = "input-panel",
              div(class = "input-container",
                  sliderInput(
                    inputId = "food_year_range",
                    label = "Select Year Range:",
                    min = min(food_data()$Year),
                    max = max(food_data()$Year),
                    value = c(min(food_data()$Year), max(food_data()$Year)),
                    step = 1,
                    sep = ""
                  )
              )
          ),
          div(class = "stats-container",
              div(class = "stats-box",
                  strong("Avg Per Capita (kg)"), br(),
                  textOutput("avg_per_capita")
              ),
              div(class = "stats-box",
                  strong("Total Cropped Area (Mha)"), br(),
                  textOutput("total_cropped_area")
              ),
              div(class = "stats-box",
                  strong("Avg Population"), br(),
                  textOutput("avg_population")
              ),
              div(class = "stats-box",
                  strong("Avg FAOSTAT Per Capita (kg)"), br(),
                  textOutput("avg_faostat_per_capita")
              )
          ),
          div(class = "plot-container",
              div(class = "faostat-input-container",
                  selectInput(
                    inputId = "subplot_select",
                    label = "Select Subplot:",
                    choices = c(
                      "Population",
                      "Total Cropped Area",
                      "Arable Area",
                      "Total Irrigated Area",
                      "Water Availability",
                      "Fertilizer Consumption"
                    ),
                    selected = "Population"
                  )
              ),
              plotlyOutput("summary_subplots", height = "400px")),
          div(class = "plot-container",
              div(class = "faostat-input-container",
                  selectInput(
                    inputId = "faostat_item",
                    label = "Select FAOSTAT Item/Category:",
                    choices = c("All Items", names(food_categories), unique(faostat_data()$Item)),
                    selected = "All Items"
                  ),
                  radioButtons(
                    inputId = "faostat_plot_type",
                    label = "FAOSTAT Plot Type:",
                    choices = c("Bar Chart" = "bar","Box Plot" = "box", "Line Chart" = "line"),
                    selected = "bar",
                    inline = TRUE
                  )
              ),
              plotlyOutput("faostat_plot", height = "365px")),
          div(class = "plot-container",
              plotlyOutput("category_bar_animated", height = "400px")),
          div(class = "plot-container",
              plotlyOutput("category_stacked_area", height = "500px")),
          div(class = "plot-container",
              plotlyOutput("category_heatmap", height = "400px")),
          div(class = "plot-container",
              plotlyOutput("category_pie", height = "450px"))
      )
    } else if (active_content() == "food_prediction") {
      div(class = "tab-content",
          h2("Food Predictions"),
          div(class = "input-panel",
              div(class = "input-container",
                  numericInput("food_predict_year", "Enter Prediction Year (2023-2030):", 
                               value = NULL, min = 2023, max = 2030, step = 1)
              ),
              div(class = "button-container", style = "display: flex; gap: 10px;",
                  actionButton("food_predict", "Predict", class = "btn btn-custom btn-predict"),
                  actionButton("food_clear", "Clear", class = "btn btn-custom btn-clear")
              ),
              div(id = "food-loading-spinner", class = "loading-spinner", "Processing Prediction..."),
              div(id = "food-error-message", class = "error-message", textOutput("food_prediction_error"))
          ),
          div(class = "plot-container",
              plotlyOutput("food_prediction_plot", height = "400px")),
          div(class = "plot-container",
              DTOutput("food_prediction_result_table", height = "auto")),
          div(class = "plot-container",
              verbatimTextOutput("food_performance_metrics"))
      )
    } else if (active_content() == "food_tables") {
      div(class = "tab-content",
          h2("Data Tables"),
          div(class = "plot-container",
              DTOutput("food_data_table", height = "auto")),
          div(class = "plot-container",
              DTOutput("faostat_data_table", height = "auto")),
          div(class = "button-container",
              actionButton("food_next_page", "Next Page", class = "btn btn-custom btn-next")
          )
      )
    }
  })
  
  # Navigation
  
  observeEvent(input$food, {
    current_page("food")
    active_content(NULL)
  })
  
  observeEvent(input$back_food, {
    current_page("main")
    active_content(NULL)
  })
  
  # Scroll navigation for food dashboard
  observeEvent(input$scroll_up_food, {
    shinyjs::runjs("window.scrollBy({ top: -window.innerHeight, behavior: 'smooth' });")
  })
  
  observeEvent(input$scroll_down_food, {
    shinyjs::runjs("window.scrollBy({ top: window.innerHeight, behavior: 'smooth' });")
  })
  
  # Toggle between Food Overview, Prediction, and Tables
  observeEvent(input$food_overview_btn, {
    active_content("food_overview")
  })
  
  observeEvent(input$food_prediction_btn, {
    active_content("food_prediction")
  })
  
  observeEvent(input$food_tables_btn, {
    active_content("food_tables")
    food_table_page(1)
  })
  
  # Next page button for food table
  observeEvent(input$food_next_page, {
    df <- food_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- food_table_page()
    if (current < total_pages) {
      food_table_page(current + 1)
    }
  })
  
  # Disable Next Page button when food dataset ends
  observe({
    df <- food_data()
    rows_per_page <- 10
    total_rows <- nrow(df)
    total_pages <- ceiling(total_rows / rows_per_page)
    current <- food_table_page()
    shinyjs::toggleState("food_next_page", current < total_pages)
  })
  
  # Food stats calculations
  output$avg_per_capita <- renderText({
    df <- food_data()
    years <- input$food_year_range
    if (is.null(years) || length(years) == 0) years <- 2015:2021
    filtered <- df %>% filter(Year %in% years)
    avg <- mean(filtered$Per_Capita_kg, na.rm = TRUE)
    format(round(avg, 2), big.mark = ",")
  })
  
  output$total_cropped_area <- renderText({
    df <- food_data()
    years <- input$food_year_range
    if (is.null(years) || length(years) == 0) years <- 2015:2021
    filtered <- df %>% filter(Year %in% years)
    total <- sum(filtered$Total_Cropped_Area, na.rm = TRUE)
    format(round(total, 2), big.mark = ",")
  })
  
  output$avg_population <- renderText({
    df <- food_data()
    years <- input$food_year_range
    if (is.null(years) || length(years) == 0) years <- 2015:2021
    filtered <- df %>% filter(Year %in% years)
    avg <- mean(filtered$Population, na.rm = TRUE)
    format(round(avg, 0), big.mark = ",")
  })
  
  output$avg_faostat_per_capita <- renderText({
    df <- faostat_data()
    years <- input$food_year_range
    if (is.null(years) || length(years) == 0) years <- 2015:2021
    filtered <- df %>% filter(Year %in% years)
    if (input$faostat_item == "All Items") {
      avg <- mean(filtered$Per_Capita_kg, na.rm = TRUE)
    } else if (input$faostat_item %in% names(food_categories)) {
      avg <- mean(filtered$Per_Capita_kg[filtered$Item %in% food_categories[[input$faostat_item]]], na.rm = TRUE)
    } else {
      avg <- mean(filtered$Per_Capita_kg[filtered$Item == input$faostat_item], na.rm = TRUE)
    }
    format(round(avg, 2), big.mark = ",")
  })
  
  # Food Summary Subplots
  output$summary_subplots <- renderPlotly({
    plot_data <- food_data() %>% filter(Year >= input$food_year_range[1] & Year <= input$food_year_range[2])
    
    subplot_choice <- input$subplot_select
    if (is.null(subplot_choice)) subplot_choice <- "Population"
    
    if (subplot_choice == "Population") {
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Population,
        type = "scatter",
        mode = "lines+markers",
        name = "Population",
        line = list(color = "#4e79a7"),
        text = ~paste("Year:", Year, "<br>Population:", round(Population, 0)),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Population (count)", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else if (subplot_choice == "Total Cropped Area") {
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Total_Cropped_Area,
        type = "scatter",
        mode = "lines+markers",
        name = "Total Cropped Area",
        line = list(color = "#f28e2b"),
        text = ~paste("Year:", Year, "<br>Total Cropped Area:", round(Total_Cropped_Area, 2), " Mha"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Total Cropped Area (Mha)", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else if (subplot_choice == "Arable Area") {
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Arable_Area,
        type = "scatter",
        mode = "lines+markers",
        name = "Arable Area",
        line = list(color = "#59a14f"),
        text = ~paste("Year:", Year, "<br>Arable Area:", round(Arable_Area, 2), " Mha"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Arable Area (Mha)", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else if (subplot_choice == "Total Irrigated Area") {
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Total_Irrigated_Area,
        type = "scatter",
        mode = "lines+markers",
        name = "Total Irrigated Area",
        line = list(color = "#e15759"),
        text = ~paste("Year:", Year, "<br>Total Irrigated Area:", round(Total_Irrigated_Area, 2), " Mha"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Irrigated Area (Mha)", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else if (subplot_choice == "Water Availability") {
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Water_Availibility,
        type = "scatter",
        mode = "lines+markers",
        name = "Water Availability",
        line = list(color = "#b07aa1"),
        text = ~paste("Year:", Year, "<br>Water Availability:", round(Water_Availibility, 2), " MAF"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Water Availability (MAF)", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    } else if (subplot_choice == "Fertilizer Consumption") {
      fig <- plot_ly(
        plot_data,
        x = ~Year,
        y = ~Fertilizer_Consumption_Total,
        type = "scatter",
        mode = "lines+markers",
        name = "Fertilizer Consumption",
        line = list(color = "#edc948"),
        text = ~paste("Year:", Year, "<br>Fertilizer Consumption:", round(Fertilizer_Consumption_Total, 2), " '000 tonnes"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Fertilizer Consumption ('000 tonnes)", gridcolor = "rgba(255,255,255,0.2)"),
          xaxis = list(title = "Year")
        )
    }
    
    fig %>% layout(
      title = list(text = paste("Trend in", subplot_choice), x = 0.5),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = FALSE
    )
  })
  
  # FAOSTAT Plot
  output$faostat_plot <- renderPlotly({
    data <- faostat_data() %>%
      filter(Year >= input$food_year_range[1] & Year <= input$food_year_range[2])
    
    # Filter based on faostat_item
    if (input$faostat_item != "All Items") {
      if (input$faostat_item %in% names(food_categories)) {
        data <- data %>% filter(Category == input$faostat_item)
      } else {
        data <- data %>% filter(Item == input$faostat_item)
      }
    }
    
    # Vibrant color palette
    vivid_colors <- c(
      "#FF1F5B", "#00CD6C", "#009ADE", "#AF58BA", "#FFC107",
      "#F06292", "#4CAF50", "#1976D2", "#AB47BC", "#FF5722",
      "#E91E63", "#8BC34A", "#0288D1", "#673AB7", "#FF9800"
    )
    all_items <- unique(data$Item)
    colors <- rep(vivid_colors, length.out = length(all_items))
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% layout(
        title = "No Data Available",
        template = "plotly_dark",
        plot_bgcolor = "rgba(0,0,0,0.6)",
        paper_bgcolor = "rgba(0,0,0,0.6)"
      ))
    }
    
    if (input$faostat_plot_type == "line") {
      fig <- plot_ly(
        data,
        x = ~Year,
        y = ~Per_Capita_kg,
        color = ~Item,
        colors = colors,
        type = "scatter",
        mode = "lines+markers",
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<extra></extra>"
      )
    } else if (input$faostat_plot_type == "bar") {
      fig <- plot_ly(
        data,
        x = ~Year,
        y = ~Per_Capita_kg,
        color = ~Item,
        colors = colors,
        type = "bar",
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<extra></extra>"
      )
    } else if (input$faostat_plot_type == "box") {
      fig <- plot_ly(
        data,
        y = ~Item,
        x = ~Per_Capita_kg,
        color = ~Item,
        colors = colors,
        type = "box",
        orientation = "h",
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<extra></extra>"      )
    }
    
    fig %>% layout(
      title = list(
        text = paste("FAOSTAT Food Supply (", input$faostat_item, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = ifelse(input$faostat_plot_type == "box", "Food Supply (kg/capita/yr)", "Year"),
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white"),
        tickangle = 0
      ),
      yaxis = list(
        title = ifelse(input$faostat_plot_type == "box", "Item", "Food Supply (kg/capita/yr)"),
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(size = 10, color = "white")
      ),
      
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "x unified"
    )
  })
  
  # Animated Horizontal Bar Plot (fixed hover template)
  output$category_bar_animated <- renderPlotly({
    # Prepare data
    plot_data <- faostat_data() %>%
      filter(Year >= input$food_year_range[1] & Year <= input$food_year_range[2]) %>%
      group_by(Year, Category) %>%
      summarise(Per_Capita_kg = sum(Per_Capita_kg, na.rm = TRUE), .groups = "drop")
    
    # Complete missing Year-Category combinations with zeros
    all_categories <- unique(plot_data$Category)
    all_years <- seq(min(plot_data$Year), max(plot_data$Year))
    plot_data <- plot_data %>%
      tidyr::complete(Year = all_years, Category = all_categories, fill = list(Per_Capita_kg = 0))
    
    # Define vibrant colors similar to px.colors.qualitative.Vivid
    vivid_colors <- c(
      "#FF1F5B", "#00CD6C", "#009ADE", "#AF58BA", "#FFC107",
      "#F06292", "#4CAF50", "#1976D2", "#AB47BC", "#FF5722"
    )
    colors <- rep(vivid_colors, length.out = length(all_categories))
    
    # Create plot
    fig <- plot_ly(
      data = plot_data,
      x = ~Per_Capita_kg,
      y = ~Category,
      type = "bar",
      color = ~Category,
      colors = colors,
      frame = ~Year,
      customdata = ~Year,  # Pass Year as custom data for hover
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Year: %{customdata}<br>",
        "Food Supply: %{x:.2f} kg/capita/yr<extra></extra>"
      ),
      hoverlabel = list(
        font = list(color = "black", size = 12),
        bgcolor = "white",
        bordercolor = "black"
      )
    ) %>%
      layout(
        title = list(
          text = paste("Food Supply Trends for All Food Categories (", input$food_year_range[1], "-", input$food_year_range[2], ")"),
          x = 0.5
        ),
        xaxis = list(
          title = "Food Supply (kg/capita/yr)",
          gridcolor = "rgba(255,255,255,0.2)",
          titlefont = list(color = "white"),
          tickfont = list(color = "white")
        ),
        yaxis = list(
          title = "Food Category",
          autorange = "reversed",
          tickfont = list(size = 10, color = "white")
        ),
        template = "plotly_dark",
        font = list(color = "white", size = 12),
        plot_bgcolor = "rgba(0,0,0,0.6)",
        paper_bgcolor = "rgba(0,0,0,0.6)",
        showlegend = FALSE,
        hovermode = "closest"
      ) %>%
      animation_opts(
        frame = 300,
        transition = 300,
        easing = "cubic-in-out"
      )
    
    fig
  })
  
  
  # Category Heatmap
  output$category_heatmap <- renderPlotly({
    plot_data <- faostat_data() %>%
      filter(Year >= input$food_year_range[1] & Year <= input$food_year_range[2]) %>%
      group_by(Year, Category) %>%
      summarise(Per_Capita_kg = sum(Per_Capita_kg, na.rm = TRUE), .groups = "drop")
    
    pivot_data <- plot_data %>%
      tidyr::pivot_wider(names_from = Year, values_from = Per_Capita_kg, values_fill = 0) %>%
      arrange(Category)
    
    categories <- pivot_data$Category
    years <- as.numeric(colnames(pivot_data)[-1])
    z_values <- as.matrix(pivot_data[, -1])
    
    fig <- plot_ly(
      x = years,
      y = categories,
      z = z_values,
      type = "heatmap",
      colorscale = "Plasma",
      colorbar = list(title = "kg/capita/yr"),
      text = matrix(paste("Category:", categories, "<br>Year:", rep(years, each = length(categories)),
                          "<br>Food Supply:", round(z_values, 2), " kg/capita/yr"), nrow = length(categories)),
      hoverinfo = "text"
    )
    
    fig %>% layout(
      title = list(text = "Food Supply Heatmap for Main Categories", x = 0.5),
      xaxis = list(title = "Year", tickmode = "linear", range = c(input$food_year_range[1] - 0.5, input$food_year_range[2] + 0.5)),
      yaxis = list(title = "Food Category", tickfont = list(size = 10), automargin = TRUE),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      hovermode = "closest"
    )
  })
  
  # Updated food_categories to match Python categories
  food_categories <- list(
    Beverages = c("Coffee and products", "Tea (including mate)", "Beverages, Fermented"),
    Cereals = c("Wheat and products", "Rice and products", "Maize and products", 
                "Barley and products", "Millet and products", "Sorghum and products", "Cereals, other"),
    `Fish and Seafood` = c("Fish, Seafood", "Freshwater Fish", "Demersal Fish", "Pelagic Fish"),
    Fruits = c("Oranges, Mandarines", "Lemons, Limes and products", "Citrus, Other", 
               "Bananas", "Apples and products", "Pineapples and products", "Dates", "Fruits, other"),
    `Meat and Animal` = c("Bovine Meat", "Mutton & Goat Meat", "Poultry Meat", "Meat, Other", 
                          "Milk - Excluding Butter", "Eggs"),
    `Nuts and Oilseeds` = c("Groundnuts", "Sesamseed", "Sunflowerseed", "Oilcrops, Other"),
    Oils = c("Soyabean Oil", "Groundnut Oil", "Sunflowerseed Oil", "Palm Oil", 
             "Sesameseed Oil", "Olive Oil", "Ricebran Oil", "Maize Germ Oil", "Oilcrops Oil, Other"),
    Other = c("Infant food", "Miscellaneous"),
    Pulses = c("Beans", "Peas", "Pulses, Other and products"),
    `Roots and Tubers` = c("Potatoes and products", "Cassava and products", "Sweet potatoes", "Roots, Other"),
    Spices = c("Pepper", "Pimento", "Cloves", "Spices, Other"),
    `Sugars & Sweeteners` = c("Sugar non-centrifugal", "Sugar (Raw Equivalent)", "Sweeteners, Other"),
    Vegetables = c("Tomatoes and products", "Onions", "Vegetables, other")
  )
  output$category_stacked_area <- renderPlotly({
    # Prepare data
    df <- faostat_data()
    # Aggregate by Year and Category
    category_trend <- df %>%
      filter(Year >= input$food_year_range[1] & Year <= input$food_year_range[2]) %>%
      group_by(Year, Category) %>%
      summarise(Per_Capita_kg = sum(Per_Capita_kg, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Category, values_from = Per_Capita_kg, values_fill = 0)
    
    # Get categories (columns except Year)
    categories <- setdiff(colnames(category_trend), "Year")
    
    # Define colors (approximating px.colors.qualitative.Pastel)
    colors <- brewer.pal(min(length(categories), 9), "Pastel1")
    if (length(categories) > 9) {
      colors <- c(colors, brewer.pal(min(length(categories) - 9, 8), "Pastel2"))
    }
    
    # Initialize figure
    fig <- plot_ly()
    
    # Add stacked area traces for each category
    for (i in seq_along(categories)) {
      fig <- fig %>% add_trace(
        x = category_trend$Year,
        y = category_trend[[categories[i]]],
        type = "scatter",
        mode = "lines",
        name = categories[i],
        stackgroup = "one",
        line = list(width = 0), # No line border
        fill = "tonexty",
        fillcolor = colors[i],
        hovertemplate = "<b>%{data.name}</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<br>",
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black")
      )
    }
    
    # Update layout
    fig %>% layout(
      title = list(text = "Food Supply by Category Over Time", x = 0.5),
      xaxis = list(
        title = "Year",
        tickmode = "linear",
        range = c(min(category_trend$Year) - 0.5, max(category_trend$Year) + 0.5),
        gridcolor = "rgba(255,255,255,0.2)"
      ),
      yaxis = list(
        title = "Food Supply (kg/capita/yr)",
        gridcolor = "rgba(255,255,255,0.2)"
      ),
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.7,
        xanchor = "center",
        x = 0.5,
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.5)"
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  
  output$category_pie <- renderPlotly({
    # Prepare faostat_data with Category column
    df <- faostat_data()
    # Assign Category based on updated food_categories
    item_to_category <- unlist(lapply(names(food_categories), function(cat) {
      setNames(rep(cat, length(food_categories[[cat]])), food_categories[[cat]])
    }))
    df$Category <- item_to_category[df$Item]
    # Assign 'Other' to unmapped items
    df$Category[is.na(df$Category)] <- "Other"
    
    # Aggregate data by Year and Category
    yearly_data <- df %>%
      filter(Year >= input$food_year_range[1] & Year <= input$food_year_range[2]) %>%
      group_by(Year, Category) %>%
      summarise(Per_Capita_kg = sum(Per_Capita_kg, na.rm = TRUE), .groups = "drop")
    
    # Compute average across all years
    avg_data <- df %>%
      group_by(Category) %>%
      summarise(Per_Capita_kg = mean(Per_Capita_kg, na.rm = TRUE), .groups = "drop")
    
    # Get unique years and categories
    years <- sort(unique(yearly_data$Year))
    categories <- unique(yearly_data$Category)
    
    # Define colors
    colors <- brewer.pal(min(length(categories), 8), "Set1")
    if (length(categories) > 8) {
      colors <- c(colors, brewer.pal(min(length(categories) - 8, 8), "Dark2"))
    }
    
    # Initialize figure
    fig <- plot_ly()
    
    # Add pie traces for each year
    for (i in seq_along(years)) {
      year_data <- yearly_data %>% filter(Year == years[i])
      fig <- fig %>% add_trace(
        type = "pie",
        labels = year_data$Category,
        values = year_data$Per_Capita_kg,
        textinfo = "percent+label",
        textposition = "auto",
        hovertemplate = "<b>%{label}</b><br>Food Supply: %{value:.2f} kg/capita/yr<br>Percentage: %{percent:.2%}<br>",
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black"),
        marker = list(colors = colors[1:length(year_data$Category)], line = list(color = "white", width = 1)),
        pull = rep(0.02, length(year_data$Category)),
        rotation = 45,
        sort = FALSE,
        visible = (years[i] == years[1]) # Show first year by default
      )
    }
    
    # Add pie trace for average
    fig <- fig %>% add_trace(
      type = "pie",
      labels = avg_data$Category,
      values = avg_data$Per_Capita_kg,
      textinfo = "percent+label",
      textposition = "auto",
      hovertemplate = "<b>%{label}</b><br>Food Supply: %{value:.2f} kg/capita/yr<br>Percentage: %{percent:.2%}<br>",
      hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), bordercolor = "black"),
      marker = list(colors = colors[1:length(avg_data$Category)], line = list(color = "white", width = 1)),
      pull = rep(0.02, length(avg_data$Category)),
      rotation = 45,
      sort = FALSE,
      visible = FALSE
    )
    
    # Create dropdown menu
    buttons <- lapply(seq_along(years), function(i) {
      visible <- rep(FALSE, length(years) + 1)
      visible[i] <- TRUE
      list(
        label = as.character(years[i]),
        method = "update",
        args = list(list(visible = visible), list(title = paste("Food Supply Proportion by Category in", years[i])))
      )
    })
    buttons <- c(buttons, list(
      list(
        label = "Average",
        method = "update",
        args = list(list(visible = c(rep(FALSE, length(years)), TRUE)),
                    list(title = "Average Food Supply Proportion by Category"))
      )
    ))
    
    # Update layout
    fig %>% layout(
      title = list(text = paste("Food Supply Proportion by Category in", years[1]), x = 0.5),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      legend = list(orientation = "v", x = 1.1, xanchor = "left", y = 0.5, yanchor = "middle",
                    font = list(size = 10), bgcolor = "rgba(255,255,255,0.5)"),
      margin = list(t = 80, b = 50, l = 50, r = 150),
      hovermode = "closest",
      updatemenus = list(
        list(
          buttons = buttons,
          direction = "down",
          showactive = TRUE,
          x = 0.05,
          xanchor = "left",
          y = 1.1,
          yanchor = "top"
        )
      )
    )
  })
  # Reactive values for prediction results
  food_prediction_result <- reactiveVal(NULL)
  food_models <- reactiveVal(NULL)
  
  # Prediction event
  observeEvent(input$food_predict, {
    message("Food Predict button clicked at ", Sys.time(), ": Year = ", input$food_predict_year)
    shinyjs::runjs("document.getElementById('food-loading-spinner').classList.add('active');")
    
    year <- input$food_predict_year
    
    if (is.null(year) || is.na(year) || !is.numeric(year) || year < 2023 || year > 2030) {
      output$food_prediction_error <- renderText("Invalid year. Please enter a year between 2023 and 2030.")
      food_prediction_result(data.frame(Error = "Invalid year"))
    } else {
      tryCatch({
        df <- food_data()
        features <- c("Year", "Population", "Total_Cropped_Area", "Arable_Area", 
                      "Total_Irrigated_Area", "Water_Availibility", "Fertilizer_Consumption_Total")
        
        if (!all(features %in% colnames(df))) {
          output$food_prediction_error <- renderText("Error: Missing required features in data.")
          food_prediction_result(data.frame(Error = "Missing features"))
          return()
        }
        
        # Fit linear regression model (as a placeholder for LSTM/Holt-Winters)
        X <- df[, features]
        y <- df$Per_Capita_kg
        model <- lm(y ~ ., data = cbind(X, y = y))
        
        # Calculate growth stats for simulation
        growth_stats <- lapply(features[-1], function(feature) {
          growth <- diff(df[[feature]]) / lag(df[[feature]])[-1]
          growth <- growth[is.finite(growth)]
          list(mean = mean(growth, na.rm = TRUE), sd = sd(growth, na.rm = TRUE))
        })
        names(growth_stats) <- features[-1]
        
        n_years <- year - 2022
        n_simulations <- 1000
        last_row <- tail(df[features], 1)
        
        simulated_results <- matrix(NA, nrow = n_years, ncol = n_simulations)
        for (sim in 1:n_simulations) {
          current <- last_row
          future_values <- matrix(NA, nrow = n_years, ncol = length(features))
          colnames(future_values) <- features
          for (year_offset in 1:n_years) {
            new_row <- current
            new_row$Year <- 2022 + year_offset
            for (feature in features[-1]) {
              growth <- rnorm(1, mean = growth_stats[[feature]]$mean, sd = growth_stats[[feature]]$sd)
              new_row[[feature]] <- current[[feature]] * (1 + growth)
            }
            future_values[year_offset, ] <- as.numeric(new_row)
            current <- new_row
          }
          future_df <- as.data.frame(future_values)
          simulated_results[, sim] <- predict(model, newdata = future_df)
        }
        
        mean_forecast <- mean(simulated_results[n_years, ], na.rm = TRUE)
        lower_bound <- quantile(simulated_results[n_years, ], probs = 0.05, na.rm = TRUE)
        upper_bound <- quantile(simulated_results[n_years, ], probs = 0.95, na.rm = TRUE)
        
        forecast_year <- data.frame(Year = year, Linear_Estimated = round(mean_forecast, 1), 
                                    Linear_Lower = round(lower_bound, 1), Linear_Upper = round(upper_bound, 1))
        
        if (!all(is.na(forecast_year[, -1]))) {
          food_prediction_result(forecast_year)
          output$food_prediction_error <- renderText("")
        } else {
          output$food_prediction_error <- renderText("No prediction available for the selected year.")
          food_prediction_result(data.frame(Error = "No prediction available"))
        }
        
        # Store model for metrics
        food_models(list(linear = model))
        
      }, error = function(e) {
        message("Food Prediction error: ", e$message)
        output$food_prediction_error <- renderText(paste("Prediction failed:", e$message))
        food_prediction_result(data.frame(Error = "Prediction failed"))
      })
    }
    
    shinyjs::runjs("document.getElementById('food-loading-spinner').classList.remove('active');")
  })
  
  # Clear prediction
  observeEvent(input$food_clear, {
    message("Food clear button clicked at ", Sys.time())
    updateNumericInput(session, "food_predict_year", value = NULL)
    output$food_prediction_error <- renderText("")
    food_prediction_result(NULL)
    food_models(NULL)
    showNotification("Food input cleared.", type = "message")
  })
  
  # Prediction plot output (unchanged)
  output$food_prediction_plot <- renderPlotly({
    target_year <- input$food_predict_year
    if (is.null(target_year) || is.na(target_year) || !is.numeric(target_year) || target_year < 2023 || target_year > 2030) {
      return(NULL)
    }
    
    df <- food_data()
    features <- c(
      "Year", "Population", "Total_Cropped_Area", "Arable_Area",
      "Total_Irrigated_Area", "Water_Availibility", "Fertilizer_Consumption_Total"
    )
    
    # Verify data
    if (!all(features %in% colnames(df))) {
      output$food_prediction_error <- renderText("Error: Missing required features in data.")
      return(NULL)
    }
    
    # Fit linear regression model
    X <- df[, features]
    y <- df$Per_Capita_kg
    model <- lm(y ~ ., data = cbind(X, y = y))
    
    # Calculate percentage growth stats
    growth_stats <- lapply(features[-1], function(feature) { # Exclude Year
      growth <- diff(df[[feature]]) / lag(df[[feature]])[-1]
      growth <- growth[is.finite(growth)]
      list(mean = mean(growth, na.rm = TRUE), sd = sd(growth, na.rm = TRUE))
    })
    names(growth_stats) <- features[-1]
    
    # Simulation parameters
    n_years <- target_year - 2022
    n_simulations <- 1000
    last_row <- tail(df[features], 1)
    
    # Monte Carlo simulation
    simulated_results <- matrix(NA, nrow = n_years, ncol = n_simulations)
    for (sim in 1:n_simulations) {
      current <- last_row
      future_values <- matrix(NA, nrow = n_years, ncol = length(features))
      colnames(future_values) <- features
      
      for (year_offset in 1:n_years) {
        new_row <- current
        new_row$Year <- 2022 + year_offset
        for (feature in features[-1]) {
          growth <- rnorm(1, mean = growth_stats[[feature]]$mean, sd = growth_stats[[feature]]$sd)
          new_row[[feature]] <- current[[feature]] * (1 + growth)
        }
        future_values[year_offset, ] <- as.numeric(new_row)
        current <- new_row
      }
      
      future_df <- as.data.frame(future_values)
      simulated_results[, sim] <- predict(model, newdata = future_df)
    }
    
    # Compute statistics
    future_years <- 2023:(2022 + n_years)
    mean_forecast <- rowMeans(simulated_results)
    lower_bound <- apply(simulated_results, 1, quantile, probs = 0.05)
    upper_bound <- apply(simulated_results, 1, quantile, probs = 0.95)
    
    # Create plot
    fig <- plot_ly() %>%
      add_trace(
        x = df$Year,
        y = df$Per_Capita_kg,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical",
        line = list(color = "#4e79a7"),
        marker = list(color = "#4e79a7"),
        hovertemplate = "<b>Historical</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<extra></extra>"
      ) %>%
      add_trace(
        x = future_years,
        y = mean_forecast,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(color = "orange"),
        marker = list(color = "orange"),
        hovertemplate = "<b>Forecast</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<extra></extra>"
      )
    
    # Add confidence interval only if n_years > 1 to avoid Plotly errors
    if (n_years > 1) {
      fig <- fig %>%
        add_trace(
          x = future_years,
          y = upper_bound,
          type = "scatter",
          mode = "lines",
          line = list(width = 0, color = "rgba(255,165,0,0)"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          x = future_years,
          y = lower_bound,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = "rgba(255,165,0,0.2)",
          line = list(width = 0, color = "rgba(255,165,0,0)"),
          name = "90% Confidence Interval",
          hovertemplate = "<b>Confidence Interval</b><br>Year: %{x}<br>Food Supply: %{y:.2f} kg/capita/yr<extra></extra>"
        )
    }
    
    fig %>% layout(
      title = list(
        text = paste("Food Supply Forecast (up to", target_year, ")"),
        x = 0.5
      ),
      xaxis = list(
        title = "Year",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      yaxis = list(
        title = "Per Capita Food (kg)",
        gridcolor = "rgba(255,255,255,0.2)",
        titlefont = list(color = "white"),
        tickfont = list(color = "white")
      ),
      template = "plotly_dark",
      font = list(color = "white"),
      plot_bgcolor = "rgba(0,0,0,0.6)",
      paper_bgcolor = "rgba(0,0,0,0.6)",
      showlegend = TRUE,
      hovermode = "closest"
    )
  })
  
  # Prediction result table
  output$food_prediction_result_table <- renderDT({
    message("Rendering food_prediction_result_table at ", Sys.time())
    result <- food_prediction_result()
    validate(need(!is.null(result), "No prediction results available."))
    datatable(
      result,
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        paging = FALSE,
        info = FALSE
      ),
      class = "table-dark",
      style = "bootstrap5"
    )
  })
  
  # Performance metrics
  #output$food_performance_metrics <- renderPrint({
  #  message("Rendering food_performance_metrics at ", Sys.time())
  #  models <- food_models()
  #  df <- food_data()
  #  validate(need(!is.null(models) && nrow(df) > 0, "No model or data for metrics."))
  
  #  y <- df$Per_Capita_kg
  #  X <- df[, c("Population", "Total_Cropped_Area", "Arable_Area", "Total_Irrigated_Area", "Water_Availibility", "Fertilizer_Consumption_Total")]
  #  fitted <- predict(models$linear, newdata = X)
  #  actual <- y
  #  
  #  mse <- mean((actual - fitted)^2, na.rm = TRUE)
  #  rmse <- sqrt(mse)
  #  mape <- mean(abs((actual - fitted) / actual), na.rm = TRUE) * 100
  #  
  # metrics <- list(linear = list(MSE = round(mse, 2), RMSE = round(rmse, 2), MAPE = round(mape, 2)))
  
  #  cat("Model Performance Metrics:\n")
  #  if (!is.null(metrics$linear)) {
  #    cat("Linear Regression Metrics:\n")
  #    cat("MSE:", metrics$linear$MSE, "\n")
  #    cat("RMSE:", metrics$linear$RMSE, "\n")
  #    cat("MAPE:", metrics$linear$MAPE, "%\n")
  #  }
  #})
  
  
  # Food Data Table (paginated)
  output$food_data_table <- renderDT({
    message("Food data rows: ", nrow(food_data())) # Debug
    datatable(
      food_data(),
      options = list(
        scrollX = TRUE,
        responsive = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && typeof data === 'number' ?",
              "    parseFloat(data).toFixed(2) : data;",
              "}"
            )
          )
        ),
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE,
        dom = 'lfrtip' # Ensure default layout with length, filter, table, info, pagination
      ),
      class = "display nowrap",
      style = "bootstrap"
    )
  })
  
  output$faostat_data_table <- renderDT({
    message("FAOSTAT data rows: ", nrow(faostat_data())) # Debug
    datatable(
      faostat_data(),
      options = list(
        scrollX = TRUE,
        responsive = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && typeof data === 'number' ?",
              "    parseFloat(data).toFixed(2) : data;",
              "}"
            )
          )
        ),
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100, -1),
        lengthMenuNames = c("10", "25", "50", "100", "All"),
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE,
        dom = 'lfrtip' # Ensure default layout
      ),
      class = "display nowrap",
      style = "bootstrap"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)