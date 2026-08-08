# ==============================================================================
# APAC Pet Food — Econometric Country Forecast Dashboard (2012–2030)
# Complete Standalone R Shiny Application (app.R)
# CDAR 2026 Capstone — Country x Subcategory Volume Forecast Artefact
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

# ------------------------------------------------------------------------------
# 1. COLOR PALETTE & TAXONOMY DEFINITIONS (Editorial Muted Sunset Aesthetic)
# ------------------------------------------------------------------------------
SUBCAT_PALETTE <- c(
  "Cat Treats & Milk"                = "#C84B31", # Terracotta Red
  "Dog Treats & Chews"               = "#D96B27", # Burnt Warm Orange
  "Premium Wet Cat Food"             = "#9A3412", # Deep Rust / Sienna
  "Premium Dry Cat Food"             = "#B45309", # Warm Ochre / Amber
  "Standard Wet Dog Food"            = "#78350F", # Deep Chestnut
  "Premium Dry Dog Food"             = "#D97706", # Golden Sunset
  "Standard Dry Dog Food"            = "#CA8A04", # Warm Sand Gold
  "Standard Dry Cat Food"            = "#65A30D", # Olive Sage
  "Standard Wet Cat Food"            = "#15803D", # Muted Cypress
  "Economy Dry Dog Food"             = "#854D0E", # Bronze Wood
  "Specialized Dietary Supplements" = "#701A75", # Muted Sunset Plum
  "Legacy Veterinary Formulations"  = "#57534E"  # Slate Oxide
)

SEGMENT_TAXONOMY <- data.frame(
  Subcategory = c(
    "Cat Treats & Milk", "Dog Treats & Chews", "Premium Wet Cat Food", "Premium Dry Cat Food",
    "Standard Wet Dog Food", "Premium Dry Dog Food", "Standard Dry Dog Food", "Standard Dry Cat Food",
    "Standard Wet Cat Food", "Economy Dry Dog Food", "Specialized Dietary Supplements", "Legacy Veterinary Formulations"
  ),
  Segment_Name = c(
    "High Growth Drivers", "High Growth Drivers", "High Growth Drivers", "Steady Expansion Core",
    "Steady Expansion Core", "Steady Expansion Core", "Mature Staples", "Mature Staples",
    "Mature Staples", "Declining / Value Shift", "High Growth Drivers", "Declining / Value Shift"
  ),
  Price_Tier = c(
    "Ultra-Premium", "Premium", "Super-Premium", "Premium",
    "Mid-Tier", "Premium", "Value-Mass", "Mid-Tier",
    "Mid-Tier", "Economy", "Super-Premium", "Legacy-Mid"
  ),
  stringsAsFactors = FALSE
)

COUNTRIES <- c("APAC Total", "China", "Japan", "Australia", "India", "Indonesia")
BASE_YEAR <- 2025

# ------------------------------------------------------------------------------
# 2. DATA LOADER / SYNTHETIC DATA FALLBACK GENERATOR
# ------------------------------------------------------------------------------
load_forecast_data <- function() {
  rds_path <- "country_forecast_artefact.rds"
  if (file.exists(rds_path)) {
    df <- readRDS(rds_path)
    return(df)
  }
  
  # Fallback: Synthetic data matching exact Capstone schema if RDS absent
  set.seed(2026)
  years <- 2012:2030
  
  base_vols <- c(
    "Cat Treats & Milk"                = 185000000,
    "Dog Treats & Chews"               = 310000000,
    "Premium Wet Cat Food"             = 420000000,
    "Premium Dry Cat Food"             = 680000000,
    "Standard Wet Dog Food"            = 540000000,
    "Premium Dry Dog Food"             = 890000000,
    "Standard Dry Dog Food"            = 1250000000,
    "Standard Dry Cat Food"            = 920000000,
    "Standard Wet Cat Food"            = 380000000,
    "Economy Dry Dog Food"             = 450000000,
    "Specialized Dietary Supplements" = 95000000,
    "Legacy Veterinary Formulations"  = 110000000
  )
  
  cagr_rates <- c(
    "Cat Treats & Milk"                = 0.082,
    "Dog Treats & Chews"               = 0.071,
    "Premium Wet Cat Food"             = 0.065,
    "Premium Dry Cat Food"             = 0.048,
    "Standard Wet Dog Food"            = 0.035,
    "Premium Dry Dog Food"             = 0.052,
    "Standard Dry Dog Food"            = 0.018,
    "Standard Dry Cat Food"            = 0.022,
    "Standard Wet Cat Food"            = 0.015,
    "Economy Dry Dog Food"             = -0.024,
    "Specialized Dietary Supplements" = 0.089,
    "Legacy Veterinary Formulations"  = -0.012
  )
  
  country_multipliers <- c(
    "APAC Total" = 1.00,
    "China"      = 0.42,
    "Japan"      = 0.24,
    "Australia"  = 0.14,
    "India"      = 0.12,
    "Indonesia"  = 0.08
  )
  
  records <- list()
  idx <- 1
  
  for (cntry in COUNTRIES) {
    c_mult <- country_multipliers[cntry]
    for (subcat in names(base_vols)) {
      b_vol <- base_vols[subcat] * c_mult
      cagr <- cagr_rates[subcat]
      seg <- SEGMENT_TAXONOMY$Segment_Name[SEGMENT_TAXONOMY$Subcategory == subcat]
      
      for (yr in years) {
        # Exponential trajectory relative to 2025
        t_diff <- yr - 2025
        mean_vol <- b_vol * ((1 + cagr) ^ t_diff)
        
        # Add slight historical noise vs clean forecast
        if (yr <= 2025) {
          noise <- rnorm(1, mean = 0, sd = 0.015 * mean_vol)
          vol <- max(1000, mean_vol + noise)
          lo95 <- vol
          hi95 <- vol
        } else {
          vol <- mean_vol
          margin <- 0.025 * (yr - 2025) * vol
          lo95 <- vol - margin
          hi95 <- vol + margin
        }
        
        records[[idx]] <- data.frame(
          Country = cntry,
          Subcategory = subcat,
          Segment_Name = seg,
          Year = yr,
          Volume_KG = round(vol, 2),
          Volume_Lo95 = round(lo95, 2),
          Volume_Hi95 = round(hi95, 2),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }
  
  bind_rows(records)
}

forecast_df <- load_forecast_data()

# Helper function to format volumes
fmt_vol <- function(x) {
  ifelse(x >= 1e9, paste0(round(x / 1e9, 2), " B"),
         ifelse(x >= 1e6, paste0(round(x / 1e6, 2), " M"),
                ifelse(x >= 1e3, paste0(round(x / 1e3, 1), " K"),
                       paste0(round(x, 0)))))
}

# ------------------------------------------------------------------------------
# 3. SHINY USER INTERFACE (Editorial Muted Sunset Theme)
# ------------------------------------------------------------------------------
custom_theme <- bs_theme(
  version = 5,
  bg = "#FAF7F2",
  fg = "#1C1917",
  primary = "#C84B31",
  secondary = "#D96B27",
  base_font = font_google("Plus Jakarta Sans"),
  heading_font = font_google("Newsreader")
)

ui <- page_navbar(
  theme = custom_theme,
  title = div(
    class = "d-flex items-center gap-2",
    span(class = "badge bg-danger text-white font-serif px-2 py-1", "e"),
    span(class = "fw-bold font-serif fs-4", "APAC Pet Food Forecast"),
    span(class = "text-muted fs-6 font-sans ms-2", "CDAR 2026 Econometric Outlook")
  ),
  
  # Inject Custom CSS for Editorial Styling
  header = tags$head(
    tags$style(HTML("
      body { background-color: #FAF7F2; color: #1C1917; }
      .card { border-color: #E7E5E4; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
      .metric-card { border-top: 3px solid #C84B31; background: #FFFFFF; }
      .nav-tabs .nav-link.active { background-color: #C84B31 !important; color: #FFFFFF !important; border-radius: 4px; }
      .btn-primary { background-color: #C84B31; border-color: #C84B31; }
      .btn-primary:hover { background-color: #B9381E; border-color: #B9381E; }
      .sidebar { background-color: #FFFFFF; border-right: 1px solid #E7E5E4; }
    "))
  ),
  
  # TAB 1: Country Forecast
  nav_panel(
    title = "Country Forecast",
    icon = icon("chart-line"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = tags$h5("Controls & Filters", class = "font-serif text-danger fw-bold border-bottom pb-2"),
        
        selectInput(
          "country_select",
          "Country Jurisdiction:",
          choices = COUNTRIES,
          selected = "APAC Total"
        ),
        
        sliderInput(
          "horizon_slider",
          "Forecast Horizon (+yrs past 2025):",
          min = 1, max = 5, value = 3, step = 1
        ),
        
        hr(),
        
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$label("Subcategories Spectrum:", class = "fw-bold font-serif text-dark small"),
          div(
            actionButton("btn_select_all", "All", class = "btn-sm btn-outline-danger py-0 px-2"),
            actionButton("btn_select_none", "None", class = "btn-sm btn-outline-secondary py-0 px-2")
          )
        ),
        
        checkboxGroupInput(
          "subcat_select",
          label = NULL,
          choices = unique(SEGMENT_TAXONOMY$Subcategory),
          selected = unique(SEGMENT_TAXONOMY$Subcategory)
        )
      ),
      
      # Main Panel Content
      layout_column_wrap(
        width = 1,
        
        # Metric Summary Cards
        layout_column_wrap(
          width = 1/4,
          card(
            class = "metric-card p-3",
            div(class = "text-muted small fw-bold uppercase", "2025 Baseline Volume"),
            div(class = "fs-2 font-serif fw-bold text-dark", textOutput("card_2025_vol")),
            div(class = "text-muted small", textOutput("card_2025_sub"))
          ),
          card(
            class = "metric-card p-3", style = "border-top-color: #D96B27;",
            div(class = "text-muted small fw-bold uppercase", "Forecast Target Volume"),
            div(class = "fs-2 font-serif fw-bold text-danger", textOutput("card_fc_vol")),
            div(class = "text-muted small", textOutput("card_fc_sub"))
          ),
          card(
            class = "metric-card p-3", style = "border-top-color: #B45309;",
            div(class = "text-muted small fw-bold uppercase", "Net Volume Growth"),
            div(class = "fs-2 font-serif fw-bold text-success", textOutput("card_growth_pct")),
            div(class = "text-muted small", textOutput("card_growth_net"))
          ),
          card(
            class = "metric-card p-3", style = "border-top-color: #701A75;",
            div(class = "text-muted small fw-bold uppercase", "Top Growth Driver"),
            div(class = "fs-5 font-serif fw-bold text-dark text-truncate", textOutput("card_top_cat")),
            div(class = "text-muted small", "Highest relative expansion")
          )
        ),
        
        # Forecast Time-Series Plotly Chart
        card(
          card_header(
            class = "bg-white font-serif fw-bold fs-5 border-bottom",
            "Econometric Volume Forecast Trajectory (2012–2030)"
          ),
          plotlyOutput("plot_forecast", height = "480px")
        ),
        
        # Exact Data Table
        card(
          card_header(
            class = "bg-white font-serif fw-bold fs-5 border-bottom d-flex justify-content-between align-items-center",
            span("Detailed Forecast Volume Breakdown (KG)"),
            downloadButton("download_fc_csv", "Export CSV", class = "btn-sm btn-outline-danger")
          ),
          DTOutput("table_forecast")
        )
      )
    )
  ),
  
  # TAB 2: Country Comparison
  nav_panel(
    title = "Country Comparison",
    icon = icon("chart-column"),
    
    layout_column_wrap(
      width = 1,
      card(
        card_header(
          class = "bg-white font-serif fw-bold fs-5 border-bottom",
          "Aggregate Volume Comparison Across APAC Jurisdictions (2025 Baseline vs. Forecast)"
        ),
        plotlyOutput("plot_country_compare", height = "420px")
      ),
      card(
        card_header(
          class = "bg-white font-serif fw-bold fs-5 border-bottom d-flex justify-content-between align-items-center",
          span("Cross-Country Econometric Comparison Matrix"),
          downloadButton("download_compare_csv", "Export Matrix CSV", class = "btn-sm btn-outline-danger")
        ),
        DTOutput("table_country_compare")
      )
    )
  ),
  
  # TAB 3: Segment Reference Taxonomy
  nav_panel(
    title = "Segment Taxonomy",
    icon = icon("book"),
    
    layout_column_wrap(
      width = 1,
      card(
        card_header(
          class = "bg-white font-serif fw-bold fs-5 border-bottom d-flex justify-content-between align-items-center",
          span("Subcategory Behavioural Clustering & Price Tier Reference"),
          downloadButton("download_ref_csv", "Export Taxonomy CSV", class = "btn-sm btn-outline-danger")
        ),
        DTOutput("table_segment_ref")
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 4. SHINY SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Select All / None handlers
  observeEvent(input$btn_select_all, {
    updateCheckboxGroupInput(session, "subcat_select", selected = unique(SEGMENT_TAXONOMY$Subcategory))
  })
  
  observeEvent(input$btn_select_none, {
    updateCheckboxGroupInput(session, "subcat_select", selected = character(0))
  })
  
  # Reactive filtered data for Country Forecast
  filtered_fc_df <- reactive({
    req(input$country_select, input$horizon_slider)
    subcats <- input$subcat_select
    if (length(subcats) == 0) return(data.frame())
    
    max_yr <- BASE_YEAR + input$horizon_slider
    
    forecast_df %>%
      filter(
        Country == input$country_select,
        Subcategory %in% subcats,
        Year <= max_yr
      )
  })
  
  # Metric Card Outputs
  output$card_2025_vol <- renderText({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return("0 KG")
    v2025 <- sum(df$Volume_KG[df$Year == 2025], na.rm = TRUE)
    paste0(fmt_vol(v2025), " KG")
  })
  
  output$card_2025_sub <- renderText({
    paste0(input$country_select, " • ", length(input$subcat_select), " subcategories")
  })
  
  output$card_fc_vol <- renderText({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return("0 KG")
    max_yr <- BASE_YEAR + input$horizon_slider
    v_fc <- sum(df$Volume_KG[df$Year == max_yr], na.rm = TRUE)
    paste0(fmt_vol(v_fc), " KG")
  })
  
  output$card_fc_sub <- renderText({
    paste0("Target Horizon: ", 2025 + input$horizon_slider)
  })
  
  output$card_growth_pct <- renderText({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return("0.0%")
    max_yr <- BASE_YEAR + input$horizon_slider
    v2025 <- sum(df$Volume_KG[df$Year == 2025], na.rm = TRUE)
    v_fc <- sum(df$Volume_KG[df$Year == max_yr], na.rm = TRUE)
    if (v2025 == 0) return("0.0%")
    pct <- ((v_fc - v2025) / v2025) * 100
    paste0(ifelse(pct > 0, "+", ""), round(pct, 1), "%")
  })
  
  output$card_growth_net <- renderText({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return("(0 KG)")
    max_yr <- BASE_YEAR + input$horizon_slider
    v2025 <- sum(df$Volume_KG[df$Year == 2025], na.rm = TRUE)
    v_fc <- sum(df$Volume_KG[df$Year == max_yr], na.rm = TRUE)
    net <- v_fc - v2025
    paste0("(", ifelse(net > 0, "+", ""), fmt_vol(net), " KG)")
  })
  
  output$card_top_cat <- renderText({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return("None")
    max_yr <- BASE_YEAR + input$horizon_slider
    
    cat_summary <- df %>%
      filter(Year %in% c(2025, max_yr)) %>%
      group_by(Subcategory, Year) %>%
      summarise(vol = sum(Volume_KG), .groups = "drop") %>%
      pivot_wider(names_from = Year, values_from = vol, names_prefix = "yr_")
    
    if (nrow(cat_summary) == 0) return("None")
    
    cat_summary <- cat_summary %>%
      mutate(growth = (yr_2030 / yr_2025) - 1) %>%
      arrange(desc(growth))
    
    cat_summary$Subcategory[1]
  })
  
  # Plotly Time-Series Chart
  output$plot_forecast <- renderPlotly({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return(NULL)
    
    p <- plot_ly()
    
    subcats <- unique(df$Subcategory)
    for (sc in subcats) {
      sub_df <- df %>% filter(Subcategory == sc) %>% arrange(Year)
      clr <- SUBCAT_PALETTE[sc]
      if (is.na(clr)) clr <- "#C84B31"
      
      # 95% CI Ribbon for Forecast Period
      fc_sub <- sub_df %>% filter(Year >= 2025)
      if (nrow(fc_sub) > 0) {
        p <- p %>% add_ribbons(
          data = fc_sub,
          x = ~Year,
          ymin = ~Volume_Lo95,
          ymax = ~Volume_Hi95,
          fillcolor = paste0(clr, "22"),
          line = list(color = "transparent"),
          showlegend = FALSE,
          name = paste(sc, "95% CI")
        )
      }
      
      # Line Trace
      p <- p %>% add_trace(
        data = sub_df,
        x = ~Year,
        y = ~Volume_KG,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = clr, width = 2.5),
        marker = list(color = clr, size = 6),
        name = sc,
        hoverinfo = "text",
        text = ~paste0(
          "<b>", Subcategory, "</b><br>",
          "Year: ", Year, "<br>",
          "Volume: ", format(round(Volume_KG, 2), big.mark = ","), " KG"
        )
      )
    }
    
    p %>% layout(
      xaxis = list(title = "Year", gridcolor = "#E7E5E4"),
      yaxis = list(title = "Volume (KG)", gridcolor = "#E7E5E4"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF",
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    )
  })
  
  # DT Forecast Table
  output$table_forecast <- renderDT({
    df <- filtered_fc_df()
    if (nrow(df) == 0) return(NULL)
    max_yr <- BASE_YEAR + input$horizon_slider
    
    tbl_df <- df %>%
      filter(Year %in% c(2025, max_yr)) %>%
      group_by(Subcategory, Segment_Name, Year) %>%
      summarise(vol = sum(Volume_KG), .groups = "drop") %>%
      pivot_wider(names_from = Year, values_from = vol, names_prefix = "Yr_")
    
    col_fc <- paste0("Yr_", max_yr)
    tbl_df <- tbl_df %>%
      mutate(
        Pct_Change = round(((get(col_fc) - Yr_2025) / Yr_2025) * 100, 2),
        Yr_2025 = format(round(Yr_2025, 2), big.mark = ","),
        !!col_fc := format(round(get(col_fc), 2), big.mark = ",")
      )
    
    datatable(
      tbl_df,
      options = list(pageLength = 12, dom = 'tip'),
      rownames = FALSE,
      colnames = c("Subcategory", "Segment", "2025 Baseline (KG)", paste0("Forecast ", max_yr, " (KG)"), "% Growth")
    )
  })
  
  # Country Comparison Plot
  output$plot_country_compare <- renderPlotly({
    subcats <- input$subcat_select
    if (length(subcats) == 0) return(NULL)
    max_yr <- BASE_YEAR + input$horizon_slider
    
    cmp_df <- forecast_df %>%
      filter(Subcategory %in% subcats, Year %in% c(2025, max_yr)) %>%
      group_by(Country, Year) %>%
      summarise(tot_vol = sum(Volume_KG), .groups = "drop") %>%
      pivot_wider(names_from = Year, values_from = tot_vol, names_prefix = "Yr_")
    
    col_fc <- paste0("Yr_", max_yr)
    
    plot_ly(cmp_df, x = ~Country) %>%
      add_bars(y = ~Yr_2025, name = "2025 Baseline", marker = list(color = "#A8A29E")) %>%
      add_bars(y = ~get(col_fc), name = paste0("Forecast ", max_yr), marker = list(color = "#C84B31")) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Jurisdiction"),
        yaxis = list(title = "Total Volume (KG)"),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF"
      )
  })
  
  # Country Comparison Table
  output$table_country_compare <- renderDT({
    subcats <- input$subcat_select
    if (length(subcats) == 0) return(NULL)
    max_yr <- BASE_YEAR + input$horizon_slider
    col_fc <- paste0("Yr_", max_yr)
    
    cmp_df <- forecast_df %>%
      filter(Subcategory %in% subcats, Year %in% c(2025, max_yr)) %>%
      group_by(Country, Subcategory, Segment_Name, Year) %>%
      summarise(vol = sum(Volume_KG), .groups = "drop") %>%
      pivot_wider(names_from = Year, values_from = vol, names_prefix = "Yr_") %>%
      mutate(
        Pct_Change = round(((get(col_fc) - Yr_2025) / Yr_2025) * 100, 2),
        Yr_2025 = format(round(Yr_2025, 2), big.mark = ","),
        !!col_fc := format(round(get(col_fc), 2), big.mark = ",")
      )
    
    datatable(cmp_df, options = list(pageLength = 10, dom = 'ftip'), rownames = FALSE)
  })
  
  # Segment Taxonomy Table
  output$table_segment_ref <- renderDT({
    datatable(SEGMENT_TAXONOMY, options = list(pageLength = 12, dom = 't'), rownames = FALSE)
  })
  
  # CSV Handlers
  output$download_fc_csv <- downloadHandler(
    filename = function() { paste0("APAC_Pet_Food_Forecast_", input$country_select, ".csv") },
    content = function(file) { write.csv(filtered_fc_df(), file, row.names = FALSE) }
  )
}

# ------------------------------------------------------------------------------
# 5. LAUNCH SHINY APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
