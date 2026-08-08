# ==============================================================================
# CDAR 2026 Capstone — Country-Level Forecast Artefact Builder
#
# PURPOSE: Pre-compute ARIMA forecasts for every Country x Subcategory
#          combination found in the source CSV, plus the APAC aggregate.
#          Saves results as country_forecast_artefact.rds.
#
# HOW TO RUN:
#   1. Place this file in the SAME folder as Petfood_2012-2025_APAC.csv
#   2. Source the script (Ctrl+Shift+S) in RStudio
#   3. Takes ~2-3 minutes; writes country_forecast_artefact.rds to same folder
#   4. Then open CDAR2026_Shiny_Country_Dashboard.R and click Run App
#
# REQUIRED PACKAGES (install once if missing):
#   install.packages(c("tidyverse", "tidymodels", "modeltime", "timetk"))
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)

tidymodels_prefer(quiet = TRUE)

say <- function(...) cat("\n=====", ..., "=====\n")

# ==============================================================================
# 1. Load data
# ==============================================================================
say("Step 1: Load data")

petfood_raw <- read_csv("data/Petfood 2012-2025 APAC.csv", show_col_types = FALSE) %>%
  # Defensive: drop any fully-blank rows before grouping/summarising, so a
  # spurious NA "country"/"subcategory" group can never sneak into downstream
  # unique()/sort() calls even if a future export of this file reintroduces
  # blank separator rows (as an earlier version of this file did).
  filter(!is.na(Country), !is.na(Subcategory), !is.na(Year)) %>%
  mutate(
    Year        = as.integer(Year),
    Country     = as.character(Country),
    Subcategory = as.character(Subcategory),
    Actual      = as.numeric(Actual)
  )

# Country-level data (keep individual countries)
petfood_country <- petfood_raw %>%
  group_by(Country, Year, Subcategory) %>%
  summarise(Volume_KG = sum(Actual, na.rm = TRUE), .groups = "drop")

# APAC aggregate (sum across all countries)
petfood_apac <- petfood_raw %>%
  group_by(Year, Subcategory) %>%
  summarise(Volume_KG = sum(Actual, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = "APAC Total")

# Combined dataset
petfood_all <- bind_rows(petfood_country, petfood_apac)

all_countries <- c("APAC Total", sort(unique(petfood_country$Country)))
all_subcats   <- sort(unique(petfood_raw$Subcategory))

cat("Countries:", paste(all_countries, collapse = ", "), "\n")
cat("Subcategories:", length(all_subcats), "\n")
cat("Total models to fit:", length(all_countries) * length(all_subcats), "\n")

# ==============================================================================
# 2. M2 Segment assignment (k=4, set.seed(619) — matches main analysis script,
#    Capstone_Petfood_Grp1_v20260726.R line ~342)
#    Computed on APAC totals only (same as original analysis)
# ==============================================================================
say("Step 2: Assign M2 segments (APAC totals)")

set.seed(619)

subcat_profiles <- petfood_apac %>%
  pivot_wider(names_from = Year, values_from = Volume_KG, names_prefix = "Y_") %>%
  mutate(
    CAGR_12_25 = (Y_2025 / Y_2012)^(1 / (2025 - 2012)) - 1,
    Abs_Change = Y_2025 - Y_2012,
    Vol_2025   = Y_2025
  ) %>%
  select(Subcategory, CAGR_12_25, Abs_Change, Vol_2025)

subcat_scaled <- subcat_profiles %>%
  select(CAGR_12_25, Abs_Change, Vol_2025) %>%
  scale() %>%
  as.data.frame()
rownames(subcat_scaled) <- subcat_profiles$Subcategory

km_res <- kmeans(subcat_scaled, centers = 4, nstart = 25)

subcat_profiles <- subcat_profiles %>%
  mutate(
    Cluster = as.factor(km_res$cluster),
    Segment_Name = case_when(
      Cluster == 1 ~ "Growth Engines",
      Cluster == 2 ~ "Declining Segment",
      Cluster == 3 ~ "Mature Volume Leader",
      Cluster == 4 ~ "Emerging Niches",
      TRUE ~ "Other"
    ),
    Price_Tier = case_when(
      str_detect(Subcategory, "Premium")    ~ "Premium",
      str_detect(Subcategory, "Mid-Priced") ~ "Mid-Priced",
      str_detect(Subcategory, "Economy")    ~ "Economy",
      TRUE ~ "Other"
    )
  )

# ==============================================================================
# 3. Fit ARIMA models for every Country x Subcategory combination
#
#    Per series workflow:
#      a) Split: train 2012-2022, calibrate 2023-2025 (for CI computation)
#      b) Fit ARIMA on training data
#      c) Calibrate on holdout
#      d) Refit on full data (2012-2025)
#      e) Forecast 5 years ahead (2026-2030)
# ==============================================================================
say("Step 3: Fit ARIMA models (this takes ~2-3 minutes)")

HORIZON <- 5
forecast_rows <- list()
total   <- length(all_countries) * length(all_subcats)
counter <- 0

for (ctry in all_countries) {
  for (sc in all_subcats) {
    counter <- counter + 1
    cat(sprintf("  [%d/%d] %s — %s\n", counter, total, ctry, sc))

    ts_data <- petfood_all %>%
      filter(Country == ctry, Subcategory == sc) %>%
      filter(!is.na(Year), !is.na(Volume_KG)) %>%
      mutate(Year = as.integer(Year)) %>%
      filter(!is.na(Year)) %>%
      arrange(Year) %>%
      mutate(Date = as.Date(sprintf("%04d-01-01", Year))) %>%
      select(Date, Volume_KG)

    # Skip if insufficient data
    if (nrow(ts_data) < 5 || all(ts_data$Volume_KG == 0, na.rm = TRUE)) {
      cat("    SKIPPED (insufficient data)\n")
      next
    }

    train_data <- ts_data %>% filter(Date <= as.Date("2022-01-01"))
    test_data  <- ts_data %>% filter(Date >= as.Date("2023-01-01"))

    # Need at least 3 training points
    if (nrow(train_data) < 3) {
      cat("    SKIPPED (too few training points)\n")
      next
    }

    tryCatch({
      fit <- arima_reg(seasonal_period = 1) %>%
        set_engine("auto_arima") %>%
        fit(Volume_KG ~ Date, data = train_data)

      calib_tbl <- modeltime_table(fit) %>%
        modeltime_calibrate(new_data = test_data)

      refit_tbl <- calib_tbl %>%
        modeltime_refit(data = ts_data)

      fc_tbl <- refit_tbl %>%
        modeltime_forecast(
          h             = paste0(HORIZON, " years"),
          actual_data   = ts_data,
          conf_interval = 0.95
        )

      fc_cols <- names(fc_tbl)
      has_ci  <- ".conf_lo" %in% fc_cols

      hist_rows <- fc_tbl %>%
        filter(.model_desc == "ACTUAL") %>%
        select(.index, .value) %>%
        transmute(
          Country     = ctry,
          Subcategory = sc,
          Year        = as.integer(format(.index, "%Y")),
          Type        = "Historical",
          Volume_KG   = .value,
          Lo95        = NA_real_,
          Hi95        = NA_real_
        )

      fc_only <- fc_tbl %>% filter(.model_desc != "ACTUAL")

      fc_rows <- tibble(
        Country     = ctry,
        Subcategory = sc,
        Year        = as.integer(format(fc_only$.index, "%Y")),
        Type        = "Forecast",
        Volume_KG   = fc_only$.value,
        Lo95 = if (has_ci) fc_only[[".conf_lo"]] else rep(NA_real_, nrow(fc_only)),
        Hi95 = if (has_ci) fc_only[[".conf_hi"]] else rep(NA_real_, nrow(fc_only))
      )

      key <- paste0(ctry, "|||", sc)
      forecast_rows[[key]] <- bind_rows(hist_rows, fc_rows)

    }, error = function(e) {
      cat("    ERROR:", conditionMessage(e), "\n")
    })
  }
}

# ==============================================================================
# 4. Combine and save
# ==============================================================================
say("Step 4: Combine and save artefact")

forecast_artefact <- bind_rows(forecast_rows) %>%
  left_join(
    subcat_profiles %>%
      select(Subcategory, Segment_Name, Price_Tier, CAGR_12_25, Vol_2025),
    by = "Subcategory"
  ) %>%
  arrange(Country, Segment_Name, Subcategory, Year)

artefact <- list(
  forecast  = forecast_artefact,
  profiles  = subcat_profiles,
  countries = all_countries,
  subcats   = all_subcats
)

saveRDS(artefact, "country_forecast_artefact.rds")

cat("\nDone! country_forecast_artefact.rds written to:\n")
cat(" ", normalizePath("country_forecast_artefact.rds"), "\n\n")
cat("Rows:", nrow(forecast_artefact), "\n")
cat("Countries:", length(unique(forecast_artefact$Country)), "\n")
cat("Subcategories:", length(unique(forecast_artefact$Subcategory)), "\n")
cat("Years:", paste(sort(unique(forecast_artefact$Year)), collapse = ", "), "\n")
cat("CI available:", !all(is.na(forecast_artefact$Lo95[forecast_artefact$Type == "Forecast"])), "\n")
cat("\nYou can now open CDAR2026_Shiny_Country_Dashboard.R and click Run App.\n")
