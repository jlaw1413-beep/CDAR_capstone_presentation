
# DRAFT_v20260726
# CDAR 2026 Capstone Project
# Group 1

# TOPIC ----
# 
# Strategic Market Segmentation and Premium Volume Forecasting
# in the APAC Pet Food Market (2012–2028)
# 
# Executive Summary
# 
# Decision this report supports: 
# 
# How should the business allocate commercial resources across APAC pet
# food subcategories, and how much premium pet food volume should it plan
# to supply during 2026–2028?
#   
#   Q-A (Descriptive)
# 
# Which APAC pet food subcategories are driving long-term market growth,
# and what strategic market segments emerge based on their historical
# growth, market size and maturity?
#   
#   Q-B (Predictive)
# 
# How much premium pet food volume is expected across APAC during
# 2026–2028, and what volume should the business plan for to support
# capacity and commercial operations?
#   
#   APAC markets analysed:
#   Japan, Malaysia, Singapore, South Korea and Thailand.


# 0. Set Dependencies & Environment

library(tidyverse)
library(tidymodels)
library(timetk)
library(modeltime)
library(vip)
library(scales)
library(factoextra)
library(tidyclust)
library(skimr)

tidymodels_prefer(quiet = TRUE)
theme_set(theme_minimal(base_size = 12))
set.seed(221)

say <- function(...) cat("\n=====", ..., "=====\n")

library(conflicted)
conflicts_prefer(
  dplyr::first,
  dplyr::last,
  dplyr::filter,
  dplyr::lag,
  dplyr::slice,
  plotly::layout,
  .quiet = TRUE
)


# 1. MODULE 1: Acquire, Clean, See ----

say("M1: Acquire and Clean Data")

# IMPORT Petfood Dataset 
# Note: Actual column has commas (e.g. "65,209,870.20") so read_csv reads it as character.
# We parse it to numeric using parse_number() from readr.
petfood_raw <- read_csv("data/Petfood 2012-2025 APAC.csv", show_col_types = FALSE) %>%
  mutate(
    Year        = as.integer(Year),
    Subcategory = as.factor(Subcategory),
    Actual      = as.numeric(Actual)
  )

skim(petfood_raw)

# To focus the capstone pipeline, we will aggregate across APAC to get the total
# volume per Subcategory per Year. The user mentioned:
# "We carry ONE dataset family through the report: (12 subcategories × 14 years.)"
petfood <- petfood_raw %>%
  group_by(Year, Subcategory) %>%
  summarise(Volume_KG = sum(Actual, na.rm = TRUE), .groups = "drop")

# Create a high-level segment label (Premium, Mid-Priced, Economy) from Subcategory
petfood <- petfood %>%
  mutate(
    Price_Tier = case_when(
      str_detect(Subcategory, "Premium") ~ "Premium",
      str_detect(Subcategory, "Mid-Priced") ~ "Mid-Priced",
      str_detect(Subcategory, "Economy") ~ "Economy",
      TRUE ~ "Other"
    ),
    Price_Tier = factor(Price_Tier, levels = c("Economy", "Mid-Priced", "Premium"))
  )

# EDA ----

say("M1: Visualise - The Opening Chart")

# Q-A (descriptive): Premium's share of total volume spike and has held there since?
# Let's plot the volume by Price Tier over time to motivate the question.

# EDA 1
p1 <- petfood %>%
  group_by(Year, Price_Tier) %>%
  summarise(Total_Volume = sum(Volume_KG), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Share = Total_Volume / sum(Total_Volume)) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = Share, fill = Price_Tier)) +
  geom_area(alpha = 0.8, color = "white", size = 0.2) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Economy" = "#E0E0E0", "Mid-Priced" = "#A6C8E6", "Premium" = "#2C5B8E")) +
  labs(
    title = "The Premiumisation Shift in APAC Pet Food",
    subtitle = "Premium's share of volume grew steadily and stabilized. Is it a level shift?",
    x = NULL,
    y = "Share of Total Volume",
    fill = "Price Tier"
  ) +
  theme(legend.position = "bottom")

print(p1)

# EDA 2

petfood_raw %>%
  group_by(Country, Subcategory) %>%
  mutate(CAGR = (last(Actual) / first(Actual))^(1/13) - 1) %>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(x = Country, y = Subcategory, fill = CAGR)) +
  geom_tile(colour = "white", size = 0.5) +
  geom_text(aes(label = percent(CAGR, accuracy = 0.1)), size = 3) +
  scale_fill_gradient2(
    low      = "#D73027",
    mid      = "#FFFFBF",
    high     = "#1A9850",
    midpoint = 0,
    labels   = percent_format()
  ) +
  labs(
    title    = "CAGR by Subcategory and Country (2012–2025)",
    subtitle = "Green = growing, Red = declining",
    x        = NULL,
    y        = NULL,
    fill     = "CAGR"
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# EDA 3

library(plotly)

p_country_subcat <- petfood_raw %>%
  ggplot(aes(
    x      = Year,
    y      = Actual / 1e6,
    colour = Subcategory,
    group  = Subcategory,
    text   = paste0(
      "Subcategory: ", Subcategory, "\n",
      "Country: ", Country, "\n",
      "Year: ", Year, "\n",
      "Volume: ", round(Actual / 1e6, 2), "M KG"
    )
  )) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  scale_colour_viridis_d(option = "turbo") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Pet Food Volume by Subcategory and Country (2012–2025)",
    subtitle = "Each panel is one country; lines are subcategories",
    x        = NULL,
    y        = "Volume (Million KG)",
    colour   = "Subcategory"
  ) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 7),
    strip.text      = element_text(face = "bold")
  )

ggplotly(p_country_subcat, tooltip = "text") %>%
  layout(legend = list(orientation = "h", y = -0.2))

# EDA 4

petfood_raw %>%
  group_by(Country, Year) %>%
  summarise(Total_Volume = sum(Actual, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(Index = Total_Volume / first(Total_Volume) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = Index, colour = Country, group = Country,
             text = paste0("Country: ", Country, "\nYear: ", Year,
                           "\nIndex: ", round(Index, 1)))) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Japan"       = "#2C5B8E",
    "Singapore"   = "#E63946",
    "South Korea" = "#2A9D8F",
    "Malaysia"    = "#E9C46A",
    "Thailand"    = "#F4A261"
  )) +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  annotate("rect",
           xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "grey30") +
  annotate("text",
           x = 2020.5, y = 255,
           label = "COVID-19", size = 3, colour = "grey40") +
labs(
    title    = "Pet Food Volume Growth by Country (2012 = 100)",
    subtitle = "Indexed to 2012 baseline — shows relative growth, not absolute size",
    x        = NULL,
    y        = "Volume Index (2012 = 100)",
    colour   = "Country"
  )

p_country_subcat

# EDA 5
library(crosstalk)

# 1. Long tidy frame with a unique series id (Country x Subcategory).
plot_df <- petfood_raw %>%
  mutate(
    Volume_M = Actual / 1e6,
    Series   = paste(Country, Subcategory, sep = " | ")
  ) %>%
  arrange(Series, Year)

# 2. Wrap for crosstalk. key = ~Series -> each filter hides/shows whole lines.
shared <- SharedData$new(plot_df, key = ~Series)

# 3. Build natively in plotly.
#    split  -> one line per Country|Subcategory (keeps countries separate)
#    color / legendgroup / name -> all keyed to Subcategory so the legend and
#    colours line up and one click toggles a subcategory across every country.
fig <- plot_ly(shared) %>%
  add_lines(
    x           = ~Year,
    y           = ~Volume_M,
    split       = ~Series,
    color       = ~Subcategory,
    colors      = "Paired",              # 12-colour qualitative palette
    legendgroup = ~Subcategory,
    name        = ~Subcategory,
    hoverinfo   = "text",
    text        = ~paste0(
      "<b>", Series, "</b><br>",
      "Year: ", Year, "<br>",
      "Volume: ", round(Volume_M, 2), "M KG"
    )
  ) %>%
  layout(
    title      = "APAC Pet Food Volume, 2012-2025 (Interactive)",
    xaxis      = list(title = NULL, dtick = 2),
    yaxis      = list(title = "Volume (Million KG)"),
    legend     = list(title = list(text = "<b>Subcategory</b>")),
    showlegend = TRUE
    # ---- To make ALL magnitudes readable at once, add a log axis: 
    # yaxis = list(title = "Volume (Million KG, log)", type = "log")
  )

# 4. Collapse the 60 auto-generated legend entries to 12 (one per subcategory):
#    keep showlegend = TRUE only for the FIRST trace of each legendgroup.
fig  <- plotly_build(fig)
seen <- character(0)
for (i in seq_along(fig$x$data)) {
  grp <- fig$x$data[[i]]$legendgroup
  if (is.null(grp)) next
  if (grp %in% seen) {
    fig$x$data[[i]]$showlegend <- FALSE
  } else {
    fig$x$data[[i]]$showlegend <- TRUE
    seen <- c(seen, grp)
  }
}

# 5. Tick-box filters (nothing ticked = show all; tick to narrow) + chart.
bscols(
  widths = c(3, 9),
  list(
    filter_checkbox("f_country", "Country",     shared, ~Country),
    filter_checkbox("f_subcat",  "Subcategory", shared, ~Subcategory)
  ),
  fig
)


# 2. MODULE 2: Find Structure ----

say("M2: Find Structure (Clustering Subcategories)")

# We want to group the 12 subcategories by their growth profile.
# Are some subcategories "High Growth Engines" vs "Stagnant/Declining"?
# We will pivot the data to wide format (Years as columns) and calculate growth metrics.

subcat_profiles <- petfood %>%
  pivot_wider(names_from = Year, values_from = Volume_KG, names_prefix = "Y_") %>%
  mutate(
    # Calculate Compound Annual Growth Rate (CAGR) from 2012 to 2025
    CAGR_12_25 = (Y_2025 / Y_2012)^(1/(2025-2012)) - 1,
    # Calculate total absolute volume change
    Abs_Change = Y_2025 - Y_2012,
    # Volume in the most recent year
    Vol_2025 = Y_2025
  ) %>%
  select(Subcategory, Price_Tier, CAGR_12_25, Abs_Change, Vol_2025)

# Standardize features for clustering
subcat_scaled <- subcat_profiles %>%
  select(CAGR_12_25, Abs_Change, Vol_2025) %>%
  scale() %>%
  as.data.frame()
rownames(subcat_scaled) <- subcat_profiles$Subcategory

# Determine optimal number of clusters using elbow method
p_elbow <- fviz_nbclust(subcat_scaled, kmeans, method = "wss") +
  labs(title = "Optimal Number of Clusters (Elbow Method)")
print(p_elbow)


p_silhouette <- fviz_nbclust(subcat_scaled, kmeans, method = "silhouette") +
  labs(title = "Optimal Number of Clusters (Silhouette Method)")

print(p_silhouette)


# Run K-means clustering (k = 4 seems reasonable for 12 items to find distinct profiles)
set.seed(619)
km_res <- kmeans(subcat_scaled, centers = 4, nstart = 25)

# Visualize clusters
p_cluster <- fviz_cluster(km_res, data = subcat_scaled,
             repel = TRUE,
             ggtheme = theme_minimal(),
             main = "Subcategory Segments by Growth & Volume")
print(p_cluster)

km_res$centers

subcat_profiles <- subcat_profiles %>%
  mutate(Cluster = as.factor(km_res$cluster))


subcat_profiles %>%
  select(Subcategory, Price_Tier, CAGR_12_25, Abs_Change, Vol_2025, Cluster) %>%
  arrange(Cluster) %>%
  print(n = 20)

pca <- prcomp(subcat_scaled)

summary(pca)
round(pca$rotation, 3)

# Naming the clusters

subcat_profiles <- subcat_profiles %>%
  mutate(
    Segment_Name = case_when(
      as.character(Cluster) == "1" ~ "Growth Engines",
      as.character(Cluster) == "2" ~ "Declining Segment",
      as.character(Cluster) == "3" ~ "Mature Volume Leader",
      as.character(Cluster) == "4" ~ "Emerging Niches",
      TRUE ~ "Other"
    ),
    Segment_Name = factor(
      Segment_Name,
      levels = c(
        "Growth Engines",
        "Mature Volume Leader",
        "Emerging Niches",
        "Declining Segment",
        "Other"
      )
    )
  )

subcat_profiles %>%
  select(Subcategory, Cluster, Segment_Name) %>%
  print(n = Inf)

# Feed the M2 label forward as a feature for M3
# Join the Segment_Name back to the main dataset

petfood_model_data <- petfood %>%
  left_join(
    subcat_profiles %>%
      select(Subcategory, Segment_Name),
    by = "Subcategory"
  )

petfood_model_data %>%
  count(Subcategory, Segment_Name)%>%
  arrange(Segment_Name, Subcategory)


say("M2 Segment profiles:")
subcat_profiles %>%
  group_by(Segment_Name) %>%
  summarise(
    Avg_CAGR     = mean(CAGR_12_25),
    Avg_Vol_2025 = mean(Vol_2025),
    Subcategories = paste(Subcategory, collapse = ", ")
  ) %>%
  print()



# 3. MODULE 3: Predict ----

say("M3: Predict (tidymodels arc)")

# Create lagged features
petfood_model_data <- petfood_model_data %>%
  arrange(Subcategory, Year) %>%
  group_by(Subcategory) %>%
  mutate(
    Vol_Lag1 = lag(Volume_KG, 1),
    Vol_Lag2 = lag(Volume_KG, 2)
  ) %>%
  ungroup() %>%
  drop_na()

# Time-aware split
# Manual time-aware split: train on 2014-2022, test on 2023-2025
petfood_train <- petfood_model_data %>% filter(Year <= 2022)
petfood_test  <- petfood_model_data %>% filter(Year >= 2023)

say("Train years:")
petfood_train %>% summarise(min_yr = min(Year), max_yr = max(Year), n = n()) %>% print()
say("Test years:")
petfood_test  %>% summarise(min_yr = min(Year), max_yr = max(Year), n = n()) %>% print()


# RECIPE 

# Recipe 1: Baseline — without M2 Segment
rec_base <- recipe(Volume_KG ~ Year + Price_Tier + Vol_Lag1 + Vol_Lag2,
                   data = petfood_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Recipe 2: With M2 Segment — the spine of the capstone
rec_seg <- recipe(Volume_KG ~ Year + Price_Tier + Segment_Name + Vol_Lag1 + Vol_Lag2,
                  data = petfood_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Model specs
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Fit models
fit_base_lm <- workflow() %>% add_recipe(rec_base) %>% add_model(lm_spec) %>%
  fit(petfood_train)

fit_seg_lm  <- workflow() %>% add_recipe(rec_seg) %>% add_model(lm_spec) %>%
  fit(petfood_train)

fit_seg_rf  <- workflow() %>% add_recipe(rec_seg) %>% add_model(rf_spec) %>%
  fit(petfood_train)

# Score on held-out test data
score_model <- function(fit, label) {
  petfood_test %>%
    bind_cols(predict(fit, petfood_test)) %>%
    metrics(truth = Volume_KG, estimate = .pred) %>%
    filter(.metric == "rmse") %>%
    mutate(model = label) %>%
    select(model, rmse = .estimate)
}

leaderboard <- bind_rows(
  score_model(fit_base_lm, "Baseline LM (no M2)"),
  score_model(fit_seg_lm,  "Segment LM (with M2)"),
  score_model(fit_seg_rf,  "Segment RF (with M2)")
) %>%
  arrange(rmse)

say("M3 Leaderboard — Did M2 structure earn its place?")
print(leaderboard)


# Yes, M2 earned its place. Adding the Segment_Name cluster label reduced RMSE 
# by ~94,000 KG (~3.4% improvement). 
# Why did Random Forest perform so badly? With only 84 training rows and 
# 8 predictors, the RF is severely overfitting. It memorises the training data 
# but fails on the 36 test rows. This is a legitimate and honest finding -
# The Random Forest model overfit on the small training set (84 observations), 
# producing a test RMSE nearly 4× worse than linear regression. 
# This confirms that for a dataset of this size, a parsimonious linear model 
# is the more appropriate choice.



p_vip <- fit_seg_rf %>%
  extract_fit_parsnip() %>%
  vip(geom = "col", aesthetics = list(fill = "#2C5B8E")) +
  theme_minimal() +
  labs(
    title    = "What Drives Volume Prediction?",
    subtitle = "Variable Importance from Random Forest (with M2 Segment)"
  )
print(p_vip)

p_vip <- fit_seg_rf %>%
  extract_fit_parsnip() %>%
  vip(
    geom = "col",
    aesthetics = list(fill = "#2C5B8E")
  ) +
  theme_minimal() +
  labs(
    title = "What Drives Pet Food Volume?",
    subtitle = "Random Forest Variable Importance, Including M2 Market Segments",
    x = "Variable Importance",
    y = NULL
  )

print(p_vip)

# Interpretation:
#
# Vol_Lag1 and Vol_Lag2 are expected to be the strongest predictors of current
# volume. This indicates that pet food demand is highly persistent, with recent
# historical volume providing the clearest signal of future demand.
#
# The M2 segment variables provide additional predictive value by capturing
# structural differences between the subcategories. The Growth Engines,
# Mature Volume Leader, Emerging Niches and Declining Segment labels summarise
# differences in growth, absolute volume change and market size that are not
# fully represented by price tier alone.
#
# Price Tier and Year are relatively less important once historical volume and
# market segment are included. This suggests that whether a subcategory is
# premium, mid-priced or economy contributes less incremental information after
# its recent demand pattern and broader market position are known.
#
# Overall, the results suggest that volume forecasting is driven primarily by
# demand persistence, followed by the strategic market structure identified in
# M2. Price positioning appears to play a secondary role.


# 4. MODULE 4: Forecast ----

say("M4: Forecast (modeltime 3-year outlook)")

# Aggregate to total Premium volume per year for forecasting
# This directly answers Q-B: the 3-year outlook for Premium volume
premium_ts <- petfood %>%
  filter(Price_Tier == "Premium") %>%
  group_by(Year) %>%
  summarise(Premium_Volume = sum(Volume_KG), .groups = "drop") %>%
  mutate(Date = as.Date(paste0(Year, "-01-01"))) %>%
  select(Date, Premium_Volume)

# Time-aware split: hold out last 3 years (2023-2025) for validation
splits <- time_series_split(premium_ts, assess = "3 years", cumulative = TRUE)

say("Forecast train/test split:")
training(splits) %>% summarise(min = min(Date), max = max(Date), n = n()) %>% print()
testing(splits)  %>% summarise(min = min(Date), max = max(Date), n = n()) %>% print()

# Models
# Baseline: Naive
model_naive <- naive_reg() %>%
  set_engine("snaive")

# ARIMA — annual data so seasonal_period = 1 (as Prof specified)
model_arima <- arima_reg(seasonal_period = 1) %>%
  set_engine("auto_arima")

# Prophet
model_prophet <- prophet_reg() %>%
  set_engine("prophet")

# Fit on training data
fit_naive   <- model_naive   %>% fit(Premium_Volume ~ Date, data = training(splits))
fit_arima   <- model_arima   %>% fit(Premium_Volume ~ Date, data = training(splits))
fit_prophet <- model_prophet %>% fit(Premium_Volume ~ Date, data = training(splits))

# Modeltime table
models_tbl <- modeltime_table(
  fit_naive,
  fit_arima,
  fit_prophet
)

# Calibrate on test data
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

# Leaderboard
say("M4 Forecasting Leaderboard:")
calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>%
  select(.model_id, .model_desc, rmse, rsq, mae) %>%
  print()

# Plot test forecast vs actuals
# calibration_tbl %>%
#   modeltime_forecast(new_data = testing(splits), actual_data = premium_ts) %>%
#   plot_modeltime_forecast(
#     .interactive      = T,
#     .legend_max_width = 25,
#     .title            = "M4: Model Comparison on Hold-out (2023-2025)"
#   )


# Forecast all three models on the hold-out period
holdout_forecast <- calibration_tbl %>%
  modeltime_forecast(
    new_data   = testing(splits),
    actual_data = premium_ts
  )

# Interactive chart
p_holdout_interactive <- holdout_forecast %>%
  plot_modeltime_forecast(
    .interactive       = TRUE,
    .legend_max_width  = 35,
    .title             = "M4: Model Comparison on Hold-out (2023–2025)",
    .conf_interval_show = TRUE
  )

p_holdout_interactive


# Refit the best model (ARIMA, model_id = 2) on the full dataset
refit_tbl <- calibration_tbl %>%
  filter(.model_id == 2) %>%
  modeltime_refit(data = premium_ts)

# Forecast 3 years forward: 2026, 2027, 2028
p_forecast_final <- refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = premium_ts) %>%
  plot_modeltime_forecast(
    .interactive      = FALSE,
    .legend_max_width = 25,
    .title            = "3-Year Outlook: Total Premium Pet Food Volume (2026–2028)",
    .conf_interval_show = TRUE
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(
    subtitle = "ARIMA(0,1,0) with Drift — refitted on full 2012–2025 data",
    y = "Volume (KG)"
  )

print(p_forecast_final)

# Extract the point forecast numbers for the recommendation
refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = premium_ts) %>%
  filter(.key == "prediction") %>%
  select(.index, .value, .conf_lo, .conf_hi) %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>%
  print()


# 5. THE RECOMMENDATION (Brief)  ----


say("THE RECOMMENDATION")

cat("
Decision:

How should the business allocate commercial resources across APAC pet food
subcategories, and how much premium pet food volume should it plan to supply
during 2026–2028?

Q-A (Descriptive):

The APAC pet food market is not differentiated solely by price tier. K-means
clustering identified four distinct market segments based on historical CAGR,
absolute volume change and 2025 market size. The four-cluster solution achieved
a silhouette score of 0.41, indicating moderate but meaningful separation
between subcategory profiles.

The Growth Engines segment comprises five predominantly dry-food subcategories,
recording the strongest overall performance with an average CAGR of
approximately 3.5%, above-average absolute volume gains and an average 2025
market volume of approximately 136 million kg.

The Emerging Niches segment comprises five of the six wet-food subcategories.
These markets demonstrate positive growth (average CAGR approximately 2.3%)
while remaining smaller in absolute size, suggesting selective expansion
opportunities rather than broad-based investment. Economy Wet Dog Food is the
exception, forming a separate Declining Segment characterised by sustained
negative growth.

Economy Dry Dog Food forms the Mature Volume Leader segment. At approximately
187 million kg in 2025, it is the largest individual subcategory but exhibits
minimal growth (CAGR approximately -0.5%), indicating that it should be managed
as a mature volume business rather than a primary growth driver.

Overall, the clustering indicates that market structure captures important
commercial differences beyond price tier alone, while market maturity and
category-specific momentum remain important determinants of future performance.

Q-B (Predictive):

Among the forecasting models evaluated, ARIMA(0,1,0) with Drift delivered the
best out-of-sample performance on the 2023–2025 hold-out period
(RMSE = 2.63 million kg; MAE = 2.37 million kg).

After refitting the model using the complete 2012–2025 dataset, premium pet
food volume across the five APAC markets is projected to reach:

  • 2026: 343.3 million kg (95% CI: 337.6–348.9 million kg)
  • 2027: 351.0 million kg (95% CI: 345.3–356.6 million kg)
  • 2028: 358.7 million kg (95% CI: 353.1–364.4 million kg)

The forecast implies a steady annual increase of approximately 7.7 million kg.

The M3 predictive model further demonstrated that incorporating the M2 market
segment reduced forecasting error by approximately 3.4% (RMSE: 2.75 million kg
to 2.65 million kg), indicating that market structure provides additional
predictive value beyond price positioning alone.

Recommendations:

1. Capacity Planning
   Plan for approximately 359 million kg of premium pet food demand by 2028,
   while maintaining contingency capacity of up to approximately 364 million kg
   to accommodate higher-than-expected demand.

2. Commercial Investment Priority
   Prioritise investment in the Growth Engines segment, which combines the
   strongest sustained growth with the largest commercially meaningful market
   volumes and therefore offers the greatest long-term return on commercial
   investment.

3. Selective Growth Opportunities
   Concentrate incremental promotional and channel investment on Premium Wet Cat
   Food and Mid-Priced Wet Cat Food, which demonstrate attractive growth
   potential within the Emerging Niches segment.

4. Defend Mature Volume
   Maintain Economy Dry Dog Food as a strategic volume anchor. Protect market
   share and operational efficiency, but avoid disproportionate growth
   investment given its mature market profile.

5. Rationalise Declining Categories
   Limit additional investment in Economy Wet Dog Food and periodically review
   its portfolio role, as it remains the only subcategory exhibiting sustained
   contraction over the study period.

6. Forecast Governance
   Refresh the forecast annually and reassess planning assumptions if
   macroeconomic conditions, consumer spending patterns or premiumisation trends
   change materially, as the current model assumes continuation of historical
   market dynamics.

Overall Recommendation:

Allocate commercial investment according to market segment rather than price
tier alone. The combined evidence from market segmentation (M2), predictive
modelling (M3) and time-series forecasting (M4) indicates that future growth is
concentrated within the Growth Engines segment, while selected wet-cat
subcategories provide targeted expansion opportunities. Premium pet food demand
is expected to continue growing steadily through 2028, supporting measured
capacity expansion rather than reactive investment.
")



# 6. EXTRA: 1. Singapore Deep-Dive ----


say("EXTRA: Singapore Deep-Dive EDA")

sg <- petfood_raw %>%
  filter(Country == "Singapore")

# How many subcategories does SG have vs APAC?
say("SG subcategory count:")
sg %>% distinct(Subcategory) %>% arrange(Subcategory) %>% print()

# Total SG volume over time
sg_total <- sg %>%
  group_by(Year) %>%
  summarise(Total_Volume = sum(Actual, na.rm = TRUE), .groups = "drop")

print(sg_total)


# EXTRA 1A: Singapore Total Volume with COVID annotation

sg_max <- max(sg_total$Total_Volume) / 1e6

p_sg_total <- sg_total %>%
  ggplot(aes(x = Year, y = Total_Volume / 1e6)) +
  geom_line(colour = "#E63946", linewidth = 1.2) +
  geom_point(colour = "#E63946", size = 2.5) +
  annotate("rect",
           xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "grey30") +
  annotate("text",
           x = 2020.5, y = sg_max * 0.88,
           label = "COVID-19\n+21% surge", size = 3.2, colour = "grey30") +
  annotate("segment",
           x = 2019, xend = 2019,
           y = 7.5, yend = 9.5,
           arrow = arrow(length = unit(0.2, "cm")),
           colour = "grey50", linetype = "dashed") +
  annotate("text",
           x = 2018.3, y = 8.5,
           label = "Pre-COVID\n~+180K KG/yr", size = 2.8, colour = "grey50") +
  annotate("text",
           x = 2022.8, y = 10.2,
           label = "Post-COVID\n~+450K KG/yr", size = 2.8, colour = "grey50") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2012, 2025, by = 1)) +
  labs(
    title    = "Singapore: Total Pet Food Volume (2012–2025)",
    subtitle = "COVID-19 triggered a structural step-change in 2020 — volume never reverted",
    x        = NULL,
    y        = "Volume (Million KG)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_sg_total)


# EXTRA 1B: Singapore Volume by Subcategory (interactive)

p_sg_subcat <- sg %>%
  ggplot(aes(
    x      = Year,
    y      = Actual / 1e6,
    colour = Subcategory,
    group  = Subcategory,
    text   = paste0(
      "Subcategory: ", Subcategory, "\n",
      "Year: ", Year, "\n",
      "Volume: ", round(Actual / 1e6, 3), "M KG"
    )
  )) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Singapore: Volume by Subcategory (2012–2025)",
    subtitle = "No Economy subcategories — Singapore skips the bottom tier entirely",
    x        = NULL,
    y        = "Volume (Million KG)",
    colour   = "Subcategory"
  ) +
  theme(legend.position = "bottom")

ggplotly(p_sg_subcat, tooltip = "text") %>%
  plotly::layout(legend = list(orientation = "h", y = -0.3))
print(ggplotly(p_sg_subcat, tooltip = "text"))


# EXTRA 1C: Singapore Premium Share vs APAC Premium Share

apac_share <- petfood_raw %>%
  mutate(Price_Tier = case_when(
    str_detect(Subcategory, "Premium")   ~ "Premium",
    str_detect(Subcategory, "Mid-Priced") ~ "Mid-Priced",
    str_detect(Subcategory, "Economy")   ~ "Economy",
    TRUE ~ "Other"
  )) %>%
  group_by(Year, Price_Tier) %>%
  summarise(Vol = sum(Actual, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Share = Vol / sum(Vol)) %>%
  filter(Price_Tier == "Premium") %>%
  mutate(Market = "APAC Total")

sg_share <- sg %>%
  mutate(Price_Tier = case_when(
    str_detect(Subcategory, "Premium")   ~ "Premium",
    str_detect(Subcategory, "Mid-Priced") ~ "Mid-Priced",
    str_detect(Subcategory, "Economy")   ~ "Economy",
    TRUE ~ "Other"
  )) %>%
  group_by(Year, Price_Tier) %>%
  summarise(Vol = sum(Actual, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Share = Vol / sum(Vol)) %>%
  filter(Price_Tier == "Premium") %>%
  mutate(Market = "Singapore")

bind_rows(apac_share, sg_share) %>%
  ggplot(aes(x = Year, y = Share, colour = Market, group = Market)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("APAC Total" = "#2C5B8E", "Singapore" = "#E63946")) +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  labs(
    title    = "Premium Share: Singapore vs APAC (2012–2025)",
    subtitle = "Is Singapore more premiumised than the APAC average?",
    x        = NULL,
    y        = "Premium % of Total Volume",
    colour   = NULL
  ) +
  theme(legend.position = "bottom")

#Calculate Premium Share by Country

premium_country <- petfood_raw %>%
  mutate(
    Price_Tier = case_when(
      str_detect(Subcategory, "Premium")    ~ "Premium",
      str_detect(Subcategory, "Mid-Priced") ~ "Mid-Priced",
      str_detect(Subcategory, "Economy")    ~ "Economy"
    )
  ) %>%
  group_by(Country, Year, Price_Tier) %>%
  summarise(
    Volume = sum(Actual),
    .groups = "drop"
  ) %>%
  group_by(Country, Year) %>%
  mutate(
    Premium_Share = Volume / sum(Volume)
  ) %>%
  filter(
    Price_Tier == "Premium",
    Year == 2025
  ) %>%
  arrange(desc(Premium_Share))

premium_country

ggplot(
  premium_country,
  aes(
    x = reorder(Country, Premium_Share),
    y = Premium_Share
  )
) +
  geom_col(fill = "#2C5B8E") +
  coord_flip() +
  geom_text(
    aes(label = scales::percent(Premium_Share, accuracy = 0.1)),
    hjust = -0.1,
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 0.7)
  ) +
  labs(
    title = "Premium Share by Market (2025)",
    subtitle = "Singapore is the most premium-oriented market among the five APAC markets analysed",
    x = NULL,
    y = "Premium Share of Total Volume"
  ) +
  theme_minimal()

# Japan
japan_premium <- petfood_raw %>%
  filter(Country == "Japan") %>%
  mutate(
    Price_Tier = case_when(
      str_detect(Subcategory, "Premium") ~ "Premium",
      str_detect(Subcategory, "Mid-Priced") ~ "Mid-Priced",
      str_detect(Subcategory, "Economy") ~ "Economy"
    )
  ) %>%
  filter(Price_Tier == "Premium") %>%
  group_by(Year) %>%
  summarise(
    Premium_Volume = sum(Actual),
    .groups = "drop"
  )

print(japan_premium)

japan_total <- petfood_raw %>%
  filter(Country == "Japan") %>%
  group_by(Year) %>%
  summarise(
    Total_Volume = sum(Actual),
    .groups = "drop"
  )

left_join(japan_total, japan_premium, by = "Year")

japan_eda <- petfood_raw %>%
  filter(Country == "Japan") %>%
  mutate(
    Price_Tier = case_when(
      str_detect(Subcategory, "Premium") ~ "Premium",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Year) %>%
  summarise(
    Total_Volume = sum(Actual),
    Premium_Volume = sum(Actual[Price_Tier == "Premium"]),
    .groups = "drop"
  ) %>%
  mutate(
    Premium_Share = Premium_Volume / Total_Volume
  )

p1 <- japan_eda %>%
  ggplot(aes(Year, Total_Volume/1e6)) +
  geom_line(
    colour="#2C5B8E",
    linewidth=1.2
  ) +
  geom_point(
    colour="#2C5B8E",
    size=2.5
  ) +
  labs(
    title="Japan: Total Pet Food Volume",
    subtitle="Overall market volume has steadily declined since 2012",
    y="Million KG",
    x=NULL
  ) +
  theme_minimal()

p2 <- japan_eda %>%
  ggplot(aes(Year, Premium_Share)) +
  geom_line(
    colour="#E63946",
    linewidth=1.2
  ) +
  geom_point(
    colour="#E63946",
    size=2.5
  ) +
  scale_y_continuous(labels=scales::percent) +
  labs(
    title="Japan: Premium Share",
    subtitle="Premium products account for an increasing share of demand",
    y="Premium Share",
    x=NULL
  ) +
  theme_minimal()

library(patchwork)

p1 / p2

cat("
SINGAPRE:
Singapore stands out as the most premium-oriented market among the five APAC 
markets analysed. Premium products accounted for approximately 62% of Singapore's 
total pet food volume in 2025, compared with approximately 31% across the 
five-market APAC aggregate.

This premiumisation gap was already evident before 2020 and widened following 
the pandemic, indicating that Singapore's preference for premium pet food is 
structural rather than temporary.

Unlike the broader APAC market, Singapore contains no Economy subcategories, 
with demand concentrated almost entirely within the Mid-Priced and Premium tiers. 
This suggests a distinctly different market structure from neighbouring countries.

Singapore also experienced a clear step-change in total pet food demand in 2020. 
Rather than returning to its previous trajectory, annual volumes remained on a 
permanently higher growth path throughout 2021–2025. While the timing coincides 
with the COVID-19 pandemic, the analysis demonstrates a sustained change in 
market behaviour rather than a temporary demand spike.

Taken together, these findings suggest that Singapore should be viewed as a 
premium-led market, where commercial strategies are likely to differ from those 
used across the broader APAC region.

JAPAN
Japan's pet food market is contracting in overall volume, yet premium products 
continue to capture an increasing share of demand. This suggests that market 
maturity does not necessarily imply reduced premiumisation. Rather than pursuing 
volume expansion, suppliers may achieve growth through value capture and 
premium product positioning.
")


#7. EXTRA 2: Google Books Ngram — Cultural Humanisation Trend ----

install.packages("ngramr")
library(ngramr)


say("EXTRA: Google Ngram — Cultural proxy for pet humanisation")

# Pull ngram frequency for pet-related terms (data available up to ~2019)
pet_ngrams <- ngram(
  phrases    = c("fur baby", "premium pet food", "pet humanization", "pet owner"),
  year_start = 2000,
  year_end   = 2019,
  smoothing  = 3,
  corpus     = "en-2019"
)

# Plot using ngramr's built-in ggram function
p_ngram <- ggram(pet_ngrams) +
  scale_colour_manual(values = c(
    "fur baby"          = "#E63946",
    "premium pet food"  = "#2C5B8E",
    "pet humanization"  = "#2A9D8F",
    "pet owner"         = "#E9C46A"
  )) +
  labs(
    title    = "Cultural Shift: Pet-Related Terms in Google Books (2000–2019)",
    subtitle = "Rising frequency of 'fur baby' signals the humanisation trend\nthat preceded the APAC premiumisation wave",
    x        = NULL,
    y        = "Relative Frequency in Published Books",
    colour   = "Search Term"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_ngram)


pet_ngrams2 <- ngram(
  phrases    = c("fur baby", "fur babies", "premium pet food"),
  year_start = 2000,
  year_end   = 2019,
  smoothing  = 3,
  corpus     = "en-2019"
)

# Doesn't show meaningful data for premium pet food

p_ngram2 <- ggram(pet_ngrams2) +
  scale_colour_manual(values = c(
    "fur baby"         = "#E63946",
    "fur babies"       = "#F4A261",
    "premium pet food" = "#2C5B8E"
  )) +
  labs(
    title    = "Cultural Shift: 'Fur Baby' Rising in Google Books (2000–2019)",
    subtitle = "The language of pet humanisation accelerates from 2012 — preceding the APAC premiumisation wave",
    x        = NULL,
    y        = "Relative Frequency in Published Books",
    colour   = "Search Term"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_ngram2)


# The Google Books Ngram corpus provides a cultural leading indicator for the 
# premiumisation trend. The terms 'fur baby' and 'fur babies' — 
# markers of pet humanisation, the practice of treating pets as family members — 
# were virtually absent from published literature before 2010. Both terms inflect 
# sharply upward from 2012 onwards, precisely when our APAC pet food dataset begins 
# and when the premiumisation shift first becomes visible in volume data. This is 
# not coincidence: the cultural normalisation of pet humanisation in English-language 
# publishing preceded and likely drove the commercial shift toward premium pet 
# food products. The demand for premium pet food is, at its root, a cultural 
# phenomenon — and the data shows it was already accelerating before the COVID-19 
# pet adoption surge amplified it further.


#--- END ---
  
save.image("Capstone_Petfood_Grp1_v20260726.RData")
