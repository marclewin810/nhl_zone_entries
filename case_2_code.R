setwd("C:/Users/marcl/OneDrive/Documents")
library(dplyr)
library(readr)
entries  <- read_csv("entries.csv")
offense  <- read_csv("offense.csv")
# Convert shooting% from whole-number to decimal
offense <- offense %>%
  mutate(
    shooting_pct = shooting_pct / 100
  )
#drop total pp
offense <- offense %>% select(-total_pp)
df <- entries %>%
  left_join(offense, by = c("Season", "Team"))

df <- df %>%
  mutate(
    pp_pct = pp_pct / 100,
    points_pct = points_pct / 100
  )

model1 <- lm(
  xGF_60 ~ controlled_entry_rate +
    FF_60 +
    CF_60 +
    shooting_pct +
    pp_pct +
    points_pct +
    Season,
  data = df
)

summary(model1)

model2 <- lm(
  GF_60 ~ controlled_entry_rate + FF_60 + CF_60 + 
    shooting_pct + pp_pct + points_pct + Season,
  data = df
)
summary(model2)

model3 <- lm(
  entries_with_chances_60 ~ controlled_entry_rate + FF_60 + CF_60 +
    shooting_pct + pp_pct + points_pct + Season,
  data = df
)
summary(model3)

model4 <- lm(
  xGF_60 ~ controlled_entry_rate + FF_60 +
    shooting_pct + pp_pct + Season,
  data = df
)
summary(model4)

#SUMMARY STATISTICS TABLE ----------------------------------------------------------
library(dplyr)
library(gt)

summary_stats <- df %>% 
  select(
    xGF_60, 
    controlled_entry_rate, FF_60, CF_60,
    shooting_pct, pp_pct, points_pct
  ) %>% 
  summarise(
    Variable = names(.),
    Mean = sapply(., mean),
    SD   = sapply(., sd),
    Min  = sapply(., min),
    Max  = sapply(., max)
  )

summary_stats_gt <- summary_stats %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics for Zone Entry Efficiency Model"
  ) %>%
  fmt_number(
    columns = c(Mean, SD, Min, Max),
    decimals = 3
  )

summary_stats_gt

#REGRESSION RESULTS TABLE -------------------------------------------------------
library(dplyr)
library(broom)
library(gt)

# --- 1. Tidy and Format Model 1 Results ---

# Tidy the model results using broom::tidy()
results <- tidy(model1) %>%
  # Select the term, estimate, and p-value
  select(term, estimate, p.value) %>%
  mutate(
    # Create significance stars based on p-value
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Format the estimate to 3 decimal places
    # Now, CONCATENATE the estimate with the stars
    estimate = paste0(round(estimate, 3), stars),
    # Format the p-value (without stars)
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  )

# --- 2. Define and Apply Renaming Map ---

# Define a renaming map for your current model's terms
# NOTE: Please ensure these match the actual term names from R, 
# especially if Season generated multiple dummy variables (e.g., Season2019, etc.)
rename_map <- c(
  "(Intercept)" = "Intercept",
  "controlled_entry_rate" = "Controlled Entry Rate",
  "FF_60" = "Fenwick For (FF/60)",
  "CF_60" = "Corsi For (CF/60)",
  "shooting_pct" = "Shooting Percentage",
  "pp_pct" = "Power Play Percentage",
  "points_pct" = "Points Percentage",
  # Example for a continuous Season variable (adjust if factored)
  "Season" = "Season (Continuous Year)" 
  # If Season is a factor, you'd need to map all Season2019, Season2020 terms
)

# Apply this map to your results.
results$term <- rename_map[results$term]
# Clean up any potential NAs from the map
results <- results %>% filter(!is.na(term))

# --- 3. Get Glance Statistics ---

glance_stats <- glance(model1) %>%
  select(r.squared, adj.r.squared, nobs) %>%
  mutate(across(c(r.squared, adj.r.squared), round, 3))

# --- 4. Create the final gt table (Single Column) ---

gt_table_model1_final <- gt(results) %>%
  # Set the title
  tab_header(title = "OLS Regression Results: Factors Affecting Expected Goals For (xGF/60)") %>%
  # Define column labels
  cols_label(
    term = "Term", 
    estimate = "Estimate", 
    p.value = "p-value"
  ) %>%
  # Hide the 'stars' column used for computation
  cols_hide(
    columns = stars
  ) %>%
  # Add R-squared, N, and significance notes to the source note
  tab_source_note(
    source_note = paste0(
      "R² = ", glance_stats$r.squared, 
      " | Adjusted R² = ", glance_stats$adj.r.squared,
      " | Observations (N) = ", glance_stats$nobs,
      " | Stars: *** p<0.001, ** p<0.01, * p<0.05"
    )
  )

# Display the table
gt_table_model1_final


#MODEL COMPARISON TABLE ------------------------------------------------------------------
library(dplyr)
library(broom)
library(gt)

# --- 1. Extract Goodness-of-Fit Stats & Reorder ---

# We adjust the order here to match your request:
# 1. Base Model (model2 - GF/60)
# 2. Alt Model 1 (model3 - Chances/60)
# 3. Alt Model 2 (model4 - xGF/60 Reduced)
# 4. Alt Model 3 (model1 - xGF/60 Full)

gof_data <- bind_rows(
  glance(model2) %>% mutate(Model = "Base Model - GF/60"),
  glance(model3) %>% mutate(Model = "Alt Model 1 - Chances/60"),
  glance(model4) %>% mutate(Model = "Alt Model 2 - Parsimonious (robust check)"),
  glance(model1) %>% mutate(Model = "Alt Model 3 - xGF/60 (final)")
)

# --- 2. Select Columns and Create the GT Table ---

comparison_table_reordered <- gof_data %>%
  # Select only the columns we want to display
  select(Model, r.squared, adj.r.squared, AIC, BIC, nobs) %>%
  gt() %>%
  # Add a Title
  tab_header(
    title = "Comparison of Model Fit Statistics"
  ) %>%
  # Format the decimal places
  fmt_number(
    columns = c(r.squared, adj.r.squared),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(AIC, BIC),
    decimals = 1
  ) %>%
  # Rename the columns to look professional
  cols_label(
    r.squared = "R-Squared",
    adj.r.squared = "Adj. R-Squared",
    AIC = "AIC",
    BIC = "SIC (BIC)",
    nobs = "Observations"
  ) %>%
  # Optional: Highlight the rows if you want distinct styling for the Base Model
  # tab_style(
  #   style = cell_text(weight = "bold"),
  #   locations = cells_body(rows = Model == "Base Model")
  # ) %>%
  tab_source_note(
    source_note = "Note: Lower AIC/SIC and Higher R² indicate better fit."
  )

# Display the table
comparison_table_reordered


#TESTS -------------------------------------------------------------------------------------
library(car)      # For VIF and Durbin-Watson
library(lmtest)   # For Breusch-Pagan test
library(dplyr)
library(gt)
library(tibble)

# ==============================================================================
# TABLE 1: MODEL ASSUMPTIONS (Heteroscedasticity & Autocorrelation)
# ==============================================================================

# 1. Run the Tests
bp_test <- bptest(model1)
dw_test <- dwtest(model1)

# 2. Create the Data Frame
assumptions_data <- tibble(
  Test = c("Heteroscedasticity (Breusch-Pagan)", "Autocorrelation (Durbin-Watson)"),
  Statistic = c(bp_test$statistic, dw_test$statistic),
  `P-Value` = c(bp_test$p.value, dw_test$p.value),
  Interpretation = c(
    ifelse(bp_test$p.value < 0.05, "Non-Constant Variance (Bad)", "Constant Variance (Good)"),
    ifelse(dw_test$statistic < 1.5 | dw_test$statistic > 2.5, "Possible Autocorrelation", "No Autocorrelation")
  )
)

# 3. Create GT Table
table_assumptions <- assumptions_data %>%
  gt() %>%
  tab_header(
    title = "Model Assumption Checks",
    subtitle = "Analysis of Residuals for Zone Entry"
  ) %>%
  fmt_number(columns = c(Statistic), decimals = 2) %>%
  fmt_number(columns = `P-Value`, decimals = 3) %>%
  # Highlight significant p-values in red
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(rows = `P-Value` < 0.05)
  )

# ==============================================================================
# TABLE 2: MULTICOLLINEARITY (VIF) - FIXED FOR GVIF
# ==============================================================================

# 1. Calculate VIF
vif_values <- vif(model1)

# 2. Check if output is a Matrix (GVIF) or Vector (VIF) and extract accordingly
if (is.matrix(vif_values)) { 
  # CASE A: Matrix (occurs when Factors/Season are present)
  # We take the 1st column (GVIF) and use rownames for variable names
  vif_df <- data.frame(
    Variable = rownames(vif_values), 
    VIF = as.numeric(vif_values[, 1]), # Take the first column (GVIF)
    stringsAsFactors = FALSE
  )
} else {
  # CASE B: Standard Vector
  vif_df <- data.frame(
    Variable = names(vif_values), 
    VIF = as.numeric(vif_values),
    stringsAsFactors = FALSE
  )
}

# 3. Rename Variables (Using Base R to avoid errors)
vif_df$Variable[vif_df$Variable == "controlled_entry_rate"] <- "Controlled Entry Rate"
vif_df$Variable[vif_df$Variable == "FF_60"] <- "Fenwick For (FF/60)"
vif_df$Variable[vif_df$Variable == "CF_60"] <- "Corsi For (CF/60)"
vif_df$Variable[vif_df$Variable == "shooting_pct"] <- "Shooting Percentage"
vif_df$Variable[vif_df$Variable == "pp_pct"] <- "Power Play Percentage"
vif_df$Variable[vif_df$Variable == "points_pct"] <- "Points Percentage"
vif_df$Variable[grep("Season", vif_df$Variable)] <- "Season"

# 4. Create GT Table
table_vif <- vif_df %>%
  gt() %>%
  tab_header(
    title = "Multicollinearity Check (VIF)",
    subtitle = "Variance Inflation Factor by Variable"
  ) %>%
  fmt_number(columns = "VIF", decimals = 2) %>%
  cols_label(VIF = "VIF Score") %>%
  tab_source_note(
    source_note = "Note: VIF > 5 indicates potential multicollinearity. VIF > 10 indicates severe multicollinearity."
  ) %>%
  # Highlight "Severe" VIFs in Red
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(rows = VIF > 10)
  ) %>%
  # Highlight "Moderate" VIFs in Orange
  tab_style(
    style = cell_text(color = "orange", weight = "bold"),
    locations = cells_body(rows = VIF > 5 & VIF <= 10)
  )

# ==============================================================================
# DISPLAY TABLES
# ==============================================================================

table_assumptions
table_vif