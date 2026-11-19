#---- Introduction ----
# R-Project: Datamining in Asset Pricing

# Group 1:
# Benjamin Phumin Schmidhalter
# Yen Fang Tseng
# Bui Ngoc Minh Quach
# Nikolas Arent
 
# In this analysis we try to recreate three financial anomalies: 1. beta, 2, asset growth, 3. operating leverage.
# To assess whether our recreation of these anomalies is successful, we examine their influence on returns using the short-long term function and apply different data filters.
# Our approach follows Chen and Zimmerman (https://www.openassetpricing.com/)
# while our data filtering process is inspired by Guo et al. (https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4990134)
# Most of the code and explanations have been taken from TidyFinance and adapted accordingly.

rm(list=ls())
#---- 1. Installing/loading packages and initial setup ----

# Load packages
rm(list=ls())
library(tidyverse)
library(tidyfinance)
library(scales)
library(RSQLite)
library(dbplyr)
library(RPostgres)
library(frenchdata)
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)
library(slider)
library(furrr)
library(rlang)

# Initial setup
# Connection to WRDS
# set_wrds_credentials()
wrds <- get_wrds_connection()

# Create database for project
if (!dir.exists("data")) {
  dir.create("data")
}

# Prepare multisession for efficiency
n_cores = max(1, parallelly::availableCores() - 2)
future::plan(future::multisession, workers = n_cores)

#---- 2. Downloading and preparing data ----
start_date <- ymd("1980-01-01")
end_date <- ymd("2023-12-31")

msf_db <- tbl(wrds, I("crsp.msf_v2"))
stksecurityinfohist_db <- tbl(wrds, I("crsp.stksecurityinfohist"))

tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

# Fama-French 3 Factors
factors_ff3_monthly <- download_data(
  type = "factors_ff_3_monthly", 
  start_date = start_date, 
  end_date = end_date
)

# Fama-French 5 Factors
factors_ff5_monthly <- download_data(
  type = "factors_ff_5_2x3_monthly", 
  start_date = start_date, 
  end_date = end_date
)

# CRSP Monthly Data (Already Cleaned per: https://www.tidy-finance.org/r/wrds-crsp-and-compustat.html)
crsp_monthly <- download_data(
  type = "wrds_crsp_monthly", 
  start_date = start_date,
  end_date = end_date
)

# Compustat data
compustat <- download_data(
  type = "wrds_compustat_annual",
  start_date = start_date,
  end_date = end_date
)

# Data cleaning
# Exclude observations with missing returns or market cap
crsp_monthly <- crsp_monthly |>
  drop_na(ret_excess, mktcap, mktcap_lag)

# Save everything in our database
dbWriteTable(
  tidy_finance,
  "factors_ff3_monthly",
  value = factors_ff3_monthly,
  overwrite = TRUE
)

dbWriteTable(
  tidy_finance,
  "factors_ff5_monthly",
  value = factors_ff5_monthly,
  overwrite = TRUE
)

dbWriteTable(
  tidy_finance,
  "crsp_monthly",
  value = crsp_monthly,
  overwrite = TRUE
)

dbWriteTable(
  tidy_finance,
  "compustat",
  value = compustat,
  overwrite = TRUE
)

# Merging CRSP with Compustat
ccm_linking_table_db <- tbl(wrds, I("crsp.ccmxpf_lnkhist"))

ccm_linking_table <- ccm_linking_table_db |>
  filter(
    linktype %in% c("LU", "LC") &
      linkprim %in% c("P", "C")
  ) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

ccm_links <- crsp_monthly |>
  inner_join(ccm_linking_table, 
             join_by(permno), relationship = "many-to-many") |>
  filter(!is.na(gvkey) & 
           (date >= linkdt & date <= linkenddt)) |>
  select(permno, gvkey, date)

# Adding gvkey identifiers to CRSP table
crsp_monthly <- crsp_monthly |>
  left_join(ccm_links, join_by(permno, date))

dbWriteTable(
  tidy_finance,
  "crsp_monthly",
  value = crsp_monthly,
  overwrite = TRUE
)

#---- 3. Replicating anomalies ----
#---- First anomaly: Beta ----
# The beta anomaly (or low-beta anomaly) refers to the empirical finding that stocks with low beta (low risk relative to the market) tend to generate higher risk-adjusted returns than high-beta stocks
tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

# Load monthly stock returns and relevant factors
crsp_monthly_beta <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, date, industry, ret_excess) |>
  collect()

factors_ff3_monthly_beta <- tbl(tidy_finance, "factors_ff3_monthly") |>
  select(date, mkt_excess) |>
  collect()

# Merge stock returns with market excess returns to prepare for beta estimation
crsp_monthly_beta <- crsp_monthly_beta |>
  left_join(factors_ff3_monthly_beta, join_by(date))

# Function to estimate beta using the Capital Asset Pricing Model (CAPM)
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret_excess ~ mkt_excess, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

# Function to compute rolling beta over a given window
roll_capm_estimation <- function(data, months, min_obs) {
  data <- data |>
    arrange(date)
  
  betas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = "month",
    .f = ~ estimate_capm(., min_obs),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(tibble(
    date = unique(data$date),
    beta = betas
  ))
}

crsp_monthly_nested <- crsp_monthly_beta |>
  nest(data = -c(permno, industry))

# Compute rolling beta for each stock using a 60-month window
# Minimum of 48 observations (max. 60) required to ensure robust estimation
beta <- crsp_monthly_nested |>
  mutate(beta = future_map(
    data, ~roll_capm_estimation(., months = 60, min_obs = 48)
  )) |>
  unnest(c(beta)) |>
  select(permno, date, beta_monthly = beta) |>
  drop_na()

dbWriteTable(
  tidy_finance,
  "beta",
  value = beta,
  overwrite = TRUE
)

# Lag beta by one month to avoid look-ahead bias in future analyses
beta_lag <- beta |>
  mutate(date = date %m+% months(1)) |>
  select(permno, date, beta_lag = beta_monthly) |>
  drop_na()

#---- Second anomaly: Asset Growth ----
# The Asset Growth Anomaly refers to the tendency of firms with high asset growth (expanding balance sheets) to experience lower future stock returns compared to firms with low or negative asset growth.
tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

crsp_monthly_asset_growth <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, date, mktcap, mktcap_lag, industry, ret_excess) |>
  collect()

factors_ff3_asset_growth <- tbl(tidy_finance, "factors_ff3_monthly") |>
  select(date, mkt_excess) |>
  collect()

# Merge stock data with market excess return for asset growth analysis
crsp_monthly_asset_growth <- crsp_monthly_asset_growth |>
  left_join(factors_ff3_asset_growth, join_by(date))

# Compute asset growth as the percentage change in market capitalization
crsp_monthly_asset_growth <- crsp_monthly_asset_growth %>%
  mutate(asset_growth = ((mktcap - mktcap_lag) / mktcap_lag) * 100)

# Lag asset growth by one month to prevent look-ahead bias
asset_growth_lag <- crsp_monthly_asset_growth |>
  mutate(date = date %m+% months(1)) |>
  select(permno, date, asset_growth_lag = asset_growth) |>
  drop_na()

#---- Third anomaly: Operating Leverage ----
# The Operating ( OP) Leverage Anomaly suggests that firms with high operating leverage (high fixed costs relative to variable costs) tend to have higher stock return volatility and lower future returns than expected
tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

crsp_monthly_OL <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, date, mktcap, mktcap_lag, industry, ret_excess, gvkey) |>
  collect()

factors_ff3_OL <- tbl(tidy_finance, "factors_ff3_monthly") |>
  select(date, mkt_excess) |>
  collect()

compustat_OL <- tbl(tidy_finance, "compustat") |>
  select(gvkey, date, xsga, cogs, at) |>
  collect() |>
  mutate(
    year = year(date),
    xsga = ifelse(is.na(xsga), 0, xsga),
    cogs = ifelse(is.na(cogs), 0, cogs)
  )

crsp_monthly_OL <- crsp_monthly_OL |>
  left_join(factors_ff3_OL, join_by(date)) |>
  mutate(
    year = year(date)
  )

# Merge with Compustat accounting data based on gvkey and year
crsp_monthly_OL <- crsp_monthly_OL |>
  left_join(
    compustat_OL |> select(-date),
    join_by(gvkey, year)
  )

# Calculate Operating Leverage (OL) as a percentage of total assets
crsp_monthly_OL <- crsp_monthly_OL %>%
  mutate(OL = ((xsga + cogs) / at) * 100)

# Lag operating leverage to prevent look-ahead bias
op_leverage_lag <- crsp_monthly_OL |>
  select(permno, date, op_leverage_lag = OL) |>
  drop_na()

#---- 4. Creating portfolios based on our anomalies ----
# Merge different risk measures (Beta, Asset Growth, Operating Leverage) for portfolio sorting
data_for_sorts <- crsp_monthly |>
  inner_join(beta_lag, join_by(permno, date)) |>
  inner_join(asset_growth_lag, join_by(permno, date))|>
  inner_join(op_leverage_lag, join_by(permno, date))

# Function to Assign Portfolios Based on a Sorting Variable (Beta, Asset Growth, Operating Leverage)
assign_portfolio <- function(data, 
                             sorting_variable,
                             n_portfolios,
                             filters = NULL
) {
  
  ## Filter data if a condition is provided
  # This allows flexibility in applying different filters for portfolio construction. Default setting, all data will be used.
  if (!is.null(filters)) {
    filtered_data <- data %>%
      filter(!!filters) 
  } else {
    filtered_data <- data
  }
  
  # Compute breakpoints for quantile-based portfolio sorting
  breakpoints <- filtered_data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios based on sorting variable breakpoints
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  return(assigned_portfolios)
}


#---- 5. Filters for portfolios ----
# Function to compute portfolio returns based on a given sorting variable (Beta, Asset Growth, Operating Leverage) and filtering conditions
portfolios_filter <- function(filter_condition = NULL, value_weighted, sorting_variable) {
  data_for_sorts |>
    group_by(date) |>
    mutate(
      portfolio = assign_portfolio(
        data = pick(everything()),
        sorting_variable = !!sym(sorting_variable),
        n_portfolios = 10,
        filters = filter_condition
      ),
      portfolio = as.factor(portfolio)
    ) |>
    group_by(portfolio, date) |>
    summarize(
      ret = if_else(value_weighted,
                    weighted.mean(ret_excess, mktcap_lag),  # Value-weighted returns
                    mean(ret_excess)  # Equal-weighted returns
      ),
      .groups = "drop"
    ) |>
    left_join(factors_ff5_monthly, join_by(date)) # Merge with Fama-French five-factor model data
}

## Define various filters to test robustness
'These filters help examine whether portfolio returns vary by excluding certain stocks. The filters are adapted from Guo et al. (2024), p.10.'
filters <- list(
  all_data = NULL, # Use all available data
  exclude_nasdaq = quo(exchange != "NASDAQ"), # Exclude NASDAQ stocks
  exclude_finance = quo(industry != "Finance"), # Exclude financial sector stocks
  exlude_price_under_5 = quo(prc > 5), # Exclude stocks priced below $5
  exclude_price_under_100 = quo(prc > 100) # Exclude stocks priced below $100
)

filters2 <- list(
  exclude_nasdaq_finance = quo(exchange != "NASDAQ" & industry != "Finance"), # Exclude NASDAQ & Finance
  exclude_nasdaq_price = quo(exchange != "NASDAQ" & prc > 5), # Exclude NASDAQ & low-priced stocks
  exclude_nasdaq_finance_price = quo(exchange != "NASDAQ" & industry != "Finance" & prc > 5), # Exclude all three
  exclude_finance_price = quo(industry != "Finance" & prc > 5) # Exclude Finance & low-priced stocks
)

#---- 6. Evaluation ----
## Function for calculating the long-short portfolio and evaluating performance
evaluate_strategy <- function(filter_condition, value_weighted, sorting_variable) {
  # Compute portfolio returns based on sorting variable and filter condition
  anomaly_longshort <- portfolios_filter(filter_condition, value_weighted, sorting_variable)
  # Construct long-short portfolio: Going short on the 10th decile (highest beta/asset growth/operating leverage values) and long on the 1st decile
  longshort_portfolio <- anomaly_longshort |>
    pivot_wider(id_cols = date, names_from = portfolio, values_from = ret) |>
    mutate(long_short = `10` - `1`) # Captures return differential between selected portfolios
  
  # Estimate average excess return of the long-short strategy
  model_fit <- lm(long_short ~ 1, data = longshort_portfolio)
  
  # Compute total return of the long-short portfolio
  total_return <- sum(longshort_portfolio$long_short, na.rm = TRUE)
  
  # Merge long-short portfolio returns with the broader dataset for further analysis
  anomaly_longshort <- anomaly_longshort |>
    left_join(longshort_portfolio |>
                select(date, long_short), by = "date")
  
  # Compute correlations of long-short portfolio returns with Fama-French factors
  correlations <- anomaly_longshort |>
    summarize(
      cor_mkt = cor(anomaly_longshort$long_short , anomaly_longshort$mkt_excess),
      cor_smb = cor(anomaly_longshort$long_short , anomaly_longshort$smb),
      cor_hml = cor(anomaly_longshort$long_short , anomaly_longshort$hml),
      cor_rmw = cor(anomaly_longshort$long_short , anomaly_longshort$rmw),
      cor_cma = cor(anomaly_longshort$long_short , anomaly_longshort$cma),
      .groups = "drop"
    )
  
  # Compute Sharpe Ratios for performance evaluation
  sharpe_ratios <- anomaly_longshort |>
    summarize(
      SR_port = mean(long_short, na.rm=TRUE) / sd(long_short, na.rm=TRUE),
      SR_mkt_excess = mean(mkt_excess, na.rm=TRUE) / sd(mkt_excess, na.rm=TRUE),
      SR_hml = mean(hml, na.rm=TRUE) / sd(hml, na.rm=TRUE),
      SR_smb = mean(smb, na.rm=TRUE) / sd(smb, na.rm=TRUE),
      SR_rmw = mean(rmw, na.rm=TRUE) / sd(rmw, na.rm=TRUE),
      SR_cma = mean(cma, na.rm=TRUE) / sd(cma, na.rm=TRUE),
      .groups = "drop"
    )
  
  # Run a Fama-French five-factor regression to evaluate abnormal returns (alpha)
  five_factor_model <- lm(long_short ~ mkt_excess + smb + hml + rmw + cma, data = anomaly_longshort)
  
  # Store model summaries for interpretation
  model_summaries <- list(
    ff5 = summary(five_factor_model)
  )
  
  # Return a list with key results based on the different anomaly and filters used
  list(
    data = anomaly_longshort,
    model = model_fit,
    total_return = total_return,
    coeftest = coeftest(model_fit, vcov = NeweyWest),
    correlations = correlations,
    sharpe_ratios = sharpe_ratios,
    model_summaries = model_summaries
  )
}

#---- 7. (Organizing) Results ----
# Apply the function to all filters and save models
# The filtering is split into two sets to prevent RAM overload.
' Equal-weighted: Each stock gets equal weighting regardless of size.
  Value-weighted: Portfolio returns are weighted by firm market capitalization.'

# Beta Anomaly: Testing whether stock market beta predicts future returns
results_equal_weighted_beta <- map(filters, ~ evaluate_strategy(.x, FALSE, "beta_lag"))
results_value_weighted_beta <- map(filters, ~ evaluate_strategy(.x, TRUE, "beta_lag"))  
results2_equal_weighted_beta <- map(filters2, ~ evaluate_strategy(.x, FALSE, "beta_lag"))
results2_value_weighted_beta <- map(filters2, ~ evaluate_strategy(.x, TRUE, "beta_lag"))

# Asset Growth Anomaly: Examining whether firms with high past growth underperform
results_equal_weighted_growth <- map(filters, ~ evaluate_strategy(.x, FALSE, "asset_growth_lag"))
results_value_weighted_growth <- map(filters, ~ evaluate_strategy(.x, TRUE, "asset_growth_lag"))
results2_equal_weighted_growth <- map(filters2, ~ evaluate_strategy(.x, FALSE, "asset_growth_lag"))
results2_value_weighted_growth <- map(filters2, ~ evaluate_strategy(.x, TRUE, "asset_growth_lag"))

# Operating Leverage Anomaly: Testing if firms with high fixed costs exhibit return predictability
results_equal_weighted_leverage <- map(filters, ~ evaluate_strategy(.x, FALSE, "op_leverage_lag"))
results_value_weighted_leverage <- map(filters, ~ evaluate_strategy(.x, TRUE, "op_leverage_lag"))
results2_equal_weighted_leverage <- map(filters2, ~ evaluate_strategy(.x, FALSE, "op_leverage_lag"))
results2_value_weighted_leverage <- map(filters2, ~ evaluate_strategy(.x, TRUE, "op_leverage_lag"))

# Organizing results
  
  # list with categories
  elements <- list(
    Beta = list(
      value_weighted1 = results_value_weighted_beta,
      value_weighted2 = results2_value_weighted_beta,
      equal_weighted1 = results_equal_weighted_beta,
      equal_weighted2 = results2_equal_weighted_beta
    ),
    Growth = list(
      value_weighted1 = results_value_weighted_growth,
      value_weighted2 = results2_value_weighted_growth,
      equal_weighted1 = results_equal_weighted_growth ,
      equal_weighted2 = results2_equal_weighted_growth
    ),
    Leverage = list(
      value_weighted1 = results_value_weighted_leverage,
      value_weighted2 = results2_value_weighted_leverage,
      equal_weighted1 = results_equal_weighted_leverage,
      equal_weighted2 =results2_equal_weighted_leverage
    )
  )

# Empty list of all results
results_list <- list()

# Extracting Model Coefficients and Significance
# Iterates through all anomalies and filters to extract key statistics
for (anomaly in names(elements)) {
  for (weighted in names(elements[[anomaly]])) {
    element <- elements[[anomaly]][[weighted]]
    
    # Extract "value" or "equal" for identification
    weighted_clean <- ifelse(grepl("value", weighted), "value", "equal")
    
    for (filter in names(element)) {
      coeftest_matrix <- element[[filter]]$coeftest
      
      estimate_values <- round(coeftest_matrix[, "Estimate"], 4)
      p_value_values <- round(coeftest_matrix[, "Pr(>|t|)"], 4)
      
      # Assign significance levels based on p-values
      significance <- sapply(p_value_values, function(p) {
        if (p < 0.001) {
          return("***")
        } else if (p < 0.01) {
          return("**")
        } else if (p < 0.05) {
          return("*")
        } else if (p < 0.1) {
          return(".")
        } else {
          return(" ")
        }
      })
      
      # Save results in a structured format
      results_list[[length(results_list) + 1]] <- data.frame(
        anomaly = anomaly,
        weighted = weighted_clean,
        filter = filter,
        coefficient = estimate_values,
        p_value = p_value_values,
        significance = significance
      )
    }
  }
}

# Combine results into a single table
final_results <- do.call(rbind, results_list)

# Print final table of regression coefficients, p-values, and significance levels for long-short portfolios across anomalies, weightings, and filters
print(final_results)

# Beta Anomaly
View(results_equal_weighted_beta)
View(results_value_weighted_beta)
View(results2_equal_weighted_beta)
View(results2_value_weighted_beta)

# Asset Growth Anomaly
View(results_equal_weighted_growth)
View(results_value_weighted_growth)
View(results2_equal_weighted_growth)
View(results2_value_weighted_growth)

# Operating Leverage Anomaly
View(results_equal_weighted_leverage)
View(results_value_weighted_leverage)
View(results2_equal_weighted_leverage)
View(results2_value_weighted_leverage)

# --------------------------------------------------------------------------------
# How the Results Are Stored:
# The function returns a named list containing key metrics for performance evaluation.
#
# List Element         | Description
# ---------------------|----------------------------------------------------------
# data                | Full dataset, including long-short returns
# model               | Linear regression model for average return estimation
# total_return        | Sum of all long-short portfolio returns over time
# coeftest            | Statistical test results for the model (Newey-West standard errors)
# correlations        | Correlations of long-short returns with Fama-French factors
# sharpe_ratios       | Sharpe Ratios for various factors and the long-short portfolio
# model_summaries     | Fama-French five-factor regression summary
#
# How to Access the Results:
# results <- evaluate_strategy(filter_condition, value_weighted, sorting_variable)
#
# Access individual components:
# results$data            # View full dataset with long-short returns
# results$total_return    # Get total return of long-short strategy
# summary(results$model)  # View summary of the regression model
# results$coeftest        # Check statistical significance with Newey-West correction
# results$correlations    # View correlations with Fama-French factors
# results$sharpe_ratios   # Get Sharpe Ratios
# results$model_summaries$ff5  # View Fama-French five-factor model summary
# Example: results2_value_weighted_leverage$exclude_finance_price$model_summaries accesses the Fama-French five-factor regression summaries for the value weighted operating leverage portfolio with filtering condition exclude finance stocks and stocks with price < 5.
# --------------------------------------------------------------------------------
print(results_equal_weighted_beta$all_data$model_summaries)

print(results_equal_weighted_growth$exclude_finance$model_summaries)

print(results2_value_weighted_leverage$exclude_nasdaq_finance$model_summaries)



#---- 8. Key findings and conclusion ----
# Interpretation of Results

# 1. Beta Anomaly:
# - The Beta anomaly suggests that low-beta stocks should outperform high-beta stocks on a risk-adjusted basis.
# - No strong evidence that the beta anomaly generates excess returns.
# - Coefficients are small (0.0016 - 0.0028), indicating marginal excess returns.
# - All p-values are above 0.05, meaning results are not statistically significant.
# - The best case (excluding Nasdaq & price filters) shows p-values ~0.4.
# - FF5: The beta portfolio is strongly influenced by market movements, with small-cap stocks contributing positively. High-profitability stocks underperform, while growth stocks slightly outperform value stocks. Asset growth has no significant impact on portfolio returns.
# - Conclusion: No support for the Beta anomaly; any observed differences are likely due to random variation.

# 2. Growth Anomaly:
# - The Growth anomaly suggests high-growth stocks underperform low-growth stocks, supporting value investing theories.
# - Value-weighted strategies show no significance.
# - Equal-weighted strategies show significant negative returns (coefficients: -0.0217 to -0.0149).
# - P-values are below 0.001, meaning high-growth stocks underperform low-growth stocks.
# - FF5: The growth portfolio is negatively impacted by market movements, with value stocks underperforming. Small-cap stocks show no significant effect, while high-profitability stocks slightly underperform. Asset growth has a strong positive impact on returns contradicting our assumption.
# - Conclusion: Strong evidence that high-growth stocks underperform, aligning with value investing principles.

# 3. Leverage Anomaly:
# - The Leverage anomaly examines whether firms with higher debt levels generate excess returns.
# - Mostly no significant relationship between leverage and returns.
# - Coefficients range from -0.0030 to 0.0033, with most p-values above 0.10.
# - The strongest case is value-weighted portfolios excluding Nasdaq & Finance (coef: 0.0033, p = 0.0448), but effects are weak elsewhere.
# - FF5: The Operating Leverage portfolio is negatively impacted by market movements, with value stocks significantly underperforming. Small-cap stocks contribute positively, while high-profitability stocks show strong positive effects. Asset growth has a strong positive impact, suggesting that firms with higher asset growth outperform.
# - Conclusion: Leverage does not predict excess returns in most cases; effects may already be priced in.

# Final Takeaways:
# - No support for the Beta anomaly.
# - Strong evidence that growth stocks underperform in equal-weighted strategies.
# - Weak support for the Leverage anomaly.

# Comparison with Chen & Zimmermann (2020):
# 1. Beta Anomaly: 
# - Their study also only found weak and non-significant evidence (t-value = 1.72) for beta predictability (p.61).
# - Differences may arise due to sample periods, weighting methods, and portfolio sorting techniques.
# 2. Growth Anomaly:
# - Their study found strong negative predictability (t-value = -4.84) for asset growth (p.64).
# - Our study confirms this, but differences in magnitude could be due to time period, factor controls, or data filtering.
# 3. Leverage Anomaly:
# - Their study also found no significant effect (t-value = -1.59) of leverage (p.64).
# - Our findings align mostly, suggesting leverage is not a reliable predictor of excess returns.