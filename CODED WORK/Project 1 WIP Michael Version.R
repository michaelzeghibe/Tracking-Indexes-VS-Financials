install.packages(c("readxl","dplyr","tidyr","stringr","lubridate","purrr","ggplot2"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)

setwd("C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials/Data Scraped Online")

voo  <- read_excel("VOO_PriceHistory.xlsx")
qqq  <- read_excel("QQQ_PriceHistory.xlsx")
arkk <- read_excel("ARKK_PriceHistory.xlsx")
mags <- read_excel("MAGS_PriceHistory.xlsx")

get_monthly_log_returns <- function(df, ticker){
  
  # standardize column names
  names(df) <- names(df) |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("_$", "")
  
  # --- find date column robustly ---
  date_candidates <- grep("^date$", names(df), value = TRUE)
  if (length(date_candidates) == 0) {
    # sometimes it's like "timestamp" or "trading_date"
    date_candidates <- grep("date", names(df), value = TRUE)
  }
  if (length(date_candidates) == 0) stop(paste("No date-like column found for", ticker))
  date_col <- date_candidates[1]
  
  # --- find price column robustly ---
  # Prefer "adj...close" then fallback to "close" then fallback to anything with "price"
  adj_candidates   <- grep("adj.*close", names(df), value = TRUE)
  close_candidates <- grep("^close$|close$", names(df), value = TRUE)
  price_candidates <- grep("price|last", names(df), value = TRUE)
  
  price_col <- NA_character_
  if (length(adj_candidates) > 0) {
    price_col <- adj_candidates[1]
  } else if (length(close_candidates) > 0) {
    price_col <- close_candidates[1]
  } else if (length(price_candidates) > 0) {
    price_col <- price_candidates[1]
  }
  
  if (is.na(price_col)) {
    stop(paste(
      "No usable price column found for", ticker,
      "\nColumns are:\n", paste(names(df), collapse = ", ")
    ))
  }
  
  df2 <- df |>
    dplyr::mutate(
      date  = as.Date(.data[[date_col]]),
      price = as.numeric(.data[[price_col]]),
      month = lubridate::floor_date(date, "month")
    ) |>
    dplyr::select(date, month, price) |>
    dplyr::arrange(date) |>
    dplyr::filter(!is.na(date), !is.na(price))
  
  # month-end price -> monthly log return
  monthly <- df2 |>
    dplyr::group_by(month) |>
    dplyr::summarise(price_month_end = price[which.max(date)], .groups = "drop") |>
    dplyr::arrange(month) |>
    dplyr::mutate(
      ticker = ticker,
      log_ret_month = log(price_month_end / dplyr::lag(price_month_end))
    ) |>
    dplyr::filter(!is.na(log_ret_month)) |>
    dplyr::select(month, ticker, log_ret_month)
  
  monthly
}

ret_m_long <- dplyr::bind_rows(
  get_monthly_log_returns(voo,  "VOO"),
  get_monthly_log_returns(qqq,  "QQQ"),
  get_monthly_log_returns(arkk, "ARKK"),
  get_monthly_log_returns(mags, "MAGS")
)

common_months <- ret_m_long |>
  dplyr::count(month, name = "n") |>
  dplyr::filter(n == 4) |>
  dplyr::pull(month)

ret_m_long <- ret_m_long |>
  dplyr::filter(!is.na(match(month, common_months)))

## Summary Stats

summary_stats <- ret_m_long |>
  dplyr::group_by(ticker) |>
  dplyr::summarise(
    n_months = dplyr::n(),
    mean = mean(log_ret_month),
    sd   = sd(log_ret_month),
    min  = min(log_ret_month),
    q25  = stats::quantile(log_ret_month, 0.25),
    med  = stats::median(log_ret_month),
    q75  = stats::quantile(log_ret_month, 0.75),
    max  = max(log_ret_month),
    .groups = "drop"
  )

print(summary_stats)

## Correlation Matrix

ret_m_wide <- ret_m_long |>
  tidyr::pivot_wider(names_from = ticker, values_from = log_ret_month) |>
  dplyr::arrange(month)

cor_mat <- stats::cor(ret_m_wide |> dplyr::select(-month), use = "pairwise.complete.obs")
print(cor_mat)


## Corr heat Map
cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("ETF1", "ETF2", "corr")

ggplot2::ggplot(cor_long, ggplot2::aes(x = ETF1, y = ETF2, fill = corr)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", corr)), size = 4) +
  ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Correlation Heatmap (Monthly Log Returns)", x = NULL, y = NULL)

## Attempt at making line chart showing prices over time monthly 

get_monthly_prices <- function(df, ticker){
  
  # standardize column names
  names(df) <- names(df) |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("_$", "")
  
  # detect date column
  date_candidates <- grep("date", names(df), value = TRUE)
  if (length(date_candidates) == 0) stop(paste("No date column found for", ticker))
  date_col <- date_candidates[1]
  
  # detect price column
  adj_candidates   <- grep("adj.*close", names(df), value = TRUE)
  close_candidates <- grep("^close$|close$", names(df), value = TRUE)
  price_candidates <- grep("price|last", names(df), value = TRUE)
  
  price_col <- if (length(adj_candidates) > 0) adj_candidates[1]
  else if (length(close_candidates) > 0) close_candidates[1]
  else if (length(price_candidates) > 0) price_candidates[1]
  else stop(paste("No price column found for", ticker))
  
  df2 <- df |>
    dplyr::mutate(
      date  = as.Date(.data[[date_col]]),
      price = as.numeric(.data[[price_col]]),
      month = lubridate::floor_date(date, "month")
    ) |>
    dplyr::select(date, month, price) |>
    dplyr::arrange(date) |>
    dplyr::filter(!is.na(date), !is.na(price))
  
  # month-end price
  monthly <- df2 |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      price_month_end = price[which.max(date)],
      .groups = "drop"
    ) |>
    dplyr::mutate(ticker = ticker) |>
    dplyr::select(month, ticker, price_month_end)
  
  monthly
}

## Building monthly prices 

prices_m_long <- dplyr::bind_rows(
  get_monthly_prices(voo,  "VOO"),
  get_monthly_prices(qqq,  "QQQ"),
  get_monthly_prices(arkk, "ARKK"),
  get_monthly_prices(mags, "MAGS")
)

# align to common months
common_months_p <- prices_m_long |>
  dplyr::count(month, name = "n") |>
  dplyr::filter(n == 4) |>
  dplyr::pull(month)

prices_m_long <- prices_m_long |>
  dplyr::filter(!is.na(match(month, common_months_p)))


## Line chart 
ggplot2::ggplot(prices_m_long,
                ggplot2::aes(x = month, y = price_month_end, color = ticker)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "ETF Prices Over Time (Monthly, Month-End)",
    x = "Month",
    y = "Price",
    color = "ETF"
  )

# Volatility (Annualized) using monthly log returns: sd * sqrt(12)
vol_tbl <- dplyr::summarise(
  dplyr::group_by(ret_m_long, ticker),
  n_months = dplyr::n(),
  vol_monthly = stats::sd(log_ret_month),
  vol_annual = stats::sd(log_ret_month) * sqrt(12),
  .groups = "drop"
)

print(vol_tbl)


## Shapre ratio assuming 4% risk free rate 
rf_annual <- 0.04
rf_monthly <- rf_annual / 12

# Add excess returns (no pipes)
ret_m_long_excess <- dplyr::mutate(
  ret_m_long,
  excess_log_ret = log_ret_month - rf_monthly
)

sharpe_tbl <- dplyr::summarise(
  dplyr::group_by(ret_m_long_excess, ticker),
  mean_monthly_excess = mean(excess_log_ret),
  sd_monthly = stats::sd(log_ret_month),
  sharpe_annual = (mean(excess_log_ret) * 12) / (stats::sd(log_ret_month) * sqrt(12)),
  .groups = "drop"
)

print(sharpe_tbl)

### Betas vs VOO using monthly log returns 

ret_m_wide <- tidyr::pivot_wider(
  ret_m_long,
  names_from = ticker,
  values_from = log_ret_month
)

ret_m_wide <- dplyr::arrange(ret_m_wide, month)

# computing betas 

var_voo <- stats::var(ret_m_wide$VOO, na.rm = TRUE)

beta_tbl <- data.frame(
  ticker = c("QQQ", "ARKK", "MAGS"),
  beta_vs_VOO = c(
    stats::cov(ret_m_wide$QQQ,  ret_m_wide$VOO, use = "complete.obs") / var_voo,
    stats::cov(ret_m_wide$ARKK, ret_m_wide$VOO, use = "complete.obs") / var_voo,
    stats::cov(ret_m_wide$MAGS, ret_m_wide$VOO, use = "complete.obs") / var_voo
  )
)

print(beta_tbl)

# Clean combined table
risk_tbl <- dplyr::left_join(vol_tbl, sharpe_tbl, by = "ticker")
risk_tbl <- dplyr::left_join(risk_tbl, beta_tbl, by = "ticker")

print(risk_tbl)






