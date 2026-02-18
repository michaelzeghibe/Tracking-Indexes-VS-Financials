library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)  # For date handling

folder_path <- "C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials/Data Scraped Online"

read_price_sheet <- function(filename, asset_name) {
  full_path <- file.path(folder_path, filename)
  df <- read_excel(full_path, sheet = "Price History", skip = 2)
  colnames(df)[1] <- "Date"
  colnames(df)[2] <- asset_name
  df <- df %>%
    filter(!is.na(Date), !is.na(!!sym(asset_name))) %>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
    select(Date, all_of(asset_name)) %>%
    arrange(Date)
  return(df)
}

# Load data
arkk <- read_price_sheet("ARKK_PriceHistory.xlsx", "ARKK")
qqq <- read_price_sheet("QQQ_PriceHistory.xlsx", "QQQ")
voo <- read_price_sheet("VOO_PriceHistory.xlsx", "VOO")
mags <- read_price_sheet("MAGS_PriceHistory.xlsx", "MAGS")
vix <- read_price_sheet("VIX_PriceHistory.xlsx", "VIX")  # Loaded but not used in this script

# Define prices list using the loaded dataframes
prices <- list(
  ARKK = arkk %>% rename(Price = ARKK),
  MAGS = mags %>% rename(Price = MAGS),
  QQQ = qqq %>% rename(Price = QQQ),
  VOO = voo %>% rename(Price = VOO)
)

# Align dates to common period
all_dates <- Reduce(intersect, lapply(prices, function(p) p$Date))
df_prices <- data.frame(Date = all_dates)
for (etf in names(prices)) {
  df_prices[[etf]] <- prices[[etf]] %>% filter(Date %in% all_dates) %>% pull(Price)
}

# Calculate daily returns
returns <- df_prices %>%
  mutate(across(-Date, ~ (.-lag(.))/lag(.))) %>%
  na.omit()

# Annualized metrics
mean_returns <- colMeans(returns[, -1]) * 252
cov_matrix <- cov(returns[, -1]) * 252

# Simulate portfolios
num_portfolios <- 10000
results <- data.frame(return = numeric(num_portfolios), vol = numeric(num_portfolios), sharpe = numeric(num_portfolios))
for (i in 1:num_portfolios) {
  weights <- runif(4)
  weights <- weights / sum(weights)
  port_return <- sum(weights * mean_returns)
  port_vol <- sqrt(t(weights) %*% cov_matrix %*% weights)
  results$return[i] <- port_return
  results$vol[i] <- port_vol
  results$sharpe[i] <- (port_return - 0.04) / port_vol  # Sharpe (risk-free 4%)
}

# Plot
ggplot(results, aes(x = vol, y = return, color = sharpe)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(x = "Volatility", y = "Return", title = "Efficient Frontier for ARKK, MAGS, QQQ, VOO (Real Data)", color = "Sharpe Ratio") +
  theme_minimal()
