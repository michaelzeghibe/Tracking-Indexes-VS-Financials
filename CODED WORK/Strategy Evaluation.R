library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

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

# Normalize prices to cumulative returns (starting from 1)
cum_returns <- df_prices %>%
  mutate(across(-Date, ~ ./.[1]))

# Strategy weights (from updated allocations)
strategies <- list(
  Aggressive = c(0.25, 0.30, 0.35, 0.10),
  Moderate = c(0.15, 0.25, 0.35, 0.25),
  Conservative = c(0.05, 0.15, 0.30, 0.50)
)

# Calculate portfolio cumulative returns (assuming quarterly rebalancing; for simplicity, use static weights here)
port_cum_returns <- data.frame(Date = cum_returns$Date)
for (name in names(strategies)) {
  weights <- strategies[[name]]
  port_returns <- rowSums(cum_returns[, -1] * weights)
  port_cum_returns[[name]] <- port_returns
}

# Pivot for plotting
port_long <- port_cum_returns %>%
  pivot_longer(-Date, names_to = "Strategy", values_to = "Value")

# Plot
ggplot(port_long, aes(x = Date, y = Value, color = Strategy)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Value (Start = 1)", title = "Backtested Cumulative Returns of Strategies (Real Data)") +
  theme_minimal()