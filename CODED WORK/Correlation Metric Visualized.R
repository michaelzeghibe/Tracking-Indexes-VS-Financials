library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)  # For melting correlation matrix
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

# Calculate daily returns
returns <- df_prices %>%
  mutate(across(-Date, ~ (.-lag(.))/lag(.))) %>%
  na.omit()

# Correlation matrix
corr_matrix <- cor(returns[, -1])
assets <- c('ARKK', 'MAGS', 'QQQ', 'VOO')

# Melt for ggplot
corr_melt <- melt(corr_matrix)

# Plot heatmap
ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(title = "Correlation Matrix of ETFs (Real Data)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))