
# MONTHLY Rolling Correlations with QQQ 

library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(lubridate)

folder_path <- "C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials"

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
qqq  <- read_price_sheet("QQQ_PriceHistory.xlsx",  "QQQ")
voo  <- read_price_sheet("VOO_PriceHistory.xlsx",  "VOO")
mags <- read_price_sheet("MAGS_PriceHistory.xlsx", "MAGS")
vix  <- read_price_sheet("VIX_PriceHistory.xlsx",  "VIX")

# Merge (common period only)
prices <- arkk %>%
  full_join(qqq,  by = "Date") %>%
  full_join(voo,  by = "Date") %>%
  full_join(mags, by = "Date") %>%
  full_join(vix,  by = "Date") %>%
  arrange(Date) %>%
  filter(if_all(-Date, ~ !is.na(.)))

# Monthly
monthly_prices <- prices %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month) %>%
  summarise(across(-Date, ~ last(.)), .groups = "drop") %>%
  rename(Date = Month)

monthly_returns <- monthly_prices %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))))) %>%
  drop_na()

# Rolling correlation
rolling_cor <- function(data, width, col1, col2) {
  rollapply(data[, c(col1, col2)], width = width,
            FUN = function(x) {
              if (nrow(na.omit(x)) < 6) return(NA)
              cor(x[,1], x[,2], use = "complete.obs")
            },
            by.column = FALSE, align = "right", fill = NA)
}

w1 <- 12; w2 <- 24; w3 <- 36

pairs <- list(
  c("QQQ", "ARKK"), c("QQQ", "VOO"), c("QQQ", "MAGS"), c("QQQ", "VIX"),
  c("VOO", "MAGS")
)

results <- list()
for (p in pairs) {
  asset1 <- p[1]; asset2 <- p[2]
  results[[paste(asset1, asset2, "1Y", sep = "_")]] <- rolling_cor(monthly_returns, w1, asset1, asset2)
  results[[paste(asset1, asset2, "2Y", sep = "_")]] <- rolling_cor(monthly_returns, w2, asset1, asset2)
  results[[paste(asset1, asset2, "3Y", sep = "_")]] <- rolling_cor(monthly_returns, w3, asset1, asset2)
}

# Build data frame
n <- nrow(monthly_returns)
rolling_df <- monthly_returns %>% select(Date)
for (name in names(results)) {
  vec <- results[[name]]
  pad <- n - length(vec)
  rolling_df[[name]] <- c(rep(NA, pad), vec)
}


plot_data <- rolling_df %>%
  pivot_longer(-Date, names_to = "Pair_Window", values_to = "Correlation") %>%
  separate(Pair_Window, into = c("Asset1", "Asset2", "Window"), sep = "_") %>%
  mutate(
    Pair = paste(Asset1, Asset2, sep = "-"),
    Label = paste(Pair, Window, sep = " ")
  ) %>%
  filter(!is.na(Correlation))

ggplot(plot_data, aes(x = Date, y = Correlation, color = Label)) +
  geom_line(linewidth = 0.95, alpha = 0.9) +
  labs(title = "Rolling Correlations with QQQ (Monthly)",
       subtitle = "All pairs and windows on one graph | 1Y (12m), 2Y (24m), 3Y (36m)",
       x = "", y = "Correlation",
       color = "Pair & Window") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) +
  scale_color_viridis_d(option = "plasma") +
  ylim(-1, 1)

