# Load required libraries (install if missing: install.packages(c("readxl", "dplyr", "tidyr", "lubridate", "zoo")))
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

# Function to read and clean ETF price history from Excel
read_etf_data <- function(file_path, etf_name) {
  # Check if file exists before reading
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path, "C:/Users/micha/OneDrive/Data Cap/Tracking-Indexes-VS-Financials"))
  }
  
  # Read the Excel sheet, skipping the first two rows (headers)
  data <- read_excel(file_path, sheet = "Price History", skip = 2)
  
  # Select only Date and Price columns, convert types
  data <- data %>%
    select(Date, Price) %>%
    mutate(
      Date = as.numeric(Date),  # Ensure Date is numeric (Excel serial)
      Date = as.Date(Date - 2, origin = "1899-12-30"),  # Convert to proper Date
      Price = as.numeric(Price)  # Ensure Price is numeric
    ) %>%
    filter(!is.na(Date) & !is.na(Price)) %>%  # Remove rows with NA
    arrange(Date) %>%  # Sort by Date
    rename_with(~ etf_name, Price)  # Rename Price column to ETF name
  
  return(data)
}


# Read data for each ETF (update paths if not in working directory)
arkk <- read_etf_data("C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials/ARKK_PriceHistory.xlsx", "ARKK")
mags <- read_etf_data("C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials/MAGS_PriceHistory.xlsx", "MAGS")
qqq <- read_etf_data("C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials/QQQ_PriceHistory.xlsx", "QQQ")
voo <- read_etf_data("C:/Users/micha/OneDrive - Providence College/Data Cap/Tracking-Indexes-VS-Financials/VOO_PriceHistory.xlsx", "VOO")

# Align dates by inner joining on Date (only keep common dates)
aligned_data <- arkk %>%
  inner_join(mags, by = "Date") %>%
  inner_join(qqq, by = "Date") %>%
  inner_join(voo, by = "Date") %>%
  arrange(Date)

View(aligned_data)
