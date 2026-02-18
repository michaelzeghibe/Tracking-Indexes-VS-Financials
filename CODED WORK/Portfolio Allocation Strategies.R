library(ggplot2)
library(dplyr)

assets <- c('ARKK', 'MAGS', 'QQQ', 'VOO')
strategies <- list(
  Aggressive = c(0.25, 0.30, 0.35, 0.10),
  Moderate = c(0.15, 0.25, 0.35, 0.25),
  Conservative = c(0.05, 0.15, 0.30, 0.50)
)

# Function to create pie chart
create_pie <- function(weights, title) {
  df <- data.frame(Asset = assets, Weight = weights * 100)  # Convert to percentages
  ggplot(df, aes(x = "", y = Weight, fill = Asset)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Weight, 1), "%")), position = position_stack(vjust = 0.5)) +
    labs(title = title) +
    theme_void() +
    theme(legend.position = "right")
}

# Plot side-by-side (use gridExtra for multiple plots)
library(gridExtra)
p1 <- create_pie(strategies$Aggressive, "Aggressive")
p2 <- create_pie(strategies$Moderate, "Moderate")
p3 <- create_pie(strategies$Conservative, "Conservative")
grid.arrange(p1, p2, p3, ncol = 3, top = "Portfolio Allocations by Strategy")