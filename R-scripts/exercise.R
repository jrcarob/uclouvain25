##### Scripts for the lecture on Portfolio Optimization with R 
##### UCLouvain 24/04/2025
##### (c) José Rafael Caro-Barrera

# Load required libraries
library(tidyverse)
library(ggplot2)

# Input data
er_ford <- 0.15    # Expected return of Ford Motors
sd_ford <- 0.42    # Volatility of Ford Motors
er_molson <- 0.11  # Expected return of Molson Coors
sd_molson <- 0.32  # Volatility of Molson Coors
corr <- 0          # Correlation between stocks (uncorrelated)

# Function to calculate portfolio metrics
calculate_portfolio <- function(w_ford) {
  w_molson <- 1 - w_ford
  
  # Expected return
  er_portfolio <- w_ford * er_ford + w_molson * er_molson
  
  # Portfolio volatility (simplified for uncorrelated assets)
  sd_portfolio <- sqrt(w_ford^2 * sd_ford^2 + w_molson^2 * sd_molson^2)
  
  return(data.frame(w_ford, er_portfolio, sd_portfolio))
}

# Generate portfolios with different weights
weights <- seq(0, 1, by = 0.01)
portfolios <- map_df(weights, calculate_portfolio)

# Find special portfolios
min_var_portfolio <- portfolios[which.min(portfolios$sd_portfolio), ]
portfolio_73_27 <- calculate_portfolio(0.73)

# Calculate Sharpe ratios (assuming 3% risk-free rate)
rf <- 0.03
portfolios$sharpe <- (portfolios$er_portfolio - rf) / portfolios$sd_portfolio
optimal_sharpe <- portfolios[which.max(portfolios$sharpe), ]

# Create annotation data frame
annotations <- data.frame(
  label = c("100% Ford", "100% Molson", "Minimum Variance Portfolio", "73% Ford, 27% Molson"),
  x = c(sd_ford, sd_molson, min_var_portfolio$sd_portfolio, portfolio_73_27$sd_portfolio),
  y = c(er_ford, er_molson, min_var_portfolio$er_portfolio, portfolio_73_27$er_portfolio)
)

# Plot efficient frontier
ggplot(portfolios, aes(x = sd_portfolio, y = er_portfolio)) +
  geom_line(color = "blue", linewidth = 1) +  # Changed from size to linewidth
  
  # Add points and labels using annotate() to avoid warnings
  annotate("point", x = sd_ford, y = er_ford, color = "black", size = 3) +
  annotate("point", x = sd_molson, y = er_molson, color = "black", size = 3) +
  annotate("point", 
           x = min_var_portfolio$sd_portfolio, 
           y = min_var_portfolio$er_portfolio, 
           color = "red", size = 3) +
  annotate("point",
           x = portfolio_73_27$sd_portfolio,
           y = portfolio_73_27$er_portfolio,
           color = "green", size = 3) +
  
  # Add text annotations
  annotate("text", x = sd_ford, y = er_ford + 0.005, 
           label = "100% Ford", hjust = 0) +
  annotate("text", x = sd_molson, y = er_molson + 0.005, 
           label = "100% Molson", hjust = 0) +
  annotate("text", 
           x = min_var_portfolio$sd_portfolio, 
           y = min_var_portfolio$er_portfolio - 0.005,
           label = "Minimum Variance Portfolio", vjust = 1) +
  annotate("text",
           x = portfolio_73_27$sd_portfolio,
           y = portfolio_73_27$er_portfolio + 0.005,
           label = "73% Ford, 27% Molson", hjust = 0) +
  
  # Formatting
  labs(title = "Efficient Frontier: Ford Motors vs. Molson Coors",
       subtitle = "Uncorrelated Assets (ρ = 0)",
       x = "Portfolio Volatility (Standard Deviation)",
       y = "Portfolio Expected Return",
       caption = "Modern Portfolio Theory") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# Print results
cat("Part (a) Portfolio with 73% Ford, 27% Molson:\n")
cat(sprintf("Expected Return: %.2f%%\n", portfolio_73_27$er_portfolio * 100))
cat(sprintf("Volatility: %.2f%%\n", portfolio_73_27$sd_portfolio * 100))

cat("\nPart (b) Is 100% Molson efficient?\n")
cat("No - The 73/27 portfolio dominates it (higher return for similar risk)\n")

cat("\nPart (c) Is 100% Ford efficient?\n")
cat("No - The 73/27 portfolio has better risk-adjusted return (higher Sharpe ratio)\n")

cat(sprintf("\nOptimal Portfolio (Max Sharpe Ratio): %.0f%% Ford, %.0f%% Molson\n",
            optimal_sharpe$w_ford * 100, (1 - optimal_sharpe$w_ford) * 100))
cat(sprintf("Sharpe Ratio: %.3f\n", optimal_sharpe$sharpe))
cat(sprintf("Expected Return: %.2f%%\n", optimal_sharpe$er_portfolio * 100))
cat(sprintf("Volatility: %.2f%%\n", optimal_sharpe$sd_portfolio * 100))

