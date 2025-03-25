##### Scripts for the lecture on Portfolio Optimization with R 
##### UCLouvain 24/04/2025
##### (c) José Rafael Caro-Barrera

# Install required packages

# install.packages("PortfolioAnalytics")
# install.packages("ROI")
# install.packages("ROI.plugin.quadprog")
# install.packages("ggplot2")
# install.packages("quantmod")

# Load necessary library

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ggplot2)

########## Example 1: Downloading and Visualizing Stock Data

# Download Apple stock data from Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2023-01-01")

# View the first few rows of the data
head(AAPL)

# Plot the adjusted closing price
chartSeries(AAPL$AAPL.Adjusted, theme = "white", name = "Apple Adjusted Close")

########## Example 2: Calculating Daily Returns

# Calculate daily returns
aapl_returns <- dailyReturn(AAPL$AAPL.Adjusted)

# Plot the distribution of returns
hist(aapl_returns, breaks = 50, main = "Distribution of Apple Daily Returns", xlab = "Daily Returns", col = "lightblue")

########## Example 3: Portfolio Risk and Return Analysis

# Download stock data
getSymbols(c("AAPL", "XOM", "GLD"), src = "yahoo", from = "2020-01-01", to = "2023-01-01")

# Calculate daily returns
aapl_returns <- dailyReturn(AAPL$AAPL.Adjusted)
xom_returns <- dailyReturn(XOM$XOM.Adjusted)
gld_returns <- dailyReturn(GLD$GLD.Adjusted)

# Combine returns into a portfolio
portfolio_returns <- cbind(aapl_returns, xom_returns, gld_returns)
colnames(portfolio_returns) <- c("AAPL", "XOM", "GLD")

# Calculate portfolio statistics
portfolio_stats <- table.AnnualizedReturns(portfolio_returns)
print(portfolio_stats)

# Calculate correlation matrix
correlation_matrix <- cor(portfolio_returns)
print(correlation_matrix)

########## Example 4: Portfolio Optimization 

# Optimize a portfolio to maximize the Sharpe ratio using the PortfolioAnalytics package.

# Define portfolio specification
portfolio_spec <- portfolio.spec(assets = colnames(portfolio_returns))

# Add constraints
portfolio_spec <- add.constraint(portfolio_spec, type = "weight_sum", min_sum = 1, max_sum = 1)
portfolio_spec <- add.constraint(portfolio_spec, type = "box", min = 0, max = 0.5)

# Add objective: maximize Sharpe ratio
portfolio_spec <- add.objective(portfolio_spec, type = "return", name = "mean")
portfolio_spec <- add.objective(portfolio_spec, type = "risk", name = "StdDev")

# Optimize portfolio
optimized_portfolio <- optimize.portfolio(portfolio_returns, portfolio_spec, optimize_method = "ROI")
print(optimized_portfolio)

############ Example 5: Calculation of the 95% VaR and CVaR for a portfolio.

# Example portfolio returns
portfolio_returns <- c(-0.02, -0.01, 0.03, 0.01, -0.05, 0.02, -0.03, 0.04, -0.02, 0.01)

# Calculate 95% VaR
VaR_95 <- VaR(portfolio_returns, p = 0.95, method = "historical")
print(VaR_95)

# Calculate 95% CVaR
CVaR_95 <- CVaR(portfolio_returns, p = 0.95, method = "historical")
print(CVaR_95)

############  Example 6: Technical Analysis with Moving Averages

# Plot Apple’s stock price with a 50-day and 200-day moving average.

# Calculate moving averages
aapl_50ma <- SMA(AAPL$AAPL.Adjusted, n = 50)
aapl_200ma <- SMA(AAPL$AAPL.Adjusted, n = 200)

# Plot the stock price and moving averages
chartSeries(AAPL$AAPL.Adjusted, theme = "white", name = "Apple Adjusted Close")
addTA(aapl_50ma, col = "blue", lwd = 2, on = 1)
addTA(aapl_200ma, col = "red", lwd = 2, on = 1)

############ Example 7: Efficient Frontier

# Download stock data
getSymbols(c("AAPL", "XOM", "GLD"), src = "yahoo", from = "2020-01-01", to = "2023-01-01")

# Calculate daily returns
aapl_returns <- dailyReturn(AAPL$AAPL.Adjusted)
xom_returns <- dailyReturn(XOM$XOM.Adjusted)
gld_returns <- dailyReturn(GLD$GLD.Adjusted)

# Combine returns into a single data frame
portfolio_returns <- cbind(aapl_returns, xom_returns, gld_returns)
colnames(portfolio_returns) <- c("AAPL", "XOM", "GLD")

# Define portfolio specification
portfolio_spec <- portfolio.spec(assets = colnames(portfolio_returns))

# Add constraints
portfolio_spec <- add.constraint(portfolio_spec, type = "weight_sum", min_sum = 1, max_sum = 1)
portfolio_spec <- add.constraint(portfolio_spec, type = "box", min = 0, max = 0.5)

# Add objectives
portfolio_spec <- add.objective(portfolio_spec, type = "return", name = "mean")
portfolio_spec <- add.objective(portfolio_spec, type = "risk", name = "StdDev")

# Generate random portfolios
set.seed(123)
random_portfolios <- random_portfolios(portfolio_spec, permutations = 5000, rp_method = "sample")

# Optimize portfolios for different target returns
efficient_frontier <- create.EfficientFrontier(portfolio_returns, portfolio_spec, type = "mean-StdDev")

# Extract efficient frontier data
ef_data <- efficient_frontier$frontier[, c("mean", "StdDev")]
colnames(ef_data) <- c("Return", "Risk")

# Convert random portfolios to a data frame
random_portfolios_df <- data.frame(
  Return = apply(random_portfolios, 1, function(w) mean(portfolio_returns %*% w)),
  Risk = apply(random_portfolios, 1, function(w) sd(portfolio_returns %*% w))
)

# Plot the efficient frontier
ggplot() +
  geom_point(data = random_portfolios_df, aes(x = Risk, y = Return), color = "gray", alpha = 0.5) +
  geom_line(data = ef_data, aes(x = Risk, y = Return), color = "blue", size = 1.2) +
  labs(
    title = "Efficient Frontier",
    x = "Risk (Standard Deviation)",
    y = "Return (Mean)"
  ) +
  theme_minimal()
