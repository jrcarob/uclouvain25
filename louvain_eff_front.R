##### Scripts for the lecture on Portfolio Optimization with R 
##### UCLouvain 24/04/2025
##### (c) José Rafael Caro-Barrera

## (1) Define the packages that will be needed
packages <- c('quantmod', 'ggplot2', 'dplyr')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## (3) Load the packages into R session
invisible(lapply(packages, library, character.only = TRUE))


## Create a character vector that has the stock codes we need
portfolio <- c('AAPL', 'MSFT', 'GOOG', 'AMZN', 'JNJ')

## Load the stocks needed into R 
portfolio <- lapply(portfolio, function(x) {getSymbols(
  x, periodicity='weekly', auto.assign=FALSE)})

## Get adjusted prices of all stocks
portfolio_adjusted <- lapply(portfolio, Ad)

## Transform into xts
portfolio_adjusted <- do.call(merge, portfolio_adjusted)

## View the first few rows
head(portfolio_adjusted)


## Make a list that contains log weekly returns of each stock
portfolio_adjusted <- lapply(portfolio_adjusted, weeklyReturn, type='log')

## Transform into an xts object
portfolio_adjusted <- do.call(merge, portfolio_adjusted)

## Adjust the column names 
colnames(portfolio_adjusted) <- c('AAPL', 'MSFT', 'GOOG', 'AMZN', 'JNJ')

## Remove first row since these do not have returns
portfolio_adjusted <- portfolio_adjusted[-1]

## View the first few rows of the log returns 
head(portfolio_adjusted)

## Get variance-covariance matrix
var_covar <- var(portfolio_adjusted)

## Print results
print(var_covar)

## Set seed for reproducibility
set.seed(123)

## Get 50,000 random uniform numbers
random_numbers <- runif(50000)

## Transform random numbers into matrix to distribute across all symbols
all_weights <- matrix(random_numbers, nrow=10000, ncol=5)

## Add sixth column with just NAs
all_weights <- cbind(all_weights, rep(NA, 10000))

## Add names
colnames(all_weights) <- c('AAPL', 'MSFT', 'GOOG', 'AMZN', 'JNJ', 'total')

## Loop to convert into actual weights
for (i in 1:10000) {
  
  ## Get sum of random numbers in each row
  all_weights[i, 6] <- sum(all_weights[i, 1:5])
  
  ## Get the actual weights of the random numbers
  all_weights[i, 1:5] <- all_weights[i, 1:5] / all_weights[i, 6]
}

## Delete total column
all_weights <- all_weights[, -6]

## Create column placeholders
portfolio_risk <- rep(NA, 10000)
portfolio_returns <- rep(NA, 10000)
sharpe_ratios <- rep(NA, 10000)

## Define risk-free rate
risk_free_rate <- 0.02 / 52  ## annualized risk-free rate converted to weekly

## loop to calculate risk and return per weights 
for (i in 1:10000) {
  weights <- all_weights[i, ]
  portfolio_risk[i] <- sqrt(sum((weights %*% var_covar) * weights))
  portfolio_returns[i] <- sum(weights * colMeans(portfolio_adjusted))
  sharpe_ratios[i] <- (portfolio_returns[i] - risk_free_rate) / portfolio_risk[i]
}


## Identify the portfolio with the highest Sharpe ratio
max_sharpe_index <- which.max(sharpe_ratios)
max_sharpe_portfolio <- all_weights[max_sharpe_index, ]
tangency_portfolio_risk <- portfolio_risk[max_sharpe_index]
tangency_portfolio_return <- portfolio_returns[max_sharpe_index]

## Make a data frame to be used for ggplot2
portfolio_df <- data.frame(portfolio_risk, portfolio_returns)

## Plot the efficient frontier with the tangency line
portfolio_df %>% 
  ggplot(aes(x=portfolio_risk, y=portfolio_returns)) + 
  geom_point(alpha=0.2) + 
  theme_minimal() +
  geom_abline(intercept = risk_free_rate, 
              slope = (tangency_portfolio_return - risk_free_rate) / tangency_portfolio_risk, 
              color = 'blue', 
              linetype = 'dashed') +
  geom_point(aes(x=tangency_portfolio_risk, y=tangency_portfolio_return), 
             color='red', size=3) +
  labs(
    title='Efficient Frontier graph of 5 assets with Tangency Line',
    subtitle='AAPL, MSFT, GOOG, AMZN, JNJ',
    x = 'Portfolio Risk (Standard Deviation)',
    y = 'Portfolio Return'
  )
