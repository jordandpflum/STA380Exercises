rm(list=ls())

############ Setup ############ 
#### Libraries ####
library(quantmod)
library(timeSeries)
library(fPortfolio)
library(PerformanceAnalytics)
library(ggplot2)


# Bootstrapping
library(mosaic)

# Optimization
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

#### Get Returns ####
# Portfolio
tickers <- c("AGG", "GLD", "VUG")

# Get AdjClose prices
adjClosePrices <- NULL
for (ticker in tickers){
  adjClosePrices <- cbind(adjClosePrices,
                          quantmod::getSymbols(ticker, from="2015-08-01", to = '2020-08-01', verbose=FALSE, auto.assign=FALSE)[,6])
}

# keep only the dates that have closing prices for all tickers
adjClosePrices <- adjClosePrices[apply(adjClosePrices,1,function(x) all(!is.na(x))),]

# Convert AdjClose to Returns
returns <- Return.calculate(adjClosePrices)[-1,1:length(tickers)]

############ Construct Portfolios ############ 
#### Portfolio 1 ####
port1_weights <- c(0.8, 0.15, 0.05)

# Create portfoli (Rebalancing Daily) 
port1_returns <- Return.portfolio(returns, weights = port1_weights, rebalance_on = "days")

#### Portfolio 2 ####
port2_weights <- c(0.1, 0.2, 0.7)

# Create portfoli (Rebalancing Daily) 
port2_returns <- Return.portfolio(returns, weights = port2_weights, rebalance_on = "days")

#### Portfolio 3 ####
portfolioAssets <- colnames(returns)
init <- portfolio.spec(assets=portfolioAssets)
# Add Constraint (Weights sum to 1, ie full investment)
init <- add.constraint(portfolio=init,
                       type="weight_sum",
                       min_sum=1,
                       max_sum=1)

# Add Constraint (No shorting or borrowing, and no over/under investment)
init <- add.constraint(portfolio=init,
                       type="box",
                       min=0.05,
                       max=0.5
)

# Add Constraint (Target Return = 10% annual, 0.00055% daily)
init <- add.constraint(portfolio=init, type="return", return_target=0.00055)

# Add Objective (Minimize risk)
minvar <- add.objective(portfolio=init,
                        type='risk',
                        name='var'
)

# Optimize Portfolio
opt_maxret <- optimize.portfolio(R=returns, portfolio=minvar,
                                 optimize_method="ROI",
                                 trace=FALSE
)

# Create portfoli (Rebalancing Daily) 
port3_returns <- Return.portfolio(returns, weights = opt_maxret$weights, rebalance_on = "days")


#### Bench Mark Portfolio ####
tickers <- c("SPY")

# Get AdjClose prices
adjClosePrices <- NULL
for (ticker in tickers){
  adjClosePrices <- cbind(adjClosePrices,
                          quantmod::getSymbols(ticker, from="2015-08-01", to = '2020-08-01', verbose=FALSE, auto.assign=FALSE)[,6])
}

# keep only the dates that have closing prices for all tickers
adjClosePrices <- adjClosePrices[apply(adjClosePrices,1,function(x) all(!is.na(x))),]

# Convert AdjClose to Returns
#returns <- as.timeSeries((tail(adjClosePrices,-1) / as.numeric(head(adjClosePrices,-1)))-1)
market_returns <- Return.calculate(adjClosePrices)[-1,]

############ Exploratory Analysis ############
#### Graphs ####
charts.PerformanceSummary(returns,main='Portfolio Assets Summary', rf=.0071)
chart.Boxplot(apply.monthly(returns, Return.cumulative))
charts.RollingPerformance(apply.monthly(returns, Return.cumulative))
chart.RiskReturnScatter(returns, add.sharpe = TRUE, Rf=.0071)
chart.RelativePerformance(apply.weekly(returns, Return.cumulative), apply.weekly(market_returns, Return.cumulative), legend.loc='topright')
chart.RollingRegression(apply.monthly(returns, Return.cumulative), apply.monthly(market_returns, Return.cumulative), legend.loc='bottomright')
#### Table ####
table.Stats(returns)



############ VaR Calculation ############
#### Conceptual ####
conceptualCaR <- function(portfolio, confidence){
  portfolio_resampled <- mosaic::resample(portfolio)
  portfolioReturns <- coredata(portfolio_resampled$portfolio.returns)
  sortedPortfolioReturns <- sort(portfolioReturns, decreasing=FALSE)
  port_dailyVar <- quantile(sortedPortfolioReturns,c(confidence))
  port_monthlyVaR <- port_dailyVar*sqrt(20)
  return(port_monthlyVaR)
}

# Analysis (Port 1)
# Bootstrap
monthlyVaR_bootstrap = do(1000)*conceptualCaR(port1_returns,.05)
port1_montlyVaR <- mean(monthlyVaR_bootstrap$X5.)

# Analysis (Port 2)
# Bootstrap
monthlyVaR_bootstrap = do(1000)*conceptualCaR(port2_returns,.05)
port2_montlyVaR <- mean(monthlyVaR_bootstrap$X5.)


# Analysis (Port 3)
# Bootstrap
monthlyVaR_bootstrap = do(1000)*conceptualCaR(port3_returns,.05)
port3_montlyVaR <- mean(monthlyVaR_bootstrap$X5.)

#### Normal ####
# Analysis (Port 1)
# Bootstrap
dailyVaR_bootstrap = do(1000)*PerformanceAnalytics::VaR(mosaic::resample(port1_returns))
port1_montlyVaR <- mean(dailyVaR_bootstrap$VaR)*sqrt(20)

# Analysis (Port 2)
# Bootstrap
dailyVaR_bootstrap = do(1000)*PerformanceAnalytics::VaR(mosaic::resample(port2_returns))
port2_montlyVaR <- mean(dailyVaR_bootstrap$VaR)*sqrt(20)


# Analysis (Port 3)
# Bootstrap
dailyVaR_bootstrap = do(1000)*PerformanceAnalytics::VaR(mosaic::resample(port3_returns))
port3_montlyVaR <- mean(dailyVaR_bootstrap$VaR)*sqrt(20)




#### Graphing ####
# Returns
port1_monthly = apply.weekly(port1_returns, Return.cumulative)
port1 <- data.frame(port1_monthly)
port1$portfolio <- 'Portfolio 1'

port2_monthly = apply.weekly(port2_returns, Return.cumulative)
port2 <- data.frame(port2_monthly)
port2$portfolio <- 'Portfolio 2'

port3_monthly = apply.weekly(port3_returns, Return.cumulative)
port3 <- data.frame(port3_monthly)
port3$portfolio <- 'Portfolio 3'

portfolioReturns <- rbind(port1, port2, port3)

# Portfolio VaRs
port1_VaR <- data.frame(PortfolioVaR=port1_montlyVaR)
port1_VaR$portfolio <- 'Portfolio 1'

port2_VaR <- data.frame(PortfolioVaR=port2_montlyVaR)
port2_VaR$portfolio <- 'Portfolio 2'

port3_VaR <- data.frame(PortfolioVaR=port3_montlyVaR)
port3_VaR$portfolio <- 'Portfolio 3'

portfolioVaRs <- rbind(port1_VaR, port2_VaR, port3_VaR)


ggplot(portfolioReturns, aes(portfolio.returns, fill = portfolio)) + 
  geom_density(alpha = 0.2) + 
  geom_vline(data=portfolioVaRs, aes(xintercept=PortfolioVaR, color = portfolio), linetype="dashed") + 
  ggtitle("Historical Returns (Monthly) vs VaR") +
  xlab("Returns")



############ Summary ############
histRet_port1 = (1+mean(port1_returns))^20 - 1
histRet_port2 = (1+mean(port2_returns))^20 - 1
histRet_port3 = (1+mean(port3_returns))^20 - 1

risk_port1 = sd(port1_returns)*sqrt(20)
risk_port2 = sd(port2_returns)*sqrt(20)
risk_port3 = sd(port3_returns)*sqrt(20)

rf_monthly = 6.984126e-05

sharpeRatio_port1 = (histRet_port1-rf_monthly)/(risk_port1)
sharpeRatio_port2 = (histRet_port2-rf_monthly)/(risk_port2)
sharpeRatio_port3 = (histRet_port3-rf_monthly)/(risk_port3)







