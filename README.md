# Grandma-s-Portfolio

##

library (quantmod) 
library(PerformanceAnalytics)

tickers <- c("MO", "STAG", "GTY", "SLG")
weights <- c(.25, .25, .25, .25)

## - auto.assign = FALSE - allow you to put downloaded data in Global Environment, pertains to your environment

## - you are taking the 4th column in the data, and your combining it into your portfolioPrices object

portfolioPrices <- NULL
for(ticker in tickers){
  portfolioPrices <- cbind(portfolioPrices, 
                           getSymbols.yahoo(ticker, from = "2016-1-1", periodicity = "monthly", auto.assign = FALSE) [,4])
}

                      ## Portfolio Reutrns

## na.omit gets rids of any missing price data or NA data (first row) (for Stocks that aren't as well tracked)

## ROC is 'rate of change'

portfolioReturns <- na.omit(ROC(portfolioPrices))

                           
## To check if you have any missing Data: if = 0, then you dont

colSums(is.na(portfolioPrices))

## Benchmark

benchmarkPrices <- getSymbols.yahoo("^GSPC", from = "2016-1-1", periodicity = "monthly", auto.assign = FALSE) [,4]
colSums(is.na(benchmarkPrices))

benchmarkReturns <- na.omit(ROC(benchmarkPrices))

## Metrics: BETA, ALPHA, SHARPE Ratio

portfolioReturn <- Return.portfolio(portfolioReturns)

## for daily, you divide by 252 (number of trading days in the year)

BETA <- CAPM.beta(portfolioReturn, benchmarkReturns, .035/12)

ALPHA <- CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/12)
