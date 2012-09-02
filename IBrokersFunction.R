library(IBrokers)
tws <- twsConnect()

getEQ_PriceHistory.IB <- function(Connection=tws, ticker='AAPL', metric = 'TRADES') {
  equity.contract <- twsEquity(ticker)
  equity.hist.price <- reqHistory(Connection,
                                  equity.contract,
                                  whatToShow = metric)
  #Sys.sleep(10)
  # mandatory 10s between request to avoid IB pacing violation see the following link for more info
  #http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm
  return(equity.hist.price)
}

getEQ <- function(Connection=tws, tickers.df) {
  res <- list()
  res$name <- tickers.df$SECURITY
  res$ticker <- tickers.df$TICKER
  res$prices <- getEQ_PriceHistory.IB(Connection, ticker= res$ticker)
  return(res)
}

getEQ_Port <- function(Connection=tws, ticker.df=top15Tickers) {
  require(plyr)
  res <- dlply(ticker.df, 'SECURITY', getEQ, Connection=tws )
  return(res)
}

