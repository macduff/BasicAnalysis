library(IBrokers)
tws <- twsConnect()

getPriceHistory.IB <- function(Connection=tws, ticker='AAPL') {
  contract.test <- twsEquity(ticker)
  priceHistOfTicker <- reqHistory(tws,
                                  contract.test,
                                  barSize = "30 secs",
                                  duration = "1 D",
                                  useRTH = "1",
                                  whatToShow = "BID_ASK",
                                  timeFormat = "1",
                                  tzone = "",
                                  verbose = TRUE,
                                  tickerId = "1")
  
  
  Sys.sleep(10)
  # mandatory 10s between request to avoid IB pacing violation see the following link for more info
  #http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm
  
  return(priceHistOfTicker)
}
 