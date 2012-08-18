library(IBrokers)
tws <- twsConnect()


getEQ_PriceHistory.IB <- function(Connection=tws, ticker='AAPL') {
  equity.contract <- twsEquity(ticker)
  equity.hist.price <- reqHistory(Connection,
                                  equity.contract,
                                  whatToShow = "BID_ASK")
  
  
  #Sys.sleep(10)
  # mandatory 10s between request to avoid IB pacing violation see the following link for more info
  #http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm
  
  
  
  return(equity.hist.price)
}
 