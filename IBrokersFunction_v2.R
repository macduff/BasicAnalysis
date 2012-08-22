tws <- twsConnect()


#Pull equity prices "after hours (AH)" using reqHistoricalData function.

IB.EQ.1day.AH <- function(Connection=tws, ticker='?') {

  equity.contract <- twsEquity(ticker)
  equity.hist.price <- reqHistoricalData(Connection,
                                         equity.contract,
                                         whatToShow = "BID_ASK",
                                         barSize = "1 day",
                                         duration ="1 Y"
                                         )
  #Sys.sleep(10)
  # mandatory 10s between request to avoid IB pacing violation see the following link for more info
  #http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm 
  return(equity.hist.price)
}
 
IB.EQ.1min.AH <- function(Connection=tws, ticker='?') {
  
  equity.contract <- twsEquity(ticker)
  equity.hist.price <- reqHistoricalData(Connection,
                                         equity.contract,
                                         whatToShow = "BID_ASK",
                                         barSize = "1 min",
                                         duration ="1 D"
  )
  #Sys.sleep(10)
  # mandatory 10s between request to avoid IB pacing violation see the following link for more info
  #http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm 
  return(equity.hist.price)
}