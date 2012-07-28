library(IBrokers)
tws <- twsConnect()
contract.test <- twsEquity('AAPL')
apple.test <- reqHistoricalData(tws,
                  contract.test,
                  barSize = "30 secs",
                  duration = "1 D",
                  useRTH = "1",
                  whatToShow = "BID_ASK",
                  timeFormat = "1",
                  tzone = "",
                  verbose = TRUE,
                  tickerId = "1")

#WTF IS THIS CODE BOOK3???
zy = 155
book3 = 2000

Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation see the following link for more info
#http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm