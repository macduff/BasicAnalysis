require(fUnitRoots)#need for function adfTest

#define training set.
#define test set.

#arguments:
#return: hedge ratio for second price series.
PairsTradeScreen <- function(series){

#X1 <- coredata(series[paste(start.date,end.date,sep=""),1])  if more flexibility on start and end dates needed
X1 <- coredata(series[,1])
Y <- coredata(series[,2])
time.index <- index(series) #account for start and end date

results <- lm(Y ~ X1 + 0, )#data=t
beta <- coef(results)[1]
sprd <- Y - beta*X1
cat("Assumed hedge ratio is", beta, "\n")

spreadMean <- mean(sprd)

spreadSD <- apply(sprd,2,sd)
zscore.spread <- (sprd - spreadMean) / spreadSD

zscore.spread.xts <- xts(zscore.spread,time.index)
plot(zscore.spread.xts)

Stationary.Test <- adfTest(zscore.spread,lags=0)#does not detrend like tseries version adf.test.
#(http://quanttrader.info/public/testForCoint.html)  The adf.test function detrends your data before testing for stationarity.  If your data contains a strong trend, you might be very surprised to learn it is "mean reverting" when it is obvously moving upward or downward.  If this is a problem for you, consider the  fUnitRoots package which contains the adfTest function (note the spelling!).  That function lets you analyze either with or without the trend assumption
print(Stationary.Test)
#Dickeyf.test <- ur.df(as.ts(spreads))
#print(Dickeyf.test)
#Test.Stationary <- adf.test(sprd, alternative="stationary", k=0)
return(beta)
}

#The next functions can be used for both training set and test set.
#arguments: series could be training set or test set
#output: zspreads which will be backtested for P&L.
PairsTrade.zSpreads <- function(beta, series){
  
  time.index <- index(series)
  sprd <- series[,2] - beta * series[,1]
  spreadMean <- mean(sprd)
  spreadSD <- apply(sprd,2,sd)
  zscore.spread <- (sprd - spreadMean) / spreadSD
  
  zscore.spread.xts <- xts(zscore.spread,time.index)
  return(zscore.spread.xts)

}
#argument: training set of spreads.  
#Output: backtest P&L
PairsBackTest <- function(spreads,series){

EnterTrade.param <- 1.5
ExitTrade.param <- 0.5

#the spread on day t corresponds to the end of day spread.  This will be used in the trading rule on day t+1

Trade.LongSpread <- spreads <= -1 * EnterTrade.param #Short first stock(X), long second stock (Y)
Trade.ShortSpread <- spreads >= 1 * EnterTrade.param 
Trade.Exit <- spreads < (1 * ExitTrade.param) & spreads > (-1 *ExitTrade.param)  #close out trades

#Positions corresponds to what we should hold at time t, ignoring impact of trading one day later than on signal.
Positions <- rep(NA,length(spreads)) #initially nil
Positions <- cbind(Positions,Positions)

#column one is stock 1 order, column 2 is stock 2 order:
Positions[which(Trade.LongSpread),1] <- -1
Positions[which(Trade.LongSpread),2] <- 1

Positions[which(Trade.ShortSpread),1] <- 1#short spread implies long stock 1 (stock2 - beta stock1)
Positions[which(Trade.ShortSpread),2] <- -1#short spread implies short stock 2 (stock2 - beta stock1)
Positions[which(Trade.Exit),] <- 0

TrainingSet <- series  #set up the training set window.
Returns <- diff(TrainingSet)/lag(TrainingSet)#The return at time t is equal to the return over [t-1,t]

Positions.Lag <- Positions[-length(Positions[,1]),]#Remove the position for the very last time point
#Positions.Lag <- rbind(Positions.Lag) #Place NA for the very first time point.

Positions.Filled <- Positions.Lag

  for (i in 2:length(Positions.Filled[,1])){
    is.data.miss <- !is.finite(Positions.Lag[i,])
    Positions.Filled[i,is.data.miss] = Positions.Filled[i-1,is.data.miss]
  }
print(Positions.Filled)

pnl.ts.components <- Positions.Filled * Returns[-1,]#first return is NA, and we want to match Positions(t) * Returns(t+1)
daily.pnl <- rowSums(Positions.Filled * Returns[-1,])#get the P&L for each trading day.

Profit.Loss <-sum(daily.pnl,na.rm =TRUE) #the first part of the series could be NA if we do not enter a trade on day 1
cat("The cumulative training set P&L is",Profit.Loss)
return(daily.pnl)
}
#SharpeRatio <- sqrt(252)* mean(pnl.ts,na.rm=TRUE)/ sd(pnl.ts,na.rm=TRUE)  sharpe ratio depends on frequency.

#

# Now compute the spread
#

# for i = 1,2,3,4
# j = i+1,..,4

#apply  X [(1,...,n)  x 2,3, 4]