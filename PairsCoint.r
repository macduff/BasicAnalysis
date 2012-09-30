# Getting Unique pairs of indices to loop over
require(signal)
require(urca)
require(xts)
require(quantmod)
require(plyr)

n <- length(Port.Prices)
indx <- expand.grid(1:n, 1:n)
indx <- indx[!duplicated(t(apply(indx, 1, sort))) & (indx$Var1 != indx$Var2), ]

# creating a low pass filter to smooth the residual time for zero crossing estimation
bf <- butter(3, 0.1, type = "low") # 10 Hz low-pass filter


# Function to test for "good" securities to for potential pair-trade

testPairs.Coint <- function(pairs, Port.Prices = Port.Prices, period = "days", len.period = 1, smoother = bf, min.pts = 60) {
  res <- list()
  i <- pairs[[1]]
  j <- pairs[[2]]
  print(c(i,j))
#    if all(pairs == c(8,1))
#      browser()
  # Using closing Prices for now
  p1.cl <- Cl(to.period(Port.Prices[[i]]$prices, period = period, k = len.period))
  p2.cl <- Cl(to.period(Port.Prices[[j]]$prices, period = period, k = len.period))
  p.cl <- na.omit(merge.xts(p1.cl, p2.cl))
  if(any(length(index(p.cl)) < min.pts, length(index(p1.cl)) < min.pts, length(index(p2.cl)) < min.pts))
    return()
  else
  {
    reg <- lm(p.cl[,1] ~ p.cl[,2] - 1)
    resid <- reg$residuals
    res$residual <- resid
    
    # Performing Dickey-Fuller test for the residual to test for cointegration
    DF.test <- ur.df(as.ts(resid))
    res$PassTest <- any(which(DF.test@cval > as.numeric(DF.test@teststat)))
    if(res$PassTest)
      res$ConfidenceLevel <- colnames(DF.test@cval)[min(which(DF.test@cval > as.numeric(DF.test@teststat)))] 
    else  res$ConfidenceLevel <- "NA"
    
    # Estimating the zero Crossing for the residual time series
    smoothed <- filter(smoother, resid)
    smoothed.laged <- lag(smoothed)
    zero.crossing <- length(which(sign(smoothed * smoothed.laged) == -1))
    # Saving results
    res$zero.crossing <- zero.crossing
    res$residual.smooth <- as.xts(as.numeric(smoothed),order.by=index(resid))
    res$tickers <- c(Port.Prices[[i]]$ticker, Port.Prices[[j]]$ticker)
    res$names   <- c(Port.Prices[[i]]$name,   Port.Prices[[j]]$name)
    res$numPts <- length(resid)
    return(res)
  }
}

# Testing each pairs for potential cointegration
res <- apply(indx, 1, FUN=testPairs.Coint, Port.Prices,period = "days", len.period = 1, smoother = bf, min.pts = 60)

# A function to get a dataframe which summarize the results
PassTest <- ldply(res,function(x) c(x$tickers, x$PassTest, x$ConfidenceLevel, x$zero.crossing, x$numPts))
PassTest <- PassTest[,-1]
names(PassTest)  <- c('Ticker A', 'Ticker B', 'Pass.Fail_ADF_test', 'CI', 'ZC', 'num.Pts')
Pass <- PassTest[which(PassTest$Pass.Fail_ADF_test == "TRUE"),]
View(Pass)
# to plot a residual of the corresponding data in row 5 in the Pass dataframe, use the following command
plot(res[[row.names(Pass[5,])]]$residual)


# Getting a list of Names & Tickers from Port.Prices
Names.tickers <- ldply(Port.Prices, function(x) x$ticker)
