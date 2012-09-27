

#define training set.
#define test set.

X1 <- coredata(Merge.IBM.WFC[,1])#WFC
Y <- coredata(Merge.IBM.WFC[,2])#IBM

results <- lm(Y ~ X1 + 0, )#data=t
beta <- coef(results)[1]
sprd <- Y - beta*X1
cat("Assumed hedge ratio is", beta, "\n")

spreadMean <- mean(sprd)
spreadSD <- apply(sprd,2,sd)
zscore.spread <- (sprd - spreadMean) / spreadSD


Test.Stationary <- adf.test(sprd, alternative="stationary", k=0)

Trade.Long <- zscore.spread <= -2 #Buy first stock(Y), short second stock (X)
Trade.Short <- zscore.spread >= 2  # Short trade when spread rises above 2 sds.

#Stationarity test condition.

Stationary.Test <- adfTest(sprd,lags=0)#does not detrend like tseries version adf.test.


Trade.Exit <- zscore.spread < 1 & zscore.spread > -1  #close out trades

Positions <- rep(NA,length(Merge.IBM.WFC[,2]))#initially nil
Positions <- cbind(Positions,Positions)

#column one is stock 1 order, column 2 is stock 2 order
Positions[which(Trade.Long),1] <- 1
Positions[which(Trade.Long),2] <- -1

Positions[which(Trade.Short),1] <- -1#implies short stock 1
Positions[which(Trade.Short),2] <- 1
Positions[which(Trade.Exit),] <- 0

Returns <- diff(Merge.IBM.WFC)/lag(Merge.IBM.WFC)

Positions.Lag <- Positions[-1,]#remove last index is -length(Positions[,1])
Positions.Lag <- rbind(c(NA,NA),Positions.Lag)

Positions.Filled <- Positions.Lag
for (i in 2:length(Positions.Filled[,1])){
  is.data.miss <- !is.finite(Positions.Lag[i,])
  Positions.Filled[i,is.data.miss] = Positions.Filled[i-1,is.data.miss]

}
pnl.ts.components <- Positions.Filled * Returns
pnl.ts <- rowSums(Positions.Filled * Returns)

SharpeRatio.DailyRet <- sqrt(252)* mean(pnl.ts,na.rm=TRUE)/ sd(pnl.ts,na.rm=TRUE)


# Now compute the spread
#

# for i = 1,2,3,4
# j = i+1,..,4

#apply  X [(1,...,n)  x 2,3, 4]