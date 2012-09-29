## Code taken from the book: Data Mining with R- Learning with Case Studies

Target.ind <- function(quotes, tgt.margin = 0.025, period = "days", len.period = 1, num.period = 10) {
#   Targent.ind is a k-days period signal (default k=10) that have
#   several days with average daily prices clearly above the target variation. High
#   positive values of Targent.ind mean that there are several average daily prices that are
#   p% higher than today’s close. Such situations are good indications of potential
#   opportunities to issue a buy order, as we have good expectations that the prices
#   will rise. On the other hand, highly negative values of Targent.ind suggest sell actions,
#   given the prices will probably decline. Values around zero can be caused by
#   periods with “flat” prices or by conflicting positive and negative variations
#   that cancel each other
  
  # to allow quotes to be of arbitrary sampling freq.
  quotes <- to.period(quotes, period = period, k = len.period) 
  
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = num.period, nrow = NROW(quotes))
  for (x in 1:num.period) r[, x] <- Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x <  -tgt.margin]))
  if (is.xts(quotes)) xts(x, time(quotes))
  else x
 }
require(quantmod)
candleChart(last(OHLC(to.daily(Port.Prices[[3]]$prices)), "6 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = Target.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()


  # Defining Technical Indicator Function
require(TTR)
myATR <- function(x) ATR(HLC(x), n = 14)[, "atr"]
mySMI <- function(x) SMI(HLC(x), n = 13)[, "SMI"]
myADX <- function(x) ADX(HLC(x), n = 14)[, "ADX"]
myAroon <- function(x) aroon(HLC(x)[, c(1, 2)], n = 20)$oscillator
myBB <- function(x) BBands(HLC(x), n = 20, sd = 2)[, "pctB"]
myDeltaChaikinVol <- function(x) Delt(chaikinVolatility(HLC(x)[,c(1,2)], n = 10))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)), n = 10)[, 1]
myEMV <- function(x) EMV(HLC(x)[,c(1,2)], Vo(x), n = 9)[, 2]
myMACD <- function(x) MACD(Cl(x), nSig = 9)[, 2]
myMFI <- function(x) MFI(HLC(x), Vo(x), n = 14)
mySAR <- function(x) SAR(HLC(x)[, c(1,3)])[, 1]
myVolat <- function(x) volatility(OHLC(x), n = 10, calc = "yang.zhang")[, 1]
  #  see reference for calc method @ http://happywednesday.org/portal/content/historical-open-high-low-close-volatility-garman-and-klass-yang-zhang

require(randomForest)
x <- to.daily(Port.Prices[[3]]$prices)
data.model <- specifyModel(Target.ind(x) ~ Delt(Cl(x),k=1:10) + 
                              myATR(x) + mySMI(x) + myADX(x) + myAroon(x) +
                              myBB(x) + myDeltaChaikinVol(x) + myCLV(x) +
                              CMO(Cl(x)) + EMA(Delt(Cl(x))) + myEMV(x) +
                              myVolat(x) + myMACD(x) + myMFI(x) + RSI(Cl(x)) +
                              mySAR(x) + runMean(Cl(x)) + runSD(Cl(x)))
# set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c(start(x),index(x["2012-05-31"])),
                 ntree=50, importance=T)
## possible methods are: lm, glm, loess, step, ppr, rpart[rpart], tree[tree], randomForest[randomForest], mars[mda], polymars[polspline], lars[lars], rq[quantreg], lqs[MASS], rlm[MASS], svm[e1071], and nnet[nnet].

#Plotting the Importance of the variable for the above random forest
varImpPlot(rf@fitted.model, type = 1)

# Ranking the variable based on importance, and selecting the top 10
imp <- importance(rf@fitted.model, type = 1)
top10var <- rownames(imp)[which(imp > 10)]


## Based on list of variable calc above (top10Var), re-write the optimal model to use afterwards.


Tdata.train <- as.data.frame(modelData(data.model, data.window=c('2012-03-31','2012-05-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2012-06-01','2012-07-31'))))
Tform <- as.formula('T.ind.GSPC ~ .')