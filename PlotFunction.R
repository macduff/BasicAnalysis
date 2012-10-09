###
#Merge.IBM.WFC['/2012-05-01']  this will pull up to May 01.
#Merge.IBM.WFC['2012-03-01/2012-05-01'] this will pull from/to
####

PlotTwoPriceSeries <- function(xts.input){
####   argument: must be an xts object containing one or more time series, for the same time points.  e.g. close or open series.
#argument should contained standardised values for ylim to make sense here  
zoo.merge.series <- as.zoo(xts.input)

seriesnames <- names(zoo.merge.series)

xlab="Date"
ylab="Price"
main="Price History"
lty=c("dotted", "solid")
ylim=range(coredata(zoo.merge.series))
# Plot the two time series in two plots
plot(zoo.merge.series, screens=1, lty=lty, main=main, xlab=xlab, ylab=ylab, ylim=ylim)
# Plot the two time series in one plot
#legend(x="bottomright", legend = seriesnames, lty=c("dotted", "solid"))
}

#standardize univariate time series
Stdize.uni.xts <- function(xts.input){
  ####   argument: must be a univariate xts object.  e.g. close or open series.
  ####   return: a list containing the xts and dataframe form of the standardized price series
  rawdata <- as.vector(coredata(xts.input))
  mean <- mean(rawdata)
  std <- sd(rawdata,na.rm = TRUE)
  sprd.xts <- ( xts.input - mean ) / std 
  return(sprd.xts)
}


#requires function Stdize.uni.xts
PlotTwoPriceSeriesScaled <- function(xts.input){
  ####   argument: must be an xts object containing one or more time series, for the same time points.  e.g. close or open series.
  #argument should contained standardised values for ylim to make sense here  
  
  y1 <- Stdize.uni.xts(xts.input[,1])
  y2 <- Stdize.uni.xts(xts.input[,2])
  
  scaled.comb.ser <- merge(y1,y2)#combine the standardized series
  
  zoo.scaled.comb.ser <- as.zoo(scaled.comb.ser)
  seriesnames <- names(zoo.scaled.comb.ser)
  
  xlab="Date"
  ylab="Price"
  main="Price History"
  lty=c("dotted", "solid")
  ylim=range(coredata(zoo.scaled.comb.ser))
  # Plot the two time series in two plots
  plot(zoo.scaled.comb.ser, screens=1, lty=lty, main=main, xlab=xlab, ylab=ylab, ylim=ylim)
  # Plot the two time series in one plot
  #legend(x="bottomright", legend = seriesnames, lty=c("dotted", "solid"))
}

