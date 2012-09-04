#e.g.
#xts.merge.series <- merge(std.WFC.Close.Daily[[1]], std.IBM.Close.Daily[[1]])


###
#To DO: extend to multiple time series in one plot.  how to make sure that each line has a different format?
###

PlotTwoPriceSeries <- function(xts.input){
####   argument: must be an xts object containing one or more time series, for the same time points.  e.g. close or open series.
#argument should contained standardised values for ylim to make sense here  
zoo.merge.series <- as.zoo(xts.input)

seriesnames <- names(zoo.merge.series)

xlab="Date"
ylab="Price"
main="Price History"
#lty=c("dotted", "solid")
ylim=range(coredata(zoo.merge.series))
# Plot the two time series in two plots
plot(zoo.merge.series, screens=1, lty=lty, main=main, xlab=xlab, ylab=ylab, ylim=ylim)
# Plot the two time series in one plot
legend(x="bottomright", legend = seriesnames, lty=c("dotted", "solid"))
}
