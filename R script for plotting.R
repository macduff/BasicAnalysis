
Dates <-row.names(df.BAC.Prices)
Dates2 <- as.Date(Dates)
plot(Dates2,df.BAC.Prices$BAC.Open,type="l", col = 1, ylim=c(0,40),ylab="Prices",xlab="Dates")
lines(Dates2,df.C.Prices$C.Open,type="l",col = 2)

BAC.ScaledPrices <- (df.BAC.Prices$BAC.Open - mean(df.BAC.Prices$BAC.Open))/sd(df.BAC.Prices$BAC.Open)
C.ScaledPrices <- (df.C.Prices$C.Open - mean(df.C.Prices$C.Open))/sd(df.C.Prices$C.Open
                                                                     
plot(Dates2,BAC.ScaledPrices,type="l", col = 1, ylim=c(-5,5),ylab="Prices2",xlab="Dates")
lines(Dates2,C.ScaledPrices,type="l",col = 2)

legendvals <- c("bank of america","Citigroup")
locationPoints <- locator(1)
legend(locationPoints,legendvals)
title('Testing spreads')