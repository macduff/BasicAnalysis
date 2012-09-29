library(quantmod)

##################################################################################
#TO DO:  need to modify function to exclude gaps at end of each day.
##################################################################################

PriceToReturns <- function(xts.input){
  ####   argument: must be a univariate xts object.  e.g. close or open series.
  ####   return: a list containing the xts and dataframe form of the return series
  n <- length(xts.input)
  stopifnot(n>1)   #do not proceed if we don't have data.
  
  ######  Want a dataframe to calculate returns #####
  names(xts.input) <- "price.data"
  df.xts.input <- as.data.frame(xts.input)
  time.label <-row.names(df.xts.input)
  df.xts.input["time"] <-time.label#hold onto the time labels 
  
  one.period.ret <- (df.xts.input$price.data[2:n] - df.xts.input$price.data[1:(n-1)])/ df.xts.input$price.data[1:(n-1)]
  
  #### This is a check on the return numbers   ######
  one.period.ret.check <- diff(xts.input[1:10])/lag(xts.input[1:10])
  #gives slightly different answers to one.period.ret !!! ???
  browser()
  
  time.label.ret <- time.label[-1]#drop first time point for the return series.
  df.rets <- data.frame(one.period.ret)
  rownames(df.rets) <- time.label.ret
  xts.rets <- as.xts(df.rets)
  
  rets.list <- list(xts.rets = xts.rets, df.rets = df.rets)
  return(rets.list)
}


##################################################################################
#TO DO:  need to modify function to calculate sharpe ratio for any time frequncy. Currently based on daily returns
##################################################################################
SharpeRatio <- function(strat.rets){
  ####   argument: a returns object containing portfolio returns as df and xts objects
  ####   return: the Sharpe Ratio performance measure
  df.strat.rets <-strat.rets$df.returns
  one.period.rf.ret = .03 / 252# risk-free return.  This could come in as one number, or as a vector giving daily rate.
  
  excess.ret <- df.strat.rets[[1]] - one.period.rf.ret
  
  SharpeRatio <- mean(excess.ret) / sd(excess.ret)
  
  return(SharpeRatio)
  #notes: (p21 qt book)
  #Can expect Sharpe ratio > 3 if strategy is profitable every day
  #Sharpe ratio <1 suggests strategy is not so good.
  #strategy with profitability in most months produces sharpe ratio of >2
}

##################################################################################
#TO DO:  need to modify function to calculate drawdowns for any time frequncy. Currently based on daily returns
##################################################################################
MaxDrawDownStats <- function(strat.rets){

  delta_t = 1  # length of time period in use
  cum.ret <- cumprod(1+coredata(strat.rets))-1
  high.water.mark <- rep(0, length(cum.ret))
  draw.down <- rep(0,length(cum.ret))
  draw.down.duration <- rep(0,length(cum.ret))
  # we need to add 0 to cum.ret because index 1 is really time 0.
  cum.ret <- c(0, cum.ret)
  
  draw.down2 <- rep(0,length(cum.ret))
  draw.down.duration2 <- rep(0,length(cum.ret))
    
  f_max <- function(x1, x2){ return(max(x1,x2))}
  high.water.mark2 = Reduce(f_max  ,cum.ret[2:length(cum.ret)],init = 0, accumulate=TRUE)
      
  for(t in 2:length(cum.ret)){
    high.water.mark[t] =  max(high.water.mark[t-1], cum.ret[t])#most recent fund maximum in terms of cum returns.
    draw.down[t] <- (1 + high.water.mark[t])/(1 + cum.ret[t]) - 1   #portfolio loss at time t
    draw.down.duration[t] <- ifelse(draw.down[t] == 0,  0, draw.down.duration[t-1] + delta_t)
  
  }
    draw.down2 <- (1 +high.water.mark2)/(1+cum.ret) -1
   # draw.down.duration2[which(draw.down2 == 0)] = 0
  
    f_cond <- function(x1,x2)
    {
      return(ifelse(x2 == 0, 0, x1 + delta_t))  
    }
    draw.down.duration2 = Reduce(f_cond,draw.down2[2:length(draw.down2)], init = 0, accumulate=TRUE)
  
  max.draw.down = max(draw.down)
  max.draw.down.duration = max(draw.down.duration)
  
  draw.down.summary <- list(max.draw.down = max.draw.down, max.draw.down.duration = max.draw.down.duration)
  
  return(draw.down.summary)
}



StdizeSeries <- function(xts.input){
  ####   argument: must be a univariate xts object.  e.g. close or open series.
  ####   return: a list containing the xts and dataframe form of the standardized price series
  
  names(xts.input) <- "price.data"
  df.xts.input <- as.data.frame(xts.input)
  time.label <-row.names(df.xts.input)
  df.xts.input["time"] <-time.label
  
  stdize.price.ser <- (df.xts.input$price.data - mean(df.xts.input$price.data))/sd(df.xts.input$price.data)
  
  df.stdize.price.ser <- data.frame(stdize.price.ser)
  rownames(df.stdize.price.ser) <- time.label
  xts.stdize.price.ser <- as.xts(df.stdize.price.ser)
  
  stdized.price.ser.list <- list(xts.stdized.prices = xts.stdize.price.ser, df.stdized.prices = df.stdize.price.ser)
  return(stdized.price.ser.list)
}