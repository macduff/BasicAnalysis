getHedgeFund.ExcelData <- function(workbook = 'Hedge Fund Managers of interest.xlsx') {
  require(XLConnect)
  require(zoo) # using as.yearqtr to get date
  wb <- loadWorkbook(workbook)
  sheets.name <- getSheets(wb)
  n <- length(sheets.name)
  Port.Cont <- vector("list", n) # pre-allocating the list size for efficiency
  
  n2 <- 0
  for (i in 1:n){
    if (sheets.name[i] != 'Comments') {
      n2 <- n2 + 1
      data <- readWorksheet(wb, sheet = sheets.name[i], startRow = 4, startCol = 2)
      tmp <- strsplit(sheets.name[i], " ")
      port.manager <- tmp[[1]][1]
      rebalance.date <- as.Date(as.yearqtr(tmp[[1]][2], "%Y-%m"), frac = 1) # converting 2nd part of sheet name to date
      Port.Cont[[n2]] <- list(Data =  data, Portfolio.manager = port.manager, Rebalancing.date = rebalance.date)
    }
  }
  length(Port.Cont) <- n2  # resizing the list
  
  return(Port.Cont)
}
getTop <- function(p = 0.1, portfolio.data) {
  ## usefull function to could have been used instead of missing/in conjuction with missing below
  ## http://r.789695.n4.nabble.com/get-arguments-passed-to-function-inside-a-function-td3783894.html
  # t <- as.list(match.call())
  # a <- which(names(t) == 'portfolio.data')
  # if(length(a)) { print(we have it)}
  # else { print("we don't have a second argument")}
  #
  ## another way is to save the enviroment variable in a list, and check this list:
  # t <- as.list(environment())
  # which(names(t) == 'portfolio.data')
  if (missing(portfolio.data)){
    portfolio.data <- getHedgeFund.ExcelData()
    print("Getting portfolio data")
  }

  x1 <- ldply(portfolio.data, function(x) x[["Data"]][c("SECURITY","TICKER","X..PORT")])
  x2 <- count(x1, c("SECURITY","TICKER"),"X..PORT")
  x3 <- na.omit(arrange(x2,desc(freq))) ## OR x3 <- na.omit(x2[order(x2$freq,decreasing=T),])
  x3$p <- x3$freq/sum(x3$freq)
  x3$P <- cumsum(x3$p)
  if (p < 1) indx <- which(x3$P < (1-p))
  else {
    #warning("MyWarn: p has to be less than 1")
    indx <- 1:length(x3$freq)
  }
  #return(x3[indx,])
  return(t)
}
