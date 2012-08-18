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
      data <- readWorksheet(wb, sheet = sheets.name[i], startRow = 3, startCol = 2)
      tmp <- strsplit(sheets.name[i], " ")
      port.manager <- tmp[[1]][1]
      rebalance.date <- as.Date(as.yearqtr(tmp[[1]][2], "%Y-%m"), frac = 1) # converting 2nd part of sheet name to date
      Port.Cont[[n2]] <- list(Data =  data, Portfolio.manager = port.manager, Rebalancing.date = rebalance.date)
    }
  }
  length(Port.Cont) <- n2  # resizing the list
  
  return(Port.Cont)
}


