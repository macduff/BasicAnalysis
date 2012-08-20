
#*******************************************
#Our basic set up for storing info on companies.
#*******************************************


# This function reminds us of what information we want to store on each company.
# The returned list is an input to the function CompanyObject

companyInfo <- function(Ticker = "?", Name ="?", Sector ="?",Exchange ="?", Indices = "?")
{
  #indices and exchange may be a vector of character values.
  comp_info <- list(Ticker=Ticker,Name = Name, Sector=Sector,Exchange = Exchange, Indices = Indices)
  return(comp_info)
}


companyObject <- function(comp_info,priceHist)#This function will be extended to include option price history lists e.g. 3rd parameter might be one year call options at strike X.
{
  #company object will contain all info on a particular company.  It's descriptive info in first list, second list is price history, future lists appended include option prices.
  companyObj <- list("Information" = comp_info, "Stock.PH" = priceHist)
  return(companyObj)
}


#An aside R coding reminder for CM:
#to rename a list component use:e.g. first component names(AAPL.List)[1] <-"Stock.PH.Daily"

