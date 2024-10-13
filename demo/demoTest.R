library(PCRA)
library(xts)
stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2004-01-01", "2005-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", 
                "MktIndexCRSP", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")

returns <- returnsAll[ , 1:4]
tsPlotMP(returns)