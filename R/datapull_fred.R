## TODO: Add comment
## 
## Author: klei
################################################################################
#
##install.packages("fredr")
#
#library(dplyr)
#library(data.table)
#library(fredr)
#library(purrr)
##library(lubridate)
#
#fredr_set_key("caf71401e549e0570139e5188af2e40b")
#map_dfr(c("UNRATE", "FEDFUNDS"), fredr)
#rates = map_dfr(c("DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"), fredr)
#
#
#write.table(rates,file="rates.csv")
#
#
#IRdaily = data.table(rates)
#
#
#IRdaily = dcast(IRdaily,date~series_id)
#
#
#IRdaily[,TwosFives:=(DGS5-DGS2)] 
#IRdaily[,FivesTwentys:=(DGS20-DGS5)] 
#IRdaily[,TwosTens:=(DGS10-DGS2)] 
#
#
#col= setdiff(names(IRdaily),"date")
#
#
#deltaIRdaily = IRdaily[,lapply(.SD,diff),.SDcols=col]
#
#
#IRweekly = IRdaily[weekdays(date)=="Wednesday"]
#
#deltaIRweekly = IRweekly[,lapply(.SD,diff),.SDcols=col]
#
#
#IRmonthly = IRdaily[order(date), lapply(.SD,function(x){x[which.max(date)]}), .SDcol=c("date",col), by = .(year(date), month(date))] 
#
#deltaIRmonthly = IRmonthly[,lapply(.SD,diff),.SDcols=col]
#
#
#
#filteredDeltaIRdaily = deltaIRdaily[(!is.na(deltaIRdaily[,DGS5]))&(!is.na(deltaIRdaily[,TwosTens]))]
#
#
#data.table(cor(filteredDeltaIRdaily))[,TwosTens]
#
#
#startDate = IRdaily$date[unlist(deltaIRdaily[,lapply(.SD,function(x)min(which(!is.na(x)))),.SDcols=col])+1]
#
#names(startDate) = col
#
#IRdaily[date=="1993-10-11"]
