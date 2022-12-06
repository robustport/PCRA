# cleanstocksCRSP.R

# sectors incorrectly named in factorsSPGMI/stocksCRSP - need to be fixed.
# four stocks included in the data set within sectors that we want to exclude 
  # (Financials and Real Estate) and replace with four different securities from 
  # the master list of 310 securities found within 
  # "stocksTickers310GICSgovindSPGMI".
# market Capitalization groups ("LargeCap", "MidCap", "SmallCap", and 
  # "MicroCap") assignments are not correct for about 25% of the data set 
  # within factorsSPGMI and should be re-assigned.


# load data from FA & create tmp copies
load("C:/FA/Braverock/FactorAnalytics/data/stocksCRSP.rda")
load("C:/FA/Braverock/FactorAnalytics/data/factorsSPGMI.rda")
factorsSPGMI_tmp <- factorsSPGMI
stocksCRSP_tmp <- stocksCRSP

### 1. clean up list of sectors for factorsSPGMI & stocksCRSP

# confirm factorsSPGMI and stocksCRSP have incorrect sectors (misspelled)
unique(factorsSPGMI_tmp$Sector) # contains 16 sectors with dupes
unique(stocksCRSP_tmp$Sector) # containts 16 sectors with dupes

# replacement data as per issue #86 description
bad_sectors <- unique(factorsSPGMI_tmp$Sector)
good_sectors <- c("Information Technology","Industrials","Health Care",
                  "Consumer Staples","Energy","Materials",
                  "Consumer Discretionary","Communication Services","Utilities",
                  "Real Estate","Health Care","Financials",
                  "Consumer Discretionary","Information Technology",
                  "Consumer Staples","Communication Services")
sector_table <- data.frame(cbind(bad_sectors,good_sectors))
colnames(sector_table) <- c("BadSectors","GoodSectors")
sector_table

# replacements
factorsSPGMI_tmp$Sector <- sector_table$GoodSectors[match(factorsSPGMI_tmp$Sector,sector_table$BadSectors)]
stocksCRSP_tmp$Sector <- sector_table$GoodSectors[match(stocksCRSP_tmp$Sector,sector_table$BadSectors)]

# confirm factorsSPGMI and stocksCRSP have correct sectors
unique(factorsSPGMI_tmp$Sector) # contains 11 sectors with no dupes
unique(stocksCRSP_tmp$Sector) # contains 11 sectors with no dupes

# check membership
stocksCRSPLast <- stocksCRSP_tmp[Date == lastDateCRSP]
stocksCRSPLast[ , .N, by = Sector] 

### 2. Sector membership fixes

# sector names incorrect for a few securities
right_sectors <- data.frame(cbind(c("AVP","CSH","CTS","PIR",
                                    "RTN","STJ","TSS"),
                                  c("Consumer Staples",
                                    "Financials",
                                    "Information Technology",
                                    "Consumer Discretionary",
                                    "Industrials",
                                    "Health Care",
                                    "Information Technology")))
colnames(right_sectors) <- c("TickerLast","Sector")

# first factorsSPGMI
factorsSPGMI_tmp$Sector2 <- right_sectors$Sector[match(factorsSPGMI_tmp$TickerLast,right_sectors$TickerLast)]
factorsSPGMI_tmp$Sector <- ifelse(is.na(factorsSPGMI_tmp$Sector2),factorsSPGMI_tmp$Sector,factorsSPGMI_tmp$Sector2)
factorsSPGMI_tmp$Sector2 <- NULL

# stocksCRSP
stocksCRSP_tmp$Sector2 <- right_sectors$Sector[match(stocksCRSP_tmp$TickerLast,right_sectors$TickerLast)]
stocksCRSP_tmp$Sector <- ifelse(is.na(stocksCRSP_tmp$Sector2),stocksCRSP_tmp$Sector,stocksCRSP_tmp$Sector2)
stocksCRSP_tmp$Sector2 <- NULL

# GICS code also incorrect
right_gics <- data.frame(cbind(c("AVP","CSH","CTS","PIR",
                                 "RTN","STJ","TSS"),
                               c(30302010,
                                 40202010,
                                 45203020,
                                 25504060,
                                 20101010,
                                 35101010,
                                 45102020)))
colnames(right_gics) <- c("TickerLast","GICS")

# first factorsSPGMI
factorsSPGMI_tmp$GICS2 <- right_gics$GICS[match(factorsSPGMI_tmp$TickerLast,right_sectors$TickerLast)]
factorsSPGMI_tmp$GICS <- ifelse(is.na(factorsSPGMI_tmp$GICS2),factorsSPGMI_tmp$GICS,factorsSPGMI_tmp$GICS2)
factorsSPGMI_tmp$GICS2 <- NULL

# stocksCRSP
stocksCRSP_tmp$GICS2 <- right_gics$GICS[match(stocksCRSP_tmp$TickerLast,right_gics$TickerLast)]
stocksCRSP_tmp$GICS <- ifelse(is.na(stocksCRSP_tmp$GICS2),stocksCRSP_tmp$GICS,stocksCRSP_tmp$GICS2)
stocksCRSP_tmp$GICS2 <- NULL


res <- data.frame(factorsSPGMI_tmp[TickerLast 
                                   %in% 
                                     c("AVP","CSH","CTS","PIR",
                                       "RTN","STJ","TSS"), 
                                   unique(paste(TickerLast,Sector, GICS))])
colnames(res) <- "Ticker/Sector/GICS"
res

# confirm factorsSPGMI and stocksCRSP have correct sectors
unique(factorsSPGMI_tmp$Sector) # 11 sectors with no dupes
unique(stocksCRSP_tmp$Sector)
stocksCRSPLast <- stocksCRSP_tmp[Date == lastDateCRSP]
stocksCRSPLast[ , .N, by = Sector] 
