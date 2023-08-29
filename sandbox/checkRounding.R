library(PCRA)
factorsSPGMI2 <- factorsSPGMI
cols <- c(9:22)
factorsSPGMI2[,cols] <- round(factorsSPGMI2[,cols,with=FALSE],6)
save(factorsSPGMI2,file ="factorsSPGMI2.RDA",compress="xz")

