#!/usr/bin/env Rscript
## compare PDO with Seldovia water temperature data

## not working as of 2022 -- update or abandone?

rm (list = ls())
## read-in PDO data
dsource <- "http://research.jisao.washington.edu/pdo/PDO.latest.txt"
# dsource <- "~/GISdata/TimeSeries/PDO.latest.txt"
pdo <- read.fwf (dsource, width = c(4, 2, rep (7, 12)), skip = 34
               , col.names = c("year", "comment", month.abb)
               , nrows = 155-34
                 )
pdo <- subset (pdo, !is.na (pdo$Dec))
rm (dsource)

pdoL <- reshape (pdo, varying = month.abb, direction = "long"
               , v.names = "pdo")
names (pdoL)[names (pdoL)=="time"] <- "month"
pdoL$date <- with (pdoL
                 , as.POSIXct (paste (year, "-", month, "-1 00:00:00", sep = ""))
                   )

## read in Seldovia temp data
require ("XLConnect")
sldvia <- readWorksheetFromFile ("~/GISdata/LCI/Seldovia_SWMP_deep_TempSal_MonAVG_ANOM_2004-2016.xls", sheet = 1, startRow = 3)


## Seldovia Temperature data -- straight from SWMP -- still need to aggregate
if (1){ ## outsourced to SeldoviaTemp.R
## downloaded from https://cdmo.baruch.sc.edu: station KACSDWQ
## pick zip file through advanced download (no way to automate that?)
sldvia <- read.csv ("~/GISdata/LCI/SeldoviaWaterTemp/KACSDWQ.csv", skip = 2)
require ("SWMPr")
sldvia <- subset (sldvia, sldvia$Station_Code == "kacsdwq")
sldvia <- subset (sldvia, !is.na (time_vec (sldvia$DateTimeStamp, "kacswq")))
## swS <- swmpr (sldvia, meta_in = "kacswq")
## monTemp <- aggreswmp (swW, "monthly", "Temp")

sldvia$F_Temp <- substr (sldvia$F_Temp, 1, 4)
sldvia$F_Temp <- trimws (sldvia$F_Temp)
sldvia <- subset (sldvia, as.character (sldvia$F_Temp) %in% c("<0>", "<1>", "<4>"))

    sldvia$dateTime <- time_vec (sldvia$DateTimeStamp, station_code = "kacswq", tz_only = FALSE)
    sldvia$month <- as.numeric (strftime(sldvia$dateTime, format='%m'))
    sldvia$year <- as.numeric (strftime (sldvia$dateTime, format = '%Y')) # sldvia$dateTime$year + 1900
    sldvia$day <- as.numeric (strftime (sldvia$dateTime, format = "%d"))


       tempM <- aggregate (Temp~month+year, sldvia, FUN = "mean")
    moMean <- aggregate (Temp~month, sldvia, FUN = "mean")
    tempM$TempAnom <- tempM$Temp - (moMean$Temp [match (tempM$month, moMean$month)])
    tempM$mMonth <- with (tempM, as.POSIXct (paste (year, month, "15", sep = "-")))
#    plot (TempAnom~mMonth, tempM, type = "lines")
    save (tempM, file = "~/tmp/LCI_noaa/cach/tempAnomalyMonth.RData")

    ## aggregate daily, make ts object
    if (0){
        tempD <- aggregate (Temp~day+month+year, sldvia, FUN = "mean")
        ##    tempTS <- ts (tempD$Temp, start = c(2004, 1, 1),
        tempTS <- ts (sldvia$Temp, start = 1, frequency = 4*24)
    library (forecast)
    # tempTS <- ts (sldvia$Temp, start =
    sTemp <- with (sldvia, data.frame (time = sldvia$dateTime, Temp))
    }




## sldvia$dateTime <- strptime (sldvia$DateTimeStamp, format = "%m/%d/%Y %H:%M") #, tz = "-0900") #, tz = "America/Anchorage"), tz = "AKST"
# sldvia <- subset (sldvia, !is.na (dateTime))
# SSal <- swmpr (sldvia, meta_in = "kacswq")
## sldviaS <- import_local ("~/GISdata/LCI/SeldoviaWaterTemp/72106.zip", station_code = "kacswq")
## sldviaS <- import_local ("/Users/martin/GISdata/LCI/SeldoviaWaterTemp/72106.zip", station_code = "kacswq")
## sldviaS <- import_local ("/Users/martin/GISdata/LCI/SeldoviaWaterTemp/KACSDWQ.csv", station_code = "kacswq")
## "~/GISdata/LCI/SeldoviaWaterTemp/KACSDWQ.csv"
# sldviaS <- swmpr ("/Users/martin/GISdata/LCI/SeldoviaWaterTemp/KACSDWQ.csv", meta_in = "kacswq")  # KACSWQ")
## # require ("rnoaa")
# summary (sldvia$dateTime)
# plot (Temp~dateTime, sldvia)
## calculate ARIMA -- annual and daily signals
    ## dD <- decomp (sTemp, para = "Temp", frequency = "daily")
## aD <- decomp (sTemp, para = "Temp", frequency = "annual")
## x <- tbats (sTemp$Temp, seasonal.periods = c(4*60*24, 4*60*24*265, start = 2004)) # way to go -- fix XXX
}


sldvia$pdo <- pdoL$pdo [match (sldvia$Mon_Yr, pdoL$date)] # look up corresponding pdo
pCor <- cor (sldvia[,which (names (sldvia) == c("Anomaly..C.", "pdo"))])


sldvia$AnomalyS <- sldvia$Anomaly..C. / sd (sldvia$Anomaly..C.)
save.image ("~/tmp/LCI_noaa/cache/pdo1.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/pdo1.RData")





## plot time series
pdf ("~/tmp/LCI_noaa/media/pdoTS.pdf")

plot (AnomalyS~Mon_Yr, sldvia, type = "l", ylab = "", xlab = "")
lines (pdo~Mon_Yr, sldvia, col = "red")
legend ("bottomleft", legend = c("Seldovia", "pdo"), col = c("black", "red"), lwd = 2)
dev.off()

par (mfrow = c(2,1))
par (mar = c(1,4,2,1))
plot (Anomaly..C.~Mon_Yr, sldvia, type = "l", ylab = "Seldovia Anomaly [K]"
    , xlab = "", axes = FALSE)
axis (2); box()

par (mar = c(2,4,1,1))
plot (pdo~Mon_Yr, sldvia, type = "l"
    , main = paste ("r =", round (pCor [1,2], 2))
    , xlab = "")
dev.off()






## define as time series
## calculate seasonal anomalies --- already done in Excel?!

## reshape temp to wide
sTS <- ts (sldvia$Monthly.Average..C., frequency = 12, start = c(2004, 1))


sldL <- data.frame (
    year = factor (substr (sldvia$Mon_Yr, 1,4))
    , month = factor (substr (sldvia$Mon_Yr, 6,7))
    , temp = sldvia$Monthly.Average..C.
    )

slW <- matrix (sldL$temp, ncol = 12, byrow = TRUE
             , dimnames = list (levels (sldL$year), month.abb)
               )
## plot (apply (slW, 2, mean), type = "l") # checked that monthly means are smooth!
slWa <- apply (slW, 2, function (x){x - mean (x)})

sldL$tAnom <- as.numeric (t (slWa))
tsA <- ts (sldL$tAnom, frequency = 12, start = c(2004, 1))
tsAs <- filter (tsA, filter = rep (1/3,3), method = "convolution", sides = 2)


pdf ("~/tmp/LCI_noaa/media/seldoviaAnomTemp.pdf")
plot (tsA, ylab = "seasonal temperature anomaly [K]", xlab = "") #, type = "n")
abline (h = 0, lty = "dashed", col = "gray", lwd = 2)
lines (tsAs, lwd = 3) #, col = ifelse (tsAs > 0, "red", "blue"))
## barplot (height = as.numeric (tsAs), col = ifelse (tsAs > 0, "red", "blue")
##          , names.arg = sldvia$Mon_Yr)
abline (v = 0:3+2014, col = "gray", lwd = 1)
legend ("topleft", legend = c("monthly", "MA smoother"), lwd = c(1,3))
         # col = c("black", "red"), lwd = 2)
dev.off()


pdf ("~/tmp/LCI_noaa/media/seldoviaTemp.pdf")
plot (ts (sldL$temp, frequency = 12, start = c(2004,1))
, type = "l", ylab = "Seldovia temperature [C]")#   plot(x~y,ylab=~degree~C) ;  plot(x~y,ylab=expression(~degree~C))
abline (v = 0:3+2014, col = "gray", lwd = 1)
dev.off()
