#!/usr/bin/env Rscript

## calculate time series and decomposition from Seldovia water temperature records
## and salinity

## downloaded from https://cdmo.baruch.sc.edu: station KACSDWQ
## pick zip file through advanced download (no way to automate that?)

## missing feature: turn this into daily smoothed anomalies, using multi-seasonal decomposition
# rm (list = ls())


## set-up parameters

# if (.Platform$OS.type == "windows"){
#   Require ("R.utils")
#   SMPfile <- readWindowsShortcut ("~/GISdata/LCI/SWMP/current.lnk")$path
# }else{ # MacOS or Linux
#   SMPfile <- "~/GISdata/LCI/SWMP/current"
# }
source ("annualPlotFct.R") # already loads SWMPr

suppressPackageStartupMessages (Require ("R.utils"))
SMPfile <- filePath ("~/GISdata/LCI/SWMP/current", expandLinks = "local") # works on all platforms?


## set-up local environment and load functions
dir.create("~/tmp/LCI_noaa/media/2019/", showWarnings = FALSE, recursive = TRUE)
dir.create ("~/tmp/LCI_noaa/media/StateOfTheBay", showWarnings = FALSE, recursive = TRUE)
#  setwd("~/myDocs/amyfiles/NOAA-LCI/")


## load and process SWMP data
## defines getSWMP () -- use this to replace import_local()
# unlink ("~/tmp/LCI_noaa/cache/SWMP", recursive = TRUE) # remove cache if downloaded new zip file from CDNA SWMP

Require ("SWMPr")
sldvia <- getSWMP ("kacsdwq") # Seldovia deep
sldvia1 <- getSWMP ("kacsewq") # Seldovia deep -- early, no longer updated
sldvia <- rbind (sldvia1, sldvia); rm (sldvia1)
sldvia$station <- "SeldoviaDeep"

## seldovia surface
sldviaS <- getSWMP ("kacsswq") # Seldovia shallow
sldviaS$station <- "SeldoviaShallow"

# homer as well or water temp and salinity?
homer <- getSWMP ("kachdwq")
homer1 <- getSWMP ("kachowq") ## legacy, no longer updated  ## also see kacdlwq  ## kachowq not active
homer <- rbind (homer1, homer); rm (homer1)
homer$station <- "HomerDeep"

homerS <- getSWMP ("kachswq") # Homer Dolphin shallow
homerS1 <- getSWMP ("kach3wq") # Homer Dolphin surface 3
homerS <- rbind (homerS, homerS1); rm (homerS1)
homerS$station <- "HomerShallow"


## rbind all to apply my own QAQC
sldvia <- rbind (sldvia, sldviaS, homer, homerS); rm (homer, homerS)
## swS <- swmpr (sldvia, meta_in = "kacswq")
## monTemp <- aggreswmp (swW, "monthly", "Temp")


## QAQC in addition to what's in SWMP's qaqc
sldvia$temp <- ifelse (sldvia$temp < -5, NA, sldvia$temp)  # -99 = NA code
names (sldvia) <- tolower (names (sldvia))

if (0){  ## instead of SWMP's qaqc function -- is this more reliable?
  sldvia$f_temp <- substr (sldvia$f_temp, 1, 4)
  sldvia$f_temp <- trimws (sldvia$f_temp)
  is.na (sldvia$temp)[!sldvia$f_tem %in% c("<0>", "<1>", "<4>")] <- TRUE
  print (summary (sldvia$temp)) # ; q()

  sldvia$f_sal <- substr (sldvia$f_sal, 1,4)
  sldvia$f_sal <- trimws (sldvia$f_sal)
  is.na (sldvia$sal)[!sldvia$f_sal %in% c("<0>", "<1>", "<4>")] <- TRUE

  sldvia$f_chlfluor <- substr (sldvia$f_chlfluor, 1,4)
  sldvia$f_chlfluor <- trimws (sldvia$f_chlfluor)
  is.na (sldvia$chlfluor)[!sldvia$f_chlfluor %in% c("<0>", "<1>", "<4>")] <- TRUE
}

## Fahrenheit -- do not compute here, but rather indicate in right-hand legend



## more QCQA -- fix spikes
# data.frame (sldvia$datetimestamp, sldvia$sal)[which (sldvia$sal < 14),]
is.na (sldvia$sal)[sldvia$sal > 33.5] <- TRUE # cluster in first year -- error?
is.na (sldvia$sal)[sldvia$sal < 14] <- TRUE # 2 single, disjoint values
# is.na (sldvia$sal)[sldvia$sal < 24] <- TRUE # XXX too fresh?

## impossible negative fluorescence values (should be gone already!)
is.na (sldvia$chlfluor)[sldvia$chlfluor <= 0] <- TRUE

sldvia$dateTime <- time_vec (sldvia$datetimestamp, station_code = "kacswq", tz_only = FALSE)

homerS <- sldvia [sldvia$station == "HomerShallow",]
homer <- sldvia [sldvia$station == "HomerDeep",]
sldviaS <- sldvia [sldvia$station == "SeldoviaShallow",]
sldvia <- sldvia [sldvia$station == "SeldoviaDeep",] ## have to keep for last!
## fill gapswith NA, as appropriate
sldvia <- fixGap (sldvia)
sldviaS <- fixGap (sldviaS)
homer <- fixGap (homer)
homerS <- fixGap (homerS)

# sldvia <- sldvia [sldvia$year > 2001,] # for a clean start of time series -- subset() fails



tempM <- aggregate (temp~month+year, sldvia, FUN = "mean")
moMean <- aggregate (temp~month, sldvia, FUN = "mean")
tempM$TempAnom <- tempM$temp - (moMean$temp [match (tempM$month, moMean$month)])
## tempM$mMonth <- with (tempM, as.POSIXct (paste (year, month, "15", sep = "-")))
rm (moMean)

# save (tempM, file = "~/tmp/LCI_noaa/cache/tempAnomalyMonth.RData")
save.image ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## pass this on to annual-salinity.R etc.
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")


###
## put above code into a script "annualAquireSWMP.R", seperating it from below plotting code?
###
# x <- subset (sldvia, datetimestamp > as.POSIXct("2020-01-01 00:00"))
# plot (temp~datetimestamp, x)



## aggregate daily, then calculate anomalies and smoothes
## reason for aggregation: fix NAs. HOWEVER, up to 3871 consecutive NAs in sldvia (that's 1 month!)
## could fill-in with Homer as an along?
## find consecutive NAs them with rle
## still would be better to do a formal TS seasonal decomposition. try again
aggFct <- function (swmp){
  tempDay <- aggregate (month~jday+year, swmp, FUN = function (x){x[1]})
  swmp$day <- as.numeric (strftime (swmp$datetimestamp, format = "%d")) # seems to be the only time day is needed
  # tDay <- aggregate (day~jday+year, swmp, FUN = function (x){x[1]})
  # tempDay$Day <- tDay$day [match (paste (tempDay$jday, tempDay$year)
  #                                 , paste (tDay$jday, tDay$year))]
  tempDay$Day <- aggregate (day~jday+year, swmp, FUN = function (x){x[1]})$day
  tempDayX <- aggregate (temp~jday+year, swmp, FUN = mean, na.rm = FALSE) # single NA should not kill full day
  tempDay$Temp <- tempDayX$temp [match (paste (tempDay$jday, tempDay$year) # do this to maintain NAs
                                        , paste (tempDayX$jday, tempDayX$year))]
  tempDayX <- aggregate (sal~jday+year, swmp, FUN = mean, na.rm = FALSE)
  tempDay$Sal <- tempDayX$sal [match (paste (tempDay$jday, tempDay$year)
                                      , paste (tempDayX$jday, tempDayX$year))]
  tempDay
}
tempDay <- aggFct (sldvia)
tHom <- aggFct (homer)
rm (homer, sldvia)

tempDay$homerT <- tHom$Temp [match (paste (tempDay$year, tempDay$jday), paste (tHom$year, tHom$jday))]
tempDay$homerS <- tHom$Sal  [match (paste (tempDay$year, tempDay$jday), paste (tHom$year, tHom$jday))]
## fill-in NA in homerS and homerT from homerDL
rm (tHom)

sltFit <- function (varN){
    Require ("stlplus")                       # use stlplus because it handles NAs gracefully
    stlplus (ts (tempDay [,which (names (tempDay) == varN)], start = c (min (tempDay$year),1)
               , frequency = 365.25)
           , s.window = "periodic")
}
Require ("stlplus")                       # use stlplus because it handels NAs gracefully
fit <- sltFit ("Temp")
fitS <- sltFit ("Sal")
fitSH <- sltFit ("homerS")
## should work, but too much effort -- takes long time to run
## fit <- stlplus (ts (sldvia$Temp, start = c (min (tempDay$year),1), frequency = 365*24*4)
##          , s.window = "periodic"
##          )

pdf ("~/tmp/LCI_noaa/media/2019/SeldoviaTemp_ts_decomp.pdf")
plot (fit)
plot (fitS)
plot (fitSH)
dev.off()
## system ("convertHQ ~/tmp/LCI_noaa/media/2019/SeldoviaTemp_ts_decomp.pdf ~/tmp/LCI_noaa/media/2019/SeldoviaTemp_ts_decomp.png")

## assume that stlplus seasonal is better than (somewhat noisy) dayMean
## dayMean <- aggregate (Temp~jday, sldvia, FUN = mean, na.rm = TRUE)
## tempDay$Anom <- tempDay$Temp - (dayMean$Temp [match (tempDay$jday, dayMean$jday)])
## rm (dayMean)

# tempDay$seasonal <- fit$data$seasonal
# Require ("zoo")
seaStd <- function (x, seasonC){ # subtract mean and standardize by range of season
    Require ("zoo")
    xM <- na.approx (x, na.rm = FALSE) - seasonC
    xM <- xM - mean (xM, na.rm = TRUE)
    xM <- xM / diff (range (seasonC))
    return (xM)
}

tempDay$Anom <- seaStd (tempDay$Temp, fit$data$seasonal)
tempDay$SalAnom <- seaStd (tempDay$Sal, fitS$data$seasonal)
tempDay$homerSAnom <- seaStd (tempDay$homerS, fitSH$data$seasonal)
rm (seaStd)


## moving average with different smoothing windows
## now in annualPlotFct.R
# maT <- function (temS, backWin){ #, sFUN = mean){ ## took care of NAs by averaging over days
#     ## backWin = n days prior to day in question to average over
#     ## dplyr:lag behaves differently than, and MASKS (!) stats:lag. Original here = dplyr:lag?
#     lag2 <- function (timeS, lg){       # somehow lag() stopped working -- arrrgh XXXX
#         ## lg <- lg*24*4
#         c (rep (NA, lg), timeS [1:(length (timeS)-lg)])
#     }
#     lagM <- sapply (0:backWin, function (x){lag2 (temS, x)})
#     winSum <- apply (lagM, 1, FUN = mean, na.rm = TRUE) # leave NAs to remove leading NAs
#     return (winSum)
# }
# save (maT, file = "~/tmp/LCI_noaa/cache/MAfunction.RData")
Require ("SWMPr")
maT <- function (df, maO){unlist (smoother (df, maO))}  # XXX move to annualPlotFct.R

lagV <- c(1,7,30,60,90, 180, 365, 730)
## lgMx <- sapply (lagV, FUN = function (x){maT (sldvia)})
lagMx <- sapply (lagV, FUN = function (x){maT (tempDay$Anom, x)})
colnames (lagMx) <- paste0 ("days", lagV)
tMTS <- lagMx [730:nrow (lagMx),]       # cut off poorly smoothed start years

lagMxS <- sapply (lagV, FUN = function (x){maT (tempDay$SalAnom, x)})
colnames (lagMxS) <- paste0 ("SalD", lagV)
sMTS <- lagMxS [730:nrow (lagMxS),]

lagMxSH <- sapply (lagV, FUN = function (x){maT (tempDay$homerSAnom, x)})
colnames (lagMxSH) <- paste0 ("HSalD", lagV)
sMTH <- lagMxSH [730:nrow (lagMxSH),]

## ## standardize by SD of seasonal variation
## tMTS <- tMTS/sd (fit$data$seasonal)
## sMTS <- sMTS/sd (fitS$data$seasonal)
## sMTH <- sMTH/sd (fitSH$data$seasonal)

pdf ("~/tmp/LCI_noaa/media/2019/Seldovia_tempTimeSeries_ma.pdf")
plot (ts (tMTS, deltat = 1/365.25, start = 2+min (tempDay$year))
      , main = "Moving average of Seldovia SST anomaly [°C]") # add zero-line
plot (ts (sMTS, deltat = 1/365.25, start = 2+min (tempDay$year))
    , main = "Moving average of Seldovia salinity anomaly")
plot (ts (sMTH, deltat = 1/365.25, start = 2+min (tempDay$year))
    , main = "Moving average of Homer salinity anomaly")
plot (ts (cbind (tMTS [,7], sMTS [,7], sMTH [,7]), deltat = 1/365.25, start = 2+min (tempDay$year))
    , main = "Moving average [1 year] of Seldovia SST and salinity anomalies")
dev.off()

pdf ("~/tmp/LCI_noaa/media/2019/Seldovia_Homer-TS-1yearMA.pdf")
plot (ts (sMTS [,7], deltat = 1/365.25, start = 2+min (tempDay$year))
    , type = "l", col = "blue", lwd = 2, xlab = "", ylab = "anomaly")
xA <- seq (2+min (tempDay$year), by = 1/365.25, length.out = nrow (tMTS))
lines (xA, tMTS [,7], col = "orange", lwd = 2)
lines (xA, sMTH [,7], col = "green", lwd = 2)
legend ("bottomleft", legend = c ("Seldovia S", "Seldovia T", "Homer S"), lwd = 2
      , col = c("blue", "orange", "green"))
dev.off()


pdf ("~/tmp/LCI_noaa/media/2019/Seldovia_tempSal-90d.pdf")
plot (ts (cbind (Temp=tMTS [,which (lagV == 90)], Sal=sMTS [,which (lagV == 90)]
                 , HomerSal = sMTH [,which (lagV == 90)])
        , deltat = 1/365.25, start = 2+min (tempDay$year))
    , main = "Moving average [90 day] of Seldovia-Deep SST and salinity anomalies")
dev.off()


## export temperature DF for use of anomalies elsewhere
tempDay <- cbind (tempDay, lagMx, lagMxS, lagMxSH)
rm (tMTS, sMTS, lagV, lagMx, lagMxS, maT)
rm (fit, fitS)


tempDay$MonthlyAnom <- tempM$TempAnom [match (with (tempDay, paste (year, month, sep = "-"))
                                            , with (tempM, paste (year, month, sep = "-"))
                                              )]
rm (tempM)



save (tempDay, file = "~/tmp/LCI_noaa/cache/dailyTempAnomalies.RData")
save.image ("~/tmp/LCI_noaa/cache/SeldTempAnom.RData")
## plot monthly anomalies along with monthly MA, 3-month, 365 days
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTempAnom.RData")

## plot (ts (tempDay [730:nrow (tempDay),c(16, 10, 12, 14)]
##         , deltat = 1/365.25, start = 2+min (sldvia$year)))

poDay <- with (tempDay, as.POSIXct (paste (year, month, Day, sep = "-"))) ## should not fail
if (0){ # graph is too cluttered
    poDay <- with (tempDay, as.POSIXct (paste (year, month, Day, sep = "-")))
    plot (tempDay$MonthlyAnom~poDay
        , col = "red", type = "l", lwd = 2)
    lines (tempDay$days30~poDay, col = "blue", lwd = 2)
    lines (tempDay$days90~poDay, col = "blue", lwd = 2, lty = "dashed")
}


pdf ("~/tmp/LCI_noaa/media/2019/Seldovia_testMonthlyTempAnom.pdf")
badClass <- with (tempDay, ((days30 < 0) & (MonthlyAnom > 0)) |
                                     ((days30 > 0) & (MonthlyAnom < 0))
                )
plot (MonthlyAnom~days30, data = tempDay
    , ylab = "Monthly temperature anomaly"
    , xlab = "30 day moving average of daily temperature anomaly"
    , col = ifelse (tempDay$days30 < 0, "blue", "red")
    , pch = ifelse (badClass, 4, 1)
    # , type = "n"
      )
abline (h = 0, lwd = 2)
abline (v = 0, lwd = 2)
abline (a = 0, b =1, lwd = 3, col = "gray")
# abline (lm (MonthlyAnom~days30, tempDay))
legend ("topleft", legend = paste0 ("bad classifications = "
                                  , round (sum (badClass)/length (badClass), 2)*100, "%")
      , bty = "n")
with (tempDay, abline (v = mean (days30) + c(0.5,-0.5) * sd (days30)
      , lty = "dashed", col = "gray"))
with (tempDay, abline (h = mean (MonthlyAnom) + c(0.5,-0.5) * sd (MonthlyAnom)
      , lty = "dashed", col = "gray"))
## points (MonthlyAnom~days30, data = tempDay
##       , col = ifelse (tempDay$days30 < 0, "blue", "red")
##     , pch = ifelse (badClass, 4, 1)
##       )
dev.off()
rm (badClass)


## make nice new temperature anomaly plot -- moved to zoopCommunity to show sampling effort
if (0){
pdf ("~/tmp/LCI_noaa/media/2019/SeldSalinity_vsTemp.pdf", width = 12, height = 7)
## plot (ts (tempDay [,which (names (tempDay) %in% c ("days90", "sal")), ]
##           , start = min (tempDay$year)))

nArgs <- with (tempDay, ifelse ((month == 1)&(Day == 1)&(year%%4==0), year, NA))
barplot (# tempDay$days90~poDay
         tempDay$Sal~poDay
       # , subset = tempDay$year > 2008
       , space = 0
#       , col = ifelse (tempDay$days30 > 0, "red", "blue")
       , col = ifelse (tempDay$days90 > 0, "red", "blue")
       , border = NA # ifelse (tempDay$days90 > 0, "red", "blue")
  , xlab = "time"
    , ylab = "Salinity at Seldovia"
#       , ylab = "90 day moving average temperature anomaly [°C]"
#       , ylab = "30 day temperature anomaly [°C]"
       , axes = FALSE
       , names.arg = "" #nArgs
         )
axis (2)
axis (1, at = which (!is.na (nArgs)), labels = FALSE) # subset (nArgs, !is.na (nArgs)))
axis (1, at = which (!is.na (nArgs)) + 182, tick = FALSE, labels = subset (nArgs, !is.na (nArgs)))
# axis (3)
abline (#v = as.POSIXct (paste0 (2001:2030, "-01-01"))
    v = which (with (tempDay, (month == 1) & (Day == 1)))
  , lty = "dashed", col = "gray")
box()
dev.off()
rm (nArgs)
}



if (0){ ## not so pretty
    tAn <- tempDay$Temp - mean (tempDay$Temp, na.rm = TRUE)
## plot (tAn~poDay, type = "l"
##     , col = ifelse (tAn < tempDay$season, "blue", "red")
##       )
## lines (tempDay$season~poDay)
plot (tempDay$season~poDay, type = "l")
lines (tAn~poDay
     , col = ifelse (tAn < tempDay$season, "blue", "red")
       , lwd = 2
)
}

tempDay$year <- factor (tempDay$year)

anAx <- function (){
    monD <- cumsum (c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))-31
    axis (1, at = monD [which ((1:length (monD))%%2 == 0)]
          , label = month.abb [which (1:length (month.abb)%%2 == 0)]
          )
    }
sdPlot <- function (tsVar, df = tempDay, ax1 = TRUE, yAn = ""){
    sdT <- aggregate (tsVar~jday, df, FUN = sd)
    plot (sdT, type = "l"
        , xlab = "" # "time of the year"
        , ylab = yAn
        , axes = FALSE)
    axis (2)
    if (ax1){anAx()}
    box()
    abline (h = mean (sdT$tsVar), col = "gray", lty = "dashed")
    ## add circular smoothing spline
    Require ("pbs")
    splS <- glm (tsVar~pbs::pbs (jday, df = 4, Boundary.knots=c(1,366)), data = sdT)
    splP <- predict (splS, type = "response", newdata = data.frame (jday = 1:366), se.fit = TRUE)
    lines (sdT$jday, splP$fit, col = "blue", lwd = 2)
    lines (sdT$jday, splP$fit - 2*splP$se.fit, col = "blue", lwd = 2, lty = "dashed")
    lines (sdT$jday, splP$fit + 2*splP$se.fit, col = "blue", lwd = 2, lty = "dashed")
}


## Require ("colorspace")
## tsCol <- rep (rainbow_hcl (length (levels (tempDay$year))/2), 2)
## Require ("RColorBrewer")
## tsCol <- rep (brewer.pal (length (levels (tempDay$year))/2, name = "Set3"), 2)
tsCol <- rep (1:(length (levels (tempDay$year))/2), 2)


pdf("~/tmp/LCI_noaa/media/2019/Seldovia_TempSal-AnnualCycle.pdf", height = 6, width = 8)
par (mfrow =c(2,2)
   , mar = c(3,4,0.2,1)+0.1
     )
# plot (tempDay$Temp~tempDay$season)
plot (Temp~jday, tempDay, ylab = "daily temperature [°C]"
    , xlab = "" # "time of the year"
    , type = "n"
    , axes = FALSE)
axis (2)
# anAx()
box()
for (i in 1:length (levels (tempDay$year))){
    lines (Temp~jday, subset = year == levels (year)[i], data = tempDay
         , col = tsCol [i], lwd = 2#, lty = ifelse (i <= 8,"dashed", "dotted")
           )
}
if (0){           # too cluttered on small panel
legend ("topleft", legend = levels (tempDay$year)
      , col = tsCol
      , lwd = 2, lty = c(rep ("dashed", 8), rep ("dotted", i-8))
      , bty = "n", ncol = 2)
}

## seasonal variation in interannual variability of temperature
sdPlot (tempDay$Temp, ax1 = FALSE
      , yAn = "variation in daily temperature [°C]")

## same for salinity
plot (Sal~jday, tempDay, ylab = "daily salinity"
      , xlab = "" # "time of the year"
    , type = "n", axes = FALSE)
axis (2)
anAx()
box()
for (i in 1:length (levels (tempDay$year))){
    lines (Sal~jday, subset = year == levels (year)[i], data = tempDay
         , col = tsCol [i], lwd = 2#, lty = ifelse (i <= 8,"dashed", "dotted")
           )
}
sdPlot (tempDay$Sal, ax1= TRUE, yAn = "variation in salinity")
dev.off()


rm (poDay, anAx, i, sdPlot)


pdf ("~/tmp/LCI_noaa/media/2019/Seldo-crosscor_TempSal.pdf")
crossCor <- with (tempDay, ccf (days180, SalD180, lag.max =50
                              , type = "correlation", na.action = na.pass))
dev.off()


### give up on everything below?
## would be fun to model effect of tidal and diurnal cycle, but not worth it

if (0){

## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData") #; require (...)
## seasonal decomposition

## interpolate NAs
Require ("zoo")
## na.locf
# sldvia$Temp <- na.approx (sldvia$Temp,
sldviaTS <- ts (sldvia$Temp, delta = 1/(4*24*365.25), start = min (sldvia$year))
sldviaTS <- na.approx (sldviaTS)

Require ("forecast")
tempTS <- msts (sldviaTS, seasonal.periods = c(24*4,
                                                  12.4206 * 4, # M2 tide
                                                  24*4*365.25) # daily and annual signal
             # , start = min (sldvia$year)
                )

tB <- tbats (tempTS, # seasonal.periods = NULL,
             use.parallel = TRUE, num.cores = 12)
tARMA <- auto.arima (tempTS, parallel = TRUE, num.cores = 12)

pdf ("~/tmp/LCI_noaa/media/2019/Seldovia_tempTS_tbats.pdf")
plot (tB)
plot (tARMA)
dev.off()
save.image ("~/tmp/LCI_noaa/cache/SeldTempx.RData")



## fourier transformation
library (stats)
tempFF <- fft (sldvia$Temp)             # doesn't seem productive -- may need tuning or dealing with NAs? Inf to -Inf


}

## EOF
