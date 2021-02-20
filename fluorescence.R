#!/bin/env Rscript

## analyze fluorescence, climatology, anomaly, length of bloom
## compare Seldovia and T9S6 station


rm (list = ls())

load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")   ## from SeldoviaTemp.R
# variable of interest: sldviaS, sldvia, homer, homerS
load ("~/tmp/LCI_noaa/cache/ctdT9S6_fw.RData")  ## from ctd_T9-anomaly.R
# variable of interest: xC, physOs



## QAQC -- move elsewhere -- drift of fluorescence callibration?
physOc$File.Name <- as.character (physOc$File.Name)
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
physOc$instrument <- factor (substrRight (physOc$File.Name, 4))
rm (substrRight)
# summary (physOc$instrument)

cDate <- list.files("~/GISdata/LCI/CTD-processing/Workspace/conFiles/"
                    , "con", ignore.case = TRUE)
cD <- list (c4141 = c("2012-04-01", "2016-06-01", "2018-10-01")
            , c5028 = c("2012-10-01", "2014-09-01"))
pdf ("~/tmp/LCI_noaa/media/fluorescence-callibration-2x.pdf"
     , width = 6, height = 12)
par (mfrow = c(2,1))
for (i in 1:length (levels (physOc$instrument))){
  fD <- aggregate (Fluorescence_mg_m3~isoTime, physOc, FUN = min
                   , subset = instrument == levels (physOc$instrument)[i])
  fD <- subset (fD, Fluorescence_mg_m3 > 0) # better to set NAs?
  plot (Fluorescence_mg_m3~isoTime, fD, type = "l", lwd = 1.5
        , xlab = "", ylab = "min Fluorescence reading per cast"
        , main = levels (physOc$instrument)[i]
  )
  abline (v = as.POSIXct (cD [[i]]), lty = "dashed", col = "gray")
  abline (h = 0)
  print (summary (lm (Fluorescence_mg_m3~isoTime, fD)))
}
dev.off()

## => dismiss the drifting calibration hypothesis
## => set readings < 0 to NA
# is.na (sldviaS$chlfluor [which (sldviaS$chlfluor <= 0)]) <- TRUE  # already done in SeldoviaTemp.R
# is.na (sldvia$chlfluor [which (sldvia$chlfluor <= 0)]) <- TRUE
is.na (xC$Fluorescence_mg_m3 [which (xC$Fluorescence_mg_m3 <= 0)]) <- TRUE
# hist (log (sldviaS$chlfluor))



pdf ("~/tmp/LCI_noaa/media/swamp-seldovia_fluorescence.pdf", height = 6, width = 18)
plot (chlfluor ~ datetimestamp, sldviaS, type = "l", subset = year > 2010)
plot (chlfluor ~ datetimestamp, sldvia, type = "l", subset = year > 2010)
dev.off()

pdf ("~/tmp/LCI_noaa/media/swamp-seldovia_fluorescence-agg.pdf")
plot (aggregate (chlfluor~year, sldvia, mean), type = "l", col = "green", lwd = 2
      , xlim = c(min (c (sldvia$year, sldviaS$year), na.rm = TRUE)
               , max (c (sldvia$year, sldviaS$year), na.rm = TRUE)
      )
      , ylab = "annual mean chl-fluorescence")
lines (aggregate (chlfluor~year, sldviaS, mean), col = "blue", lwd = 2)
legend ("topleft", lwd = 2, col = c("green", "blue")
        , legend = c ("Seldovia surface", "Seldovia deep")
)
dev.off()
# one year, 2015, standing out, having WAY higher chl than any other year
# BUT, only for sldviaS, not sldvia (for which 2016 is highest)


### Seldovia anomaly

sltFit <- function (varN, tempDay = tempDay){
  require (stlplus)                       # use stlplus because it handles NAs gracefully
  stlplus (ts (tempDay [,which (names (tempDay) == varN)], start = c (min (tempDay$year),1)
               , frequency = 365.25)
           , s.window = "periodic")
}
fTS <- sltFit ("chlfluor", sldviaS)





fBreaks <- quantile (log (sldviaS$chlfluor), c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
f2Breaks <- quantile (log (sldvia$chlfluor), c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
fTBreaks <- quantile (log (physOc$Fluorescence_mg_m3), c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
## yes, they do measure different things, even if related

## how long is the bloom?
pdf ("~/tmp/LCI_noaa/media/T9s6-fluores-Hist.pdf")
hist (log (subset (physOc$Fluorescence_mg_m3, physOc$Fluorescence_mg_m3 > 0)))
# abline (v = fBreaks [3], col = "blue", lwd = 2)
# abline (v = f2Breaks [3], col = "green")
# abline (v = fTBreaks [3], col = "orange")
hist (physOc$Fluorescence_mg_m3)
dev.off()

## fluorescence time series -- all years
xC$lFluorescence <- log (xC$Fluorescence_mg_m3)


pdf ("~/tmp/LCI_noaa/media/fluorescence-t9s6-TS.pdf", width = 18, height = 6)
plot.station (mkSection (xC), which = "lFluorescence", zcol = oceColorsChlorophyll (4))
anAx(dAx = seq (0, 100, by = 20))
axis (3, at = xC$isoTime, labels = FALSE)
plot.station (mkSection (xC), which = "fluorescence", zcol = oceColorsChlorophyll (11))
anAx(dAx = seq (0, 100, by = 20))
axis (3, at = xC$isoTime, labels = FALSE)
dev.off()


## fluorescence time series -- by year
pdf ("~/tmp/LCI_noaa/media/t9s6-log-fluorescence-TS_byYear.pdf"
     #, width = 6, height = 4*length (levels (xC$year))
)
# par (mfrow = c(1,length (levels (xC$year))))
for (i in 1:length (levels (xC$year))){
  yX <- subset (xC, year == levels (xC$year)[i])
  if (length (levels (factor (yX$File.Name))) > 2){
## layout    
    layout (matrix (c (1,2,3), ncol = 1), heights = c (5,2,2))
    plot.station (mkSection (xC), which = "lFluorescence", zcol = oceColorsChlorophyll (4)
                  # , xrange = as.POSIXct(paste (levels (xC$year)[i], c(1, 12), c (1,31), sep = "-"))
                  , axes = FALSE
                  , xlab = levels (xC$year)[i]
                 , zbreaks = fTBreaks
                 , xlim = as.POSIXct (paste0 (levels (xC$year)[i], c ("-01-01", "-12-31")))
                  )
    axis (1, at = as.POSIXct (paste0 (levels (xC$year)[i], "-", 1:12, "-1")), label = FALSE)
    axis (1, at = as.POSIXct (paste0 (levels (xC$year)[i], "-", 1:12, "-15")), label = month.abb, tick = FALSE)
    axis (2, at = seq (0, 100, by = 20))
    axis (3, at = yX$isoTime, labels = FALSE)
  ## add Seldovia 
    try (plot (log (chlfluor)~datetimestamp, sldviaS, lwd = 2, col = "lightgreen", type = "l"
               , subset = year == levels (xC$year)[i]
               #, ylim = range (sldviaS$chlfluor, na.rm = TRUE)
               , ylab = "shallow chl"))
    try (plot (log (chlfluor)~datetimestamp, sldvia, lwd = 2, col = "darkgreen", type = "l"
               , subset = year == levels (xC$year)[i]
               #, ylim = range (sldvia$chlfluor, na.rm = TRUE)
               , ylab = "deep chl"
    ))
  }
}
dev.off()


summary (xC$year)
aggregate (File.Name ~ year, xC, function (x){length (levels (factor (x)))})

