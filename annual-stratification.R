## use Seldovia and Homer TS data to plot stratification
## throughout the year
## calc difference in density between upper and lower TS

setwd ("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
maO <- 31  # 7 days certainly not working, 14 days not enough either
qntl = c(0.9)
pMA <- TRUE
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c ("blue", "lightblue")


source ("annualPlotFct.R")
# density sigma theta kg/m3
sigDens <- function (df, pressure = 0, lon = -151.5, lat = 59.6){
  ## assuming standard SWMP df, calculate seawater density
  require ("oce")
  if (!all (c ("temp", "sal") %in% names (df))){
    stop ("temp and sal have to be names in df")
  }
  sigThe <- with (df, swSigmaTheta (sal, temp, pressure = 0
                                    , longitude = lon, latitude = lat, eos = "gsw"
  ))
  return (sigThe)
}


homer$swDens <- sigDens (homer, pressure = swPressure (15, latitude = 59)) # what depth?
homerS$swDens <- sigDens (homerS)
sldvia$swDens <- sigDens (sldvia, pressure = swPressure (15, latitude = 59))
sldviaS$swDens <- sigDens (sldviaS)

homerS$strat <- homer$swDens [match (homerS$datetimestamp, homer$datetimestamp)] - homerS$swDens
sldviaS$strat <- sldvia$swDens [match (sldviaS$datetimestamp, sldvia$datetimestamp)] - sldviaS$swDens

homerS$strat <- homer$swDens [match (homerS$datetimestamp, homer$datetimestamp)] / homerS$swDens
sldviaS$strat <- sldvia$swDens [match (sldviaS$datetimestamp, sldvia$datetimestamp)] / sldviaS$swDens


hM <- prepDF (dat = homerS, varName = "strat", maO = maO, currentYear = currentYear, qntl = qntl)
sL <- prepDF (dat = sldviaS, varName = "strat", maO = maO, currentYear = currentYear, qntl = qntl)

# ## anomaly
# homerS$stratAnom <- homerS$strat - hM$MA_strat [match (homerS$jday, hM$jday)]
# plot (homerS$stratAnom, type = "l")
# sldviaS$stratAnom <- sldviaS$strat - sL$MA_strat [match (sldviaS$jday, sL$jday)]
# plot (sldviaS$stratAnom, type = "l")


hY <- 2014:2017

## plot stratification
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-stratificationYear.pdf")
par (mfrow = c(2,1), mar = c(3,4,3,1))
aPlot (sL, "strat", currentCol = currentCol, ylab = "water column stability", main = "Seldovia"
       , ylim = c(1, 1.3)) # c(0,5))
# for (i in 1:length (hY)){
#   sL <- prepDF (dat = sldviaS, varName = "strat", maO = maO, currentYear = hY [i], qntl = qntl)
#   lines (pYMA_strat~jday, sL, col = i)
# }
cLegend ("topleft", inset = 0.05
         , mRange = c (min (homerS$year), currentYear -1)
         , currentYear = currentYear
         , cYcol = currentCol # "blue"
         , qntl = qntl [1]
         # , sYears = hY
         # , sLcol = hY - 2013
         # , sLwd = rep (1, length (hY))
         # , sLty = rep (1, length (hY))
)
axis (1, at = 366, labels = FALSE)

aPlot (hM, "strat", currentCol = "blue", ylab = "water column stability", main = "Homer"
       , ylim = c(1, 1.3)) #c(0,5))
# for (i in 1:length (hY)){
#  hM <- prepDF (dat = homerS, varName = "strat", maO = maO, currentYear = hY [i], qntl = qntl)
#  lines (pYMA_strat~jday, hM, col = i)
# }
dev.off()

rm (hM, sL)
## hypothesis:
# warm air is linked to warm water?
# warm air is linked to glacial melting and increased stratification
# warm water is linked to reduced stratification in Seldovia (summer) because deep water becomes warmer (and less saline???)




cat ("Finished stratificationSeason\n")
# EOF