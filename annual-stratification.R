## use Seldovia and Homer TS data to plot stratification
## throughout the year
## calc difference in density between upper and lower TS

if (.Platform$OS.type == "unix"){
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}else{
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}
rm (list=ls())
load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
maO <- 31  # 7 days certainly not working, 14 days not enough either
qntl=c(0.9)
pMA <- TRUE
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c ("lightblue", "blue", "magenta")


source ("annualPlotFct.R")
# density sigma theta kg/m3
sigDens <- function (df, pressure=0, lon=-151.5, lat=59.6){
  ## assuming standard SWMP df, calculate seawater density
  require ("oce")
  if (!all (c ("temp", "sal") %in% names (df))){
    stop ("temp and sal have to be names in df")
  }
  sigThe <- with (df, swSigmaTheta (sal, temp, pressure=0
                                    , longitude=lon, latitude=lat, eos="gsw"
  ))
  return (sigThe)
}


homer$swDens <- sigDens (homer, pressure=swPressure (15, latitude=59)) # what depth?
homerS$swDens <- sigDens (homerS)
sldvia$swDens <- sigDens (sldvia, pressure=swPressure (15, latitude=59))
sldviaS$swDens <- sigDens (sldviaS)

homerS$strat <- homer$swDens [match (homerS$datetimestamp, homer$datetimestamp)] - homerS$swDens
sldviaS$strat <- sldvia$swDens [match (sldviaS$datetimestamp, sldvia$datetimestamp)] - sldviaS$swDens

# homerS$strat <- homer$swDens [match (homerS$datetimestamp, homer$datetimestamp)] / homerS$swDens
# sldviaS$strat <- sldvia$swDens [match (sldviaS$datetimestamp, sldvia$datetimestamp)] / sldviaS$swDens


hM <- prepDF (dat=homerS, varName="strat", maO=maO, currentYear=currentYear, qntl=qntl)
sL <- prepDF (dat=sldviaS, varName="strat", maO=maO, currentYear=currentYear, qntl=qntl)

# ## anomaly
# homerS$stratAnom <- homerS$strat - hM$MA_strat [match (homerS$jday, hM$jday)]
# plot (homerS$stratAnom, type="l")
# sldviaS$stratAnom <- sldviaS$strat - sL$MA_strat [match (sldviaS$jday, sL$jday)]
# plot (sldviaS$stratAnom, type="l")


hY <- 2014:2017

## plot stratification
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-stratificationYear.pdf")
par (mfrow=c(2,1), mar=c(3,4,3,1))
aPlot (sL, "strat", currentCol=currentCol, ylab="water column stability", main="Seldovia"
       , ylim=c(0, 4.3)) # c(0,5))
# for (i in 1:length (hY)){
#   sL <- prepDF (dat=sldviaS, varName="strat", maO=maO, currentYear=hY [i], qntl=qntl)
#   lines (pYMA_strat~jday, sL, col=i)
# }
cLegend ("topleft", inset=0.05
         , mRange=c (min (homerS$year), currentYear -1)
         , currentYear=currentYear
         , cYcol=currentCol # "blue"
         , qntl=qntl [1]
         # , sYears=hY
         # , sLcol=hY - 2013
         # , sLwd=rep (1, length (hY))
         # , sLty=rep (1, length (hY))
)
axis (1, at=366, labels=FALSE)

aPlot (hM, "strat", currentCol=currentCol, ylab="water column stability", main="Homer"
       , ylim=c(0, 4.3)) #c(0,5))
# for (i in 1:length (hY)){
#  hM <- prepDF (dat=homerS, varName="strat", maO=maO, currentYear=hY [i], qntl=qntl)
#  lines (pYMA_strat~jday, hM, col=i)
# }
dev.off()

rm (hM, sL)
## hypothesis:
# warm air is linked to warm water?
# warm air is linked to glacial melting and increased stratification
# warm water is linked to reduced stratification in Seldovia (summer) because deep water becomes warmer (and less saline???)



## also plot fluorescence and turbidity
currentCol <- c ("lightgreen", "green", "brown")
require ("RColorBrewer")
currentCol <- brewer.pal (3, "Greens")
# currentCol <- c("red", "green", "brown")
currentCol [2] <- "#5ca904" # leaf green

waterL <- list (homerS, homer, sldviaS, sldvia)
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-FluorescenceA.pdf")
par (mfrow=c(2,2), mar=c(3,4,3,1))

for (i in 1:length (waterL)){
  hM <- try (prepDF (dat=waterL [[i]], varName="chlfluor", maO=maO
                     , currentYear=currentYear, qntl=qntl))
  if (class (hM) != "try-error"){
  aPlot (hM, "chlfluor", currentCol=currentCol, ylab="Chlorophyll [mg/l]"
         , main=c ("Homer shallow", "Homer deep", "Seldovia shallow"
         , "Seldovia deep")[i]
         # , ylim=c(1, 1.3)
         )
  cLegend ("topleft", inset=0.05
           , mRange=c (min (homerS$year), currentYear -1)
           , currentYear=currentYear
           , cYcol=currentCol # "blue"
           , qntl=qntl [1]
  )
  axis (1, at=366, labels=FALSE)
  }
}
dev.off()
rm (waterL, hM, i)



hM <- prepDF (dat=homerS, varName="chlfluor", maO=maO, currentYear=currentYear, qntl=qntl)
sL <- prepDF (dat=sldviaS, varName="chlfluor", maO=maO, currentYear=currentYear, qntl=qntl)
summary (sL)

pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-Fluorescence.pdf", height=4, width=6)
par (#mfrow=c(2,1),
     mar=c(3,4,3,1))
aPlot (sL, "chlfluor", currentCol=currentCol, ylab="Chlorophyll [mg/l]", main="Seldovia"
       #, ylim=c(1, 1.3)
       , pastYear=FALSE, ongoingYear=FALSE
       ) # c(0,5))
cLegend ("topleft", inset=0.05
         , mRange=c (min (homerS$year), currentYear -1)
         , currentYear=currentYear
         , cYcol=currentCol
         , qntl=qntl [1]
         , pastYear=FALSE, ongoingYear=FALSE
)
# axis (1, at=366, labels=FALSE)

## Homer not quite to be trusted?
# aPlot (hM, "chlfluor", currentCol=currentCol, ylab="Chlorophyll", main="Homer"
#        #, ylim=c(1, 1.3)
#        )
dev.off()





hM <- prepDF (dat=homerS, varName="turb", maO=maO, currentYear=currentYear, qntl=qntl)
sL <- prepDF (dat=sldviaS, varName="turb", maO=maO, currentYear=currentYear, qntl=qntl)

currentCol <- c ("brown", "maroon", "yellow")
require ("RColorBrewer")
currentCol <- brewer.pal (11, "BrBG")[c(4,2,7)]
currentCol <- rev (brewer.pal (3, "Oranges"))

pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-Turbidity.pdf", width=6, height=8)
par (mfrow=c(2,1), mar=c(3,4,3,1))
aPlot (sL, "turb", currentCol=currentCol, ylab="Turbidity"
       , main="Seldovia"
       #, ylim=c(1, 1.3)
       )
cLegend ("topleft", inset=0.05
         , mRange=c (min (homerS$year), currentYear -1)
         , currentYear=currentYear
         , cYcol=currentCol # "blue"
         , qntl=qntl [1]
)
axis (1, at=366, labels=FALSE)
# title (main="Seldovia")

aPlot (hM, "turb", currentCol=currentCol, ylab="Turbidity", main="Homer"
       #, ylim=c(1, 1.3)
       )
# title (main="Homer")
dev.off()


cat ("Finished stratificationSeason\n")
# EOF