## use Seldovia and Homer TS data to plot stratification
## throughout the year
## calc difference in density between upper and lower TS

if (.Platform$OS.type == "unix"){
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}else{
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}
if (!exists ("quarterly")){
  rm (list=ls())
  quarterly <- TRUE
}
base::load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R




maO <- 31  # 7 days certainly not working, 14 days not enough either
qntl=c(0.9)
pMA <- TRUE
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
#################
# currentYear <- 2021   ## temp!! XXX
#################
currentCol <- c ("lightblue", "blue", "magenta")
require ("RColorBrewer")
currentCol <- c ("black", brewer.pal (4, "Paired"))[c(1,3,2)]
mediaD <- "~/tmp/LCI_noaa/media/StateOfTheBay/"

if (quarterly){
  pastYear <- FALSE  # plot currentYear-1 ?
  ongoingY <- TRUE
  mediaD <- paste0 (mediaD, "update/")
  currentCol <- currentCol [c(3,1,2)]
}else{
  pastYear <- TRUE  ## winter/spring publication schedule
  ongoingY <- FALSE
}

source ("annualPlotFct.R")
dir.create(mediaD, showWarnings=FALSE, recursive=TRUE)


####################
## Stratification ##
####################

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



hY <- 2014:2017

## plot stratification
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-stratificationYear.pdf")
par (mfrow=c(2,1), mar=c(3,4,3,1))
aPlot (sL, "strat", currentCol=currentCol, ylab="water column stability", main="Seldovia"
       , ylim=c(0, 4.3) # c(0,5)
       , pastYear=pastYear, ongoingYear=ongoingY
)
# for (i in 1:length (hY)){
#   sL <- prepDF (dat=sldviaS, varName="strat", maO=maO, currentYear=hY [i], qntl=qntl)
#   lines (pYMA_strat~jday, sL, col=i)
# }
cLegend ("topleft", inset=0.05
         , mRange=c (min (homerS$year), currentYear -1)
         , currentYear=currentYear
         , cYcol=currentCol # "blue"
         , qntl=qntl [1]
         , pastYear=pastYear, ongoingYear=ongoingY
         # , sYears=hY
         # , sLcol=hY - 2013
         # , sLwd=rep (1, length (hY))
         # , sLty=rep (1, length (hY))
)
axis (1, at=366, labels=FALSE)

aPlot (hM, "strat", currentCol=currentCol, ylab="water column stability", main="Homer"
       , ylim=c(0, 4.3) #c(0,5))
       , pastYear=pastYear, ongoingYear=ongoingY
)
# for (i in 1:length (hY)){
#  hM <- prepDF (dat=homerS, varName="strat", maO=maO, currentYear=hY [i], qntl=qntl)
#  lines (pYMA_strat~jday, hM, col=i)
# }
dev.off()

rm (hM, sL, hY)
## hypothesis:
# warm air is linked to warm water?
# warm air is linked to glacial melting and increased stratification
# warm water is linked to reduced stratification in Seldovia (summer) because deep water becomes warmer (and less saline???)



##################
## fluorescence ##
##################

currentCol <- c ("lightgreen", "green", "brown")
require ("RColorBrewer")
currentCol <-rev(brewer.pal (3, "Greens"))

waterL <- list (homerS, homer, sldviaS, sldvia)
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/FluorescenceA.pdf")
par (mfrow=c(2,2), mar=c(3,4,3,1))

for (i in 1:length (waterL)){
  hM <- try (prepDF (dat=waterL [[i]], varName="chlfluor", maO=maO
                     , currentYear=currentYear, qntl=qntl))
  if (class (hM) != "try-error"){
    aPlot (hM, "chlfluor", currentCol=currentCol, ylab="Chlorophyll [mg/l]"
           , main=c ("Homer shallow", "Homer deep", "Seldovia shallow"
                     , pastYear=pastYear, ongoingYear=ongoingY
                     , "Seldovia deep")[i]
           # , ylim=c(1, 1.3)
    )
    cLegend ("topleft", inset=0.05
             , mRange=c (min (homerS$year), currentYear -1)
             , currentYear=currentYear
             , pastYear=pastYear, ongoingYear=ongoingY
             , cYcol=currentCol # "blue"
             , qntl=qntl [1]
    )
    axis (1, at=366, labels=FALSE)
  }
}
dev.off()
rm (waterL, hM, i)

## for troubleshooting annualPlotFct.R::prepDF
# save.image("~/tmp/LCI_noaa/cache/annualXtmp.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/annualXtmp.RData"); source("annualPlotFct.R"); dat=homerS; varName="chlfluor"; sumFct=function (x){mean (x, na.rm=FALSE)}


hM <- prepDF (dat=homerS, varName="chlfluor", maO=maO, currentYear=currentYear, qntl=qntl)
sL <- prepDF (dat=sldviaS, varName="chlfluor", maO=maO, currentYear=currentYear, qntl=qntl)
# summary (sL)

# pdf (paste0 (mediaD, "sa-Fluorescence.pdf"), height=4, width=6)
png (paste0 (mediaD, "sa-Fluorescence.png"), height=4*300, width=6*300, res=300)
par (mar=c(3,4,3,1))
aPlot (sL, "chlfluor", currentCol=currentCol, ylab="Chlorophyll [mg/l]", main="Seldovia"
       #, ylim=c(1, 1.3)
       , pastYear=pastYear, ongoingYear=ongoingY
       # , ylim=c(0,10)  # to avoid clash of legend and plot
       , ylim=c(0, max (sL$maU1_chlfluor))
       )
cLegend ("topleft", inset=0.05
         , mRange=c (min (homerS$year), currentYear -1)
         , currentYear=currentYear
         , cYcol=currentCol
         , qntl=qntl [1]
         , pastYear=pastYear, ongoingYear=ongoingY
)
## Homer not to be trusted? Or just not enough data!
# aPlot (hM, "chlfluor", currentCol=currentCol, ylab="Chlorophyll", main="Homer"
#        #, ylim=c(1, 1.3)
#        )

## plot all years to show variability
# plot (chlfluor~jday, data=sL, type="n"
#       , ylim=range (sL [,20:ncol (sL)], na.rm=TRUE)
#       )
# for (i in 20:ncol (sL)){
#   lines (sL[,i]~sL$jday, col = i)
# }
dev.off()




###############
## Turbidity ##
###############

hM <- prepDF (dat=homerS, varName="turb", maO=maO, currentYear=currentYear, qntl=qntl)
sL <- prepDF (dat=sldviaS, varName="turb", maO=maO, currentYear=currentYear, qntl=qntl)

require ("RColorBrewer")
currentCol <- rev (brewer.pal (3, "Oranges"))

pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-Turbidity.pdf", width=6, height=8)
par (mfrow=c(2,1), mar=c(3,4,3,1))
aPlot (sL, "turb", currentCol=currentCol, ylab="Turbidity"
       , main="Seldovia"
       #, ylim=c(1, 1.3)
       , pastYear=pastYear, ongoingYear=ongoingY
       )
cLegend ("topleft", inset=0.05
         , mRange=c (min (homerS$year), currentYear -1)
         , currentYear=currentYear
         , cYcol=currentCol
         , qntl=qntl [1]
         , pastYear=pastYear, ongoingYear=ongoingY
)
axis (1, at=366, labels=FALSE)

aPlot (hM, "turb", currentCol=currentCol, ylab="Turbidity", main="Homer"
       #, ylim=c(1, 1.3)
       , pastYear=pastYear, ongoingYear=ongoingY
)
dev.off()




##############
## salinity ##
##############
currentCol <- c ("lightblue", "darkblue", "hotpink")
PSUrange <- c(24, 32.5)


tDayH <- prepDF (dat=homerS, varName="sal", qntl=qntl, maO=maO, currentYear=currentYear)
tDayS <- prepDF (dat=sldviaS, varName="sal", qntl=qntl, maO=maO, currentYear=currentYear)

## plot
# pdf (paste0 (mediaD, "sa-salinity", maO, "-d.pdf"), width=9, height=9)
png (paste0 (mediaD, "sa-salinity", maO, "-d.png"), width=9*300, height=9*300, res=300)
par (mfrow=c(2,1)
     , mar=c(3,4,4,2)+0.1
)

aPlot (df=tDayS, vName="sal", currentCol=currentCol, ylim=PSUrange
       , ylab="salinity"
       , pastYear=pastYear, ongoingYear=ongoingY
)
title (main="Seldovia surface")
box()
cLegend ("bottomleft", inset=0.05, currentYear=currentYear
         , mRange=c(min (homerS$year), currentYear -1)
         , cYcol=currentCol, qntl=qntl [1]
         , pastYear=pastYear, ongoingYear=ongoingY
         )
## add homer data
aPlot (tDayH, "sal", MA=pMA, currentCol=currentCol, ylim=PSUrange, ylab="salinity"
       , pastYear=pastYear, ongoingYear=ongoingY
)
title (main="Homer surface")
box()
dev.off()
rm (PSUrange)







#############################
## Sea Surface Temperature ##
#############################

currentCol <- c ("lightblue", "navyblue", "aquamarine")  # use RColorBrewer?
if (quarterly){
  currentCol <- currentCol [c(3,1,2)]
}
instSite <- c ("sldviaS", "sldvia", "homerS", "homer")
#
save.image ("~/tmp/LCI_noaa/cache/annualWater.RData")
# load ("~/tmp/LCI_noaa/cache/annualWater.RData"); j <- 3; source ("annualPlotFct.R")
# dat=list (sldviaS, sldvia, homerS, homer)[[j]] ; varName="temp"; sumFct=function (x){mean (x, na.rm=FALSE)}

for (j in 1: length (instSite)){
  tDay <- prepDF (dat=list (sldviaS, sldvia, homerS, homer)[[j]], varName="temp" # c ("temp", "tempF")[i]
                  , qntl=qntl, maO=maO
                  , currentYear=currentYear)
  # pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-", c ("Temp-SST-Seldovia", "Temp-Deep-Seldovia", "Temp-SST-Homer",
  #                                                           "Temp-Deep-Homer")[j]
  #              , ".pdf"), width=9, height=6)
  png (paste0 (mediaD, "sa-", c ("Temp-SST-Seldovia", "Temp-Deep-Seldovia", "Temp-SST-Homer",
                                                            "Temp-Deep-Homer")[j]
               , ".png"), width=1800, height=1200, res=300)
  par (mar=c(3,4,2,4))
  aPlot (tDay, "temp", currentCol=currentCol
         , ylab="Temperature [°C]" #expression('Temperature'~'['*degree~'C'*']')
         , pastYear=pastYear, ongoingYear=ongoingY
  )
  title (main=c("Seldovia surface water temperature", "Seldovia Harbor bottom water temperature", "Homer surface water temperature", "Homer bottom water temperature")[j])
  fAxis(c (0, 15)) # from annualPlotFct.R
  bx <- legend ("bottom", inset=0.1, bty="n", legend= "")
  cLegend ("topleft", inset=0.01
                    , currentYear=currentYear
           , mRange=c (min (list (sldviaS, sldvia, homerS, homer)[[j]]$year), currentYear -1)
           , cYcol=currentCol
           , title=paste (maO, "day moving average")
           , qntl=qntl
           , pastYear=pastYear, ongoingYear=ongoingY
  )
  box()
  dev.off()
}
rm (instSite, tDay)





#################################################################################
## plot raw Seldovia surface temperature from Aug to May (to match Aquaculture) #
#################################################################################

# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/annualWater.RData"); j <- 3; source ("annualPlotFct.R")
ssT <- subset (sldviaS, datetimestamp >=
                 # as.POSIXct (paste0 (currentYear-1, "-12-31 23:59"))
                 as.POSIXct("2020-08-01 00:00:00")
               )
ssT$aqY <- ifelse (ssT$datetimestamp < as.POSIXct("2021-05-15 23:59:59")
                   , "2020"
                   , ifelse (ssT$datetimestamp >= as.POSIXct ("2021-08-01 00:00:00") &
                               ssT$datetimestamp < as.POSIXct("2022-05-15 23:59:59")
                             , "2021"
                             , ifelse (ssT$datetimestamp >= as.POSIXct ("2022-08-01 00:00:00") &
                                         ssT$datetimestamp < as.POSIXct("2023-05-15 23:59:59")
                                       , "2022", NA)))
## adjust by21 to match by20
ssT$dateY2 <- as.POSIXct (ssT$datetimestamp - 365.25*24*3600)
ssT$dateY3 <- as.POSIXct (ssT$datetimestamp - 2*365.25*24*3600)


png ("~/tmp/LCI_noaa/media/StateOfTheBay/tutkaTempCompare.png"
     , height=2.5*300, width=4*300, res=150)
require ("RColorBrewer")
lCol <- brewer.pal(8, "Set2")[c(3,2,1)]

## by20: col 2
plot (temp~datetimestamp, ssT, subset=aqY=="2020", type="l"
      , ylab = "temperature [°C]", xlab="", col = lCol [3], lwd=2
      , xaxs="i", yaxs="r", axes=FALSE
      #, xlim=as.POSIXct(c ("2021-08-01", "2022-05-15"))
      , main= "Seldovia Surface Water")
axis (2)
axis (1, at=as.POSIXct(c (paste0 ("2020-", c("08", "09", "10", "11", "12"), "-01")
                                             , paste0 ("2021-0", 1:5, "-01")))
      , labels=month.abb[c(8:12, 1:5)])
box()

lines (temp~dateY2, ssT, subset=aqY=="2021", col=lCol [2], lwd=2)
lines (temp~dateY3, ssT, subset=aqY=="2022", col=lCol [1], lwd=2)

legend ("topright", bty="n", col=lCol, lwd=rep (3,3)
        , legend=c("2022-23", "2021-22", "2020-21"))
dev.off()
rm (ssT, lCol)


cat ("Finished annual-waterTempSal.R\n")
# EOF
