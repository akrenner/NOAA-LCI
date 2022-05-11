## current year salinity for state of bay report -- modeled on rainy.R
## also plot water temperature, shallow and deep

rm (list=ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
maO <- 31  # 7 days certainly not working, 14 days not enough either
qntl=c(0.9)
maO <- 31 # moving average window
pMA <- TRUE
currentCol <- c ("darkblue", "blue")
currentCol <- c ("lightblue", "darkblue", "hotpink")  ## temporary -- change the order to past, current, ongoing
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1

setwd("~/myDocs/amyfiles/NOAA-LCI/")
source ("annualPlotFct.R")



## QCQA -- already done?


# tDayHD <- prepDF ("sal", homerDL, maO=31, qntl=qntl)
tDayH <- prepDF (dat=homerS, varName="sal", qntl=qntl, maO=maO) # still has issue with cYMA_sal and cY_sal
tDayS <- prepDF (dat=sldviaS, varName="sal", qntl=qntl, maO=maO)

## plot
# pdf (paste0 ("~/tmp/LCI_noaa/media/sa-salinity", maO, "-d.pdf"), width=9, height=9)
png (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-salinity", maO, "-d.png")
     , width=1800, height=2400, res=300)
par (mfrow=c(2,1)
     , mar=c(3,4,4,2)+0.1
)

aPlot (df=tDayS, vName="sal", MA=pMA, currentCol=currentCol, ylim=c(24, 31.8)
       , ylab="salinity")
title (main="Seldovia surface")
# box()
cLegend ("bottomleft", inset=0.05, currentYear=currentYear
         , mRange=c(min (homerS$year), currentYear -1)
         , cYcol=currentCol, qntl=qntl [1]
         # , sYears=c(currentYear-1, currentYear)
         )
## add homer data
aPlot (tDayH, "sal", MA=pMA, currentCol=currentCol, ylim=c(24, 31.8), ylab="salinity")
# aPlot (tDayH, "sal", MA=pMA, currentCol="lightblue", ylim=c(24.5, 31.8))
title (main="Homer surface")
# box()
dev.off()




if (0){   ## reverse of salinity=freshness   (shows freshwater better than 1/sldviaS$sal)
  homerS$fresh <- 32 - homerS$sal
  sldviaS$fresh <- 32 - sldviaS$sal

  tDayH <- prepDF (dat=homerS, varName="fresh", qntl=qntl, maO=maO)
  tDayS <- prepDF (dat=sldviaS, varName="fresh", qntl=qntl, maO=maO)
  # print (summary (tDayS)); stop ("no more")

  png (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-waterFreshness", maO, "-d.png")
       , width=1800, height=2400, res=300) # taller? rainy: height=1200
  par (mfrow=c(2,1)
       # , mar=c(3,4,4,2)+0.1
       , mar=c (3,4,2,4)+0.1 # values from rainy
  )



  aPlot (tDayS, "fresh", MA=pMA, currentCol=currentCol , ylim=c(0,7)
         , ylab="freshness")
  title (main="Seldovia surface")
  # box()
  cLegend ("topleft", inset=0.05, currentYear=currentYear
           , mRange=c(min (homerS$year), currentYear -1)
           , cYcol=currentCol, qntl=qntl [1])
  ## add homer data
  aPlot (tDayH, "fresh", MA=pMA, currentCol=currentCol, ylim=c(0,7)
         , ylab="freshness")
  # aPlot (tDayH, "fresh", MA=pMA, currentCol="lightblue", ylim=c(24.5, 31.8))
  title (main="Homer surface")
  # box()
  dev.off()
}





## for completeness -- SST temperature
currentCol <- c ("navyblue", "aquamarine", "lightblue")
# currentCol <- c ("red", "pink", "orange")

instSite <- c ("sldviaS", "sldvia", "homerS", "homer")
for (j in 1: length (instSite)){
  tDay <- prepDF (dat=list (sldviaS, sldvia, homerS, homer)[[j]], varName="temp" # c ("temp", "tempF")[i]
                  , qntl=qntl, maO=maO)
  # pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-", c ("Temp-SST-Seldovia", "Temp-Deep-Seldovia", "Temp-SST-Homer",
  #                                                           "Temp-Deep-Homer")[j]
  #              , ".pdf"), width=9, height=6)
  png (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-", c ("Temp-SST-Seldovia", "Temp-Deep-Seldovia", "Temp-SST-Homer",
                                                            "Temp-Deep-Homer")[j]
               , ".png"), width=1800, height=1200, res=300)
  par (mar=c(3,4,2,4))
  aPlot (tDay, "temp", currentCol=currentCol
         , ylab=expression('Temperature'~'['*degree~'C'*']')
  )
  title (main=c("Seldovia SST", "Seldovia Harbor bottom water temperature", "Homer SST", "Homer bottom water temperature")[j])

  fAxis(c (0, 15), mT=expression('Temperature '~'['*degree~'F'*']')) # could do better XXX
  bx <- legend ("bottom", inset=0.1, bty="n", legend= "")
  cLegend ("topleft", inset=0.01
  #         cLegend ("topleft", inset=0.01
                    , currentYear=currentYear
           , mRange=c (min (list (sldviaS, sldvia, homerS, homer)[[j]]$year), currentYear -1)
           , cYcol=currentCol
           , title=paste (maO, "day moving average")
           , qntl=qntl
  )

  box()
  dev.off()
}
rm (instSite, tDay)


cat ("Finished salinityAnnual.R\n")
# EOF