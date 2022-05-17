## current year salinity for state of bay report -- modeled on rainy.R
## also plot water temperature, shallow and deep
## merge with annual-stratification.R

rm (list=ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
maO <- 31  # 7 days certainly not working, 14 days not enough either
qntl=c(0.9)
maO <- 31 # moving average window
pMA <- TRUE

if (.Platform$OS.type == "unix"){
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}else{
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}
source ("annualPlotFct.R")
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1



## QCQA -- already done?


##############
## salinity ##
##############
currentCol <- c ("lightblue", "darkblue", "hotpink")

tDayH <- prepDF (dat=homerS, varName="sal", qntl=qntl, maO=maO)
tDayS <- prepDF (dat=sldviaS, varName="sal", qntl=qntl, maO=maO)

## plot
pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-salinity", maO, "-d.pdf"), width=9, height=9)
# png (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-salinity", maO, "-d.png")
#      , width=1800, height=2400, res=300)
par (mfrow=c(2,1)
     , mar=c(3,4,4,2)+0.1
)

aPlot (df=tDayS, vName="sal", currentCol=currentCol, ylim=c(24, 31.8)
       , ylab="salinity")
title (main="Seldovia surface")
box()
cLegend ("bottomleft", inset=0.05, currentYear=currentYear
         , mRange=c(min (homerS$year), currentYear -1)
         , cYcol=currentCol, qntl=qntl [1]
         )
## add homer data
aPlot (tDayH, "sal", MA=pMA, currentCol=currentCol, ylim=c(24, 31.8), ylab="salinity")
title (main="Homer surface")
box()
dev.off()








#############################
## Sea Surface Temperature ##
#############################

currentCol <- c ("lightblue", "navyblue", "aquamarine")  # use RColorBrewer?

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
  fAxis(c (0, 15)) # from annualPlotFct.R
  bx <- legend ("bottom", inset=0.1, bty="n", legend= "")
  cLegend ("topleft", inset=0.01
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