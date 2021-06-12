## current year salinity for state of bay report -- modeled on rainy.R
## also plot water temperature, shallow and deep

rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
maO <- 31  # 7 days certainly not working, 14 days not enough either
qntl = c(0.9)
maO <- 31 # moving average window
pMA <- TRUE
currentCol <- c ("darkblue", "blue")
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1

setwd("~/myDocs/amyfiles/NOAA-LCI/")
source ("annualPlotFct.R")



## QCQA -- already done?


# tDayHD <- prepDF ("sal", homerDL, maO = 31, qntl = qntl)
tDayH <- prepDF (dat = homerS, varName = "sal", qntl = qntl, maO = maO)
tDayS <- prepDF (dat = sldviaS, varName = "sal", qntl = qntl, maO = maO)

## plot
pdf (paste0 ("~/tmp/LCI_noaa/media/sa-salinity", maO, "-d.pdf"), width = 9, height = 9)
par (mfrow = c(2,1)
     , mar = c(3,4,4,2)+0.1
)

aPlot (tDayS, "sal", MA = pMA, currentCol = currentCol, ylim = c(24, 31.8), ylab = "salinity")
title (main = "Seldovia surface")
# box()
cLegend ("bottomleft", inset = 0.05, currentYear = currentYear, mRange = c(min (homerS$year), currentYear -1)
         , cYcol = currentCol #  "darkBlue"
         , qntl = qntl [1]
)
## add homer data
aPlot (tDayH, "sal", MA = pMA, currentCol = currentCol, ylim = c(24, 31.8), ylab = "salinity")
# aPlot (tDayH, "sal", MA = pMA, currentCol = "lightblue", ylim = c(24.5, 31.8))
title (main = "Homer surface")
# box()
dev.off()

if (0){
  # par (mfrow = c(1,1))
  plotSetup (tDayS$MA_sal, tDayS$pYMA_sal, ylab = "", ylim = c (25, 31.5))
  lP <- function (...){
    lines (..., lwd = 3)
  }
  lP (pYMA_sal~jday, tDayS, col = "darkblue")
  lP (MA_sal~jday, tDayS, col = "darkblue", lty = "dashed")
  # lP (maL1_sal~jday, tDayS, col = "darkblue", lty= "dotted")
  # lP (maU1_sal~jday, tDayS, col = "darkblue", lty= "dotted")

  lP (pYMA_sal~jday, tDayH, col = "lightblue")
  lP (MA_sal~jday, tDayH, col = "lightblue", lty = "dashed")
  # lP (maL1_sal~jday, tDayH, col = "lightblue", lty= "dotted")
  # lP (maU1_sal~jday, tDayH, col = "lightblue", lty= "dotted")

  # lines (sal~jday, tDayH, col = "lightblue", lwd = 2, lty = "dashed")
  # lines (pYMA_sal~jday, tDayH, col = "lightblue", lwd = 2, lty = "solid")

  legend ("bottomleft", bty= "n"
          , legend = c(paste0 ("Seldovia-", currentYear)
                       , "Seldovia-mean"
                       , paste0 ("Homer-", currentYear)
                       , "Homer-mean"
          )
          , lwd = 3, ncol = 2
          , col = c(rep ("darkblue", 2), rep ("lightblue", 2))
          , lty = rep (c ("solid", "dashed"), 2)
  )
  # box()
  # dev.off()
}



## for completeness -- SST temperature
# currentCol <- c ("navyblue", "aquamarine")
currentCol <- c ("red", "pink", "orange")

# tScale <- c ("celsius", "fahrenheit")
instSite <- c ("sldviaS", "sldvia", "homerS", "homer")
# for (i in 1:length (tScale)){
  for (j in 1: length (instSite)){
    tDay <- prepDF (dat = list (sldviaS, sldvia, homerS, homer)[[j]], varName = "temp" # c ("temp", "tempF")[i]
                    , qntl = qntl, maO = maO)
    pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-", c ("Temp-SST-Seldovia", "Temp-Deep-Seldovia", "Temp-SST-Homer",
                                                              "Temp-Deep-Homer")[j]
                 # , c ("-C", "-F")[i]
                 , ".pdf"), width = 9, height = 6)
    par (mar = c(3,4,2,4))
    # aPlot (tDay, c("temp", "tempF")[i], currentCol = currentCol
           # , ylab = c (expression('Temperature '~'['*degree~'C'*']')
           #             , expression('Temperature'~'['*degree~'F'*']'))[i]
    aPlot (tDay, "temp", currentCol = currentCol
           , ylab = expression('Temperature'~'['*degree~'C'*']')
           )
    title (main = c("Seldovia SST", "Water temperature at Seldovia Harbor, near the bottom", "Homer SST", "Homer bottom temperature")[j])

    fAxis(c (0, 15), mT = expression('Temperature '~'['*degree~'F'*']')) # could do better XXX
    cLegend ("topleft", inset = 0.05
             , currentYear = currentYear
             , mRange = c (min (list (sldviaS, sldvia, homerS, homer)[[j]]$year), currentYear -1)
             , cYcol = currentCol
             , title = paste (maO, "day moving average")
             , qntl = qntl
    )

    box()
    dev.off()
  }
# }
# rm (tScale, instSite, tDay)
rm (instSite, tDay)




if (0){
  tDay <- prepDF (dat = homerS, varName = "temp", qntl = qntl, maO = maO)
  pdf (paste0( ("~/tmp/LCI_noaa/media/sa-SSTHomer.pdf")), width = 9, height = 6)
  par (mar = c(3,4,2,1))
  aPlot (tDay, "temp", currentCol = currentCol, ylab = expression('SST'~'['*degree~'C'*']')
         , main = "Homer SST")
  cLegend ("topleft", inset = 0.05, currentYear = currentYear, mRange = c(min (homerS$year), currentYear-1)
           , cYcol = currentCol, qntl = qntl [1])
  dev.off()

  tDay <- prepDF (dat = sldviaS, varName = "temp", qntl = qntl, maO = maO)
  pdf (paste0( ("~/tmp/LCI_noaa/media/sa-SSTSeld.pdf")), width = 9, height = 6)
  par (mar = c(3,4,2,1))
  aPlot (tDay, "temp", currentCol = currentCol, ylab = expression('SST'~'['*degree~'C'*']')
         , main = "Seldovia SST")
  cLegend ("topleft", inset = 0.05, currentYear = currentYear, mRange = c(min (sldviaS$year), currentYear-1)
           , cYcol = currentCol, qntl = qntl [1])
  dev.off()

  tDay <- prepDF (dat = sldvia, varName = "temp", qntl = qntl, maO = maO)
  pdf (paste0( ("~/tmp/LCI_noaa/media/sa-DeepTempSeld.pdf")), width = 9, height = 6)
  par (mar = c(3,4,2,1))
  aPlot (tDay, "temp", currentCol = currentCol, ylab = expression('SST'~'['*degree~'C'*']')
         , main = "Seldovia bottom temperature")
  cLegend ("topleft", inset = 0.05, currentYear = currentYear, mRange = c(min (sldvia$year), currentYear-1)
           , cYcol = currentCol, qntl = qntl [1])
  dev.off()
}

cat ("Finished salinityAnnual.R\n")
# EOF
