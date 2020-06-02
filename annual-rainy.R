
## precipitation -- standardized plot
rm (list = ls())
setwd("~/myDocs/amyfiles/NOAA-LCI/")


maO <- 31  # 7 days certainly not working, 14 days not enough either
# maO <- 1
qntl = c(0.9) #, 0.8) 
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c("blue", "lightblue")
# currentCol <- "blue"
SWMP <- FALSE
SWMP <- TRUE


source ("annualPlotFct.R") # important to call after defining currentCol! 
if (SWMP){                                   # use SWMP data or NOAA homer airport
  load ("~/tmp/LCI_noaa/cache/metDat.RData") # from windTrend.R -- SWMP
}else{
#  source ("noaaWeather.R")
  load ("~/tmp/LCI_noaa/cache/HomerAirport.RData") # from noaaWeather.R -- Airport
}



# plot (subset (hmr$totprcp, hmr$year < 2005), type = "l")
# plot (totprcp~datetimestamp, data = hmr, subset = year < 2006, type = "l")

## QCQA moved to windTrend.R


## aggregate data
tDay <- prepDF (dat = hmr, varName = "totprcp", maO = maO, qntl = qntl)







## adjust units
if (SWMP){
  aF <- 4 * 24  ## daily sums
}else{
  aF <- 1 ## NOAA comes in as daily sums
}
tDay [,which (names (tDay) == "totprcp"):ncol (tDay)] <- 
  apply (tDay [,which (names (tDay) == "totprcp"):ncol (tDay)], 2, function (x){x*aF})


## annual total percentiles (for text) -- need to work off hmr to get percentiles of annual sum
cOffY <- ifelse (SWMP, 2005, 1970)
# yA <- aggregate (totprcp~year+jday, subset (hmr, year > coffY), mean, na.rm = TRUE) ## should exclude >= currentYear
# yA2 <- aggregate (totprcp~year
#                   , subset (yA, year < currentYear) #as.numeric (format (Sys.Date(), "%Y"))) # excludeing >= currentYear
#                   , mean, na.rm = TRUE)
yA2 <- aggregate (totprcp~year, subset (hmr, (year > cOffY)&(year < currentYear))
                  , FUN = sum, na.rm = TRUE)
ARq <- quantile(yA2$totprcp, 0.5 + c(-1,1) * qntl [1] /2 , na.rm = TRUE)
rm (aF, yA2, cOffY)


## plot
# pdf ("~/tmp/LCI_noaa/media/precipAx.pdf", width = 9, height = 6)
 pdf (paste0 ("~/tmp/LCI_noaa/media/sa-precip-", ifelse (SWMP, "LE", "AP"), ".pdf"), width = 9, height = 6)
aPlot (tDay, "totprcp", ylab = "daily precipitation [mm]"
       , currentCol = currentCol
       , MA = TRUE)
# box()

## legend
if (1){ # mark big rain events (yes) OR plot 1/10 or daily rain
  cCex <- 2
  text (tDay$jday, ifelse (tDay$pY_totprcp > 10, tDay$pYMA_totprcp + 0.2, NA), labels = "*", col = "blue", cex = cCex)

  # require ("png")
  # img <- readPNG ("pictograms/rain-cloud.png")
  # hgt <- 0.5; wdh <- 15
  # tRain <- subset (tDay, pY_totprcp > 10)
  # with (tRain, rasterImage (img, xleft = jday - 6, ybottom = pYMA_totprcp + 0.1
  #              , xright = jday - 6 + wdh, ytop = pYMA_totprcp + 0.1 + hgt))
  # rm (hgt, wdh)
  
  bP <- cLegend (x = 140, y = max (tDay$totprcp, na.rm = TRUE) # + 4.2  ## better to use "top" and inset? -- or top on blank, then % shift?
                 , qntl = qntl, title = paste (maO, "day moving average")
                 , title.adj = 0.5, currentYear = currentYear
                 , mRange = c (min (hmr$year), currentYear-1)
                 , cYcol = currentCol
  )
#  legend (x = bP$rect$left+2, y = bP$rect$top - 0.95, legend = "day with > 10 mm", pch = "*", col = "blue", bty = "n", pt.cex = cCex)
  legend (x = bP$rect$left+2, y = bP$rect$top - 1.5, legend = "day with > 10 mm", pch = "*", col = "blue", bty = "n", pt.cex = cCex)
}else{
  lines (pY_totprcp/10~jday, tDay, col = "blue", type = "s", lwd = 1)
  bP <- cLegend (x = 140, y = 4.2
                 , qntl = qntl, title = paste (maO, "day moving average")
                 , title.adj = 0.5, currentYear = currentYear
                 , mRange = c (min (hmr$year), currentYear-1)
                 , cYcol = currentCol
                 , sYears = "2019/10, no smooth", sLwd = 1, sLty = 1, sLcol = "blue"
  )
}

## totals
xAl <- bP$rect$left - 50; yAl <- bP$rect$top + 0.3
text (xAl, yAl, paste0 (round (sum (tDay$pY_totprcp, na.rm = TRUE)), " mm"), col = "blue", pos = 4)
text (xAl + 35, yAl
      , paste0 (round (sum (tDay$totprcp, na.rm = TRUE)), " mm", " ["
                , round (as.numeric (ARq [1])), " : "
                , round (as.numeric (ARq [2])), " mm "
                , qntl [1]*100, "%-ile]"
      )
      , col = "darkgray", pos = 4)
# box()
 dev.off()
 
rm (bP, xAl, yAl)











##################################################################################################
## relative humidity -- relevant for fire danger more so than precipitation -- should outsource ##
##################################################################################################


rm (list = ls()); load ("~/tmp/LCI_noaa/cache/metDat.RData") # from windTrend.R 
# load from NOAA ??

maO <- 31
qntl <- 0.9
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c ("red", "pink")

source ("annualPlotFct.R") # important to call after defining currentCol! 


tDay <- prepDF (varName = "rh", dat = hmr, maO = maO, qntl = qntl)

pdf ("~/tmp/LCI_noaa/media/sa-relativeHumidity.pdf", width = 9, height = 6)
aPlot (tDay, "rh", ylab = "% relative humidity", currentCol = currentCol, MA = TRUE)
cLegend ("topleft", qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = NULL, currentYear = currentYear
         , mRange = c(min (hmr$year), currentYear - 1)
         , cYcol = currentCol)
dev.off()





## find troubles
if (0){
if (1){
  yL <- levels (factor (hmr$year))
  pdf ("~/tmp/LCI_noaa/media/precipX.pdf", width = 9, height = 6)
  
  for (i in 1:length (yL)){
    tDay <- prepDF (varName = "totprcp", dat = subset (hmr, hmr$year != yL [i])  # but but year in Q
                    , maO = 31, qntl = c(0.5, 0.75)
    )
    with (tDay, annualPlot (MA_totprcp, maL1_totprcp, maU1_totprcp, pYMA_totprcp, jday
                            , currentCol = "blue", yCaption = "daily rainfall [mm]"))
    title (main = paste0 ("long-term (grey) = all years but", yL [i]))
    box()
  }
  dev.off()
}




## test == plot
longMean <- tDay$MA_totprcp
percL <- tDay$perL1_totprcp
percU <- tDay$perU1_totprcp
jday <- tDay$jday
current <- tDay$pY_totprcp
yCaption = ""
currentCol = "red"
perc2L = NA
perc2U = NA


## test -- prep DF
varName <- "totprcp"
dat <- hmr
sumFct <- function (x){mean (x, na.rm = TRUE)}
maO <- 31
currentYear <- 2019
qntl <- c(0.75)

}

cat ("Finished rainy.R\n")
# EOF
