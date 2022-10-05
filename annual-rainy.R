
## precipitation -- standardized plot
if (!exists ("quarterly")){
  rm (list=ls())
  quarterly <- TRUE
}
# setwd("~/myDocs/amyfiles/NOAA-LCI/")



maO <- 31  # 7 days certainly not working, 14 days not enough either
# maO <- 1
qntl=c(0.9) #, 0.8)
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c("black", "blue", "lightblue")

# currentCol <- "blue"
SWMP <- FALSE
SWMP <- TRUE
## what to show
if (quarterly){
  pastYear <- FALSE  ## for fall publication
  ongoingY <- TRUE
  currentCol <- c("black", "blue", "lightblue")  ## current and ongoing
}else{
  pastYear <- FALSE  ## for winter/spring publication
  ongoingY <- TRUE
  currentCol <- c("lightblue", "blue", "black")  ## current and prev
}


## load/download/update data
source ("annualPlotFct.R") # important to call after defining currentCol!
if (SWMP){                                   # use SWMP data or NOAA homer airport
  load ("~/tmp/LCI_noaa/cache/metDat.RData") # from annual-wind.R -- SWMP
}else{
  source ("noaaWeather.R")  ## test whether re-run is necessary, somehow
  load ("~/tmp/LCI_noaa/cache/HomerAirport.RData") # from noaaWeather.R -- Airport
}



# plot (subset (hmr$totprcp, hmr$year < 2005), type="l")
# plot (totprcp~datetimestamp, data=hmr, subset=year < 2006, type="l")

## QCQA moved to windTrend.R


## aggregate data
tDay <- prepDF (dat=hmr, varName="totprcp", maO=maO, qntl=qntl)


## annual vs average precip
cat ("\n##\n##\nCurrent year precipitation compared to long-term mean\n")
anR <- aggregate (totprcp~year, subset (hmr, year <= currentYear)
                  , sum, na.rm=TRUE)
summary (subset (anR, year < currentYear)$totprcp)
subset (anR, year == currentYear)
cat ("\n##\n##")


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
# yA <- aggregate (totprcp~year+jday, subset (hmr, year > coffY), mean, na.rm=TRUE) ## should exclude >= currentYear
# yA2 <- aggregate (totprcp~year
#                   , subset (yA, year < currentYear) #as.numeric (format (Sys.Date(), "%Y"))) # excludeing >= currentYear
#                   , mean, na.rm=TRUE)
yA2 <- aggregate (totprcp~year, subset (hmr, (year > cOffY)&(year < currentYear))
                  , FUN=sum, na.rm=TRUE)
ARq <- quantile(yA2$totprcp, 0.5 + c(-1,1) * qntl [1] /2 , na.rm=TRUE)

## violin plot of annual rain
if (0){
  Require ("vioplot")
  yA2 <- aggregate (totprcp~year, subset (hmr, (year > cOffY))
                    , FUN=sum, na.rm=TRUE)
  x <- vioplot(yA2$totprcp, ylab="annual precipitation [mm]")
  cYtotprcp <- yA2$totprcp [which (yA2$year == currentYear)]
  lines (c(0.9, 1.1), rep (cYtotprcp,2), col="red")
  # points (0.5, cYtotprcp
  #         , col = "red", pch=16)
  text (1.15, cYtotprcp, currentYear, col="red")
}
rm (aF, yA2, cOffY)


## plot
pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-precip-", ifelse (SWMP, "LE", "AP"), ".pdf"), width=9, height=6)
# png (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-precip-", ifelse (SWMP, "LE", "AP"), ".png")
#      , width=1800, height=1200, res=300)
par (mar=c (3,4,2,4)+0.1)
aPlot (tDay, "totprcp", ylab="daily precipitation [mm]"
       , currentCol=currentCol
       , MA=TRUE
       , pastYear=pastYear, ongoing=ongoingY)
if (SWMP){title (main="Precipitation at Homer Spit")}else{
  title (main="Precipitation at Homer Airport")
}
## add inch scale
iAxis (tDay$totprcp, lab="daily precipitation [inch]")

## mark high rain days
cCex <- 2
text (tDay$jday, ifelse (tDay$pY_totprcp > 10
                         #, tDay$pYMA_totprcp + 0.2
                         , 0, NA)
      , labels="*", col=currentCol [2], cex=cCex)
if (pastYear){
  text (tDay$jday, ifelse (tDay$pcY_totprcp > 10
                           , 0.18
                           , NA)
        , labels="*", col=currentCol [1], cex=cCex)
}
if (ongoingY){
  rD <- ifelse (is.na (tDay$ogY_totprcp), 0, tDay$ogY_totprcp) # or next line fails
  text (tDay$jday, ifelse (rD > 10, 0.14, NA)
        , labels="*", col=currentCol [3], cex=cCex)
  rm (rD)
}
# require ("png")
# img <- readPNG ("pictograms/rain-cloud.png")
# hgt <- 0.5; wdh <- 15
# tRain <- subset (tDay, pY_totprcp > 10)
# with (tRain, rasterImage (img, xleft=jday - 6, ybottom=pYMA_totprcp + 0.1
#              , xright=jday - 6 + wdh, ytop=pYMA_totprcp + 0.1 + hgt))
# rm (hgt, wdh)

## legend
bP <- legend ("topleft", bty="n", legend="") # to get coordinates
# bP <- cLegend ("topleft" # x=140, y=max (tDay$totprcp, na.rm=TRUE) # + 4.2  ## better to use "top" and inset? -- or top on blank, then % shift?
bP <- cLegend (x=bP$rect$left + 55, y=bP$rect$top
               , qntl=qntl, title=paste (maO, "day moving average")
               , title.adj=0.5, currentYear=currentYear
               , mRange=c (min (hmr$year), currentYear-1)
               , cYcol=currentCol
               , pastYear=pastYear, ongoingYear=ongoingY)
legend (x=bP$rect$left+3 # png: 4, pdf: 3
        , y=bP$rect$top-bP$rect$h + 0.20  # +11: align text. +3 looks better (png), 0.20 (pdf)
        , legend="day with > 10 mm", pch="*", col=currentCol [2]
        , bty="n", pt.cex=cCex)  # add transparent line to fix alignment

## totals
xAl <- bP$rect$left - 50; yAl <- bP$rect$top + 0.3
xAl <- bP$rect$left + 50; yAl <- bP$rect$top + 0.3
text (xAl, yAl, paste0 (round (sum (tDay$pY_totprcp, na.rm=TRUE)), " mm")
      , col=currentCol [2], pos=4)
text (xAl + 35, yAl
      , paste0 (round (sum (tDay$totprcp, na.rm=TRUE)), " mm", " ["
                , round (as.numeric (ARq [1])), " : "
                , round (as.numeric (ARq [2])), " mm "
                , qntl [1]*100, "%-ile]"
      )
      , col="darkgray", pos=4)
box()
dev.off()

rm (bP, xAl, yAl, cCex)


## table of average N vs current year N high-rain days
hmrD <- aggregate(totprcp~jday+year, hmr, FUN=sum)
# hmrD <- addTimehelpers(hmrD)
rainSum <- nEvents(hmrD, "totprcp", thrht=10)
print (rainSum)
rm (hmrD, rainSum)
## end of rain summary






##################################################################################################
## relative humidity -- relevant for fire danger more so than precipitation -- should outsource ##
##################################################################################################


rm (list=ls()); load ("~/tmp/LCI_noaa/cache/metDat.RData") # from windTrend.R
# load from NOAA ??

maO <- 31
qntl <- 0.9
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c ("pink", "red", "orange")

source ("annualPlotFct.R") # important to call after defining currentCol!


tDay <- prepDF (varName="rh", dat=hmr, maO=maO, qntl=qntl)

# pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-relativeHumidity.pdf", width=9, height=6)
png ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-relativeHumidity.png", width=1800, height=1200, res=300)
aPlot (tDay, "rh", ylab="% relative humidity", currentCol=currentCol, MA=TRUE)
cLegend ("bottomright", qntl=qntl, title=paste (maO, "day moving average")
         , title.adj=NULL, currentYear=currentYear
         , mRange=c(min (hmr$year), currentYear - 1)
         , cYcol=currentCol)
dev.off()





## find troubles
if (0){
if (1){
  yL <- levels (factor (hmr$year))
  pdf ("~/tmp/LCI_noaa/media/precipX.pdf", width=9, height=6)

  for (i in 1:length (yL)){
    tDay <- prepDF (varName="totprcp", dat=subset (hmr, hmr$year != yL [i])  # but but year in Q
                    , maO=31, qntl=c(0.5, 0.75)
    )
    with (tDay, annualPlot (MA_totprcp, maL1_totprcp, maU1_totprcp, pYMA_totprcp, jday
                            , currentCol="blue", yCaption="daily rainfall [mm]"))
    title (main=paste0 ("long-term (grey)=all years but", yL [i]))
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
yCaption=""
currentCol="red"
perc2L=NA
perc2U=NA


## test -- prep DF
varName <- "totprcp"
dat <- hmr
sumFct <- function (x){mean (x, na.rm=TRUE)}
maO <- 31
currentYear <- 2019
qntl <- c(0.75)

}

cat ("Finished rainy.R\n")
# EOF