
## air temperature -- standardized plot
if (!exists("quarterly")) {
  rm(list = ls())
  quarterly <- TRUE
}
# setwd ("~/myDocs/amyfiles/NOAA-LCI/")



## may have to delete cache -- done now by functions
maO <- 31  # 7 days certainly not working, 14 days not enough either
# maO <- 1
qntl <- c(0.9) # , 0.8)
currentYear <- as.numeric(format(Sys.Date(), "%Y")) - 1
require("RColorBrewer")
# currentCol <- brewer.pal (3, "Paired")
currentCol <- brewer.pal(6, "Paired")[c(5, 6, 7)]
SWMP <- TRUE
# SWMP <- FALSE  ## for 2021, but maybe permanent from now on



## setup automated parameter and pull data
source("annualPlotFct.R")
mediaD <- "~/tmp/LCI_noaa/media/StateOfTheBay/"
mediaDex <- "~/tmp/LCI_noaa/media/StateOfTheBay-experimental/"


if (quarterly) {
  pastYear <- FALSE  # plot currentYear-1 ?
  ongoingY <- TRUE
  currentCol <- currentCol [c(3, 1, 2)]
  mediaD <- paste0(mediaD, "update/")
  # mediaD <- "~/tmp/LCI_noaa/media/StateOfTheBay-quarterly"
} else {
  pastYear <- TRUE  # for winter/fall publication schedule
  ongoingY <- FALSE
  currentCol <- rev(currentCol)
}

## get data from annual-fetchAirWeather.R
load("~/tmp/LCI_noaa/cache/annual-noaaAirWeather.RData")  # hmr,  Homer Airport


# if (SWMP){
#   load ("~/tmp/LCI_noaa/cache/metDat.RData") # from annual-wind.R -- SWMP
# }else{
#   source ("eather.R")
#   load ("~/tmp/LCI_noaa/cache/HomerAirport.RData") # from noaaWeather.R -- Airport
# }



# plot (subset (hmr$totprcp, hmr$year < 2005), type="l")
# plot (totprcp~datetimestamp, data=hmr, subset=year < 2006, type="l")

## QCQA moved to windTrend.R
if (0) {
  x <- subset(hmr, (year == 2021) & (month %in% c(8, 9, 10)))
  plot(atemp ~ datetimestamp, x)
  x <- subset(hmr, (year == 2021) & (month %in% c(8)))
  plot(atemp ~ datetimestamp, x, type = "l")
}
## use atemp or medtemp or mintemp or maxtemp?
hmr <- subset(hmr, !is.na(atemp))

# hmr$atemp <- ifelse (hmr$atemp < -30, NA, hmr$atemp)
# hmr$atemp <- ifelse (hmr$atemp < 0 & hmr$month == 7, NA, hmr$atemp)  # 2005-07-29, -17.8 C
# hmr$atemp <- ifelse (hmr$atemp > 29, NA, hmr$atemp) # 12.8, 13.9, 11.1, 29.4, 8.9, 8.9, 8.3 -- 2002-09-07

## units: use both, C and F (F=2nd axis on the right)
if (0) {  ## plot all years -- rainbow spaghetti
  hmr$yearF <- factor(hmr$year)
  plot(atemp ~ jday, data = hmr, type = "n")
  for (i in seq_along(levels(hmr$yearF))) {
    #  plot (atemp ~ jday, data=hmr, subset=hmr$yearF == levels (hmr$yearF)[i], col=i, type="l")
    lines(atemp ~ jday, data = hmr, subset = hmr$yearF == levels(hmr$yearF)[i]
          , col = i)
  }
}

## aggregate data
tDay <- prepDF(varName = "atemp", dat = hmr, maO = maO, qntl = qntl
               , currentYear = currentYear)

#
## plot
# pdf ("~/tmp/LCI_noaa/media/sa-airTemp.pdf", width=9, height=6)
dir.create(mediaD, showWarnings = FALSE, recursive = TRUE)
dir.create(mediaDex, showWarnings = FALSE, recursive = TRUE)

# pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-airTemp-"
#              , ifelse (SWMP, "LE", "AP"), ".pdf"), width=9, height=6)
png(paste0(mediaD, "sa-airTemp-", ifelse(SWMP, "LE", "AP"), ".png")
    , width = 1800, height = 1200, res = 300)

par(mar = c(3, 4, 2, 4))
aPlot(tDay, "atemp"
  # , ylab=expression ('air'~'temperature '~'['*degree~'C'*']')
  , ylab = "air temperature [°C]"
  , currentCol = currentCol, MA = TRUE
  , pastYear = pastYear, ongoingYear = ongoingY
)
if (SWMP) {title(main = "Air Temperature at Homer Spit")} else {
  title(main = "Air Temperature at Homer Airport")
 }
# for (i in 1:length (levels (factor (hmr$year)))){
#   lines (atemp~jday, subset (hmr, year == levels (factor (hmr$year))[i]))
# }
# fAxis (c (-15, 15), mT=expression ('air'~'temperature '~'['*degree~'F'*']'))
fAxis(c(-15, 15), mT = "air temperature [°F]")
box()
## legend
cLegend( # "bottom"# , inset=0.07
  105, 2
  , currentYear = currentYear, mRange = c(min(hmr$year), currentYear - 1)
  , cYcol = currentCol
  , title = paste(maO, "day moving average")
  , qntl = qntl
  , pastYear = pastYear, ongoingYear = ongoingY
)
dev.off()






########################
## for the gardeners: ##
########################
## what's the likelihood of frost? What's the last day of frost?
# rm (hmr)
frostTH <- c(-5, -3, -2, 0, 2, 5)
frostTH <- c(-5, -3, -2, 0)
## tomatoes: stay above 50 F -> mintemp 10 C


# source ("noaaWeather.R")
#
# load ("~/tmp/LCI_noaa/cache/HomerAirport.RData") # from noaaWeather.R -- Airport
suppressMessages(require("zoo"))
load("~/tmp/LCI_noaa/cache/annual-noaaAirWeather.RData")  # hmr, Homer Airport
hmrMin <- aggregate(atemp ~ jday + year, data = hmr, min)  # min daily temperature
# hmr$atemp <- hmr$mintemp # caring about frost, not average!?
# hmr$atemp <- with (hmr, rowMeans (cbind (mintemp, maxtemp)))
hmrMin$medtemp <- aggregate(atemp ~ jday + year, data = hmr, mean)$atemp
hmr <- hmrMin

## min of min and max? min? , atemp?
## supply a list of logical vectors instead of a threshold? To allow min and mean temperatures
frostM <- list(hmr$atemp < 0  ## recent years
 , hmr$medtemp < -2 ## hardy tubbers?  -- AFTER 14 days of overall mean > 0?
 , hmr$medtemp < 0    ## lettuce?
 , hmr$atemp < 10)  ## tomatoes
dFrostL <- lapply(seq_along(frostM), function(i) {
  dF <- hmr [, names(hmr) %in% c("jday", "year")]
  dF$fro <- frostM [[i]]
  cdFrost <- rbind(dF [, names(dF) %in% c("fro", "jday", "year")]
   , with(dF, data.frame(fro, jday = jday + 365, year)))
  cdFrost <- cdFrost [order(cdFrost$year, cdFrost$jday), ]
  cdFrost$MA <- rollapply(cdFrost$fro, width = 28, FUN = any, align = "left", fill = NA)
  dF$atemp <- cdFrost$MA [match(with(dF, paste(year, jday))  ## circular -> match
   , with(cdFrost, paste(year, jday)))]
  dF$fY <- factor(dF$year)
  dF
})

# hmr <- subset (hmr, !is.na (atemp))  ## records prior to 1990s are NA
# dFrostLx <- lapply (frostTH, FUN=function (x){
#   dFrost <- aggregate(atemp~jday+year, data=hmr, function (y){any (y < x)})
#   cdFrost <- rbind(dFrost, with (dFrost, data.frame (atemp, jday=jday + 365,year)))
#   cdFrost <- cdFrost [order (cdFrost$year, cdFrost$jday),]
#   cdFrost$MA <- rollapply(cdFrost$atemp, width=38, FUN=any, align="left", fill=NA)
#   dFrost$atemp <- cdFrost$MA [match (with (dFrost, paste (year, jday)), with (cdFrost, paste (year,jday)))]
#   dFrost$fY <- factor (dFrost$year)
#     dFrost
# })
yFrostL <- lapply(seq_along(frostTH), function(x) {aggregate(atemp ~ jday
 , data = dFrostL [[x]]
 , mean
 , subset = year < currentYear)})

pdf("~/tmp/LCI_noaa/media/StateOfTheBay/FrostDaysM.pdf")
plotSetup(yFrostL[[1]]$atemp, yFrostL[[1]]$atemp, ylim = c(0, 1)
  , ylab = paste0 ("likelihood of frost (", min(hmr$year)
    , "-", currentYear - 1, ")"))
require("RColorBrewer")
## cls <- GnBu, Spectral, RdYIGn
cls <- brewer.pal(length (frostTH), "RdYlGn")
cls <- brewer.pal(length (frostTH), "Blues")
for (i in rev(seq_along (frostTH))) {  ## reverse to plot cold THs last
  lines(atemp ~ jday, yFrostL [[i]], col = cls [i])
  points(I(atemp - 1) ~ jday, subset (dFrostL[[i]], year == currentYear)
    , col = cls [i], pch = 19)  # (atemp-1) to plot points low in graph
}
legend("right", pch = 19, col = cls, legend = paste (frostTH
  , " °C")
, bty = "n"
, title = paste ("Periods in", currentYear, "with\ntemperatures below")
)

require("jpeg")
img2 <- readJPEG ("pictograms/akgrown.jfif")
## calculate coordinates for raster-image, to avoid readjusting it each year
## or keep fixed y-axis?
(220 - 90) / 365 * 1 + 0.2
rasterImage(img2, xleft = 120, ybottom = 0.21
  , xright = 225, ytop = 0.55, interpolate = FALSE)
box()
dev.off()



## old, single-category version
dFrost <- aggregate(atemp ~ jday + year, data = hmr, function(x) {any (x < frostTH)})
cdFrost <- rbind(dFrost,  ## circular Dec -> Jan
  with (dFrost, data.frame (atemp, jday = jday + 365, year)))
cdFrost <- cdFrost [order (cdFrost$year, cdFrost$jday), ]
cdFrost$MA <- rollapply (cdFrost$atemp, width = 38, FUN = any, align = "left", fill = NA)
dFrost$atemp <- cdFrost$MA [match (with (dFrost, paste (year, jday)), with (cdFrost, paste (year, jday)))]
rm (cdFrost)
yFrost <- aggregate(atemp ~ jday, data = dFrost, mean, subset = year < currentYear) # proportion of frost


pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/FrostDays.pdf")
plotSetup (yFrost$atemp, yFrost$atemp, ylab = paste0 ("likelihood of frost (", min(hmr$year)
  , "-", currentYear - 1, ")")
# , xlim=c (5:185)  # -- expand plotSetup to show only 1/2 year
)
lines (atemp ~ jday, yFrost)
points (I(atemp - 1) ~ jday, subset (dFrost, year == currentYear), col = "blue", pch = 19)
legend ("top", pch = 19, col = "blue", legend = paste ("periods with frost in", currentYear)
  , bty = "n")
box()
## farmers' market insert
# require ("png")
# img2 <- readPNG (paste0 (tF, "ltc.png"))
require ("jpeg")
img2 <- readJPEG ("pictograms/akgrown.jfif")
## calculate coordinates for raster-image, to avoid readjusting it each year
## or keep fixed y-axis?
(220 - 90) / 365 * 1 + 0.2
rasterImage (img2, xleft = 120, ybottom = 0.21
  , xright = 225, ytop = 0.55, interpolate = FALSE)
dev.off()
# plot (atemp~jday, subset (hmr, year == 2021))



## plot progression of spring
# aggregate (atemp~year, data=subset (hmr, jday < 183), function (x))

yL <- 1:(length (levels (dFrostL[[1]]$fY)) - 1)
springL <- sapply (yL, function(i) {
  sapply (seq_along (frostTH), function(j) {
    y <- subset (dFrostL [[j]], fY == levels (fY)[i])
    if (nrow (y) > 350) {  ## some early years are incomplete
      return (min (subset (y$jday, !y$atemp)))
    } else {
      return (NA)
    }
  })
})
colnames (springL) <- levels (dFrostL[[1]]$fY)[yL]
springL <- ifelse (is.infinite (springL), NA, springL) # for years without summer (Inf)

pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/Frost_yearsM.pdf")
plot (as.numeric (colnames (springL)), springL[1, ], type = "n"
  , ylim = range (as.numeric (springL), na.rm = TRUE)
  , ylab = "first frost-free period"
  , xlab = "year", axes = FALSE)
for (i in seq_len (nrow (springL))) {
  lines (as.numeric (colnames (springL)), springL [i, ]
    , col = cls [i], type = "b")
}
legend("bottomleft", pch = 19, col = cls, legend = paste(frostTH, " °C")
, bty = "n"
, title = paste("Periods in", currentYear, "with\ntemperatures below")
)

axis(1)
axis(2, at = 15 + as.numeric(format(as.POSIXct(paste0("2019-", 1:12, "-1")), "%j"))
  , labels = month.abb, tick = FALSE) # center month-labels
axis(2, at = c (as.numeric(format(as.POSIXct (paste0("2019-", 1:12, "-1"))
  , "%j")), 366), labels = FALSE) # add 366 to mark end of Dec
box()

dev.off()




dFrost$fY <- factor(dFrost$year)
yL <- 1:(length(levels(dFrost$fY)) - 1)

spring <- sapply(yL, function(i) {
  y <- subset(dFrost, fY == levels (fY)[i])
  min(subset (y$jday, !y$atemp))  # first frost-free day -- suppress warning?
})
names(spring) <- levels(dFrost$fY)[yL]

pdf("~/tmp/LCI_noaa/media/StateOfTheBay/Frost_years.pdf")
plot(as.numeric(names (spring)), spring, type = "b"
  , ylab = "first frost-free period"
  , xlab = "year", axes = FALSE)
axis(1)
axis(2, at = 15 + as.numeric(format (as.POSIXct (paste0("2019-", 1:12, "-1")), "%j"))
  , labels = month.abb, tick = FALSE) # center month-labels
axis(2, at = c(as.numeric(format (as.POSIXct (paste0("2019-", 1:12, "-1"))
  , "%j")), 366), labels = FALSE) # add 366 to mark end of Dec
box()
dev.off()





## find troubles
if (0) {
  if (1) {
    yL <- levels(factor (hmr$year))
    pdf("~/tmp/LCI_noaa/media/precipX.pdf", width = 9, height = 6)

    for (i in seq_along (yL)) {
      tDay <- prepDF(varName = "totprcp", dat = subset(hmr, hmr$year != yL [i])  # but but year in Q
        , maO = 31, qntl = c(0.5, 0.75)
      )
      with(tDay, annualPlot(MA_totprcp, maL1_totprcp, maU1_totprcp
                              , pYMA_totprcp, jday
                              , currentCol = "blue"
                              , yCaption = "daily rainfall [mm]"))
      title(main = paste0("long-term (grey)=all years but", yL [i]))
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
  yCaption <- ""
  currentCol <- "red"
  perc2L <- NA
  perc2U <- NA


  ## test -- prep DF
  varName <- "totprcp"
  dat <- hmr
  sumFct <- function(x) {mean(x, na.rm = TRUE)}
  maO <- 31
  currentYear <- 2019
  qntl <- c(0.75)

}

cat("Finished airTemp.R\n")
# EOF
