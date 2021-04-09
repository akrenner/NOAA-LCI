#!/usr/bin/env RScript
# Martin Renner, NOAA-affiliate, 2020
## make climatology plot vs 2019 of high-wind events and average daily wind speed.
## use Lands End wind data


rm (list = ls())
setwd("~/myDocs/amyfiles/NOAA-LCI/")


## location of data -- best to curl it -- better to get this from SeldoviaTemp?? one place to spec
## SeldoviaTemp currently only other file using SWMP data
# file name ---> could use Windows shortcut?

# if (1){ ## start over with new data set?
#
#   if (.Platform$OS.type == "windows"){
#     require ("R.utils")
#     SMPfile <- readWindowsShortcut ("~/GISdata/LCI/SWMP/current.lnk")$path
#   }else{ # MacOS or Linux
#     SMPfile <- "~/GISdata/LCI/SWMP/current"
#   }
#   require ("SWMPr")
#   hmr <- import_local(SMPfile, "kachomet")
#   save (hmr, file = "~/tmp/LCI_noaa/cache/wind1.RData")
# }else{
#   rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wind1.RData") # hmr
#
#   ## updated to the latest data
#   require ("SWMPr")
#   # hmr2 <- all_params_dtrng ("kachomet", c ('01/01/2013', '02/01/2013'))
#
#   fN <- difftime(Sys.time(), max (hmr$datetimestamp), units = "hours")
#   hmr2 <- try (all_params ("kachomet", Max = ceiling (as.numeric(fN)*4)))  # XXX needs registered (static?) IP address. NCCOS VPN ok?
#   if (class (hmr2)[1] == "swmpr"){
#     ## order of field names does not match between hmr2 and hmr
#     hmr3 <- hmr2 [,sapply (1:ncol (hmr), FUN = function (i){
#       which (names (hmr)[i] == names (hmr2))
#     })]
#     hmr <- rbind (hmr, hmr3)
#     rm (hmr2, hmr3, fN)
#     hmr <- hmr [which (!duplicated(hmr$datetimestamp)),]
#     save (hmr, file = "~/tmp/LCI_noaa/cache/wind1.RData")
#   }
# }
#



##########################################################
## parameters to agree upon
currentYear <- as.numeric (format (Sys.Date(), "%Y")) -1 # year before present
maO <- 31  # moving average window
vUnit <- "knots" # or comment out to default to m/s
qntl <- 0.9 # % quantile
stormT <- 40
galeT <- 30
currentCol <- c ("blue", "lightblue")
# currentCol <- "blue"
# currentCol <- "red"
# currentCol <- c ("red", "magenta")
## leave code below as-is
##########################################################



## get up-to-date SWMP data
source ("annualPlotFct.R")
hmr <- getSWMP ("kachomet")


## apply QAQC flaggs ##
# is.na (hmr$atemp [which (hmr$f_atemp != "<0>")]) <- TRUE
# is.na (hmr$)
hmr <- qaqc (hmr, qaqc_keep = "0")  # scrutinize this further?

#######################



# also try max, sum of h over gale, N gale days...
## alternative approach: gales per month with gale = max_wspd > 30 knots
## also consider: sd on a log-scale

if (exists (vUnit)){ #} == "knots"){
  hmr$wspd <- hmr$wspd * 1.94384
  hmr$maxwspd <- hmr$maxwspd * 1.94384
  wCaption <- "wind speed [knots]"
}else{
  wCaption <- "wind speed [m/s]"
}

agFct <- function (x, thd = stormT){ ## gale days per maO
  #  ifelse (sum (x > 30, na.rm = TRUE) > 1*4, 1*maO, 0) # gale = 2 h at 20 knots -- my definition, guided by histogram
  #XXX@   ifelse (sum (x > 30, na.rm = TRUE) > 1*4, 1, 0) # gale = 2 h at 20 knots -- my definition, guided by histogram
  x1 <- sum(x>=thd, na.rm = TRUE)
  ifelse (x1 > 1*4, 1, 0) # at least 1 h above threshold
}  # 2 h at 20 knots looks good

meanNA <- function (x){mean (x, na.rm = TRUE)}




## apply QCQA -- ask Steve, check Seldovia -- not working as-is!
# summary (factor (hmr$f_wspd))
if (0){
qaqcM <- function (fVar, tVar){
  fVar <- gsub ("[<>]", "", fVar)
  fVar <- gsub ("[GIT]", "", fVar, fixed = TRUE)
  fVar <- gsub ("[SOC] (CSM)", "", fVar, fixed = TRUE)
  fVar <- trimws (fVar)
  # levels (factor (fVar))
  tVar <- ifelse (fVar %in% c("0", "1", "2", "3", "4", "5"), tVar, NA) ## check with Steve Baird!!
  tVar
}
hmr$wspd <- qaqcM (hmr$f_wspd, hmr$wspd)
hmr$maxwspd <- qaqcM (hmr$f_maxwspd, hmr$maxwspd)
hmr$wdir <- qaqcM (hmr$f_wdir, hmr$wdir)
hmr$atemp <- qaqcM (hmr$f_atemp, hmr$atemp)
hmr$totprcp <- qaqcM (hmr$f_totprcp, hmr$totprcp)
 # 2003 precip data has no zeros, 2004 is missing
hmr$totprcp <- with (hmr, ifelse (datetimestamp < as.POSIXct("2004-01-01"), NA, totprcp)) # no zeros in 2003 data
hmr$rh <- qaqcM (hmr$f_rh, hmr$rh)
rm (qaqcM)
}
## find missing values, gaps in TS
# summary (as.numeric (diff (hmr$datetimestamp)))
# which (as.numeric (diff (hmr$datetimestamp)) > 15)
hmr <- fixGap (hmr)  # this will also add helper variables year, jday, etc.


# save (hmr.... file = ....)


###############################################################
## data processing -- calculate averages and moving averages ##
###############################################################


## current/past year = year of report
dMeans <- aggregate (wspd~jday+year, hmr, FUN = meanNA) # daily means full time series
Gust <- aggregate (maxwspd~jday+year, hmr, FUN = agFct, thd = stormT)
dMeans$gale <- Gust$maxwspd [match (paste (dMeans$jday, dMeans$year), paste (Gust$jday, Gust$year))]
Gust$sca <- aggregate (maxwspd~jday+year, hmr, FUN = agFct, thd = galeT)$maxwspd
dMeans$sca <- Gust$sca [match (paste (dMeans$jday, dMeans$year), paste (Gust$jday, Gust$year))]
rm (Gust)
## mean wind direction (incl. speed)
## http://weatherclasses.com/uploads/3/6/2/3/36231461/computing_wind_direction_and_speed_from_u_and_v.pdf
uw <- with (hmr, -1 * wspd * sin (wdir*pi/180))
vw <- with (hmr, -1 * wspd * cos (wdir*pi/180))
## MR versions. u = x, v = y
dMuw <- aggregate (uw~jday+year, hmr, FUN = meanNA)
dMvw <- aggregate (vw~jday+year, hmr, FUN = meanNA)
dMeans$uw <- dMuw$uw [match (paste (dMeans$jday, dMeans$year), paste (dMuw$jday, dMuw$year))]
dMeans$vw <- dMvw$vw [match (paste (dMeans$jday, dMeans$year), paste (dMuw$jday, dMuw$year))]
rm (uw, vw, dMuw, dMvw)

meanWind <- function (u,v){ # weatherclasses.com as above
  180+(180/pi*atan2 (u,v))
  }
dMeans$wdir <- with (dMeans, meanWind (uw, vw))
dMeans$windSpd <- with (dMeans, sqrt (uw^2+vw^2))
dMeans$wdir <- with (dMeans, ifelse (wdir < 0, wdir + 360, wdir)) # needed??
require ("circular")
circWind <- circular (hmr$wdir, type = "directions", units = "degrees", template = "geographics")
wDir2 <- aggregate (circWind~jday+year, hmr, FUN = function (x){
  as.numeric (mean (x, na.rm = TRUE)) ## this is NOT right -- need to apply weight by wind speed! XXX
  })
dMeans$wDirCM <- wDir2$circWind [match (paste (dMeans$jday, dMeans$year), paste (wDir2$jday, wDir2$year))]
rm (circWind, wDir2)

## MA of current/past year -- Moving Average
# require (forecast)
# load ("~/tmp/LCI_noaa/cache/MAfunction.RData") ## gets maT -- function -- backwards MA -- is that what we want? XXX
# ma <- maT
# dMeans$maW <- as.numeric (maT (dMeans$wspd, maO))
# dMeans$galeMA <- as.numeric (maT (dMeans$gale, maO))
require ("SWMPr")
dMeans$maW <- unlist (smoother (dMeans$wspd, maO, sides = 1))
dMeans$galeMA <- unlist (smoother (dMeans$gale, maO, sides = 1))


## annual data
tDay <- aggregate (wspd~jday, dMeans, FUN = meanNA, subset = year < currentYear) # exclude current year
tDay$sdWind <- aggregate (wspd~jday, dMeans, FUN = sd, subset = year < currentYear)$wspd
tDay$smoothWindMA <- aggregate (maW~jday, dMeans, FUN = meanNA, subset = year < currentYear)$maW
tDay$sdMA <- aggregate (maW~jday, dMeans, FUN = sd, na.rm = TRUE)$maW ## it's circular(ish): av across years
tDay$lowPerMA <- aggregate (maW~jday, dMeans, FUN = quantile, probs = 0.5-0.5*qntl, na.rm = TRUE)$maW
tDay$uppPerMA <- aggregate (maW~jday, dMeans, FUN = quantile, probs = 0.5+0.5*qntl, na.rm = TRUE)$maW
lowQ <- 0.8
tDay$lowPerMAs <- aggregate (maW~jday, dMeans, FUN = quantile, probs = 0.5-0.5*lowQ, na.rm = TRUE)$maW
tDay$uppPerMAs <- aggregate (maW~jday, dMeans, FUN = quantile, probs = 0.5+0.5*lowQ, na.rm = TRUE)$maW
rm (lowQ)

tDay$uw <- aggregate (uw~jday, dMeans, FUN = meanNA, subset = year < currentYear)$uw
tDay$vw <- aggregate (vw~jday, dMeans, FUN = meanNA, subset = year < currentYear)$vw
tDay$wdir <- meanWind (tDay$uw, tDay$vw)

tDay$gale <- aggregate (gale~jday, dMeans, FUN = meanNA, subset = year < currentYear)$gale # * maO
tDay$galeMA <- aggregate (galeMA~jday, dMeans, FUN = meanNA, subset = year < currentYear)$maGale
# may need different averager for storms?? kernel-density?? XXX
tDay$sdGaleMA <- aggregate (galeMA~jday, dMeans, FUN = sd, na.rm = TRUE, subset = year < currentYear)$maGale
## XX tDay$sca <- aggregate (sca~jday, dMeans, FUN = meanNA, subset = year < currentYear)$sca

## current year under consideration
past365 <- subset (dMeans, subset = year == currentYear)
tDay$p365  <- past365$wspd [match (tDay$jday, past365$jday)]  ## has some missing days
tDay$p365ma <- past365$maW [match (tDay$jday, past365$jday)]
tDay$p365uw <- past365$uw [match (tDay$jday, past365$jday)]
tDay$p365vw <- past365$vw [match (tDay$jday, past365$jday)]

tDay$p365galeMA <- past365$galeMA [match (tDay$jday, past365$jday)]
tDay$p365galDay <- past365$gale [match (tDay$jday, past365$jday)]
tDay$p365wdir <- past365$wdir [match (tDay$jday, past365$jday)]
tDay$p365scaDay <- past365$sca [match (tDay$jday, past365$jday)]

# currently running year
c365 <- subset (dMeans, subset = year == currentYear + 1)
tDay$c365 <- c365$wspd [match (tDay$jday, c365$jday)]
tDay$c365ma <-c365$maW [match (tDay$jday, c365$jday)]
rm (past365, c365)

## weekly summary of annual data -- for wind uv only
# tWeek <- aggregate (wspd~week, hmr, meanNA)
tWeek <- aggregate (cbind (jday, uw, vw, p365uw, p365vw)~I(factor (jday %/% 7)), data = tDay, meanNA)
names (tWeek)[1] <- "week"

## cardinal wind direction
## following https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/4
cDir <- function (wd, nDir = 8){
  if (!(nDir %in% c(4,8,16))){
    error ("nDir has to be 4, 8, or 16")
  }
  if (nDir == 4){
    rose_breaks <- c (0, 360/8, (1/8 + (1:3 / 4)) * 360, 360)
    rose_labs <- c("N", "E", "S", "W", "N")
  }else if (nDir == 8){
    rose_breaks <- c (0, 360/16, (1/16 + (1:7 / 8)) * 360, 360)
    rose_labs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  }else{
    rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
    rose_labs <- c("N", "NNE", "NE", "ENE","E", "ESE", "SE", "SSE",
                   "S", "SSW", "SW", "WSW","W", "WNW", "NW", "NNW","N")
  }
  wd <- ifelse (wd < 0, 360 + wd, wd)
  cut (wd, breaks = rose_breaks, labels = rose_labs
       , right = FALSE, include.lowest = TRUE)
}
## example
# cE <- c (358, 2, 89, 177, 92, 265, 46, 20); data.frame (cDir (cE), cE)

tDay$p365wCar <- cDir (tDay$p365wdir) # cardinal directions
# rm (cDir)


### trouble-shooting
save.image ("~/tmp/LCI_noaa/cache/WindTrouble.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/WindTrouble.RData")


if (0){ ## circular spline of climatology, long-term mean.
tD1 <- tDay
tD1$jday <- tD1$jday - 365
tD2 <- tDay
tD2$jday <- tD2$jday + 365
ctday <- rbind (tD1, tDay, tD2)
rm (tD1, tD2)

require (mgcv)
gM <- gam (wspd~s(jday), data = ctday)
tDay$smoothWind <- as.numeric (predict (gM, newdata = tDay))
rm (ctday, gM)
}

tDay <- subset (tDay, jday < 366) # 366 is not stable because not many samples





################
## make PLOTS ##
################


if (0){
  pdf ("~/tmp/LCI_noaa/media/wind-test.pdf", width = 9, height = 6)
plot (p365~jday, tDay, type = "n",
      xlab = "", ylab = wCaption, axes = FALSE)
axis (2); box()
axis (1
      , at = as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1")), "%j"))
      , labels = paste0 ("1-", month.abb)
)
lines (p365~jday, tDay, col = currentCol [1], lwd = 2)
lines (wspd~jday, tDay, col = "black", lwd = 2)
lines (smoothWind~jday, tDay, col = "blue", lwd = 3)
lines (smoothWindMA~jday, tDay, col = "green", lwd = 2)
lines (p365ma~jday, tDay, col = "gray", lwd = 2, lty = "dashed")
legend ("bottomleft", legend = c("daily-mean", "lt-spline", "lt-MA"
                                 , "2019", "2019-MA")
        , col = c("black", "blue", "green"
                  , "red", "gray")
        , lty = c (rep ("solid", 4), "dashed")
        , lwd = 3, bty = "n")
dev.off()
}



## cleaned-up plot
save.image("~/tmp/LCI_noaa/cache/wind2.RData")
save (hmr, file = "~/tmp/LCI_noaa/cache/metDat.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wind2.RData")


x <- mkdirs("~/tmp/LCI_noaa/media/StateOfTheBay/"); rm (x)
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-wind.pdf", width = 9, height = 6)
# png ("~/tmp/LCI_noaa/media/wind-MA.png"), width = 9*100, height = 7*100)


if (0){  ## farewell my good friend ##
  ## combine several panels in one plot
  layout (matrix(c(1,2,3), nrow = 3)
          , widths = rep (3,3), heights = c(0.5,0.5,3))
  # par (fig = c(), new = TRUE) # see https://www.statmethods.net/advgraphs/layout.html
  ## wind vectors
  sF = 0.7
  lW = 1.0
  xGrenz <- c(5,360)
  par (mar = c(0,4,0,0.1))
  plot (1:nrow (tDay), seq (-10, 10, length.out = nrow (tDay)), type = "n", asp = 1
        , axes = FALSE, xlab = "", ylab = "long-term", xlim = xGrenz)
  with  (tDay, segments(x0 = jday, y0 = 0, x1 = jday - uw*sF, y1 = 0 - vw*sF, col = "blue", lwd = lW))
  plot (1:nrow (tDay), seq (-10, 10, length.out = nrow (tDay)), type = "n", asp = 1
        , axes = FALSE, xlab = "", ylab = currentYear, xlim = xGrenz)
  with (tDay, segments(x0 = jday, y0 = 0, x1 = jday - p365uw*sF, y1 = 0 - p365vw*sF
                       , col = ifelse (p365scaDay == 1, "black", "red")
                       , lwd = lW + p365galDay*2))
  rm (sF, lW)
}

## better to move to standard var names above
par (mar = c(3,4,0.1,0.1))
plotSetup (tDay$lowPerMA, tDay$uppPerMA, ylab = wCaption)

oP <- par()
if (1){ ## windrose insert
  ## lattice plot will start a new page, no matter what -- use temp file
  ## lattice plot is the best-looking amongst the ggplot, base-plot and lattice options
  ## all attempts to place graphics at the appropriate position in base-plot
  ## or Hmisc::subplot or using par() failed -> tmp file
  require ("openair")
  hmrS <- hmr
  hmrS$date <- hmrS$datetimestamp

  iMonth <- 12 # picked as an interesting example for year X

  hmrS <- subset (hmr, (year < 2020)&month %in% c(iMonth))
  hmrS$date <- as.POSIXct(hmrS$datetimestamp)
  hmrS$yClass <- factor (ifelse (hmrS$year < currentYear
                                 , paste ("average", month.abb [iMonth])
                                 , paste (month.abb [iMonth], currentYear)))
  rm (iMonth) # arbitrarily pick a month to tell the story
  windR <- function (){
    wR <- windRose (hmrS, ws = "wspd", wd = "wdir"
                    , type = "yClass"
                    #, type = c("season") #, "yClass")
                    , auto.text = FALSE, paddle = FALSE, annotate = FALSE
                    , breaks = c (0, 10, 20, 30, 60)
                    , key.footer = "knots"
                    , grid.line = 10 #list (value = 10, lty = 5, col = "purple")
                    #  , statistic = "prop.mean"
                    #, max.freq = 30
    )
    print (wR); rm (wR)
  }
  #  par (fig = c(0.25,0.5,0.75,1))

  tF <- tempdir()
  # postscript (paste0 (tF, "ltc.ps"), width = 9,height = 6, paper = "special")
  ## PostScriptTrace (file, outfile, ....)   ## XXX do this eventually XXX
  # readPicture or grImport
  # see https://cran.r-project.org/web/packages/grImport/vignettes/import.pdf
  png (paste0 (tF, "ltc.png"), width = 11*200,height = 6*200, res = 400)
  #  par (mar = c())
  windR ()
  dev.off()
  require ("png")
  img2 <- readPNG (paste0 (tF, "ltc.png"))
  unlink(tF, recursive = TRUE); rm (tF)
  rasterImage (img2, xleft = 60, ybottom = 8.3
               , xright = 340, ytop = 14.8, interpolate = FALSE)
  rm (img2)
  #rm (xGrez, pCo)
}

par (oP)# reset to original plotting geometry
## plot lines AFTER windrose to be able to wrap tigher around white corners of inserted plot
with (tDay, addGraphs (longMean = smoothWindMA, percL = lowPerMA, percU = uppPerMA
                       , current = cbind (p365ma, c365ma)
                       , jday = jday
                       , currentCol = currentCol
))

## add gale pictogram into this graph (in margin or within?)
# with (subset (tDay, p365galDay > 0),
#       text (jday, rep (c (5.9, 7.1), length.out = length (jday)), labels = p365wCar))
with (subset (tDay, (p365scaDay > 0)&(!p365galDay >0)),
      text (jday, p365ma + 0.5, labels = p365wCar, srt = 0, cex = 0.8))
# with (subset (tDay, p365galDay > 0), text (jday, 5.8, labels = p365wCar))
require ("png")
hgt <- 0.7; wdh <- 15
img <- readPNG("~/myDocs/amyfiles/NOAA-LCI/pictograms/cloud.png")
with (subset (tDay, p365galDay > 0), rasterImage (img, xleft = jday-9, ybottom = p365ma + 1.5
                                                  , xright = jday-9+wdh, ytop = p365ma + 1.5 + hgt
                                                  #                                                 , angle = p365wdir+ 90
                                                  ## rotation would be desirable -- but rotates around bottom-left point -- would need compensation
)
)
with (subset (tDay, p365galDay > 0), text (jday, p365ma + 1.8, labels = p365wCar, cex = 0.6))

## legend
bP <- cLegend ("bottomleft", qntl = qntl [1], inset = 0.02
               , currentYear = currentYear
               , mRange = c(min (hmr$year), currentYear - 1)
               , cYcol = currentCol
               , title = paste (maO, "day moving average"))
## legend for gale clouds in other corner
text (365, 5.3, paste0 ("N,E,S,W  gale (>", galeT, " knots)"), pos = 2)
rasterImage (img, xleft = 280, xright = 280+wdh, ybottom = 5.6, ytop = 5.6+hgt)
text (365, 5.8, paste0 ("storm (>", stormT, " knots)"), pos = 2)
par (oP)

dev.off()
rm(hgt, wdh, bP, img)


## start-over/add windrose
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wind2.RData")

# if (!require ("openair")){
# require("devtools") ## needs Rtools -- which needs VPN
# install_github('davidcarslaw/openair')
# }

# openair:windRose -- lattice-plot; struggling with type for class other than times
require ("openair")
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/dayBreeze.pdf", width = 9, height = 6)
# hmrS <- subset (hmr, (month %in% c(1,2,3,12))) # Vincent!
# hmrS$yClass <- factor (ifelse (hmrS$datetimestamp < as.POSIXct ("2019-03-01"), "mean", "2019/20"))
for (i in 1:12){
hmrS <- subset (hmr, year < 2020)
hmrS <- subset (hmrS, month == i)
hmrS$date <- as.POSIXct(hmrS$datetimestamp)
hmrS$yClass <- factor (ifelse (hmrS$year < currentYear, paste0 ("mean-", i)
                               , paste0 (currentYear, "-", i)))

windRose(hmrS, ws = "wspd", wd = "wdir"
         , type = "yClass"
         , auto.text = TRUE, paddle = FALSE, annotate = TRUE
         , breaks = c (0, 5, 10, 15,20,30,60)
#         , breaks = c (0, 15,20,30, 40, 60)
         , key.footer = "knots"
         #, max.freq = 30
)
}
dev.off()

pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/winterStorms.pdf", width = 9, height = 6)
# hmrS <- subset (hmr, (month %in% c(1,2,3,12))) # Vincent!
  hmrS <- subset (hmr, year < 2020)
  hmrS <- subset (hmrS, month == 12)
  hmrS$date <- as.POSIXct(hmrS$datetimestamp)
  hmrS$yClass <- factor (ifelse (hmrS$datetimestamp < as.POSIXct ("2019-03-01"), "mean", "2019/20"))
  windRose(hmrS, ws = "wspd", wd = "wdir"
           , type = "yClass"
           , auto.text = TRUE, paddle = FALSE, annotate = TRUE
           , breaks = c (0, 5, 10, 15,20,30,60)
           #         , breaks = c (0, 15,20,30, 40, 60)
           , key.footer = "knots"
           #, max.freq = 30
  )
dev.off()


hmrS <- subset (hmr, year < 2020)
hmrS$date <- as.POSIXct(hmrS$datetimestamp)
hmrS$yClass <- factor (ifelse (hmrS$year < currentYear, "average", currentYear))

pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/windRose.pdf", width = 9, height = 6)
windRose(hmrS, ws = "wspd", wd = "wdir"
         # , type = "yClass"
         , type = c("season", "yClass")
#         , type = c("month", "yClass")
         , auto.text = TRUE, paddle = FALSE, annotate = FALSE
          , breaks = c (0, 10, 20, 25,30,50)
         , key.footer = "knots"
         #         , breaks = c(0,20, 30, 40)
         #, max.freq = 30
)
dev.off()




### base-graphics windrose
dM <- subset (dMeans, jday >= 335) # 1 Dec = 335

windR <- function (subsV, ...){
  require ("climatol")
  wndfr <- with (subset (dM, subsV)
                 , table (cut (wspd, breaks = c(0,3,6,9,60), include.lowest = TRUE)
                          , cDir (wdir, nDir = 16)))
  # convert table to data.frame so rosavent will accept it
  wndfr <- reshape (as.data.frame(wndfr), timevar = "Var2", idvar = "Var1", direction = "wide")
  row.names (wndfr) <- wndfr$Var1
  wndfr <- wndfr [,2:ncol (wndfr)]
  names (wndfr) <- gsub ("Freq.", "", names (wndfr))
  rosavent (as.data.frame (wndfr), uni = vUnit, key = TRUE, flab = 1)
}
# require("Hmisc") # subplot incompatible with layout() :(
# spSi <- 1.7
# subplot (windR (dM$year < currentYear)
#          , x = 160, y = 12.8, vadj = 1, size = c(spSi, spSi))
# text (160, 13, "mean Dec")
# subplot (windR (dM$year == currentYear)
#          , x = 260, y = 12.8, vadj = 1, size = c (spSi, spSi)) #, main = currentYear)
# text (260, 13, paste ("Dec", currentYear))
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/windRoseBase.pdf", width = 9, height = 6)
par (mfrow = c(1,2))
windR (dM$year < currentYear)
windR (dM$year == currentYear)
dev.off()


# library(devtools)
# install_github("tomhopper/windrose")   ## not that pretty -- looks like ggplot2
# require ("windrose")
# data(wind_data)
# wind_rose <- windrose(wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
# plot(wind_rose)

# require ("clifro")  # builds on ggplot2
# example (windrose)




##################################
## Walter Leith climate diagram ##
##################################

## monthly temp and precip
climD <- function (cDF){
  ymC <- aggregate (atemp~year+month, cDF, FUN = max, na.rm = TRUE)
  ymC$minT <- aggregate (atemp~year+month, cDF, FUN = min, na.rm = TRUE)$atemp
  rain <- aggregate (totprcp~year+month, cDF, FUN = sum, na.rm = TRUE)
  ymC$precip <- rain$totprcp [match (paste (ymC$year, ymC$month), paste (rain$year, rain$month))]
  monthC <- aggregate (precip~month, ymC, mean, na.rm = TRUE)
  monthC$maxT <- aggregate (atemp~month, ymC, mean, na.rm = TRUE)$atemp
  monthC$minT <- aggregate (minT~month, ymC, FUN = mean, na.rm = TRUE)$minT
  monthC$absMin <- aggregate (atemp~month, cDF, FUN = min, na.rm = TRUE)$atemp
  monthT <- t (monthC [,2:ncol (monthC)])
  colnames (monthT) <- month.abb
  monthT
}

# png ("~/tmp/LCI_noaa/media/climateDiag.png", width = 480, height = 960)
# par (mfrow = c(2,1))
pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/climateDiag.pdf")
hmrC <- subset (hmr, year < currentYear)
## alternative: wldiag in dgolicher/giscourse on github
## for same but with more customization, see library (iki.dataclim)
require ("climatol")
diagwl(climD (hmrC)
       , est = "Homer Spit"
       , per = paste (range (subset (hmrC, !is.na (totprcp))$year), collapse = "-")
)
diagwl (climD (subset (hmr, year == currentYear))
               , est = "Homer Spit"
               , per = currentYear)
dev.off()



cat ("Finished windTrend.R\n")
# EOF
