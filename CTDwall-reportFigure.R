## replot CTD wallpaper for office
## provide line-graph alternatives

## need to manually change month range and re-run for the different pages


rm (list = ls())
## get bathymetry, standard colors, and data ranges
load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R


test <- TRUE
## EVOS final GulfWatch Report
## plot 2017-2021, March, July, October, December


startDate <- "2000-01-01" # default; all data
stopatDate <- Sys.time()
startDate <- "2017-01-01"
stopatDate <- "2022-01-01"

month.select <- c (3, 7)
month.select <- c (10, 12)
# month.select <- c(3,7,10,12)  ## have to run this manually, one at a time, for now
cat ("\n\n\n##\n##\n## Manually switch over to other set of months! XXX \n##\n##\n##\n##")
year.select <- levels (poAll$year)
year.select <- 2017:2021


## add AlongBay-short transect as a new virtual transect
levels (poAll$Transect) <- c (levels (poAll$Transect), "ABext")


if (test) {
  oceanvarC <- seq_along(oVarsF) #
  oceanvarC <- 8
  oceanvarC <- c (1, 2)
  # oceanvarC <- seq_along(oVarsF)
  transectC <- seq_along(levels (poAll$Transect))
  transectC <- 6 # AlongBay
} else {
  oceanvarC <- seq_along(oVarsF)
  transectC <- seq_along(levels (poAll$Transect)) # by transect. 5: T9
  # transectC <- c(5,6,7)  ## T9, AB, ABext
}




################################
## enf of user configurations ##
################################

if (class (stopatDate)[1] == "character") {stopatDate <- as.Date(stopatDate)}
if (class (startDate)[1] == "character") {startDate <- as.Date(startDate)}
source ("CTDsectionFcts.R")
dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", showWarnings = FALSE, recursive = TRUE)

if (!exists ("useSF")) {useSF <- FALSE}  ## should have useSF from CTDwall-setup.R
# useSF <- TRUE
mnthly <- c ("9", "4", "AlongBay")  ## for which transects to produce 12x n-year plots


if (0) { ## tests
  levels (factor (subset (poAll, year == 2012)$DateISO))
  xC <- subset (poAll, (Transect == "9") & (DateISO == "2019-09-16"))
  # xC <- subset (poAll, (Transect == "3")&(DateISO == "2012-03-14"))
  xCo <- sectionize (xC)
  plot (xCo)
  pSec (xCo, 1, zcol = oCol [[1]])
  pSec (xCo, 1, zcol = turbo (10), custcont = c(10, 11, 11.1))
  rm (xC, xCo)
}



####################################
##                                ##
## start of the big plotting loop ##
##                                ##
####################################

for (ov in oceanvarC) {  # ov = OceanVariable (temp, salinity, etc)
  for (tn in transectC) {  # tn: transect
    ## for testing
    ## ov <- 1; tn <- 6 ## AlongBay
    ## ov <- 1; tn <- 2
    cat ("\n\n", oVarsF [ov], " T-", levels (poAll$Transect)[tn], "\n")

    ## doubly-used stations:
    ## make this a function?
    if (levels (poAll$Transect)[tn] == "ABext") {
      swMN <- c ("4_3", "9_6", "6_2", "7_22")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "ABext"
      stn$Line [stn$Match_Name %in% swMN] <- "ABext"
    }
    if (levels (poAll$Transect)[tn] == "4") {
      swMN <- c("4_3")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "4"
      stn$Line [stn$Match_Name %in% swMN] <- "4"
    }
    if (levels (poAll$Transect)[tn] == "9") {
      swMN <- c("9_6")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "9"
      stn$Line [stn$Match_Name %in% swMN] <- "9"
    }
    if (levels (poAll$Transect)[tn] == "AlongBay") {
      swMN <- c (paste ("AlongBay", 1:13, sep = "_"), "4_3", "9_6")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "AlongBay"
      stn$Line [stn$Match_Name %in% swMN] <- "AlongBay"
    }

    ## to use as a reference for partial stations
    ## and for bathymetry profile
    ## turn this into a function?
    stnT <- subset (stn, stn$Line == levels (poAll$Transect)[tn])

    lati <- seq (min (stnT$Lat_decDegree), max (stnT$Lat_decDegree), length.out = 1000)
    loni <- suppressWarnings(approx (stnT$Lat_decDegree, stnT$Lon_decDegree, lati, rule = 2)$y)
    Require ("oce")
    dist <- rev (geodDist (longitude1 = loni, latitude1 = lati, alongPath = TRUE)) # [km] -- why rev??
    sect <- data.frame (loni, lati, dist); rm (loni, lati, dist)

    ## extract from bathyZ. then fill-in the missing values from get.depth
    if (useSF) {
      Require ("sf")
      sect <- st_as_sf(sect, coords = c("loni", "lati"))
      sf::st_crs(sect) <- 4326  ## WGS84 definition
      Require ("stars")
      sectP <- sf::st_transform(sect, st_crs (bathyZ))
      bottomZ <- stars::st_extract(bathyZ, at = sectP)$w001001.adf
    } else {
      Require ("sp")
      Require ("raster")  ## spTransform loaded from wrong package otherwise, leading to crash!
      coordinates (sect) <- ~ loni + lati
      proj4string(sect) <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
      sectP <- spTransform(sect, CRS (proj4string(bathyZ))) # fails if raster is not loaded first
      bottomZ <- raster::extract (bathyZ, sectP, method = "bilinear") * -1
    }
    Require ("marmap")
    ## fill-in T6/AlongBay from NOAA raster that's missing in Zimmermann's bathymetry
    bottom <- marmap::get.depth (bathyNoaa, x = sect$loni, y = sect$lati, locator = FALSE) ## fails with useSF=TRUE: coord not found. marmap uses sp and raster! -- wait for marmap update!!
    bottom$depthHR <- ifelse (is.na (bottomZ), bottom$depth, bottomZ)
    rm (sect, sectP, bottomZ)

    ## select transect, year, classify monthly/seasonal survey
    physOcY <- subset (poAll, Transect == levels (poAll$Transect)[tn])
    physOcY$year <- factor  (physOcY$year)
    physOcY$month <- factor (format (physOcY$DateISO, "%m"))
    physOcY$season <- seasonize (physOcY$month)
    physOcY$DateISO <- as.Date (physOcY$DateISO)
    pStash <- physOcY
    physOcY <- pStash
    physOcY <- subset (physOcY, (startDate < DateISO) & (DateISO < stopatDate))
    if (exists ("month.select")) {
      physOcY <- subset (physOcY, as.numeric (levels (physOcY$month)[physOcY$month]) %in% month.select)
      physOcY$month <- factor (physOcY$month)
    }


    ## set-up page size for large poster-PDF
    ### monthly or quarterly samples -- by transect. 9, 4, AlongBay = monthly
    if (exists ("year.select")) {
      pH <- 9.0; pW <- 7
      yearPP <- length (year.select)
      omcex <- 1
      Require ("stringr")
      sampleTimes <- str_pad (month.select, 2, pad = "0")
      physOcY$smplIntvl <- physOcY$month
      layoutM <- matrix (1:10, 5, byrow = TRUE) ## any way to automate this? -- XXX still needs work!
      omText <- month.name [month.select]
      # rescale colors to cover only this figure
      oRange <- t (sapply (c ("Temperature_ITS90_DegC"
        , "Salinity_PSU"
        , "Density_sigma.theta.kg.m.3"
        , "turbidity" # , "logTurbidity"
        , "Fluorescence_mg_m3"
        , "logPAR"
        , "Oxygen_umol_kg"
        , "bvf"
      )
      , FUN = function(vn) {range (physOcY [, which (names (physOcY) == vn)], na.rm = TRUE)
      }))


    } else if (levels (poAll$Transect)[tn] %in% mnthly) {
      ## monthly
      pH <- 21.25; pW <- 42  # 42 inch = common plotter size. FWS has 44 inch HP DesignJet Z5600
      ## pH <- 44; pW <- 88     # FWS plotter, but paper is 42 inch
      pH <- 42; pW <- 84     # FWS paper is 42 inches wide -- BIG version
      pH <- 32; pW <- 42  ## full-width version -- Small version of T9/AlongBay

      yearPP <- 11 # years (rows) per page
      omcex <- 2   # size of mtext annotations
      Require ("stringr")
      sampleTimes <- str_pad (1:12, 2, pad = "0")
      physOcY$smplIntvl <- physOcY$month
      nY <- as.numeric (format (stopatDate, "%Y")) - as.numeric (format (startDate, "%Y")) + 1
      nY <- yearPP
      layoutM <- matrix (1:(12 * nY), nY, byrow = TRUE) # across, then down
      omText <- month.name
      rm (nY)
    } else {
      ## quarterly
      pH <- 8.5; pW <- 14    # legal size
      yearPP <- 5
      omcex <- 1
      sampleTimes <- levels (physOcY$season)
      physOcY$smplIntvl <- physOcY$season
      layoutM <- matrix (1:16, 4, byrow = TRUE)
      omText <- sampleTimes
    }

    dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall-likeFigures/"
      , showWarnings = FALSE, recursive = TRUE)
    # pdf (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall-likeFigures/"
    #              , oVarsF [ov], " T-", levels (poAll$Transect)[tn], ".pdf")
    #      , height = pH, width = pW)
    res = 300
    png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall-likeFigures/"
      , oVarsF [ov], " T-", levels (poAll$Transect)[tn], "%01d.png")
    , height = pH * res, width = pW * res, res = res); rm (res)
    layout (layoutM); rm (layoutM)
    # if (pH > 14){
    # par (oma=c(3,5,12,2)
    #      , mar=c(4,4,3,0.1)
    # )
    # }else {
    par (oma = c(3, 4, 3, 1)
      # , mar=c(0.1,4,0.0,0)
    )
    # }

    ## test-option
    yearC <- seq_along(year.select)  ## or fix subset below
    for (iY in yearC) {
      ## for testing:
      # iY <- 7 # pick 2018
      # iY <- 2
      physOc <- subset (physOcY, year == year.select [iY])


      ## replace transDate from above!
      ## also making surveyW redundant
      physOc$transDate <- with (physOc, factor (paste0 ("T-", Transect, " ", year, "-", smplIntvl)
        , levels = paste0 ("T-", Transect [1], " ", year [1], "-"
          , sampleTimes)))

      ## define and plot sections
      ## turn this into a function to use in section plots as well
      cat ("   ",  formatC (iY, width = 3), "/", length (levels (physOcY$year))
        , " Sections/year:", length (levels (physOc$transDate)), "-- ")


      ## already defined surveys in CTDwall-setup.R -- use physOc$survey
      #      if (test){sL <- 1:5}else{sL <-  seq_along(levels (factor (physOc$transDate)))}
      sL <-  seq_along(levels (factor (physOc$transDate)))
      # sL <-  seq_along(levels (physOc$transDate))
      for (iS in sL) {              # cycle through individual survey
        # iS <- 1  # for testing
        cat (iS, " ")
        xC <- subset (physOc, transDate == levels (physOc$transDate)[iS])
        if (length (levels (factor (xC$Match_Name))) < 2) {
          ## if a scheduled survey was not done, plot a blank placeholder in the panel instead
          ## unless the scheduled survey is in the future from runtime (do nothing then)
          inFuture <- as.numeric (as.character (physOc$year))[1] >=
            as.numeric (format (stopatDate, "%Y")) &&
            iS / length (sL) > as.numeric (format (stopatDate, "%m")) / 12
          if (!inFuture) {
            nS <- length (levels (factor (xC$Match_Name)))
            plot (0:10, type = "n", axes = FALSE, xlab = "", ylab = ""   ## XXX in here or in CTDsectionFcts.R? XXX
              , main = paste0 (levels (physOc$transDate)[iS], ": ", nS
                , ifelse (nS > 1, " stations", " station")
            ))
            rm (nS)
          }
          rm (inFuture)
        } else {
          if (0) {  ## combine all casts in survey window (watch RAM!) -- MUCH faster!
            ## or pick out long survey within X days?
            nSurv <- 1
          } else {
            ## check whether there is more than one survey per survey-interval

            if (test) {save.image ("~/tmp/LCI_noaa/cache/wallCache1.RData")}
            # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wallCache1.RData"); source ("CTDsectionFcts.R")

            ## allow x-day window to make up a multi-day composite transect
            ## better to apply to allPo?
            ## make this a function for all data? -> move to CTDwall-setup.R / datasetup?

            xC$survey <- factor (xC$survey)
            nSurv <- length (levels (xC$survey))  ## used again below to mark plots
            if (nSurv > 1) {
              ## use the survey with the most stations
              nS <- sapply (levels (xC$survey), FUN = function(x) {
                length (levels (factor (subset (xC$Station, xC$survey == x))))
              })
              xC <- subset (xC, survey == levels (xC$survey)[which.max (nS)])
              rm (nS)
            }

            # remove duplicate stations (shared positions)
            ## any duplicated stations? -- if so, keep the closest in time
            posF <- factor (xC$longitude_DD * 10e6 - xC$latitude_DD)
            xC$File.Name <- factor (xC$File.Name)
            if (length (levels (xC$File.Name)) != length (levels (posF))) {
              ## there are duplicate stations
              ## identify duplicate file names; remove time outlier
              nFN <- sapply (seq_along(levels (posF)), function(iQ) {
                length (unique (subset (xC, posF == levels (posF)[iQ])$File.Name))
              })
              mT <- mean (xC$isoTime)

              for (iQ in seq_along(levels (posF))) {
                sec <- subset (xC, posF == levels (posF)[iQ])
                sec$File.Name <- factor (sec$File.Name)
                if (nFN [iQ] > 1) {
                  bF <- sapply (seq_along(levels (sec$File.Name)), function(iQ) {
                    cx <- subset (sec, File.Name == levels (sec$File.Name)[iQ])
                    # qal <- nrow (cx)
                    qal <- 1 / abs (as.numeric (difftime (mT, cx$isoTime [1], "minutes")))
                    # cx$File.Name [1]
                    qal
                  })
                  ## pick the best cast
                  sec <- subset (sec
                    , File.Name == levels (sec$File.Name)[which.max (bF)])
                }
                if (iQ == 1) {
                  nSec <- sec
                } else {
                  nSec <- rbind (nSec, sec)
                }
              }
              xC <- nSec
              rm (iQ, nFN, posF, nSec, sec, bF)
            }
          }

          ## arrange ctd data into sections
          ## define section -- see oce-class "section"
          if (test) {save.image ("~/tmp/LCI_noaa/cache/wallCache.RData")}
          ## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wallCache.RData"); source ("CTDsectionFcts.R")

          ##
          ## construct, pad, and sort section
          ##
          xCo <- sectionize (xC)

          ## sectionPad to plot incomplete sections
          xCo <- sectionPad (sect = xCo, transect = data.frame (station = stnT$Match_Name
            , line = stnT$Line
            , latitude = stnT$Lat_decDegree
            , longitude = stnT$Lon_decDegree
            , bottom = stnT$Depth_m))
          ## sectionSort--is now in sectionPad. Still need to do same to bottom
          if (xC$Transect [1] == "AlongBay") {
            bottom <- bottom [order (bottom$lat, decreasing = FALSE), ]
          } else if (xC$Transect [1] %in% c("4", "9")) {
            bottom <- bottom [order (bottom$lat, decreasing = TRUE), ]
          } else {
            bottom <- bottom [order (bottom$lon), ]
          }
          bottom$dist <- with (bottom, geodDist (longitude1 = lon, latitude1 = lat, alongPath = TRUE)) # [km]

          ## test, QAQC
          if (0) {
            sapply (seq_along(xCo@data$station), function(i) {
              #  xCo@data$station[[i]]@data$temperature
              xCo@data$station[[i]]@metadata$stationId
              # xCo@data$station[[i]]@metadata$waterDepth
            })
          }
          # plot (subset (xC, Match_Name == "9_10")$Temperature_ITS90_DegC)

          ##
          ## plot the section/transect
          ##
          ## if (ov == 2){zB <- c (28, seq (30, 33, 0.2))}else{zB <- NULL} ## fudge salinity colors
          if (oVarsF [ov] == "bvf") {cCont <- NULL} else {cCont <- pretty (oRange [ov, ], 20)}
          pSec (xCo, N = oVarsF [ov]
            , xlab = "", ylab = ""
            , zCol = oCol3 [[ov]]
            , zlim = oRange [ov, ] # fixes colors to global range of that variable
            # , zbreaks=zB # better?, slower interpolation
            # , custcont = pretty (oRange [ov,], 20)  ## may often fail? -- no contours in range
            , ylim = c(0, max (physOcY$Depth.saltwater..m., na.rm = TRUE) + 5)  ## need to fix CTDwall-setup.R first
            , showBottom = FALSE
            , drawPalette = FALSE
            , custcont = cCont
            , plotContours = FALSE
            , mar = c(1, 3, 2, 0)
          )
          tgray <- rgb (t (col2rgb ("lightgray")), max = 255, alpha = 0.5 * 255) ## transparent
          with (bottom, polygon(c(min (dist), dist, max(dist))
            , c(10000, -depthHR, 10000)
            , col = tgray))
          rm (tgray)




          # keep longest section for map
          if (!exists ("xMap")) {xMap <- xCo}
          if (length (xCo@data$station) > length (xMap@data$station)) {
            xMap <- xCo
          }
          rm (xCo, nSurv)
        }
      }
      ## covering yearPP years per page. Write out at end of each year
      if (iY == 1) { ## big title on top
        # mtext (text = oVars [ov], side=3, cex=4, outer=TRUE, line=3)  ## always?
        ## unified caption at the bottom
        mtext (text = "Along-track Distance [km]", outer = TRUE, line = 1.4, side = 1)
        mtext (text = "Depth [m]", side = 2, line = -0.6, outer = TRUE)
      }          ## addBorder (xCo, TD[ov]-1)

      mtext (text = year.select[iY]
        , side = 2, line = 1.4, outer = TRUE, cex = omcex
        , at = 1 - ((iY - 1) %% yearPP) / yearPP - 0.5 / yearPP
      )
      for (n in seq_along(omText)) {
        mtext (text = omText [n], side = 3, line = -0.5, outer = TRUE, cex = omcex
          , at = (n - 1) / length(omText) + 0.5 / length(omText))
      }
      mtext (text = oVars [ov], side = 3, line = 1.2, outer = TRUE, cex = omcex)
      cat ("\n")
    }

    ##############################################################
    ## add color scale and map after all section plots are done ##
    ##############################################################

    ###############################################
    ## draw palette, color scale into next panel ##
    ###############################################

    nCol <- 100
    t.ramp <- oCol3[[ov]](nCol)
    if (pH > 30) {
      yL <- 1.8
      par (mar = c(14, 1, 3, 1))
    } else {
      yL <- 5.0  # 0.8: too tall/broad
      par (mar = c(5, 1, 3, 12))
    }
    bp <- barplot (rep (1, nCol), axes = FALSE, space = 0, col = t.ramp, border = NA
      , ylim = c(0, yL)  # ylim to make bar narrower, less high
    )
    title (main = oVars [ov], cex = 3, line = 0.5)
    lVal <-  pretty (c (oRange [ov, 1], oRange [ov, 2]))
    axis (1, at = (lVal - oRange [ov, 1]) / (oRange [ov, 2] - oRange[ov, 1]) * nCol
      , labels = lVal, lwd = 0)
    rm (bp, lVal, nCol, yL)

    ## add date and logos for reference
    # mtext (text=paste ("NOAA Kasitsna Bay Lab and KBNERR\n", Sys.Date())
    #        , side=1, line=6, outer=FALSE, cex=0.7, adj=1)
    ## add NCCOS and KBNERR logos

    ### end of wall poster
    dev.off()
    cat ("\n")
  }
}


physOc <- poAll
rm (tn, oVars, oVarsF, ov, poAll, pSec, physOcY, yearC, iY, sL, iS, oceanvarC, transectC)
rm (swMN)






## for error checking: map of every transect
# double-used plots may appear out-of-line in chronology
## this should come at the end of CTDwall-setup.R

cat ("\n# END CTDwall.R #\n")
# EOF
