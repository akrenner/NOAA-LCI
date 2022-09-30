## replot CTD wallpaper for office
## provide line-graph alternatives




rm (list = ls())
## get bathymetry, standard colors, and data ranges
load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R


test <- TRUE
test <- FALSE


## problems:
## - fluorescence missing (all values NA), e.g. T-3 2012-05-02
## - contours fail, e.g. temperature, T-4 (2), 2019-05-14

## PAR: flag night; mark 1% light level contour
## fix distancescale to full transect
## Kris: check on surface PAR and salinity measurements

## 2021-08-03 -- issues
# x fix color scale across all graphs (across Transects as well?)

## 9-10 2012-5:  double cast?
## T-3 2012-winter: stations mislabled/out of order? 3_4 should be 3_7?

## color scales: make custom breaks to show more details


## decisions made:
# if more than 1 survey per survey-window, plot the longest section


stopatDate <- "2022-07-31"
# stopatDate <- Sys.time()


## add AlongBay-short transect as a new virtual transect
levels (poAll$Transect) <- c (levels (poAll$Transect), "ABext")


if (test){
  oceanvarC <- 1:length (oVarsF) #
  oceanvarC <- 8
 # oceanvarC <- c (4,8)
  oceanvarC <- 1:length (oVarsF)
  transectC <- 1:length (levels (poAll$Transect))
  transectC <- 6
}else{
  oceanvarC <- 1:length (oVarsF)
  transectC <- 1:length (levels (poAll$Transect))# by transect. 5: T9
  # transectC <- c(5,6,7)  ## T9, AB, ABext
}




################################
## enf of user configurations ##
################################

if (class (stopatDate)[1]=="character"){stopatDate <- as.POSIXct(stopatDate)}
source ("CTDsectionFcts.R")
dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", showWarnings = FALSE, recursive = TRUE)

if (!exists ("useSF")){useSF <- FALSE}  ## should have useSF from CTDwall-setup.R
mnthly <- c ("9", "4", "AlongBay")  ## for which transects to produce 12x n-year plots


if (0){ ## tests
  levels (factor (subset (poAll, year == 2012)$DateISO))
  xC <- subset (poAll, (Transect == "9")&(DateISO == "2019-09-16") )
  # xC <- subset (poAll, (Transect == "3")&(DateISO == "2012-03-14"))
  xCo <- sectionize (xC)
  plot (xCo)
  pSec (xCo, 1, zcol = oCol [[1]])
  pSec (xCo, 1, zcol = turbo (10), custcont = c(10, 11, 11.1))
  rm (xC, xCo)
}



for (ov in oceanvarC){  # ov = OceanVariable (temp, salinity, etc)
  for (tn in transectC){  # tn: transect
    ## for testing
    ## ov <- 1; tn <- 6 ## AlongBay
    ## ov <- 1; tn <- 2
    cat ("\n\n", oVarsF [ov], " T-", levels (poAll$Transect)[tn], "\n")

    ## doubly-used stations:
    ## make this a function?
    if (levels (poAll$Transect)[tn] == "ABext"){
      swMN <- c ("4_3", "9_6", "6_2", "7_22")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "ABext"
      stn$Line [stn$Match_Name %in% swMN] <- "ABext"
    }
    if (levels (poAll$Transect)[tn] == "4"){
      swMN <- c("4_3")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "4"
      stn$Line [stn$Match_Name %in% swMN] <- "4"
    }
    if (levels (poAll$Transect)[tn] == "9"){
      swMN <- c("9_6")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "9"
      stn$Line [stn$Match_Name %in% swMN] <- "9"
    }
    if (levels (poAll$Transect)[tn] == "AlongBay"){
      swMN <- c (paste ("AlongBay", 1:13, sep="_"), "4_3", "9_6")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "AlongBay"
      stn$Line [stn$Match_Name %in% swMN] <- "AlongBay"
    }

    ## to use as a reference for partial stations
    ## and for bathymetry profile
    ## turn this into a function?
    stnT <- subset (stn, stn$Line == levels (poAll$Transect)[tn])

    lati <- seq (min (stnT$Lat_decDegree), max (stnT$Lat_decDegree), length.out = 1000)
    loni <- suppressWarnings(approx (stnT$Lat_decDegree, stnT$Lon_decDegree, lati, rule=2)$y)
    Require ("oce")
    dist <- rev (geodDist (longitude1=loni, latitude1=lati, alongPath=TRUE)) # [km] -- why rev??
    sect <- data.frame (loni, lati, dist); rm (loni, lati, dist)

    ## extract from bathyZ. then fill-in the missing values from get.depth
    if (useSF){
      Require ("sf")
      sect <- st_as_sf(sect, coords=c("loni", "lati"))
      sf::st_crs(sect) <- 4326  ## WGS84 definition
      Require ("stars")
      sectP <- sf::st_transform(sect, st_crs (bathyZ))
      bottomZ <- stars::st_extract(bathyZ, at=sectP)$w001001.adf
    }else{
      Require ("sp")
      Require ("raster")  ## spTransform loaded from wrong package otherwise, leading to crash!
      coordinates (sect) <- ~loni+lati
      proj4string(sect) <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
      sectP <- spTransform(sect, CRS (proj4string(bathyZ))) # fails if raster is not loaded first
      bottomZ <- raster::extract (bathyZ, sectP, method="bilinear")*-1
    }
    Require ("marmap")
    ## fill-in T6/AlongBay from NOAA raster that's missing in Zimmermann's bathymetry
    bottom <- marmap::get.depth (bathyNoaa, x=sect$loni, y=sect$lati, locator=FALSE) ## fails with useSF=TRUE: coord not found. marmap uses sp and raster! -- wait for marmap update!!
    bottom$depthHR <- ifelse (is.na (bottomZ), bottom$depth, bottomZ)
    rm (sect, sectP, bottomZ)

    ## select transect, year, classify monthly/seasonal survey
    physOcY <- subset (poAll, Transect == levels (poAll$Transect)[tn])
    physOcY$year <- factor  (physOcY$year)
    physOcY$month <- factor (format (physOcY$DateISO, "%m"))
    physOcY$season <- seasonize (physOcY$month)


    ## set-up page size for large poster-PDF
    ### monthly or quarterly samples -- by transect. 9, 4, AlongBay = monthly
    if (levels (poAll$Transect)[tn] %in% mnthly){
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
      nY <- as.numeric (format (stopatDate, "%Y")) - min (as.integer (levels (poAll$year))) + 1
      nY <- yearPP
      layoutM <- matrix (1:(12*nY), nY, byrow = TRUE) # across, then down
      omText <- month.name
      rm (nY)
    }else{
      ## quarterly
      pH <- 8.5; pW <- 14    # legal size
      yearPP <- 4
      omcex <- 1
      sampleTimes <- levels (physOcY$season)
      physOcY$smplIntvl <- physOcY$season
      layoutM <- matrix (1:16, 4, byrow = TRUE)
      omText <- sampleTimes
    }


    pdf (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVarsF [ov]
                 , " T-", levels (poAll$Transect)[tn]
                 , ".pdf")
         , height = pH, width = pW)
    layout (layoutM); rm (layoutM)
    if (pH > 14){
    par (oma=c(3,5,12,2)
         , mar=c(4,4,3,0.1)
    )
    }else {
      par (oma=c(3,5,8,2)
           , mar=c(4,4,3,0.1)
      )
}
    if (0){ # test){
      par (oma=c(3,5,15,2)
           , mar=c(4,4,16,0.1)
      )
    }
    ## test-option
    yearC <- 1:length (levels (physOcY$year))  ## or fix subset below
    for (iY in yearC){
      ## for testing:
      # iY <- 7 # pick 2018
      # iY <- 2
      physOc <- subset (physOcY, year == levels (physOcY$year)[iY])


      ## replace transDate from above!
      ## also making surveyW redundant
      physOc$transDate <- with (physOc , factor (paste0 ("T-", Transect, " ", year, "-", smplIntvl)
                                                 , levels = paste0 ("T-", Transect [1], " ", year [1], "-"
                                                                    , sampleTimes)))

      ## define and plot sections
      ## turn this into a function to use in section plots as well
      cat ("   ",  formatC (iY, width = 3), "/", length (levels (physOcY$year))
           , " Sections/year:", length (levels (physOc$transDate)), "-- ")


      ## already defined surveys in CTDwall-setup.R -- use physOc$survey
      if (test){sL <- 1:5}else{sL <-  1:length (levels (physOc$transDate))}
      # sL <-  1:length (levels (physOc$transDate))
      for (iS in sL){              # cycle through individual survey
        # iS <- 2  # for testing
        cat (iS, " ")
        xC <- subset (physOc, transDate == levels (physOc$transDate)[iS])
        if (length (levels (factor (xC$Match_Name))) < 2){
          ## if a scheduled survey was not done, plot a blank placeholder in the panel instead
          ## unless the scheduled survey is in the future from runtime (do nothing then)
          inFuture <- as.numeric (as.character (physOc$year))[1] >=
            as.numeric (format (stopatDate, "%Y")) &&
            iS/length (sL) > as.numeric (format (stopatDate, "%m"))/12
          if (!inFuture){
            nS <- length (levels (factor (xC$Match_Name)))
            plot (0:10, type = "n", axes = FALSE, xlab = "", ylab = ""   ## XXX in here or in CTDsectionFcts.R? XXX
                  , main = paste0 (levels (physOc$transDate)[iS], ": ", nS
                                   , ifelse (nS > 1, " stations", " station")
                  ))
            rm (nS)
          }
          rm (inFuture)
        }else{
          if (0){  ## combine all casts in survey window (watch RAM!) -- MUCH faster!
            ## or pick out long survey within X days?
            nSurv <- 1
          }else{
            ## check whether there is more than one survey per survey-interval

            if (test){save.image ("~/tmp/LCI_noaa/cache/wallCache1.RData")}
            # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wallCache1.RData"); source ("CTDsectionFcts.R")

            ## allow x-day window to make up a multi-day composite transect
            ## better to apply to allPo?
            ## make this a function for all data? -> move to CTDwall-setup.R / datasetup?

            xC$survey <- factor (xC$survey)
            nSurv <- length (levels (xC$survey))  ## used again below to mark plots
            if (nSurv > 1){
              ## use the survey with the most stations
              nS <- sapply (levels (xC$survey), FUN = function (x){
                length (levels (factor (subset (xC$Station, xC$survey == x))))
              })
              xC <- subset (xC, survey == levels (xC$survey)[which.max (nS)])
              rm (nS)
            }

            # remove duplicate stations (shared positions)
            ## any duplicated stations? -- if so, keep the closest in time
            posF <- factor (xC$longitude_DD*10e6-xC$latitude_DD)
            xC$File.Name <- factor (xC$File.Name)
            if (length (levels (xC$File.Name)) != length (levels (posF))){
              ## there are duplicate stations
              ## identify duplicate file names; remove time outlier
              nFN <- sapply (1:length (levels (posF)), function (iQ){
                length (unique (subset (xC, posF==levels (posF)[iQ])$File.Name))
              })
              mT <- mean (xC$isoTime)

              for (iQ in 1:length (levels (posF))){
                sec <- subset (xC, posF == levels (posF)[iQ])
                sec$File.Name <- factor (sec$File.Name)
                if (nFN [iQ] > 1){
                  bF <- sapply (1:length (levels (sec$File.Name)), function (iQ){
                    cx <- subset (sec, File.Name == levels (sec$File.Name)[iQ])
                    # qal <- nrow (cx)
                    qal <- 1/abs (as.numeric (difftime (mT, cx$isoTime [1], "minutes")))
                    # cx$File.Name [1]
                    qal
                  })
                  ## pick the best cast
                  sec <- subset (sec
                                 , File.Name == levels (sec$File.Name)[which.max (bF)])
                }
                if (iQ == 1){
                  nSec <- sec
                }else{
                  nSec <- rbind (nSec, sec)
                }
              }
              xC <- nSec
              rm (iQ, nFN, posF, nSec, sec, bF)
            }
          }

          ## arrange ctd data into sections
          ## define section -- see oce-class "section"
          if (test){save.image ("~/tmp/LCI_noaa/cache/wallCache.RData")}
          ## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wallCache.RData"); source ("CTDsectionFcts.R")

          ##
          ## construct, pad, and sort section
          ##
          xCo <- sectionize (xC)

          ## sectionPad to plot incomplete sections
          xCo <- sectionPad (sect=xCo, transect = data.frame (station=stnT$Match_Name
                                                              , line=stnT$Line
                                                              , latitude=stnT$Lat_decDegree
                                                              , longitude=stnT$Lon_decDegree
                                                              , bottom=stnT$Depth_m))
          ## sectionSort--is now in sectionPad. Still need to do same to bottom
          if (xC$Transect [1] == "AlongBay"){
            bottom <- bottom [order (bottom$lat, decreasing = FALSE),]
          }else if (xC$Transect [1] %in% c("4", "9")){
            bottom <- bottom [order (bottom$lat, decreasing = TRUE),]
          }else{
            bottom <- bottom [order (bottom$lon),]
          }
          bottom$dist <- with (bottom, geodDist (longitude1=lon, latitude1=lat, alongPath=TRUE)) # [km]

          ## test, QAQC
          if (0){
            sapply (1:length (xCo@data$station), function (i){
              #  xCo@data$station[[i]]@data$temperature
              xCo@data$station[[i]]@metadata$stationId
              #xCo@data$station[[i]]@metadata$waterDepth
            })
          }
          # plot (subset (xC, Match_Name == "9_10")$Temperature_ITS90_DegC)

          ##
          ## plot the section/transect
          ##
          ## if (ov == 2){zB <- c (28, seq (30, 33, 0.2))}else{zB <- NULL} ## fudge salinity colors
          if (oVarsF [ov]=="bvf"){cCont <- NULL}else{cCont<- pretty (oRange [ov,], 20)}
          pSec (xCo, N = oVarsF [ov]
                , zCol = oCol3 [[ov]]
                , zlim = oRange [ov,] # fixes colors to global range of that variable
                # , zbreaks=zB # better?, slower interpolation
                # , custcont = pretty (oRange [ov,], 20)  ## may often fail? -- no contours in range
                , ylim = c(0,max (physOcY$Depth.saltwater..m., na.rm=TRUE)+5)  ## need to fix CTDwall-setup.R first
                , showBottom=FALSE
                , drawPalette=FALSE
                , custcont=cCont
          )
          tgray <- rgb (t (col2rgb ("lightgray")), max=255, alpha=0.5*255) ## transparent
          with (bottom, polygon(c(min (dist), dist, max(dist))
                                , c(10000, -depthHR, 10000)
                                , col=tgray))
          rm (tgray)
          if (test){   ## for QAQC: add station labels to x-axis
            dist <- unique (xCo[['distance']])
            stnID <- sapply (1:length (xCo@data$station), function (m){
              xCo@data$station[[m]]@metadata$stationId
              #              xCo@data$station[[m]]@metadata$filename
            })
            try (axis (side=3, at=dist, labels=stnID, cex=0.2, las=2))
            rm (dist, stnID)
          }

          if (nSurv > 1){
            title (main = paste (levels (physOc$transDate)[iS], "* -", nSurv), col.main = "red")
          }else{
            # title (main = paste (levels (physOc$transDate)[iS]))
            title (main = paste0 (levels (physOc$transDate)[iS], "-"
                                  , format (mean (xCo@metadata$time, na.rm = TRUE), "%d")))
          }
          if (iY==1){ ## big title on top
            mtext (text = oVars [ov], side=3, cex=4, outer=TRUE, line=3)  ## always?
          }
          ## addBorder (xCo, TD[ov]-1)

          # keep longest section for map
          if (!exists ("xMap")){xMap <- xCo}
          if (length (xCo@data$station) > length (xMap@data$station)){
            xMap <- xCo
          }
          rm (xCo, nSurv)
        }
      }
      ## covering yearPP years per page. Write out at end of each year
      mtext (text=levels (physOcY$year)[iY]
             , side=2, line=1.0,outer=TRUE,cex=omcex
             , at=1-((iY-1)%%yearPP)/yearPP-0.5/yearPP
      )
      for (n in 1:length (omText)){
        mtext (text=omText [n], side=3, line=1.0, outer=TRUE,cex=omcex
               , at=(n-1)/length(omText)+0.5/length(omText))
      }
      cat ("\n")
    }

    ##############################################################
    ## add color scale and map after all section plots are done ##
    ##############################################################

    ## maps
    xLim <- c(-154, -151)
    yLim <- c(57.5, 60.1)
    plot (xMap
          , which = 99
          , coastline = "coastlineWorldFine" ## or a coastline object (from gshhg, removing dependency on ocedata)
          , showStations = TRUE
          , gird = TRUE
          , map.xlim = c(-154, -151)
          , map.ylim = c(57.5, 60.1)
          , clatitude = 59.4
          , clongitude = -152
          , span = 250
    )
    ## add eye for perspective;  save eye to eye.ps with Inkscape
    if (1){
      if (levels (poAll$Transect)[tn] == "3"){
        xU <- -152.5; yU <- 59.4; rU <- 60
      }else if (levels (poAll$Transect)[tn] == "4"){
        xU <- -152.8; yU <- 59.4; rU <- 0
      }else if (levels (poAll$Transect)[tn] == "6"){
        xU <- -151.5; yU <- 58.4; rU <- 115
      }else  if (levels (poAll$Transect)[tn] == "7"){
        xU <- -152.5; yU <- 58.7; rU <- 85
      }else if (levels (poAll$Transect)[tn] == "9"){
        xU <- -152.8; yU <- 59.0; rU <- 20
      }else{                         # AlongBay
        xU <- -150.5; yU <- 59.1; rU <- 130
      }

      if (0){  ## vector based -- not windows compatible and doesn't rotate
        Require ("grImport")  ## requires installation of GS -- go raster after all
        grImport::PostScriptTrace("pictograms/eye.ps", "pictograms/eye.ps.xml")
        p <- readPicture("pictograms/eye.ps.xml")
        unlink ("pictograms/eye.ps.xml")
        grid.picture (p  # no easy way to rotate p by n-degrees?; placed on page, not panel
                      , x=unit (xU, "npc"), y=unit (yU, "npc")
                      # , angle=rU
                      , width=unit (0.07, "npc"), height=unit (0.07, "npc")
        )
        ## read directly from SVG -- not working yet; better in MacOs, no advantage in Windows
        # if (.Platform$OS.type=="unix"){
        #   if (!require ("grConvert")){
        #     devtools::install_github("sjp/grConvert")
        #     library (grConvert)}
        #   grConvert::convertPicture ("pictograms/eye.svg", "pictograms/eye2.svg")
        # }
        # Require ("grImport2")
        # p <- grImport2::readPicture ("pictograms/eye2.svg")
        # g <- grImport2::pictureGrob(p)
        # grid.picture(g)
      }
      if (.Platform$OS.type=="unix"){
        system ("convert pictograms/eye.svg pictograms/eye.png") # requires ImageMagic to make PNG file
      }
      Require ("png")
      p <- readPNG ("pictograms/eye.png")
      # Require ("OpenImageR")
      # p <- rotateImage (p, angle=rU, method="nearest")
      rasterImage(p
                  , xleft=xU       # -152       +3*1*(xU-0.5)
                  , xright=xU+0.3  # -152+0.3  +3*1*(xU-0.5)
                  , ybottom=yU     # 59.4     +1*1*(yU-0.5)
                  , ytop=   yU+0.3 # 59.4+0.3 +1*1*(yU-0.5)
                  , angle=rU)
      rm (p, xU, yU, rU)
      # ## fine-scale map -- no longer needed?
      # plot (xMap  # plot.section (which=99) should return xlim and ylim of map, not section
      #       , which = 99
      #       , coastline = "coastlineWorldFine"
      #       , showStations = TRUE
      #       , gird = TRUE
      #       # , span=50
      # )
    }
    rm (xMap, bottom)

    ###############################################
    ## draw palette, color scale into next panel ##
    ###############################################

    nCol <- 100
    t.ramp <- oCol3[[ov]](nCol)
    if (pH > 30){
      yL <- 1.8
      par (mar=c(14, 1,3,1))
    }else{
      yL <-1.2
      par (mar=c(10, 1,3,1))
    }
    bp <- barplot (rep (1, nCol), axes=FALSE, space=0, col = t.ramp, border=NA
                   , ylim=c(0,yL)  # ylim to make bar narrower, less high
    )
    title (main = oVars [ov], cex=3, line=0.5)
    lVal <-  pretty (c (oRange [ov,1], oRange [ov,2]))
    axis (1, at= (lVal-oRange [ov,1])/(oRange [ov,2]-oRange[ov,1]) * nCol
          , labels = lVal, lwd = 0)
    rm (bp, lVal, nCol, yL)

    ## add date and logos for reference
    mtext (text=paste ("NOAA Kasitsna Bay Lab and KBNERR\n", Sys.Date())
           , side=1, line=6, outer=FALSE, cex=0.7, adj=1)
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
if (0){
  xC <- xC [order (xC$isoTime),]
  xC$survey <- factor (xC$survey)
  pdf ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/stationmaps.pdf")
  for (i in 1:length (levels (xC$survey)))
    xCx <- subset (xC, survey == levels (xC$survey)[i])
  ## cat (xC$File.Name[i], "\n")
  xCx <- sectionize(xCx)
  plot (xCx
        , which = 99
        , coastline = "coastlineWorldFine"
        , showStations = TRUE
        , gird = TRUE
        , map.xlim = c(-154, -151)
        , map.ylim = c(57.5, 60.1)
        , clatitude = 59.4
        , clongitude = -152
        , span = 250
  )
  dev.off()

  ## map of study area, following https://clarkrichards.org/2019/07/12/making-arctic-maps/
  Require (ocedata) #for the coastlineWorldFine data
  data(coastlineWorldFine)

  mp <- function() {
    mapPlot(coastlineWorldFine, #projection=proj4string (bR),
            longitudelim = c(-154.2, -150.5),
            latitudelim = c(58.5, 60.5), col='grey')
  }
}

cat ("\n# END CTDwall.R #\n")
# EOF
