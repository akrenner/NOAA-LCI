## replot CTD wallpaper for office
## provide line-graph alternatives

rm (list = ls())


test <- TRUE
test <- FALSE


## problemss:
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
# only AlongBay and 9 are monthly -- 4?


dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", showWarnings = FALSE, recursive = TRUE)
if (exists ("sectionSort")){detach ("package:oce")}  ## reload new version of oce
require ("oce")
load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R
# x <- load ("~/tmp/LCI_noaa/cache/ctdwall1.RData")  # from CTDsections.R
source ("CTDsectionFcts.R")



mnthly <- c ("9", "AlongBay", "4")


if (0){## tests
  levels (factor (subset (poAll, year == 2012)$DateISO))
  xC <- subset (poAll, (Transect == "9")&(DateISO == "2019-09-16") )
  # xC <- subset (poAll, (Transect == "3")&(DateISO == "2012-03-14"))
  xCo <- sectionize (xC)
  plot (xCo)
  pSec (xCo, 1, zcol = oCol [[1]])
  pSec (xCo, 1, zcol = turbo (10), custcont = c(10, 11, 11.1))
  rm (xC, xCo)
}



useSF <- FALSE  ## use package sf and terra/stars instead of raster
# useSF <- TRUE

## add high-res bottom/bathymetry profile
## see https://www.clarkrichards.org/2017/04/01/adding-noaa-bottom-profile-to-section-plots/
require ("marmap")
bfer <- 0.5
bathy <- getNOAA.bathy (min (poAll$longitude_DD)-bfer, max (poAll$longitude_DD)+bfer
                        , min (poAll$latitude_DD)-bfer, max (poAll$latitude_DD)+bfer
                        , keep=TRUE, resolution=1, path="~/tmp/LCI_noaa/cache/")
rm (bfer)
if (useSF){
  require ("sf") ## or stars / terra ??
  require ("stars") ## or better to use terra?
  bathyZ <- read_stars ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
  # require ("terra")
  # bathyZ <- rast ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
}else{
  require ("raster")
  ## need to supply absolute path because raster object is just a pointer.
  ## still needs uncompressed raster file accessible.
  if (.Platform$OS.type == "windows"){
    bathyZ <- raster ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
  }else{
    bathyZ <- raster ("/Users/martin/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
  }
}

## loop over variable, then transects and then seasons
if (test){vL <- 1}else{vL <- 1:length (oVars)}
if (test){tL <- 6}else{tL <- 1:length (levels (poAll$Transect))}# by transect. 5: T9
for (ov in vL){  # ov = OceanVariable (temp, salinity, etc)
  for (tn in tL){  # tn: transect
    ## for testing
    ## ov <- 1; tn <- 6 ## AlongBay
    ## ov <- 1; tn <- 2
    cat ("\n\n", oVars [ov], " T-", levels (poAll$Transect)[tn], "\n")

    ## doubly-used stations:
    ## should make this a function?
    if (levels (poAll$Transect)[tn] == "AlongBay"){
      swMN <- c ("4_3", "9_6", "6_2", "7_22")
      poAll$Transect [poAll$Match_Name %in% swMN] <- "AlongBay"
      stn$Line [stn$Match_Name %in% swMN] <- "AlongBay"
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


    ## to use as a reference for partial stations
    ## and for bathymetry profile
    stnT <- subset (stn, stn$Line == levels (poAll$Transect)[tn])

    lati <- seq (min (stnT$Lat_decDegree), max (stnT$Lat_decDegree), length.out = 1000)
    loni <- suppressWarnings(approx (stnT$Lat_decDegree, stnT$Lon_decDegree, lati, rule=2)$y)
    dist <- rev (geodDist (longitude1=loni, latitude1=lati, alongPath=TRUE)) # [km] -- why rev??
    # if (levels (poAll$Transect)[tn] != "AlongBay"){dist <- rev (dist)}
    sect <- data.frame (loni, lati, dist); rm (loni, lati, dist)

    ## extract from bathyZ. then fill-in the missing values from get.depth
    ## need to geo-ref points and raster first?
    if (useSF){
      sect <- st_as_sf(sect, coords = c("loni", "lati"))
      st_crs(sect) <- 4326  ## WGS84 definition
      sectP <- st_transform(sect, st_crs (bathyZ))
      # bottomZ <- aggregate (bathyZ, sectP, function(x){x[1]}) ## this step fails in stars -- terra?
      bottomZ <- aggregate (bathyZ, sectP, mean, na.rm = TRUE) ## stars -- hangs
      # bottomZ <- extract (bathyZ, sectP, method="bilinear")*-1  ## terra
    }else{
      coordinates (sect) <- ~loni+lati
      proj4string(sect) <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
      sectP <- spTransform(sect, CRS (proj4string(bathyZ)))
      bottomZ <- raster::extract (bathyZ, sectP, method="bilinear")*-1
    }
    ## fill-in T6/AlongBay from NOAA raster that's missing in Zimmermann's bathymetry
    bottom <- get.depth (bathy, x=sect$loni, y=sect$lati, locator=FALSE)
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
      pH <- 44; pW <- 88     # go big on FWS plotter
      pH <- 42; pW <- 84     # FWS paper is 42 inches wide
      ## rotate, but do not scale. Autorotate should be ok.
      yearPP <- 11 # years (rows) per page
      omcex <- 2   # size of mtext annotations
      require ("stringr")
      sampleTimes <- str_pad (1:12, 2, pad = "0")
      physOcY$smplIntvl <- physOcY$month
      nY <- as.numeric (format (Sys.time(), "%Y")) - min (as.integer (levels (poAll$year))) + 1
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
    par (oma=c(3,5,5,2)
         , mar=c(4,4,3,0.1)
         )
    if (test){
      par (oma=c(3,5,15,2)
           , mar=c(4,4,16,0.1)
      )
    }

    #for (iY in 2){
    if (test){yL <- 7}else{yL <- 1:length (levels (physOcY$year))}
    # yL <- 1:length (levels (physOcY$year))
    for (iY in yL){
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
      if (test){sL <- 2}else{sL <-  1:length (levels (physOc$transDate))}
     # sL <-  1:length (levels (physOc$transDate))
      for (iS in sL){              # cycle through individual survey
        # iS <- 2  # for testing
        cat (iS, " ")
        xC <- subset (physOc, transDate == levels (physOc$transDate)[iS])
        if (length (levels (factor (xC$Match_Name))) < 2){
          ## if a scheduled survey was not done, plot a blank placeholder in the panel instead
          ## unless the scheduled survey is in the future from runtime (do nothing then)
          inFuture <- as.numeric (as.character (physOc$year))[1] >=
            as.numeric (format (Sys.time(), "%Y")) &&
            iS/length (sL) > as.numeric (format (Sys.time(), "%m"))/12
          if (!inFuture){
            plot (0:10, type = "n", axes = FALSE, xlab = "", ylab = ""
                  , main = paste0 (levels (physOc$transDate)[iS], "--"
                                   , length (levels (factor (xC$Match_Name)))
                                   , " stations")
            )
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

            ## this has already been done in CTDwall-setup.R
            # xC <- xC [order (xC$isoTime),]
            # surveyW <- ifelse (duplicated(xC$DateISO), 'NA', xC$DateISO)
            # secDiff <- xC$isoTime [2:nrow (xC)] - xC$isoTime [1:(nrow (xC)-1)] ## time difference [s]
            # for (h in 1:length (secDiff)){
            #   if (secDiff [h] < 7*24*3600){ # 7-day cut-off
            #     surveyW [h+1] <- surveyW [h]
            #   }
            # }
            # xC$surveys <- factor (surveyW); rm (surveyW, h)
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

            # remove duplicate stations -- move to CTDwall-setup.R?
            ## any duplicated stations? -- if so, keep only the longest cast
            xC$Match_Name <- factor (xC$Match_Name)
            if (any (sapply (1:length (levels (xC$Match_Name)), function (m){
              sec <- subset (xC, xC$Match_Name == levels (xC$Match_Name)[m])
              tR <- difftime (max (sec$isoTime), min (sec$isoTime), units = "min")
              tR > 20
            }))){
              # there are duplicate CTD casts -- pick the longer one
              for (m in 1:length (levels (xC$Match_Name))){
                sec <- subset (xC, xC$Match_Name == levels (xC$Match_Name)[m])
                isoTimeF <- factor (sec$isoTime)
                castSize <- sapply (1:length (levels (isoTimeF)), function (m){
                  sum (!is.na (subset (sec, isoTimeF==levels (isoTimeF)[m])$Temperature_ITS90_DegC))
                })
                tNew <- subset (sec, isoTimeF == levels (isoTimeF)[which.max(castSize)])
                rm (isoTimeF)
                if (m == 1){
                  secN <- tNew
                }else{
                  secN <- rbind (secN, tNew)
                }
              }
              xC <- secN
              rm (secN, tNew, sec, m, castSize)
            }
          }

          ## arrange ctd data into sections
          ## define section -- see oce-class "section"
          save.image ("~/tmp/LCI_noaa/cache/wallCache.RData")
          ## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wallCache.RData"); source ("CTDsectionFcts.R")

          ##
          ## construct, pad, and sort section
          ##
          xCo <- sectionize (xC)

          ## sectionPad to plot incomplete sections
          xCo <- sectionPad (sect=xCo, transect = data.frame (station=stnT$Match_Name
                                                              , latitude=stnT$Lat_decDegree
                                                              , longitude=stnT$Lon_decDegree
                                                              , bottom=stnT$Depth_m))
          ## sectionSort--is now in sectionPad. Still need to do same to bottom
          if (xC$Transect [1] == "AlongBay"){
            bottom <- bottom [order (bottom$lat, decreasing = FALSE),]   ### XXX review!!
          }else if (xC$Transect [1] %in% c("4", "9")){  # requires new version of oce
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
          pSec (xCo, N = oVarsF [ov]
                , zCol = oCol3 [[ov]]
                , zlim = oRange [ov,] # fixes colors to global range of that variable
                # , zbreaks=zB # better?, slower interpolation
                # , custcont = pretty (oRange [ov,], 20)  ## may often fail? -- no contours in range
                , ylim = c(0,max (physOcY$Bottom.Depth_survey)+5)  ## need to fix CTDwall-setup.R first
                , showBottom=FALSE
                , drawPalette=FALSE
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
          ## addBorder (xCo, TD[ov]-1)

          # keep longest section for map
          if (!exists ("xMap")){xMap <- xCo}
          if (length (xCo@data$station) > length (xMap@data$station)){
            xMap <- xCo
          }
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
    if (exists ("xMap")){
      ## draw palette, color scale into next panel (or along full length?)
      nCol <- 100
      t.ramp <- oCol3[[ov]](nCol)
      bp <- barplot (rep (1, nCol), axes=FALSE, space=0, col = t.ramp, border=NA
                     , ylim=c(0,8)  # ylim to make bar narrower, less high
                     )
      title (main = oVars [ov], cex=3, line=-6)
      lVal <-  pretty (c (oRange [ov,1], oRange [ov,2]))
      axis (1, at= (lVal-oRange [ov,1])/(oRange [ov,2]-oRange[ov,1]) * nCol, labels = lVal
            , lwd = 0)
      rm (bp, lVal, nCol)

      ## maps
      plot (xMap
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
      plot (xMap
            , which = 99
            , coastline = "coastlineWorldFine"
            , showStations = TRUE
            , gird = TRUE
      )
      rm (xMap, xCo, nSurv,  bottom)
    }

    dev.off()
    cat ("\n")
  }
}


physOc <- poAll
rm (tn, oVars, oVarsF, ov, poAll, pSec, physOcY, yL, iY, sL, iS, vL, tL)





if (0){
  ## plot CTD-profiles of station over time
  pdf ("CTDtime.pdf")
  # for (i in 1:length (levels (physOc$Match_Name))){
  for (i in 1:6){
    ## section over time? or wrap by hand?
    xCp <- subset (physOc, Match_Name == levels (physOc$Match_Name)[i])
    if (length (levels (as.factor (xC$Date))) > 1){
      xC <- with (xCp, as.section (salinity = Salinity_PSU
                                   , temperature = Temperature_ITS90_DegC
                                   , pressure = Pressure..Strain.Gauge..db.
                                   , longitude = Lon_decDegree
                                   , latitude = Lat_decDegree
                                   , station = paste0 (Match_Name, DateISO)
                                   , sectionId = transDate
      ))
      ## need to add/supply time,
      xC@metadata$time <- xCp$isoTime

      sG <- sectionGrid (xC, p = 'levitus')
    }
  }
  dev.off()
}



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
}


## map of study area, following https://clarkrichards.org/2019/07/12/making-arctic-maps/
require (ocedata) #for the coastlineWorldFine data
data(coastlineWorldFine)

mp <- function() {
  mapPlot(coastlineWorldFine, #projection=proj4string (bR),
          longitudelim = c(-154.2, -150.5),
          latitudelim = c(58.5, 60.5), col='grey')
}

cat ("\n# END CTDwall.R #\n")
# EOF
