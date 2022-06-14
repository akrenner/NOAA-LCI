## replot CTD wallpaper for office
## provide line-graph alternatives

rm (list = ls())

## check whether problems persist with most recent oce version
## make sure that if title is plotted, next plot moves on to the next field!


## problemss:
## - fluorescence missing (all values NA), e.g. T-3 2012-05-02
## - contours fail, e.g. temperature, T-4 (2), 2019-05-14

## PAR: flag night; mark 1% light level contour
## fix distancescale to full transect
## Kris: check on surface PAR and salinity measurements

## 2021-08-03 -- issues
# x multiple transects per season/month are merged -> pick first
# x fix color scale across all graphs (across Transects as well?)

# - 12-month sampling: spread over 2 pages, first plot Jan-Jun, then Jun-Dec.  --  or plotter

#' partial transects: some bathymetries are mangled -- ensure every cast has a
#' waterdepth, either local or from masterlist.

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



test <- TRUE
test <- FALSE


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
if (test){iX <- 1}else{iX <- 1:length (oVars)}
if (test){iY <- 5}else{iY <- 1:length (levels (poAll$Transect))}# by transect
for (ov in iX){  # ov = OceanVariable (temp, salinity, etc)
  for (tn in iY){  # tn: transect
    ## for testing
    ## ov <- 1; tn <- 6 ## AlongBay
    ## ov <- 1; tn <- 5 ## T9
    cat ("\n\n", oVars [ov], " T-", levels (poAll$Transect)[tn], "\n")

    ## doubly-used stations:
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
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
    bottomZ <- extract (bathyZ, sectP, method="bilinear")*-1
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

      require ("stringr")
      sampleTimes <- str_pad (1:12, 2, pad = "0")
      physOcY$smplIntvl <- physOcY$month
      nY <- as.numeric (format (Sys.time(), "%Y")) - min (as.integer (levels (poAll$year))) + 1
      layoutM <- matrix (1:(12*nY), nY, byrow = TRUE) # across, then down
      rm (nY)

    }else{
      ## quarterly
      pH <- 8.5; pW <- 14    # legal size
      sampleTimes <- levels (physOcY$season)
      physOcY$smplIntvl <- physOcY$season
      layoutM <- matrix (1:16, 4, byrow = TRUE)
    }


    pdf (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVarsF [ov]
                 , " T-", levels (poAll$Transect)[tn]
                 , ".pdf")
         , height = pH, width = pW)
    layout (layoutM); rm (layoutM)


    ## is this needed??
    #    for (k in 1:length (levels (physOcY$year))){ # by year -- assuming no surveys span New Years Eve
    #for (k in 2){
    if (test){iO <- 1}else{iO <- 1:length (levels (physOcY$year))}
    for (k in iO){
      ## for testing:
      # k <- 7 # pick 2018
      # k <- 1 # pick 2012
      physOc <- subset (physOcY, year == levels (physOcY$year)[k])


      ## replace transDate from above!
      ## also making surveyW redundant
      physOc$transDate <- with (physOc , factor (paste0 ("T-", Transect, " ", year, "-", smplIntvl)
                                                 , levels = paste0 ("T-", Transect [1], " ", year [1], "-"
                                                                   , sampleTimes)))

      ## define and plot sections
      cat ("   ",  formatC (k, width = 3), "/", length (levels (physOcY$year))
           , " Sections/year:", length (levels (physOc$transDate)), "-- ")


      iA <-  1:length (levels (physOc$transDate))
      if (test){iA <- 1:5}else{iA <-  1:length (levels (physOc$transDate))}
      for (i in iA){              # cycle through individual surveys
        # i <- 3  # for testing
        cat (i, " ")
        xC <- subset (physOc, transDate == levels (physOc$transDate)[i])
        if (length (levels (factor (xC$Match_Name))) < 2){
          ## blank plot for missing data -- unless in the future
          ## use empty slots for map rather than starting on blank page if level would be in future

          #cat ("MISSING SURVEY", levels (physOc$transDate)[i], "\n#\n")
          inFuture <- as.numeric (as.character (physOc$year))[1] >= as.numeric (format (Sys.time(), "%Y")) &&
            i/length (iA) > as.numeric (format (Sys.time(), "%m"))/12
          if (!inFuture){
            plot (0:10, type = "n", axes = FALSE, xlab = "", ylab = ""
                  , main = paste0 (levels (physOc$transDate)[i], "--"
                                   , length (levels (factor (xC$Match_Name)))
                                   , " stations")
                  )
          }
          rm (inFuture)
        }else{
          if (0){  ## combine all casts in survey window (watch RAM!)
            ## or pick out long survey within X days?
            nSurv <- 1
            xC <- xC    ## use all data for month/quarter, irrespective of how far apart
          }else{
            ## check whether there is more than one survey per survey-interval

            ## allow x-day window to make up a composite transectF
            ## better to apply to allPo?
            # algorithm:
            # set start Dates
            # give all data same ID as start date as h, IF they after element h, and are
            # within X days of start date of h
            ## make this a universal function to all data? -> to datasetup?
            xC <- xC [order (xC$isoTime),]
            surveyW <- ifelse (duplicated(xC$DateISO), 'NA', xC$DateISO)
            for (h in 2:nrow (xC)){
              surveyW <- ifelse (1:length (surveyW) >= h
                                 , ifelse (difftime (xC$isoTime, xC$isoTime [h-1]
                                                     , units = "days") < 7
                                           , surveyW [h-1], surveyW)
                                 , surveyW)
            }
            xC$surveys <- factor (surveyW); rm (surveyW, h)
            # physOc$transDate <- factor (physOc$transDate)

            nSurv <- length (levels (xC$surveys))
            if (nSurv > 1){
              if (1){
                ## use the survey with the most stations
                nS <- sapply (levels (xC$surveys), FUN = function (x){
                  length (levels (factor (subset (xC$Station, xC$surveys == x))))
                })
                xC <- subset (xC, surveys == levels (xC$surveys)[which.max (nS)])
                rm (nS)
              }else{
                ## use only the first survey
                nR <- sapply (levels (xC$surveys), FUN = function (x){sum (xC$surveys == x)})
                xC <- subset (xC, surveys == levels (xC$surveys)[which.max(nR)])  # use only the first survey
                rm (nR)
              }
            }

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
                sec$isoTime <- factor (sec$isoTime)
                castSize <- sapply (1:length (levels (sec$isoTime)), function (m){
                  nrow (subset (sec, sec$isoTime==levels (sec$isoTime)[m]))
                })
                tNew <- subset (sec, isoTime == levels (isoTime)[which.max(castSize)])
                if (m == 1){
                  secN <- tNew
                }else{
                  secN <- rbind (secN, tNew)
                }
              }
              xC <- secN
              rm (secN, tNew, sec, m)
            }
          }


          ## arrange ctd data into sections
          ## define section -- see oce-class "section"

           save.image ("~/tmp/LCI_noaa/cache/wallCache.RData")
          # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wallCache.RData"); source ("CTDsectionFcts.R")
          # section=xCo
          # transect = data.frame (stationId=stnT$Match_Name, latitude=stnT$Lat_decDegree
          #                        , longitude=stnT$Lon_decDegree, bottom=stnT$Depth_m)

          ##
          ## construct, pad, and sort section
          ##
          xCo <- sectionize (xC)
          ## sectionPad to plot incomplete sections
          xCo <- sectionPad (section=xCo, transect = data.frame (stationId=stnT$Match_Name
                                                                 , latitude=stnT$Lat_decDegree
                                                                 , longitude=stnT$Lon_decDegree
                                                                 , bottom=stnT$Depth_m))
          ## sectionSort
          if (xC$Transect [1] == "AlongBay"){
            xCo <- sectionSort (xCo, "latitude", decreasing = FALSE)
            bottom <- bottom [order (bottom$lat, decreasing = FALSE),]
          }else if (xC$Transect [1] %in% c("4", "9")){  # requires new version of oce
            xCo <- sectionSort (xCo, "latitude", decreasing = TRUE)
            bottom <- bottom [order (bottom$lat, decreasing = TRUE),]
          }else{
            xCo <- sectionSort (xCo, "longitude", decreasing = FALSE)
            bottom <- bottom [order (bottom$lon),]
          }
          bottom$dist <- with (bottom, geodDist (longitude1=lon, latitude1=lat, alongPath=TRUE)) # [km]

          ## test, QAQC
          # sapply (1:length (xCo@data$station), function (k){
          #   xCo@data$station[[k]]@data$temperature
          # })
          # plot (subset (xC, Match_Name == "9_10")$Temperature_ITS90_DegC)

          ##
          ## plot the section/transect
          ##
          pSec (xCo, N = oVarsF [ov]
                , zCol = oCol3 [[ov]]
                , zlim = oRange [ov,] # fixes colors to global range of that variable
                # , custcont = pretty (oRange [ov,], 20)  ## may often fail? -- no contours in range
                , ylim = c(0,max (physOcY$bathy, na.rm = TRUE))
                , showBottom=TRUE
                , # better?, slower interpolation
          )
          ## add high-res bottom profile
          # if (xC$Transect [1] %in% c("AlongBay", "6")){  ## AlongBay still messed up!
          #   # bottom <- bottom [nrow(bottom):1,]
          #   bottom <- rev (bottom)
          # }
          tgray <- rgb (t (col2rgb ("pink")), max=255, alpha=0.5*255) ## transparent
          with (bottom, polygon(c(min (dist), dist, max(dist))
                                , c(10000, -depthHR, 10000)
                                , col=tgray))
          rm (tgray)

          if (nSurv > 1){
            title (main = paste (levels (physOc$transDate)[i], "* -", nSurv), col.main = "red")
          }else{
            # title (main = paste (levels (physOc$transDate)[i]))
            title (main = paste0 (levels (physOc$transDate)[i], "-"
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
      cat ("\n")
    }
    if (exists ("xMap")){
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

rm (i, k, tn, oVars, oVarsF, ov, poAll, pSec, physOcY)





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

if (0){
  pdf ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/stationmaps.pdf")
  for (i in 1:length ())
    plot (xC
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
