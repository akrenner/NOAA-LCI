## replot CTD wallpaper for office
## provide line-graph alternatives



## load data
## start with file from dataSetup.R
setwd("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size


### data prep
require ("oce")
## define sections
physOc$DateISO <- format (physOc$isoTime, "%Y-%m-%d")
physOc$Transect <- factor (physOc$Transect)
physOc$year <- factor (format (physOc$isoTime, "%Y"))
## combine CTD and station meta-data
physOc <- subset (physOc, !is.na (physOc$Transect)) ## who's slipping through the cracks??
## stn should be no longer needed -- see dataSetup.R
# physOc <- cbind (physOc, stn [match (physOc$Match_Name, stn$Match_Name)
#                               , which (names (stn) %in% c(# "Line",
#                                                           "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor (physOc$Match_Name)
# print (summary (physOc))






## get coastline and bathymetry
## bathymetry and coastline
bathy <- "polygon"
## Zimmermann bathymetry
require ("raster")
require ("marmap")

## reproject?  crop? -- review!!
# nGrid <- .... ## define new grid -- the missing link
if (.Platform$OS.type == "unix"){   ## no longer necessary!
  ##  bR <- resample (bR, nGrid)
  bR <- raster ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf") ## not working in RStudio -- need to use decompressed after all?
  ## need to reproject to longlat
  ## then turn into topo object
  bathyP <- projectRaster(bR, crs = crs ("+proj=longlat +datum=WGS84 +units=m")) ## need to downsample first
  bathy <- as.topo (as.bathy (bathyP)); rm (bR, bRb)  # still not right
  # bathy <- as.topo (z = bRg [[1]][,,1])
  save (bathy, bathyP, file = "~/tmp/LCI_noaa/cache/bathymetryZ.RData")
#  rm (bR, bRg, bRb)
}else{
  # positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
  # bathyL <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay
  bathyL <- getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE) # too coarse for KBay
  load ("~/tmp/LCI_noaa/cache/bathymetryZ.RData")
}

require ("ocedata") # coastlineWorldFine



poAll <- physOc
poAll <- poAll [with (poAll, order (Transect, year, isoTime, Pressure..Strain.Gauge..db.)),]


## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object

## AlongBay (tn=6), i = 2, sections to process: 4, k= 5 fails

dir.create ("~/tmp/LCI_noaa/media/CTDwall", recursive = TRUE, showWarnings = FALSE)

poAll <- subset (poAll, Station %in% as.character (1:12)) # cut out portgraham and pogi -- or translate
poAll$Transect <- factor (poAll$Transect)

## add bathymetry to CTD metadata
require (sp)
poP <- poAll
coordinates (poP) <- ~longitude_DD+latitude_DD
proj4string(poP) <- crs ("+proj=longlat +datum=WGS84 +units=m")
poAll$bathy <- extract (bathyP, poP)
bL <- extract (as.raster (bathyL), poP)
poAll$bathy <- ifelse (is.na (poAll$bathy), -1* bL, poAll$bathy)
rm (poP, bL)

oVars <- c ("temperature", "salinity", "density", "fluorescence"
            , "turbidity", "PAR")

aggregate (Date~year+Transect, poAll, function (x){length (levels (factor (x)))})[,c(2,1,3)]

## standard-layout based on season?
## aggregate T9 and AB by season?
## special layout for T9 and AB?




pSec <- function (xsec, N, zC, cont = TRUE, custcont = NULL, ...){
  s <- try (plot (xsec, which = N
                  # , showBottom = FALSE
                  # , showBottom = "lines" #FALSE
                  , axes = TRUE
                  , zcol = zC
                  , stationTicks = TRUE
                  , showStations = TRUE
                  # , grid = TRUE
                  #, ztype = "contour"
                  , ztype = "image"
                  #, ztype = "points"
                  , ylim = c(0,max (physOc$bathy))
                  , ... # zlim?
  ))
  title (main = levels (physOc$transDate)[i])
#  if (cont){
    distance <- s [["distance", "byStation"]]
    depth <- s [["station", 1]][["depth"]]
    zvar <- matrix (s [[N]], byrow = TRUE, nrow = length (s [["station"]]))
    if (length (custcont) > 1){
      cLev <- custcont
    }else{
      cLev <- pretty (range (as.numeric (zvar), na.rm = TRUE), 5)
    }
    ## dirty hack -- still got to find out why some distances are NA! XXX
    if (any (is.na (distance))){
      cutS <- which (is.na (distance))
      distance <- distance [-cutS]
      zvar <- zvar [-cutS,]
    }

    cT <- try (contour (distance, depth, zvar, add = TRUE
                  , levels = cLev  ## error XXX
                  , col = "black", lwd = 2))
    if (class (cT) == "try-error"){
      legend ("bottomleft", legend = "no contours")
    }
#  }
  if (cont){return (s)}
}


# test <- TRUE
test <- FALSE

if (test){iX <- 2}else{iX <- 1:length (oVars)}
for (ov in iX){
  if (test){iX <- 1}else{iX <-   1:length (levels (poAll$Transect))}# by transect
  for (tn in iX){  ## XXX testing XXX
# for (ov in 1:length (oVars)){
#  for (tn in 1:length (levels (poAll$Transect))){
    ## for testing
    ## ov <- 1; tn <- 6
    cat (oVars [ov], " Transect #", levels (poAll$Transect)[tn], "\n")

    ## double-use stations:
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
    if (levels (poAll$Transect)[tn] == "AlongBay"){
      poAll$Transect [(poAll$Transect == "4") & (poAll$Station == "3")] <- "AlongBay"
      poAll$Transect [(poAll$Transect == "9") & (poAll$Station == "6")] <- "AlongBay"
    }
    if (levels (poAll$Transect)[tn] == "4"){
      poAll$Transect [(poAll$Transect == "AlongBay") & (poAll$Station == "3")] <- "4"
    }
    if (levels (poAll$Transect)[tn] == "9"){
      poAll$Transect [(poAll$Transect == "AlongBay") & (poAll$Station == "6")] <- "9"
    }
    physOcY <- subset (poAll, Transect == levels (poAll$Transect)[tn])
    physOcY$year <- factor  (physOcY$year)
    # physOcY$transDate <- factor (with (physOcY, paste0 ("T-", Transect, " ", DateISO)))
    physOcY$transDate <- with (physOcY, paste0 ("T-", Transect, " ", DateISO))


    png (paste0 ("~/tmp/LCI_noaa/media/CTDwall/", oVars [ov]
                 , " T-", levels (poAll$Transect)[tn]
                 # , "_", levels (physOcY$year)[k]
                 , ".png")
         , height = 8.5*200, width = 11*200, res = 300)

    # pdf (paste0 ("~/tmp/LCI_noaa/media/CTDwall/", oVars [ov]
    #              , " T-", levels (poAll$Transect)[tn]
    #              # , "_", levels (physOcY$year)[k]
    #              , ".pdf")
    #      , height = 8.5, width = 11)
    layout (matrix (1:12, 4, byrow = FALSE)) # across, then down


    if (test){iX <- 2}else{iX <- 1:length (levels (physOcY$year))}# by transect
    for (k in iX){
#     for (k in 1:length (levels (physOcY$year))){ # by year -- assuming no surveys span New Years Eve
      ## for testing:
      # k <- 2
      physOc <- subset (physOcY, year == levels (physOcY$year)[k])

      ## allow x-day window to make up a composite transect
      ## better to apply to allPo?
      # algorithm:
      # set start Dates
      # give all data same ID as start date as h, IF they after element h, and are
      # within X days of start date of h
      ## make this a universal function to all data? -> to datasetup?
      physOc <- physOc [order (physOc$isoTime),]
      surveyW <- ifelse (duplicated(physOc$transDate), 'NA', physOc$transDate)
      for (h in 2:nrow (physOc)){
        surveyW <- ifelse (1:length (surveyW) >= h
                           , ifelse (difftime (physOc$isoTime, physOc$isoTime [h-1]
                                               , units = "days") < 7
                                     , surveyW [h-1], surveyW)
                           , surveyW)
      }
      # ## faster version?  -- not worth the trouble
      # for (h in which (!duplicated (physOc$transDate))[-1]){
      # }
      physOc$transDate <- factor (surveyW); rm (surveyW, h)
      # physOc$transDate <- factor (physOc$transDate)


      ## define and plot sections
      cat ("Sections to process: ", length (levels (physOc$transDate)), "\n")

      if (test){iX <- 2}else{iX <-  1:length (levels (physOc$transDate))}
      for (i in iX){
      # for (i in 1:length (levels (physOc$transDate))){
          # for testing:
        # i <- 2
        cat ("sec: ", i, " ")
        xCt <- subset (physOc, transDate == levels (physOc$transDate)[i])
        if (length (levels (factor (xCt$Match_Name))) > 2){ ## shouldn't be necessary -- what's up with Transect = NA??
          xC <- xCt
          if (xC$Transect [1] %in% c("4", "9")){
            xC <- xC [order (xC$latitude_DD, decreasing = TRUE),]
          }else{
            xC <- xC [order (xC$longitude_DD, decreasing = TRUE),]
          }
          ## arrange ctd data into sections
          ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

          #if (nrow (xC) > 1){ ## better than unreliable test above

          ## average multiple casts on same date?? XXX

          stn <- factor (sprintf ("%02d", as.numeric (xC$Station)))
          stn <- factor (sprintf ("%02s", xC$Station), ordered = TRUE)

          xC$Match_Name <- factor (xC$Match_Name)
          #          xC <- as.section (lapply (1:length (levels (xC$Match_Name)) # XX rewrite with %>% pipes? XX as function?
          ## need to use station to keep factors in correct order?!?!!
          xCo <- as.section (lapply (1:length (levels (stn))
                                    , FUN = function (x){
                                      #                                      sCTD <- subset (xC, Match_Name == levels (Match_Name)[x])
                                      sCTD <- subset (xC, stn == levels (stn)[x])
                                      ocOb <- with (sCTD,
                                                    as.ctd (salinity = Salinity_PSU
                                                            , temperature = Temperature_ITS90_DegC
                                                            , pressure = Pressure..Strain.Gauge..db.
                                                            , longitude = longitude_DD
                                                            , latitude = latitude_DD
                                                            , station = Match_Name
                                                            #, sectionId = transDate
                                                            , time = isoTime
                                                    ))
                                      # ocOb@metadata$waterDepth <- sCTD$Bottom.Depth [1]
                                      ocOb@metadata$waterDepth <- sCTD$bathy [1]
                                      ocOb <- oceSetData (ocOb, "fluorescence", sCTD$Fluorescence_mg_m3)
                                      ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
                                      ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                                      ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                                      ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                                      ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                                      ocOb
                                    }))
          xCo <- sectionGrid (xCo, method = "boxcar")
          rm (stn)
          if (ov == 1){
            require ("pals")
            s <- pSec (xCo, "temperature", parula
            # s <- pSec (xCo, "temperature", oceColors9A
            # s <- pSec (xCo, "temperature", oceColorsTemperature
            # source ("turbo_colormap.R")
            # s <- pSec (xCo, "temperature", turbo_vec # oceColors9A
            # require ("viridis")  ## not great for temperature -- too green
            # s <- pSec (xCo, "temperature", viridis
                                              , zlim = range (poAll$Temperature_ITS90_DegC) # c(1, 15.4)
                       , custcont = seq (-1, 20, by = 1)
            )
            ## trouble-shoot
            # xsec = s; N = "salinity"; zC = oceColorsTemperature

          } else if (ov == 2){
            pSec (xCo, "salinity", oceColorsSalinity
                  , zlim = range (poAll$Salinity_PSU, na.rm =TRUE)
                  ) #, zlim = c(15.97, 33.22)) # non-linear scaleing?
          }else if (ov == 3){
            pSec (xCo, "sigmaTheta", oceColorsDensity
                  , zlim = range (poAll$Density_sigma.theta.kg.m.3)) #, zlim = c(11.58, 26.63))
          }else if (ov == 4){
            pSec (xCo, "fluorescence", oceColorsChlorophyll)
          }else if (ov == 5){
            pSec (xCo, "turbidity", oceColorsTurbidity)  #+, zlim = c(-1.6, 33.96)) # should NOT have negative flourescence XXX
          }else if (ov == 6){
            pSec (xCo, "PAR", oceColorsPAR)  #+, zlim = c(-1.6, 33.96)) # should NOT have negative flourescence XXX
          }
          ## tmp, just for trouble shooting
          # if (ov == 1){
          #   plot (xC, which = 99, coastline = "coastlineWorldFine", showStations = TRUE)
          # }
        }

        if (i %% 5 == 0){
          cat (i, " ")
          if (i %% 100 == 0) cat ("\n")
        }
      }
    }
    plot (xCo
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
    cat ("\n")
  }
}


physOc <- poAll
rm (xCt, xCo, i, k, tn, oVars, ov, poAll, pSec, physOcY)





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

      if (0){
        plot (xC                        # subscript out of bound
              , which = "temperature" # = 1, salinity 2, density 3
              , xtype = "time"
              , ytype = "depth"
              ## need to define proper z-matrix! -- initiates correct plot may need sectionGrid, as above?
              # , coastline = "best"
        )
      }
    }
    #   plot (xC, xtype = "time")
  }
  dev.off()
}



## for error checking: map of every transect
# double-used plots may appear out-of-line in chronology

if (0){
pdf ("~/tmp/LCI_noaa/media/CTDwall/stationmaps.pdf")
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
Require (ocedata) #for the coastlineWorldFine data
data(coastlineWorldFine)

mp <- function() {
  mapPlot(coastlineWorldFine, #projection=proj4string (bR),
          longitudelim = c(-154.2, -150.5),
          latitudelim = c(58.5, 60.5), col='grey')
}


# pdf ("~/tmp/LCI_noaa/media/CTDwall/studyareaMap.pdf")
# mp()
# # mapImage (bathy, col = oceColorsGebco, breaks = seq (-500, 0, 500))
# mapImage (bathy, col = oceColorsGebco, breaks = c (seq (-300, 0, 20), 2000))
# mapPolygon (coastlineWorldFine, col = "gray")
# mapGrid()
# dev.off()

# EOF
