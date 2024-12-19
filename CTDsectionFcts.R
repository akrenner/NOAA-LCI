## functions to plot CTD sections or transects

## derived from oce
## emulate/evolve from ODV


getBathy <- function (transect, stn){
  ## get Zimmerman bathymetry for a given transect
  ## "transect" is any one factor in stn$Line
  ## stn is the master list of stations used in Kachemak Bay/lower Cook Inlet
  require ("oce")  # for geoDist
  require ("sf")
  require ("dplyr")
  require ("stars")

  if (file.exists("KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")){
    mar_bathy <- stars::read_stars ("KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")
  }else{
    mar_bathy <- stars::read_stars ("~/GISdata/LCI/bathymetry/KBL-bathymetry/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")
  }
  names (mar_bathy) <- "topo"
  names (mar_bathy) <- "topo"
  bathyZ <- st_as_stars(ifelse (mar_bathy$topo > 0, NA, mar_bathy$topo * -1)
                       , dimensions = attr(mar_bathy, "dimensions"))
  rm (mar_bathy)

  stnT <- subset (stn, stn$Line==transect)
  lati <- seq (min (stnT$Lat_decDegree), max (stnT$Lat_decDegree), length.out = 1000)
  loni <- suppressWarnings(approx (stnT$Lat_decDegree, stnT$Lon_decDegree, lati, rule=2)$y)
  dist <- rev (geodDist (longitude1=loni, latitude1=lati, alongPath=TRUE)) # [km] -- why rev??
  sect <- data.frame (loni, lati, dist) %>%
    st_as_sf (coords=c("loni", "lati"), crs=4326) %>%
    st_transform(crs=st_crs (bathyZ))
  sect$bottom <- stars::st_extract(bathyZ, at=sectP)$topo*-1 ## depth now negative
  rm (loni, lati, dist)
  bProf <- with (sect, data.frame (dist, bottom))
  bProf
}



pSec <- function (xsec, N, cont = TRUE, zCol
                  , showBottom=TRUE, custcont = NULL, labcex=1.0
                  , plotContours=TRUE, ...){
  ## hybrid approach -- still use build-in plot.section (for bathymetry)
  ## but manually add contours
  ## XXX missing feature XXX : color scale by quantiles XXX
  require ("oce")
  if (length (xsec@data$station) < 2){
    plot (1:10, type="n")
  }else{
    s <- try (plot (xsec, which = N
                    , showBottom = showBottom
                    , axes = TRUE
                    , stationTicks = TRUE
                    , showStations = TRUE
                    , xtype="track"
                    , ztype = "image"
                    , zcol = zCol
                    , ...
    )
    , silent = TRUE)
    if (class (s) != "try-error"){
      # s <- xsec
      nstation <- length(s[['station']])
      depth <- unique(s[['depth']])
      np <- length(depth)
      zvar <- array(NA, dim=c(nstation, np))
      for (ik in 1:nstation) {  ## populate the array
        try (zvar [ik, ] <- s[['station']][[ik]][[ N ]])
      }
      distance <- unique(s[['distance']])  ## fragile when duplicate stations are present
      if (length (distance) < nstation){
        lat <- sapply (1:nstation, function (i){s@data$station[[i]]@metadata$latitude})
        lon <- sapply (1:nstation, function (i){s@data$station[[i]]@metadata$longitude})
        distance <- geodDist (longitude1=lon, latitude1=lat, alongPath=TRUE)
        ## hack to add resilience to duplicated CTD stations; repeat?
        distance <- c (ifelse (diff (distance) < 0.01, distance-0.01, distance), distance[nstation])  ## hack to make contours work?
        rm (lat, lon)
      }
      if (sum (!apply (zvar, 1, FUN = function (x){all (is.na (x))})) < 2){
        # plot (1:10, type = "n", new = FALSE)
        mtext (paste0 (N, " all NA"), side=3,line=-5, col = "red", cex=1)
      }

      # ## add contours -- see last example ?plot.section
      if (length (custcont) > 1){
        cLev <- custcont
      }else{
        cLev <- try (pretty (range (as.numeric (zvar), na.rm = TRUE), custcont), silent = TRUE)
      }
      ## dirty hack -- still got to find out why some distances are NA! XXX
      if (any (is.na (distance))){
        cat ("bad distance\n")
        cutS <- which (is.na (distance))
        distance <- distance [-cutS]
        zvar <- zvar [-cutS,]
        stop ("bad distance")
      }
      if (plotContours){
        cT <- try (contour (distance, depth, zvar, add = TRUE
                            # , nlevels = 5
                            , labcex=labcex # default: labcex=0.6
                            , levels = cLev  ## error XXX
                            , col = "black", lwd = 1), silent = TRUE)
        if (class (cT) == "try-error"){
          legend ("bottomleft", legend = "no contours")
        }
      }
    }
  }
}


addBorder <- function (s, fullT){
  ## add black border to sections that are incomplete
  distance <- unique (s [['distance']])
  if (length (distance) < fullT){
    box (lwd = 3, lty = "dashed")
  }
}


pSec0 <- function (xsec, N, cont = TRUE, custcont = NULL, zcol, ...){
  ## take code from
  ## https://www.clarkrichards.org/2016/04/25/making-section-plots-with-oce-and-imagep/
  ## this is essentially a re-write of plot.section.
  ## Reason for re-write: better bathymetry, custom-stretch distance.

# s <- sectionGrid (xsec, p = "levitus'")
  s <- xsec
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  np <- length(p)
  T <- S <- array(NA, dim=c(nstation, np))
  for (i in 1:nstation) {
    T[i, ] <- s[['station']][[i]][['temperature']]
    S[i, ] <- s[['station']][[i]][['salinity']]
  }
  distance <- unique(s[['distance']])  ## look these up from full transect. Then fix xlim

  if (exists ("zrange")){
    cm <- colormap (T
                    , breaks = seq (zrange [1], zrange [2], length.out = 20)
                    , col = zcol)
  }


  imagep (distance, p, T, col = zcol, ..., flipy = TRUE
          , filledContour = TRUE
          , ylab = "depth [m]")
  if (!is.null (custcont)){

  }
}

KBsectionSort <- function (xCo){
  ## sort section -- Kasitsna-Bay-Lab specific
  ## sort in here, rather than separately
  for (i in 1:length (xCo@data$station)){
    xCo@data$station[[i]]@metadata$stationId <-
      as.character(xCo@data$station[[i]]@metadata$stationId)
  }
  if (xC$Transect [1] %in% c ("AlongBay")){ # extended AlongBay wraps around Pogy Ptp
    xCo <- sectionSort (xCo, "latitude", decreasing = FALSE)
  }else if (xC$Transect [1] %in% c("4")){ ## include 9 here?
    xCo <- sectionSort (xCo, "latitude", decreasing = TRUE)
  }else if (xC$Transect [1] %in% c("9")){
    xCo <- sectionSort (xCo, "longitude", decreasing = FALSE)
  }else{
    xCo <- sectionSort (xCo, "longitude", decreasing = FALSE)
  }

}


sectionize <- function (xC){  ## keep this separate as this function is specific to Kachemak Bay
  if (packageVersion("oce") <= "1.7.3"){
    stop ("Need package:oce version 1.7.4 or later")
  }
  require ("oce")
  if (nrow (xC) < 2){stop ("no data to make a section")}
  # stn <- factor (sprintf ("%02s", xC$Station))
  xC$Match_Name <- factor (xC$Match_Name)
  stn <- xC$Match_Name
  xCo <- makeSection (xC, stn)
  ## sort by lat/lon instead -- or StationID (would need to re-assign)
  xCo <- KBsectionSort (xCo)
  xCo <- sectionGrid (xCo
                      # , p=100  ## some pressures are NA or Inf??--but may be prettiest if it works
                      , p=standardDepths(9)
                      , method = "boxcar", trim = TRUE)
  #  xCo <- sectionSmooth(xCo, method="barnes", pregrid=TRUE, trim=TRUE)  ## makes a mess -- not using it right; provide xr and yr
  xCo
}

makeSection <- function (xC, stn){
  require ("oce")
  # xC = data.frame of ctd data
  # stn defining the stations and their order
  as.section (lapply (1:length (levels (stn))
                      , FUN = function (x){
                        sCTD <- subset (xC, stn == levels (stn)[x])
                        ocOb <- with (sCTD,
                                      as.ctd (salinity = Salinity_PSU
                                              , temperature = Temperature_ITS90_DegC
                                              , pressure = Pressure..Strain.Gauge..db.
                                              #, startTime = isoTime
                                      ))
                        ocOb@metadata$waterDepth <- sCTD$Bottom.Depth_main [1]  # Bottom.Depth is a catastrophic mix of m and feet
                        ocOb@metadata$longitude <- sCTD$longitude_DD [1]
                        ocOb@metadata$latitude <- sCTD$latitude_DD [1]
                        ocOb@metadata$stationId <- sCTD$Match_Name [1]
                        ocOb@metadata$filename <- sCTD$File.Name [1]
                        ocOb@metadata$startTime <- sCTD$isoTime [1]


                        ocOb <- oceSetData (ocOb, "fluorescence", sCTD$Fluorescence_mg_m3)
                        # ocOb <- oceSetData (ocOb, "logFluorescence", sCTD$logFluorescence)
                        ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
                        ocOb <- oceSetData (ocOb, "logTurbidity", sCTD$logTurbidity)
                        ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                        ocOb <- oceSetData (ocOb, "logPAR", sCTD$logPAR)
                        ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                        #                        ocOb <- oceSetData (ocOb, "O2 [mg/L]", sCTD$Oxygen_SBE.43..mg.l.)
                        ocOb <- oceSetData (ocOb, "Oxygen_umol_kg", sCTD$Oxygen_umol_kg)
                        # ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                        # ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                        ocOb <- oceSetData (ocOb, "bvf", sCTD$bvf)
                        ocOb
                      }))

}



seasonize <- function (mon, breaks = c (0,2,4,8,10,13)){
  # use seasonal breaks from zooplankton study
  if (is.factor (mon)){
     mon <- as.numeric (as.character (mon))
    # mom <- as.numeric (levels(mon))[mon]
  }else if (class (mon)[1] == "POSIXct"){
    mon <- as.numeric (format (mon, "%m"))
  }
  season <- cut (mon, breaks, labels = c ("winter", "spring", "summer", "fall", "winter"))
  # season <- factor (season
  #                   , levels = c ("winter", "spring", "summer", "fall")
  #                   , ordered = TRUE)
  season
}



is.night <- function (ctd){
  require ("suncalc")
  sunAlt <- getSunlightPosition (date = as.POSIXct (ctd@data$time [1], origin = "1970-01-01 00:00")  # check origion!! XX -- or use section that doesn't have this problem?
                                 , lat = ctd@data$latitude [1]
                                 , lon = ctd@data$longitude [1])$altitude # in radians
  sunDeg <- sunAlt / pi * 180
  isTRUE (sunDeg < 0)
}
isNightsection <- function (ctdsection){
  ## check whether sun is below horizon at any one station
  sM <- ctdsection@metadata
  sunAlt <- sapply (1:length (sM$time), FUN = function (i){
    getSunlightPosition(date = sM$time [i], lat = sM$latitude [i], lon = sM$longitude [i])$altitude
  })
  sunDeg <- sunAlt / pi * 180
  isTRUE (any (sunDeg < 0))
}



## climatology of ctd casts -- from SEABIRD data-frame
## process all variables
climatologyCTDcast <- function (cast, timeVar){
  ## set-up data-frame first!
  ## return climatology for individual station? transect?

  ## simple approach: one station at a time
  if (length (levels (factor (cast$Match_Name))) > 1){
    stop ("submit only one cast at a time")
  }
  timeL <- levels (factor (timeVar))
  ctd$Depth.saltwater..m. <- factor (cast$Depth.saltwater..m.)
  agCTD <- with (ctd, expand.grid (timeL, Depth.saltwater..m.))

  for (j in which (names (ctd)=="Temperature_ITS90_DegC"):
       which (names (ctd)=="logTurbidity")){
    x <- ctd [,i]
    agCTD$x <- aggregate (x~tV+Depth.saltwater..m.
                          , data=ctd, FUN=mean, na.rm=TRUE)$x
    names (agCTD)[ncol (agCTD)] <- names (pDF)[i]
  }
}
#
#   if (is.character(timeVar)){
#     physOcY$tV <- factor (physOcY [,which (names (physOcY)==timeVar)])
#   }else{physOcY$tV <- factor (timeVar)}
#
#   physOcY$Match_Name <- factor (physOcY$Match_Name)
#   physOcY$Depth.saltwater..m. <- factor (physOcY$Depth.saltwater..m.)
#   agCTD <- with (physOcY, expand.grid())
#
#   for (j in 1:length (levels (physOcY$Match_Name))){ ## otherwise size blows up?!
#     pDF <- subset (physOcY, Match_Name == levels (physOcY$Match_Name)[j])
#     pDF$tV <- factor (pDF$tV); pDF$Depth.saltwater..m. <- factor (pDF$Depth.saltwater..m.)
#     agCTD <- with (pDF, expand.grid (tV, Depth.saltwater..m.))
#     names (agCTD) <- c ("tV", "Depth")
#
#     for (i in which (names (pDF)=="Temperature_ITS90_DegC"):
#          which (names (pDF)=="logTurbidity")){
#       x <- pDF [,i]
#       agCTD$x <- aggregate (x~tV+Depth.saltwater..m.
#                             , data=pDF, FUN=mean, na.rm=TRUE)$x
#       names (agCTD)[ncol (agCTD)] <- names (pDF)[i]
#     }
#     agCTD ## still need to assamble XXX !!!
#   }
# }


#' Clone CTD-cast, resulting in a new cast, but with all measurement fields left empty.
#' Need this to create dummy casts for incomplete transects.
#'
#' [cloneCTD]
#'
#' @param ctd a [ctd-class] object
#'
#' @param depth
#' @param stationID
#' @param latitude
#' @param longitude
#'
#' @return A [ctd] object
#'
#' @author Martin Renner
cloneCTD <- function (ctd, latitude=ctd@metadata$latitude
                      , longitude=ctd@metadata$longitude
                      , stationId=NULL, startTime=NULL
                      , bottom=NULL){
  # data (ctd)
  ## NA-out all data, other than depth and pressure
  for (i in 1:length (ctd@data)){
    #if (!names (ctd@data)[i] %in% c ("pressure", "depth")
    if (names (ctd@data)[i] != "pressure"){
      is.na (ctd@data[[i]]) <- TRUE
    }
  }
  ctd@metadata$latitude <- latitude
  ctd@metadata$longitude <- longitude

  if (length (stationId)>0){
    ctd@metadata$stationId <- stationId
  }else {ctd@metadata$stationId <- NA}
  if (length (startTime)>0){
    ctd@metadata$startTime <- startTime
  }
  if (length (bottom)>0){
    ctd@metadata$waterDepth <- bottom
  }
  ## zero-out other metadata
  ctd@metadata$header <- ""
  ctd@processingLog$time <- ""
  ctd@processingLog$value <- ""
  return (ctd)
}


#' Extend incomplete transect to the full length by adding dummy casts where
#' stations were missed.
#'
#' [sectionPad]
#'
#' This function is needed when sections are run on pre-defined transect
#' and some sections are incomplete, e.g. due to weather. Adding dummy
#' casts to where stations were missed should allow plotting the full length
#' of the transect (rather than rescaling to an incomplete one).
#'
#' @param section a [section-class] object
#'
#' @param transect a [data-frame] object with the fields latitude, longitude
#' , stationId. stationId needs to match the stationId in section.
#' @param parameters to be passed on to sectionSort() to specify how the resultant
#' expanded section should be sorted.
#'
#' @return A [section-class] object with the same extend as `transect`.
#'
#' @author Martin Renner
sectionPad <- function (sect, transect, ...){
  ## missing feature: bottom-depth of missing cast XXX
  ## pad only first and last?

  if (!all (c ("station", "latitude", "longitude")  %in% names (transect))){
    stop ("transect needs to have fields 'latitude', 'longitude', and 'stationId'")
  }
  ## match by stationId or geographic proximity? The later would need a threshold.
  ## determine whether section represents a complete transect
  ## will have to sectionSort at the end!!
  # for (i in 1:length (transect$stationId)){

  ## sort transect correctly! (esp. for AlongBay!)
  if (transect$line [1] == "AlongBay"){
    transect <- transect [order (transect$latitude, decreasing=FALSE),]
  }else if (transect$line [1] %in% c("4", "9")){
    transect <- transect [order (transect$latitude, decreasing=TRUE),]
  }else{
    transect <- transect [order (transect$longitude, decreasing=FALSE),]
  }

  # for (i in c(1,nrow (transect))){
  for (i in 1:nrow (transect)){
    ## only insert dummy first and last stations. skip all others to avoid overdoing things
    #  for (i in c(1, nrow(transect))){  ## loosing bottom-topography in the process :(
    #   if (!transect$stationId [i]  %in% levels (section@metadata$stationId)){
    stationIDs <- sapply (1:length (sect@data$station), FUN = function (k){
      sect@data$station[[k]]@metadata$stationId})  ## oce example files use "station", not "stationId"
    if (!transect$station [i]  %in% stationIDs){  ## current results are horrid. Not why=?
    # if (!as.character (transect$station [i])  %in% levels (sect@data$station[[1]]@metadata$stationId)){ ## this seems fragile! XXX
      #       cat ("No station", transect$stationId [i], "\n")
      ## add a dummy-station  (sectionAddCtd and sectionAddStation are synonymous)
      sect <- sectionAddCtd (sect, cloneCTD(sect@data$station [[1]]
                                                  , latitude=transect$latitude [i]
                                                  , longitude=transect$longitude [i]
                                                  , station=transect$station [i]
                                                  , bottom=transect$bottom [i]
      )
      )
    }
  }
  # section <- sectionSort (section, ...)
  ## warnings: make sure sectionSort is called next!
  sect <- KBsectionSort (sect)
  return (sect)
}



## multi-use stations, used for overlapping transects
flexTransect <- function (transect, stn){
  if (transect=="ABext"){
    swMN <- c ("4_3", "9_6", "6_2", "7_21", "7_22", paste ("AlongBay", 1:13, sep="_"))
  }else if (transect=="4"){
    swMN <- "4_3"
  }else if (transect=="9"){
    swMN <- "9_6"
  }else if (transect=="AlongBay"){
    swMN <- c (paste ("AlongBay", 1:13, sep="_"), "4_3", "9_6")
  }else {
    swMN <- stn$Match_Name [match (transect, stn$Line)][1]  ## think this over XXX !!!
  }
  stn$Line [stn$Match_Name %in% swMN] <- transect
  return (stn$Line)
}
## to be used like this
# stn$Line <- flexTransect (levels (poAll$Transect)[tn], stn)
# poAll$Transect <- stn$Line [poAll$Match_Name, stn$Match_Name]


# ## execute for each run rather than pull from .RData (which gets messed up)
# require ("cmocean")
# oCol3 <- list (
#   cmocean ("thermal")
#   , cmocean ("haline")
#   , cmocean ("turbid") #, cmocean ("matter")  # or turbid
#   , cmocean ("algae")
#   , cmocean ("solar")
#   , cmocean ("oxy")
# )

# EOF
