## functions to plot CTD sections or transects

## derived from oce
## emulate/evolve from ODV


pSec <- function (xsec, N, cont = TRUE, custcont = NULL, zCol
                  , showBottom=TRUE, ...){
  ## as above, but add contours. Replace pSec once this is working
  ## hybrid approach -- still use build-in plot.section (for bathymetry)
  ## but manually add contours
  ## XXX missing feature XXX : color scale by quantiles XXX
  s <- try (plot (xsec, which = N
                  , showBottom = showBottom
                  , axes = TRUE
                  , stationTicks = TRUE
                  , showStations = TRUE
                  # , grid = TRUE
                  # , ztype = "contour"
                  , ztype = "image"
                  , zcol = zCol
                  , ...
  )
  , silent = TRUE)
  if (class (s) != "try-error"){
    s <- xsec
    nstation <- length(s[['station']])
    depth <- unique(s[['depth']])
    np <- length(depth)
    zvar <- array(NA, dim=c(nstation, np))
    for (ik in 1:nstation) {
      try (zvar [ik, ] <- s[['station']][[ik]][[ N ]])
    }
    distance <- unique(s[['distance']])

    if (sum (!apply (zvar, 2, FUN = function (x){all (is.na (x))})) < 2){
      plot (1:10, type = "n", new = FALSE)
      text (5,5, paste0 (N, " all values NA"))
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
      }
      cT <- try (contour (distance, depth, zvar, add = TRUE
                          , nlevels = 5
                          # , levels = cLev  ## error XXX
                          , col = "black", lwd = 1), silent = TRUE)
      if (class (cT) == "try-error"){
        legend ("bottomleft", legend = "no contours")
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



sectionize <- function (xC){  ## keep this separate as this function is specific to Kachemak Bay
  if (packageVersion("oce") <= "1.7.3"){
    stop ("Need package:oce version 1.7.4 or later")
  }
  require ("oce")
  stn <- factor (sprintf ("%02s", xC$Station))
  xC$Match_Name <- factor (xC$Match_Name)
  xCo <- makeSection (xC, stn)
  ## sort by lat/lon instead -- or StationID (would need to re-assign)
  ## sort in here, rather than separately
  if (xC$Transect [1] %in% c ("AlongBay", "9")){ # extended AlongBay wraps around Pogy Ptp
    xCo <- sectionSort (xCo, "latitude", decreasing = FALSE)
  }else if (xC$Transect [1] %in% c("4")){  # requires new version of oce
    xCo <- sectionSort (xCo, "latitude", decreasing = TRUE)
  }else{
    xCo <- sectionSort (xCo, "longitude", decreasing = FALSE)
  }
   xCo <- sectionGrid (xCo, p=standardDepths(3), method = "boxcar", trim = FALSE) # should understand this step more fully! XXX
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
                                              , time = isoTime
                                      ))
                        # if (is.na (sCTD$bathy [1])){
                        if (1){
                          ocOb@metadata$waterDepth <- sCTD$Bottom.Depth [1]  ## always use Bottom.Depth?
                        }else{
                          ocOb@metadata$waterDepth <- sCTD$bathy [1]
                        }
                        ocOb@metadata$longitude <- sCTD$longitude_DD [1]
                        ocOb@metadata$latitude <- sCTD$latitude_DD [1]
                        ocOb@metadata$stationId <- sCTD$Match_Name [1]

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
sectionPad <- function (section, transect, ...){
  ## missing feature: bottom-depth of missing cast XXX
  ## pad only first and last!! XXX

  if (!all (c ("stationId", "latitude", "longitude")  %in% names (transect))){
    stop ("transect needs to have fields 'latitude', 'longitude', and 'stationId'")
  }
  ## match by stationId or geographic proximity? The later would need a threshold.
  ## determine whether section represents a complete transect
  ## will have to sectionSort at the end!!
  for (i in c (1,length (transect$stationId))){
    ## only insert dummy first and last stations. skip all others to avoid overdoing things
    #  for (i in c(1, nrow(transect))){  ## loosing bottom-topography in the process :(
    #   if (!transect$stationId [i]  %in% levels (section@metadata$stationId)){
    stationIDs <- sapply (1:length (section@data$station), FUN = function (k){
      section@data$station[[k]]@metadata$stationId})  ## oce example files use "station", not "stationId"
    # if (!transect$stationId [i]  %in% stationIDs){  ## current results are horrid. Not why=?
    if (!as.character (transect$stationId [i])  %in% levels (section@data$station[[1]]@metadata$stationId)){ ## this seems fragile! XXX
      #       cat ("No station", transect$stationId [i], "\n")
      ## add a dummy-station  (sectionAddCtd and sectionAddStation are synonymous)
      section <- sectionAddCtd (section, cloneCTD(section@data$station [[1]]
                                                  , latitude=transect$latitude [i]
                                                  , longitude=transect$longitude [i]
                                                  , stationId=transect$stationId [i]
                                                  , bottom=transect$bottom [i]
      )
      )
    }
  }
  # section <- sectionSort (section, ...)
  ## warnings: make sure sectionSort is called next!
  return (section)
}


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

