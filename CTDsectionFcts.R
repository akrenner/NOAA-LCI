## functions to plot CTD sections or transects

## derived from oce
## emulate/evolve from ODV


pSec <- function (xsec, N, cont = TRUE, custcont = NULL, zCol, ...){
  ## old version without contours -- keep for now, but eventually replace with pSec
  s <- try (plot (xsec, which = N
                  , zcol = zCol
                  , stationTicks = TRUE
                  , showStations = TRUE  # or roll your own -- how?
                  , ztype = "image"
                  # , grid = TRUE
                  , ...
  ), silent = TRUE)
  if (class (s) == "try-error"){
    plot (1:10, type = "n", new = FALSE)
    text (5,5, paste0 (N, " all values NA"))
  }
}



pSec <- function (xsec, N, cont = TRUE, custcont = NULL, zCol, transect = NULL, ...){
  ## as above, but add contours. Replace pSec once this is working
  ## hybrid approach -- still use build-in plot.section (for bathymetry)
  ## but manually add contours
  s <- try (plot (xsec, which = N
                  # , showBottom = FALSE, showBottom = "lines" #FALSE
                  , axes = TRUE
                  , stationTicks = TRUE
                  , showStations = TRUE
                  # , grid = TRUE
                  # , ztype = "contour"
                  , ztype = "image"
                  , zcol = zCol
                  , transect = transect
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
  if (packageVersion("oce") <= "1.7.3"){  # change <= to < after next oce release
    stop ("Need package:oce version 1.7.4 or later")
  }
  require ("oce")

  stn <- factor (sprintf ("%02s", xC$Station), ordered = TRUE)  ## does this order them??
  #  stn <- factor (sprintf ("%03.0f", as.integer (xC$Station)), ordered = TRUE)  ## does this order them??
  if (xC$Transect [1] %in% as.character (c(4,6,9))){
    stn <- factor (stn, levels = rev (levels (stn))
                   , ordered = TRUE)}
  xC$Match_Name <- factor (xC$Match_Name)
  xCo <- makeSection (xC, stn)
  xCo <- sectionGrid (xCo, p=standardDepths(3), method = "boxcar", trim = TRUE) # should understand this step more fully! XXX

  ## sort by lat/lon instead -- or StationID (would need to re-assign)
  ## sort in here, rather than separately
  xCo <- sectionize (xC)
  if (xC$Transect [1] == "AlongBay"){ # extended AlongBay wraps around Pogy Ptp
    xCo <- sectionSort (xCo, "latitude", decreasing = FALSE)
  }else if (xC$Transect [1] %in% c("4", "9")){  # requires new version of oce
    xCo <- sectionSort (xCo, "latitude", decreasing = TRUE)
  }else{
    xCo <- sectionSort (xCo, "longitude", decreasing = FALSE)
  }
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
                                              , longitude = longitude_DD
                                              , latitude = latitude_DD
                                              , station = Match_Name
                                              #, sectionId = transDate
                                              , time = isoTime
                                      ))
                        # ocOb@metadata$waterDepth <- sCTD$Bottom.Depth [1]
                        ocOb@metadata$waterDepth <- sCTD$bathy [1]
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

