## functions to plot CTD sections or transects

## derived from oce
## emulate/evolve from ODV

pSec <- function (xsec, N, zC, cont = TRUE, custcont = NULL, ...){
  s <- try (plot (xsec, which = N
                  # , showBottom = FALSE, showBottom = "lines" #FALSE
                  , axes = TRUE
                  , zcol = zC
                  , stationTicks = TRUE
                  , showStations = TRUE
                  # , grid = TRUE
                  #, ztype = "contour"
                  , ztype = "image"
                  , ...
  ))


  ## add contours
    #  if (cont){
  distance <- xsec [["distance", "byStation"]]
  depth <- xsec [["station", 1]][["depth"]]
  zvar <- matrix (xsec [[N]], byrow = TRUE, nrow = length (xsec [["station"]]))

  if (0){  ## need to get clean data in first --- binned by depth, not pressure
    # deepest <- max (sapply (1:length (xsec@data$station), function (x){length (xsec@data[[1]][[x]]@data$scan)}))
    deeps <- sapply (1:length (xsec@data$station), function (x){length (xsec [["depth", "byStation"]][[x]])})
    depth <- xsec [["station", which.max (deeps)]][["depth"]] ## depth needs to be gridded first?!?

    zvar <- sapply (1:length (xsec@data$station), function (x){
      out <- xsec [[N, "byStation"]][[x]]
      length (out) <- max (deeps)
      out
    }
    )
  }

  if (length (custcont) > 1){
    cLev <- custcont
  }else{
    cLev <- pretty (range (as.numeric (zvar), na.rm = TRUE), 4)
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
  #if (cont){return (s)}
}


pSec1 <- function (xsec, N, zC, cont = TRUE, custcont = NULL, ...){
  ## old version without contours -- keep for now, but eventually replace with pSec
    s <- try (plot (xsec, which = N
                  , zcol = zC
                  , stationTicks = TRUE
                  , showStations = TRUE  # or roll your own -- how?
                  , ztype = "image"
                  , ...
  ))
}


sectionize <- function (xC){  ## keep this separate as this function is specific to Kachemak Bay
  stn <- factor (sprintf ("%02s", xC$Station), ordered = TRUE)  ## does this order them??
  if (xC$Transect [1] %in% as.character (c(4,6,9))){stn <- factor (stn, levels = rev (levels (stn)), ordered = TRUE)}
  xC$Match_Name <- factor (xC$Match_Name)
  xCo <- makeSection (xC, stn)
  xCo
}

makeSection <- function (xC, stn){
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
                        ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                        ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                        ocOb <- oceSetData (ocOb, "logPAR", sCTD$logPAR)
                        # ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                        # ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                        ocOb
                      }))

}



seasonize <- function (mon, breaks = c (0,2,4,8,10,13)){
# use seasonal breaks from zooplankton study
   if (is.factor (mon)){
   mon <- as.numeric (as.character (mon))
 }else if (class (mon)[1] == "POSIXct"){
   mon <- as.numeric (format (mon, "%m"))
 }
  season <- cut (mon, breaks, labels = c ("winter", "spring", "summer", "fall", "winter"))
  # season <- factor (season
  #                   , levels = c ("winter", "spring", "summer", "fall")
  #                   , ordered = TRUE)
  season
}





