## functions to plot CTD sections or transects

## derived from oce
## emulate/evolve from ODV

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
                  , ...
  ))
#  title (main = levels (physOc$transDate)[i])
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


pSec1 <- function (xsec, N, zC, cont = TRUE, custcont = NULL, ...){  # ylim = NULL,
    # if (exists (""))
    # c(0,max (physOc$bathy))
    s <- try (plot (xsec, which = N
                  # , showBottom = FALSE
                  # , showBottom = "lines" #FALSE
                  # , axes = TRUE
                  , zcol = zC
                  , stationTicks = TRUE
                  , showStations = TRUE  # or roll your own -- how?
                  #, showBottom = TRUE # or could provide topo object
                  # , grid = TRUE
                  #, ztype = "contour"
                  , ztype = "image"
                  #, ztype = "points"
                 #  , ylim = yL
                  , ... # zlim?
  ))
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





