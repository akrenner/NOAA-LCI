## replot CTD wallpaper for office
## provide line-graph alternatives


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size


### data prep
require ("oce")
## define sections
physOc$DateISO <- format (physOc$isoTime, "%Y-%m-%d")
physOc$transDate <- factor (with (physOc, paste (DateISO, Transect, sep = " T-")))
physOc$transDate <- factor (with (physOc, paste0 ("T-", Transect, " ", DateISO)))
## combine CTD and station meta-data
physOc <- subset (physOc, !is.na (physOc$Transect)) ## who's slipping through the cracks??
## stn <- stn [,which (names (stn) %in%
##                     c("Line", "Station", "Match_Name", "Lon_decDegree", "Lat_decDegree", "Depth_m")
##                     )]
physOc <- cbind (physOc, stn [match (physOc$Match_Name, stn$Match_Name)
                              , which (names (stn) %in% c(# "Line",
                                                          "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor (physOc$Match_Name)
# print (summary (physOc))

## weed out for now, but HAVE TO FIX eventually??
physOc <- subset (physOc, !is.na (Salinity_PSU))
# physOc <- subset (physOc, )





## get coastline and bathymetry
## bathymetry and coastline
bathy <- "polygon"
## Zimmermann bathymetry
require ("raster")
require ("marmap")
bR <- raster ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
## need to crop, downsample (also on Mac?), and reproject to longlat
## then turn into topo object

## reproject?  crop?
# nGrid <- .... ## define new grid -- the missing link
# bR <- resample (bR, nGrid)
# bRg <- projectRaster(bR, crs = crs ("+proj=longlat +datum=WGS84 +units=m")) ## need to downsample first
# ...
# bathy <- as.topo (as.bathy (bRg)); rm (bR, bRg)
# positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
bathy <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay


require ("ocedata")
#mapPlot (coastlineWorld, type = "l", col = "gray")




pdf ("~/tmp/LCI_noaa/media/ctdWall_spSections.pdf") # doesn't work in Rterm.exe
## define and plot sections
cat ("Sections to process: ", length (levels (physOc$transDate)))
for (i in 1:length (levels (physOc$transDate))){
  xC <- subset (physOc, transDate == levels (physOc$transDate)[i])
  if (length (levels (factor (xC$Match_Name))) > 1){ ## shouldn't be necessary -- what's up with Transect = NA??
    xC <- xC [order (xC$isoTime, xC$Pressure..Strain.Gauge..db.),]
    ## arrange ctd data into sections
    ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

    #if (nrow (xC) > 1){ ## better than unreliable test above

    xC <- with (xC # subset (physOc, transDate == levels (physOc$transDate)[i])
                , as.section (salinity = Salinity_PSU
                              , temperature = Temperature_ITS90_DegC
                              , pressure = Pressure..Strain.Gauge..db.
                              , longitude = Lon_decDegree
                              , latitude = Lat_decDegree
                              , station = Match_Name
                              , sectionId = transDate
                ))

    par (mfrow = c(2,2))
    plot (xC, which = 1, showBottom = bathy, axes = TRUE
          , ztype = 'image', zcol = oceColorsTemperature, showStations = TRUE) # XXX zlim ?!?
    plot (xC, which = 2, showBottom = bathy, axes = TRUE
          , ztype = 'image', zcol = oceColorsSalinity)
    plot (xC, which = 3, showBottom = bathy, axes = TRUE
          , ztype = 'image', zcol = oceColorsDensity)
    plot (xC, which = 99, showStations = TRUE, axes = TRUE, coastline = "coastlineWorldFine")
    if (0){
      plot (xC
            , which = c (1,2,3,99) # temp, sal, sigmaTheta, map
            #          , coastline = "best"      # replace with local
            , coastline = "coastlineWorldFine"
            , showBottom = "polygon"  # better: provide a "topo" object
            # showBottom = bathy
            ,  axes = TRUE
            , showStations = TRUE
            ###          , mar = c()
      )
    }
    mtext (levels (physOc$transDate)[i], side = 3, outer = FALSE, col = "blue")
    #}
  }
  if (i %% 5 == 0){
    cat (i, " ")
    if (i %% 100 == 0) cat ("\n")
  }
}
cat ("\n")
dev.off()

rm (xC, i)


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


# EOF
