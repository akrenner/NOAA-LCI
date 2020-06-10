## replot CTD wallpaper for office
## provide line-graph alternatives


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size
## bathymetry and coastline
if (0){
 tD  <- tempdir()
 bF <- download.topo (west=-154, east=-150, south=58.5, north=60.3
                    , resolution =1
                      , destdir = tD
                          )
 bathy <- read.topo (bF)
 unlink (tD)
 rm (tD, bF)
}
require ("ocedata")
#mapPlot (coastlineWorld, type = "l", col = "gray")


### data prep
require ("oce")
## define sections
physOc$DateISO <- format (physOc$isoTime, "%Y-%m-%d")
physOc$transDate <- factor (with (physOc, paste (DateISO, Transect, sep = " T-")))
## combine CTD and station meta-data
physOc <- subset (physOc, !is.na (physOc$Transect))
## stn <- stn [,which (names (stn) %in%
##                     c("Line", "Station", "Match_Name", "Lon_decDegree", "Lat_decDegree", "Depth_m")
##                     )]
physOc <- cbind (physOc, stn [match (physOc$Match_Name, stn$Match_Name)
                            , which (names (stn) %in% c("Line", "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor (physOc$Match_Name)
# print (summary (physOc))

# pdf ("~/tmp/LCI-noaa/media/ctdWall.pdf")
# pdf ("~/Documents//tmp/media/ctdWall.pdf")
pdf ("ctdWallLines.pdf")
## define and plot sections
cat ("Sections to process: ", length (levels (physOc$transDate)))
for (i in 1:length (levels (physOc$transDate))){
# for (i in 1:6){
#    pdf (paste0 ("CTDwall-", levels (physOc$transDate)[i], ".pdf"))
    xC <- subset (physOc, transDate == levels (physOc$transDate)[i])
    if (length (levels (as.factor (xC$Match_Name))) > 1){
        xC <- xC [order (xC$isoTime, xC$Pressure..Strain.Gauge..db.),]
        ## arrange ctd data into sections
        ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html
        xC <- with (xC # subset (physOc, transDate == levels (physOc$transDate)[i])
                  , as.section (salinity = Salinity_PSU
                              , temperature = Temperature_ITS90_DegC
                              , pressure = Pressure..Strain.Gauge..db.
                              , longitude = Lon_decDegree
                              , latitude = Lat_decDegree
                              , station = Match_Name
                              , sectionId = transDate
                                ))
        plot (xC
            , which = c (1,2,3,99) # temp, sal, sigmaTheta, map
            , coastline = "best"      # replace with local
            , showBottom = "polygon"  # better: provide a "topo" object
                                        # showBottom = bathy
                                        #        , drawPalette = TRUE
           ,  axes = TRUE
           , showStations = TRUE
                                        #        , ztype = 'image'
                                        #        , zcol = oceColorsTemperature # yellow to purple
                                        #         , zcol = oceColorsSalinity   # yellow-green to blue
                                        #        , zcol = oceColorsDensity       # dark-purple to light-blue
                                        # , col = oceColorsTemperature (5)  ## default = horrible jet colors
###          , mar = c()
             )
        mtext (levels (physOc$transDate)[i], side = 3, outer = FALSE, col = "blue")
    }
    if (i %% 10 == 0){
        cat (i, " ")
        if (i %% 100 == 0) cat ("\n")
    }
}
cat ("\n")
dev.off()
rm (xC, i)


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
            , xtype = "time"
              ## need to define proper z-matrix!
            , coastline = "best"
              )
        }
    }
 #   plot (xC, xtype = "time")
}
dev.off()

# EOF
