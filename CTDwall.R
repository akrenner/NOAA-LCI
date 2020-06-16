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
# physOc$transDate <- factor (with (physOc, paste (DateISO, Transect, sep = " T-")))
physOc$transDate <- factor (with (physOc, paste0 ("T-", Transect, " ", DateISO)))
physOc$Transect <- factor (physOc$Transect)
physOc$year <- factor (format (physOc$isoTime, "%Y"))
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

## reproject?  crop?
# nGrid <- .... ## define new grid -- the missing link
if (.Platform$OS.type == "unix"){
##  bR <- resample (bR, nGrid)
  bR <- raster ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf") ## not working in RStudio -- need to use decompressed after all?
  ## need to crop, downsample (also on Mac?), and reproject to longlat
  ## then turn into topo object
  bRg <- projectRaster(bR, crs = crs ("+proj=longlat +datum=WGS84 +units=m")) ## need to downsample first
  bRb <- as.bathy (bRg)
  bathy <- as.topo (as.bathy (bRg)); rm (bR, bRg)  # still not right
  # bathy <- as.topo (z = bRg [[1]][,,1])
  save (bathy, file = "~/tmp/LCI_noaa/cache/bathymetryZ.RData")
}else{
  # positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
  bathy <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay
}

require ("ocedata") # coastlineWorldFine



poAll <- physOc
poAll <- poAll [with (poAll, order (Transect, year, isoTime, Pressure..Strain.Gauge..db.)),]



## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object

for (j in 1:length (levels (poAll$Transect))){ # by transect
  physOcY <- subset (poAll, Transect == levels (poAll$Transect)[j])
  physOcY$year <- factor  (physOcY$year)
  for (k in 1:length (levels (physOcY$year))){ # by year
    physOc <- subset (physOcY, year == levels (physOcY$year)[k])
    physOc$transDate <- factor (physOc$transDate)


  # pdf ("~/tmp/LCI_noaa/media/ctdWall_spSections%02d.pdf" # doesn't work in Rterm.exe, ok with RStudio
  #      , height = 11, width = 8.5
  #      )
  pdf (paste0 ("~/tmp/LCI_noaa/media/CTDwall_"
               , "_T-", levels (poAll$Transect)[j]
               , "_", levels (physOcY$year)[k]
               , ".pdf")
       , height = 11, width = 8.5)
#  layout (matrix (1:6, 3)) # to control down, then across
 layout (matrix (1:8, 4)) # to control down, then across

  ## define and plot sections
  cat ("Sections to process: ", length (levels (physOc$transDate)), "\n")
  for (i in 1:length (levels (physOc$transDate))){
    xC <- subset (physOc, transDate == levels (physOc$transDate)[i])
    if (length (levels (factor (xC$Match_Name))) > 1){ ## shouldn't be necessary -- what's up with Transect = NA??
#      xC <- xC [order (xC$isoTime, xC$Pressure..Strain.Gauge..db.),]
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      #if (nrow (xC) > 1){ ## better than unreliable test above

      xC$Match_Name <- factor (xC$Match_Name)
      xC <- as.section (lapply (1:length (levels (xC$Match_Name)) # XX rewrite with %>% pipes?
                                , FUN = function (x){
                                  sCTD <- subset (xC, Match_Name == levels (Match_Name)[x])
                                  with (sCTD,
                                        as.ctd (salinity = Salinity_PSU
                                                , temperature = Temperature_ITS90_DegC
                                                , pressure = Pressure..Strain.Gauge..db.
                                                , longitude = Lon_decDegree
                                                , latitude = Lat_decDegree
                                                , station = Match_Name
                                                #, sectionId = transDate
                                                , time = isoTime
                                                , other = list (flourescence = Fluorescence_mg_m3)
                                        )
                                  )
                                }))

      pSec <- function (N, zC, ...){
        plot (xC, which = N
              #, showBottom = bathy
              , showBottom = FALSE
              , axes = TRUE, ztype = 'image', zcol = zC
              , stationTicks = TRUE, ...) # zlim?
      }
      #    par (mfrow = c(2,2))
      pSec (1, oceColorsTemperature)
      title (main = levels (physOc$transDate)[i], col = "blue")
      pSec (2, oceColorsSalinity)
      pSec (3, oceColorsDensity)
      pSec ("flourescence", oceColorsChlorophyll)  ## flourescence not included here yet -- have to go back to dataSetup.R
      # pSec (99, showStations = TRUE, coastline = "coastlineWorldFine")
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
      # mtext (levels (physOc$transDate)[i], side = 3, outer = FALSE, col = "blue")
      #}
    }
    if (i %% 5 == 0){
      cat (i, " ")
      if (i %% 100 == 0) cat ("\n")
    }
  }
  dev.off()
  cat ("\n")
  }
}
physOc <- poAll
rm (xC, i, poAll, pSec)




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




## map of study area, following https://clarkrichards.org/2019/07/12/making-arctic-maps/
library(ocedata) #for the coastlineWorldFine data
data(coastlineWorldFine)

mp <- function() {
  mapPlot(coastlineWorldFine, #projection=proj4string (bR),
          longitudelim = c(-154.2, -150.5),
          latitudelim = c(58.5, 60.5), col='grey')
}


pdf ("~/tmp/LCI_noaa/media/studyareaMap.pdf")
mp()
# mapImage (bathy, col = oceColorsGebco, breaks = seq (-500, 0, 500))
mapImage (bathy, col = oceColorsGebco, breaks = c (seq (-300, 0, 20), 2000))
mapPolygon (coastlineWorldFine, col = "gray")
mapGrid()
dev.off()

# EOF
