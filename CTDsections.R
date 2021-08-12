## make image for every survey, each section
## showing all data of each survey on one page
## for QAQC and error checking0

## produce temp file for CTDwall to process further



## issues:
## - fix O2perc scale across plots
## - fix all scales across plots??
## - add contours




# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## get CTD data (physOc) directly from CTD_cleanup.R, rather than through dataSetup.R

## load data
## start with file from dataSetup.R -- better to get data directly from CTD processing? need to add only coastline + bathy
if (length (grep ("darwin", version$os)) >0 ){
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}else{
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## turbo colors
#   ## see https://github.com/jlmelville/vizier
#   # install.packages("remotes")
#   # remotes::install_github("jlmelville/vizier")
# require ('vizier')


## set-up plot and paper size

### data prep
require ("oce")
## define sections
physOc$DateISO <- as.Date (format (physOc$isoTime, "%Y-%m-%d"))
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

## FIX !!  -- already in prepData?

## reproject?  crop? -- review!!
# nGrid <- .... ## define new grid -- the missing link
# if (.Platform$OS.type == "unix"){   ## no longer necessary!
if (1){
  ##  bR <- resample (bR, nGrid)
  bR <- raster ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf") ## not working in RStudio -- need to use decompressed after all?
  ## need to reproject to longlat
  ## then turn into topo object
  bathyP <- projectRaster(bR, crs = crs ("+proj=longlat +datum=WGS84 +units=m")) ## need to downsample first
  bathy <- as.topo (as.bathy (bathyP)); rm (bR)  # still not right
  # bathy <- as.topo (z = bRg [[1]][,,1])
  save (bathy, bathyP, file = "~/tmp/LCI_noaa/cache/bathymetryZ.RData")
#  rm (bR, bRg, bRb)
}else{
  load ("~/tmp/LCI_noaa/cache/bathymetryZ.RData")
}

## more bathymetry to fill in parts zimmerman bathymetry missses
  # positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
  # bathyL <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay
try (bathyL <- getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay


require ("ocedata") # coastlineWorldFine



poAll <- physOc
poAll <- poAll [with (poAll, order (Transect, year, isoTime, Pressure..Strain.Gauge..db.)),]


## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object

## AlongBay (tn=6), i = 2, sections to process: 4, k= 5 fails


poAll <- subset (poAll, Station %in% as.character (1:12)) # cut out portgraham and pogi -- or translate
poAll$Transect <- factor (poAll$Transect)


## histogram and QAQC -- do this in previous script?!?
## add some thresholds/QAQC; log-scale?
# poAll$Salinity_PSU <- ifelse (poAll$Salinity_PSU < )

poAll$Density_sigma.theta.kg.m.3 <- ifelse (poAll$Density_sigma.theta.kg.m.3 < 15, NA, poAll$Density_sigma.theta.kg.m.3)
poAll$Oxygen_SBE.43..mg.l. <- ifelse (poAll$Oxygen_SBE.43..mg.l. <= 0, NA, poAll$Oxygen_SBE.43..mg.l.)
poAll$Oxygen.Saturation.Garcia.Gordon.mg.l. <-  ifelse (poAll$Oxygen.Saturation.Garcia.Gordon.mg.l. <= 0, NA, poAll$Oxygen.Saturation.Garcia.Gordon.mg.l.)
## recalc other O2 values here?
poAll$logPAR <- log (poAll$PAR.Irradiance)
poAll$Salinity_PSU <- ifelse (poAll$Salinity_PSU < 20, NA, poAll$Salinity_PSU)
# poAll$logFluorescence <- log (poAll$Fluorescence_mg_m3)
poAll$logTurbidity <- log (poAll$turbidity)


## add bathymetry to CTD metadata
require (sp)
poP <- poAll
coordinates (poP) <- ~longitude_DD+latitude_DD
proj4string(poP) <- crs ("+proj=longlat +datum=WGS84 +units=m")
poAll$bathy <- extract (bathyP, poP)
if (exists ("bathyL")){
bL <- extract (as.raster (bathyL), poP)
poAll$bathy <- ifelse (is.na (poAll$bathy), -1* bL, poAll$bathy)
}
rm (poP, bL, bathyP, bathyL, bathy)


## define surveys (by date)
# surveyW <- ifelse (duplicated(poAll$DateISO), NA, poAll$DateISO)
surveyW <- poAll$DateISO
is.na (surveyW [which (duplicated (poAll$DateISO))]) <- TRUE
# sqlite frame?  -- needs to be sequential (can't parallelize?)
iX <- which (!is.na (surveyW))
for (h in 1: (length (iX)-1)){
  if (difftime (poAll$isoTime [iX [h+1]], poAll$isoTime [iX [h]]
                , units = "days") < 7){
    surveyW [iX [h] : iX [h+1]] <- surveyW [iX [h]]
  }else{
    surveyW [iX [h] : (iX [h+1])-1] <- surveyW [iX [h]]
  }
}
rm (iX)
## fill last survey
surveyW [which (is.na (surveyW))] <- max (poAll$DateISO, na.rm = TRUE)
poAll$survey <- factor (surveyW); rm (surveyW, h)


save.image ("~/tmp/LCI_noaa/cache/ctdwall0.RData") # use this for CTDwall.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall0.RData")


################ define variables and their ranges #########################
# now in CTDsectionFcts.R --? no, need oRange in here and rest is dependend on it

oVars <- c ("temperature"
            , "salinity" #, "sigmaTheta"
            , "turbidity"
            # , "logTurbidity"
            , "fluorescence" #, "chlorophyll"
            , "PAR"
            #, "logPAR"
            #, "logFluorescence"
            , "O2perc"
)

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
require ('vizier')
oCol <- list (
  # turbo
  oceColorsTemperature
  , oceColorsSalinity #, oceColorsDensity
  , oceColorsTurbidity
  , oceColorsChlorophyll
  , oceColorsPAR  #, turbo #
  , oceColorsOxygen
)

oCol <- function (i, N = NULL){
#oCol <- list (
  outC <- list (
  # turbo (N)
  oceColorsTemperature (N)
  , oceColorsSalinity (N) #, oceColorsDensity
  , oceColorsTurbidity (N)
  , oceColorsChlorophyll (N)
  , oceColorsPAR (N)  #, turbo #
  , oceColorsOxygen (N)
  )
  outC [[i]]
}

oRange <- t (sapply (c ("Temperature_ITS90_DegC", "Salinity_PSU" #, "Density_sigma.theta.kg.m.3"
                        , "turbidity"
                        # , "logTurbidity"
                        , "Fluorescence_mg_m3" #, "PAR.Irradiance"
                        , "logPAR"
                        , "O2perc")
                     , FUN = function(vn){range (poAll [,which (names (poAll) == vn)], na.rm = TRUE)
                       #  quantile (poAll [,which (names (poAll) == vn)], na.rm = TRUE, c(0.01, 0.99), type = 8)
                     }))
# if (length (oVars) != length (oCol)){stop ("fix the code above: one color for each variable")}
###########################################################################


## show data availability by year
aggregate (Date~year+Transect, poAll, function (x){length (levels (factor (x)))})[,c(2,1,3)]




save.image ("~/tmp/LCI_noaa/cache/ctdwall1.RData") # use this for CTDwall.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall1.RData")


source ("CTDsectionFcts.R")  # get pSec to plot sections


test <- TRUE
test <- FALSE

dir.create("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", showWarnings = FALSE, recursive = TRUE)

if (test){iX <- 4}else{iX <- 1:length (levels (poAll$survey))}
for (sv in iX){
  if (sv %% 10 == 0){cat (sv, "/", max (iX), "\n")}
  s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
  s$Transect <- factor (s$Transect)
  if (test){iY <- 1}else{iY <-  1:length (levels (s$Transect))}# by transect
  for (tn in iY){  ## XXX testing XXX
    #  for (tn in 1:length (levels (poAll$Transect))){
    ## for testing
    ## sv <- 21; tn <- 1
    # s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
    # s$Transect <- factor (s$Transect)

    ## double-use stations:
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
    if (levels (s$Transect)[tn] == "AlongBay"){
      s$Transect [(s$Transect == "4") & (s$Station == "3")] <- "AlongBay"
      s$Transect [(s$Transect == "9") & (s$Station == "6")] <- "AlongBay"
    }
    if (levels (s$Transect)[tn] == "4"){
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "3")] <- "4"
    }
    if (levels (s$Transect)[tn] == "9"){
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "6")] <- "9"
    }


    phT <- subset (s, Transect == levels (s$Transect)[tn])
    phT$transDate <- with (phT, paste0 ("T-", Transect, " ", DateISO))


    if (length (levels (factor (phT$Match_Name))) > 2){ ## shouldn't be necessary -- what's up with Transect = NA??
      xC <- phT
      if (xC$Transect [1] %in% c("4", "9")){
        xC <- xC [order (xC$latitude_DD, decreasing = TRUE),]
      }else{
        xC <- xC [order (xC$longitude_DD, decreasing = FALSE),]
      }
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      #if (nrow (xC) > 1){ ## better than unreliable test above

      ## average multiple casts on same date?? XXX

      png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", levels (poAll$survey)[sv]
                   , " T-", levels (s$Transect)[tn]
                   # , "%02d
                   ,".png")
           # , height = 8.5*200, width = 11*200, res = 300  # landscape
           , height = 11*200, width = 8.5*200, res = 300 # portrait
      )

      # pdf (paste0 ("~/tmp/LCI_noaa/media/CTDwall/", oVars [ov]
      #              , " T-", levels (poAll$Transect)[tn]
      #              # , "_", levels (physOcY$year)[k]
      #              , ".pdf")
      #      , height = 8.5, width = 11)
      #      layout (matrix (1:9, 3, byrow = FALSE)) # across, then down
      layout (matrix (1:8, 4, byrow = FALSE)) # across, then down
      #      layout (matrix (1:8, 2, byrow = TRUE)) # across, then down


      xCo <- sectionize (xC)

      for (ov in 1:length (oVars)){
        if (ov %in% c(4,5,6)){ # fix scale for O2, fluorescence, logPAR
          zR <- oRange [ov,]
        }else{
          cDF <- with (xC, data.frame (Temperature_ITS90_DegC, Salinity_PSU, turbidity, Fluorescence_mg_m3, logPAR, O2perc))
          cDF <- sapply (1:ncol (cDF), function (i){ifelse (!is.finite (cDF[,i]), NA, cDF[,i])})
          zR <- range (cDF [,ov], na.rm = TRUE); rm (cDF)
        }
        pSec (xCo, N = oVars [ov], zcol = oCol [[ov]]
               # , zlim = oRange [ov,]
               , zlim = zR
               #               , xlim = xRange []  # range of the Transect
               # , custcont = pretty (oRange [ov,], 10)
               # , axes = FALSE  ## not worth the hassle of messing with it
        )
        rm (zR)
        # if (ov == 1){
        #   title (main = paste0 ("T", levels (s$Transect)[tn], " ", levels (poAll$survey)[sv]), col = "blue")
        # }
      }
      mtext (paste0 ("T", levels (s$Transect)[tn], " ", levels (poAll$survey)[sv])
             , side = 3, outer = TRUE, line = -0.9, cex = 0.7)
      plot (xCo  ## large LCI map -- trouble to keep range constant -- start from scratch??
            , which = 99
            , coastline = "coastlineWorldFine"
            , showStations = TRUE
            , gird = TRUE
            , map.xlim = range (poAll$longitude_DD) # +c(-0.5, 0.5)
            # , map.ylim = range (poAll$latitude_DD)+c(-0.3, 0.3)
            ## , map.xlim = c(-154, -151)
            ## , map.ylim = c(57.5, 60.1)
             , clatitude = mean (range (poAll$latitude_DD)) # 59.4
             , clongitude = mean (range (poAll$longitude_DD)) # -152
             , span = 200
            # , showSpine = TRUE
      )
      plot (xCo
            , which = 99
            , coastline = "coastlineWorldFine"  ## add hi-res topography?
            , showStations = TRUE
            , showStart = TRUE
            , gird = TRUE
            # , col = "red"
      )
      dev.off()
    }
  }
}

rm (iY)

physOc <- poAll
if (!test){
 # rm (xCo, tn, oVars, ov, poAll, pSec)
  gc()
}



# EOF
