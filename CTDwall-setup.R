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
require ("raster", quietly = TRUE)
require ("marmap")

## FIX !!  -- already in prepData? -- move to prepData!

## reproject?  crop? -- review!!
# nGrid <- .... ## define new grid -- the missing link
# if (.Platform$OS.type == "unix"){   ## no longer necessary!
if (0){
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





save.image ("~/tmp/LCI_noaa/cache/ctdwall0.RData")
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
            , "O2 [mg/L]"  # , "O2perc"
)
oVarsF <- c ("temperature"
             , "salinity" #, "sigmaTheta"
             , "turbidity"
             # , "logTurbidity"
             , "fluorescence" #, "chlorophyll"
             , "PAR"
             #, "logPAR"
             #, "logFluorescence"
             , "Oxygen"  # , "O2perc"
)

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
## move these into CTDsectionFcts.R -- or not?

require ('vizier')
require ("cmocean")

oColF <- function (i){ ## not used
  require ("cmocean")
  options ('cmocean-version' = "2.0")
  cF <- oCol3 <- list (  ## fix versions?
    oceColorsTurbo # cmocean ("thermal")
    , cmocean ("haline")
    , cmocean ("turbid") #, cmocean ("matter")  # or turbid
    , cmocean ("algae")
    , cmocean ("solar")
    , cmocean ("oxy")
    , cmocean ("haline")
  )
  cF [[i]]
}


options ('cmocean-version' = "2.0") # fix colors to cmocean 2.0
oCol3 <- list (  ## fix versions?
  # oceColorsTurbo #
  cmocean ("thermal")
  , cmocean ("haline")
  , cmocean ("turbid") #, cmocean ("matter")  # or turbid
  , cmocean ("algae")
  #, oceColorsTurbo # cmocean ("solar")
  , function (n){
    require ("vizier")
    turbo (n, start = 0.25, end = 0.8)
  }
  , cmocean ("oxy")
  , cmocean ("haline")

)
oCol <- list (  ## old, not used
  # turbo
  oceColorsTemperature
  , oceColorsSalinity #, oceColorsDensity
  , oceColorsTurbidity
  , oceColorsChlorophyll
  , oceColorsPAR  #, turbo #
  , oceColorsOxygen
)



oRange <- t (sapply (c ("Temperature_ITS90_DegC"
                        , "Salinity_PSU" #, "Density_sigma.theta.kg.m.3"
                        , "turbidity" # , "logTurbidity"
                        , "Fluorescence_mg_m3"
                        , "PAR.Irradiance"  #, "logPAR"
                        , "Oxygen_SBE.43..mg.l.")
                     , FUN = function(vn){range (poAll [,which (names (poAll) == vn)], na.rm = TRUE)
                       #  quantile (poAll [,which (names (poAll) == vn)], na.rm = TRUE, c(0.01, 0.99), type = 8)
                     }))
oRange [2,1] <- 25 # fix min salinity
oRange [5,] <- c(0,100)      # fix PAR range
# oRange [6,] <- c (-0.1,1.5)  # fix O2 perc range
oRange [6,] <- c (2,12)  # fix O2 conc range. Gulf of Mexico: low O2 = 5 and lower (down to 1-2 mg/L)

# if (length (oVars) != length (oCol)){stop ("fix the code above: one color for each variable")}
###########################################################################


## show data availability by year
aggregate (Date~year+Transect, poAll, function (x){length (levels (factor (x)))})[,c(2,1,3)]




save.image ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData") # use this for CTDwall.R
