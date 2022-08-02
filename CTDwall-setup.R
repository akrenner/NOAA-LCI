## produce temp file for CTDwall to process further
## define color ramps
## define data ranges
## clean data (move to earlier??)


## issues:
## - fix O2perc scale across plots
## - fix all scales across plots??
## - add contours
## attenuation vs turbidity



# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## get CTD data (physOc) directly from CTD_cleanup.R, rather than through dataSetup.R

## load data
## start with file from dataSetup.R -- better to get data directly from CTD processing? need to add only coastline + bathy
if (length (grep ("darwin", version$os)) >0 ){
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}else{
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}

if (!require("pacman", quietly=TRUE)){install.packages("pacman", repos = "http://cran.fhcrc.org/", dependencies = TRUE)}
Require <- pacman::p_load

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!


### data prep
Require ("oce")
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
Require ("raster")  ## move to terra/stars
Require ("marmap")

## FIX !!  -- already in prepData? -- migrate to prepData!

## reproject?  crop? -- review!!
# nGrid <- .... ## define new grid -- the missing link
if (0){
  ##  bR <- resample (bR, nGrid)
  ## migrate to terra/stars
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

## more bathymetry to fill in parts Zimmerman bathymetry missses
## here or in CTDwall.R??
# positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
# bathyL <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay
try (bathyL <- getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay


Require ("ocedata") # coastlineWorldFine

## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object


# poAll <- subset (physOc, Station %in% as.character (1:12)) # cut out portgraham and pogi -- or translate -- keep them in!
poAll <- physOc
rm (physOc)
poAll$Transect <- factor (poAll$Transect)



## log transformations
slog <- function (x){
  x <- suppressWarnings (log (x))
  x <- ifelse (is.infinite(x), NA, x)
  x
}


poAll$logPAR <- slog (poAll$PAR.Irradiance)
# poAll$logFluorescence <- slog (poAll$Fluorescence_mg_m3)
poAll$logTurbidity <- slog (poAll$turbidity)
rm (slog)



## add bathymetry to CTD metadata
poAll$Bottom.Depth_main <- stn$Depth_m [match (poAll$Match_Name, stn$Match_Name)]

Require (sp)
poP <- poAll
## migrate to sf, stars/terra
coordinates (poP) <- ~longitude_DD+latitude_DD
proj4string(poP) <- crs ("+proj=longlat +datum=WGS84 +units=m")
poAll$bathy <- extract (bathyP, poP)
poAll$Bottom.Depth_survey <- extract (bathyP, poP)
if (exists ("bathyL")){
  bL <- extract (marmap::as.raster (bathyL), poP)
  poAll$Bottom.Depth_survey <-  ifelse (is.na (poAll$Bottom.Depth_survey)
                                        , -1* bL, poAll$Bottom.Depth_survey) ## or leave them as NA?
  poAll$bathy <- poAll$Bottom.Depth_survey
}
poAll$bathy <- poAll$Bottom.Depth_survey
rm (poP, bL, bathyP, bathyL, bathy)



save.image ("~/tmp/LCI_noaa/cache/ctdwall0.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall0.RData")



##################### define surveys (by date-range) ##########################
# surveyW <- ifelse (duplicated(poAll$DateISO), NA, poAll$DateISO)
# poAll <- poAll [order (poAll$Transect, poAll$isoTime),]
# surveyT <- factor (with (poAll, paste (Transect,)))
#
# for (i in 1:length (levels (factor (poAll$Transect)))){
# }


poAll <- poAll [order (poAll$isoTime),]
surveyW <- factor (poAll$DateISO)

for (h in 2:length (levels (surveyW))){
  if (difftime (levels (surveyW)[h], levels (surveyW)[h-1], units = "days") < 7){
    surveyW [which (surveyW == levels (surveyW)[h])] <- levels (surveyW)[h-1]
  }
}
poAll$survey <- factor (surveyW)  ## need to reset factor levels after combining days
rm (surveyW, h)

## migrate code over from CTDwall.R:
## several surveys in one month
## several surveys on one day
## replicate station casts
## if two surveys in one month, asign early survey to previous month if that month is empty

# ## new, slow version -- but reliable?
# for (h in 2:length (surveyW)){
#   if (difftime (poAll$isoTime [h], poAll$isoTime [h-1], units = "days") < 7){
#     surveyW [h] <- surveyW [h-1]
#   }
# }
# poAll$survey <- factor (surveyW); rm (surveyW, h)

## check --- QAQC
if (0){
  cat ("Surveys window QAQC -- testing code\n")
  for (i in 1:length (levels (poAll$survey))){
    x <- subset (poAll, survey == levels (poAll$survey)[i])
    if (1){  #length (levels (factor (x$DataISO))) > 1){
    cat (i, levels (factor (x$DateISO)), "\n")
  }}
  rm (x, i)
}



################ define variables and their ranges #########################
# now in CTDsectionFcts.R --? no, need oRange in here and rest is dependend on it

oVars <- c ("temperature"
            , "salinity"
            , "sigmaTheta"
            , "turbidity" # it's really turbidity/attenuation # , "logTurbidity"
            , "fluorescence-chl [mg/m^3]" #, "chlorophyll" #, "logFluorescence"
            # , "PAR"
            , "logPAR"
            , "Oxygen [umol/kg]"  # , "O2perc"
)
oVarsF <- c ("temperature"    # need diffrent name for oxygen to use in function
             , "salinity"
             , "sigmaTheta"
             , "turbidity" # , "logTurbidity"
             , "fluorescence" #, "chlorophyll" #, "logFluorescence"
#             , "PAR.Irradiance" #, "logPAR"
            , "logPAR"
             , "Oxygen_umol_kg"  # , "O2perc"
)

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
## move these into CTDsectionFcts.R -- or not?

# Require ('vizier')
Require ("cmocean")  ## for color ramps


options ('cmocean-version' = "2.0") # fix colors to cmocean 2.0
oCol3 <- list (  ## fix versions?
   oceColorsTurbo  # cmocean ("thermal")
  , cmocean ("haline")
  , cmocean ("dense")
  , cmocean ("turbid") #, cmocean ("matter")  # or turbid
  , cmocean ("algae")
  #, oceColorsTurbo # cmocean ("solar")
  , function (n){
    Require ("viridis")
    turbo (n, start=0.25, end=0.8)
  }
  , cmocean ("oxy")
  , cmocean ("haline") # why is this here? should it be??
)
## oceColorsTemperature and the likes are dated -- don't use them
## (stick to algorithmic pic of scale limits. Cleanups.)


oRange <- t (sapply (c ("Temperature_ITS90_DegC"
                        , "Salinity_PSU"
                        , "Density_sigma.theta.kg.m.3"
                        , "turbidity" # , "logTurbidity"
                        , "Fluorescence_mg_m3"
                        # , "PAR.Irradiance"
                        , "logPAR"
                        # , "Oxygen_SBE.43..mg.l."  # change to umol.kg.! XXX
                        , "Oxygen_umol_kg"
                        )
                     #, FUN = function(vn){range (poAll [,which (names (poAll) == vn)], na.rm = TRUE)
                       , FUN = function(vn){quantile (poAll [,which (names (poAll) == vn)], probs = c(0.01,0.99), na.rm = TRUE)
                         #  quantile (poAll [,which (names (poAll) == vn)], na.rm = TRUE, c(0.01, 0.99), type = 8)
                     }))
## better to do this with colormap(S, breaks=...)? See https://www.clarkrichards.org/2016/04/25/making-section-plots-with-oce-and-imagep/

## manually tune some of these ranges
# oRange [1,1] <- 1.5 # fix min temperature
# oRange [2,1] <- 27 # fix min salinity  -- 28 about as high as one could go (observed: 20-32, quantile: 29-32)
# oRange [6,] <- c(-3,5)      # fix logPAR range
## what's better to use here, umol/kg or mg/l?
# oRange [7,] <- c (-0.1,1.5)  # fix O2 perc range
# oRange [7,] <- c (2,12)  # fix O2 conc range. Gulf of Mexico: low O2 = 5 and lower (down to 1-2 mg/L)
# https://repository.oceanbestpractices.org/bitstream/handle/11329/417/56281.pdf?sequence=1&isAllowed=y
## umol/l = 31.2512* cO2 mg/l
# oRange [7,] <- c (2,12) * 31.2512  ## this is messed up!
# if (length (oVars) != length (oCol)){stop ("fix the code above: one color for each variable")}
###########################################################################

if (0){
  hist (poAll$Temperature_ITS90_DegC)
  abline (v = oRange [1,])
}


save.image ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData") # use this for CTDwall.R
# save (oRange, oCol, poAll, file="~/tmp/LCI_noaa/cache/ctdwallSetup.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")
cat ("\n\n             ### End of CTDwall-setup.R ###\n\n\n")
# EOF
