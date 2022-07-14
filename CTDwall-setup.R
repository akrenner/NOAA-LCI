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
require ("raster", quietly = TRUE)  ## move to terra/stars
require ("marmap")

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

## more bathymetry to fill in parts zimmerman bathymetry missses
## here or in CTDwall.R??
# positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
# bathyL <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay
try (bathyL <- getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay


require ("ocedata") # coastlineWorldFine



# poAll <- physOc
# poAll <- poAll [with (poAll, order (Transect, year, isoTime, Pressure..Strain.Gauge..db.)),]


## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object

## AlongBay (tn=6), i = 2, sections to process: 4, k= 5 fails


# poAll <- subset (physOc, Station %in% as.character (1:12)) # cut out portgraham and pogi -- or translate -- keep them in!
poAll <- physOc
rm (physOc)
poAll$Transect <- factor (poAll$Transect)


## compare attenuation and turbidity -- ok to merge? QAQC
summary (poAll$turbidity)
summary (poAll$attenuation)
pdf ("~/tmp/LCI_noaa/media/CTDtests/atten-turb.pdf")
par (mfrow = c (2,1))
hist (log (poAll$beamAttenuation), xlim = range (log (c (poAll$attenuation, poAll$turbidity)), na.rm = TRUE))
hist (log (poAll$turbidity), xlim = range (log (c (poAll$attenuation, poAll$turbidity)), na.rm = TRUE))
dev.off()


pdf ("~/tmp/LCI_noaa/media/CTDtests/O2-temp.pdf")
plot (Oxygen_umol_kg ~ Temperature_ITS90_DegC, data = poAll, col = year)
# plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = poAll, col = year)
# legend ("bottomleft", col = levels (poAll$year), pch = 19, legend = levels (poAll$year))
# plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = poAll, col = as.numeric (CTD.serial))
for (i in 1:length (levels (poAll$year))){
#  plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = poAll
  plot (Oxygen_umol_kg ~ Temperature_ITS90_DegC, data = poAll
              , subset = year == levels (poAll$year)[i], col = as.numeric (CTD.serial))
  legend ("topright", col = levels (factor (as.numeric (poAll$CTD.serial)))
          , legend = levels (factor (poAll$CTD.serial)), pch = 19
          , title = levels (poAll$year)[i])
}
## issues: 2017!  (positive spike to >7). 2012: negative values. 2018: low values of 4141 (pre-callibration?)
## 2020: 5028 looks quite different than 4141. 4141 has two groups
dev.off()


# png ("~/tmp/LCI_noaa/media/CTDtests/O2-SBEvsGG.png", width = 600, height = 400)
# plot (Oxygen.Saturation.Garcia.Gordon.umol_kg~Oxygen_SBE.43..mg.l., poAll
#       , col = year, main = "colored by year"
#       , subset = Depth.saltwater..m. < 10)
# dev.off()



## histogram and QAQC -- do this in previous script?!?
## add some thresholds/QAQC; log-scale?
# poAll$Salinity_PSU <- ifelse (poAll$Salinity_PSU < )

## attenuation vs turbidy -- mix them here?!?
poAll$turbidity <- ifelse (is.na (poAll$turbidity), poAll$beamAttenuation, poAll$turbidity) ## is this wise or correct ? XXX

## remove implausible values
poAll$Density_sigma.theta.kg.m.3 <- ifelse (poAll$Density_sigma.theta.kg.m.3 < 15, NA, poAll$Density_sigma.theta.kg.m.3)
poAll$Oxygen_umol_kg <- ifelse (poAll$Oxygen_umol_kg <= 0, NA, poAll$Oxygen_umol_kg)
poAll$Oxygen_umol_kg <-  ifelse (poAll$Oxygen_umol_kg <= 0, NA, poAll$Oxygen_umol_kg)
# poAll$Oxygen_SBE.43..mg.l. <- ifelse (poAll$Oxygen_SBE.43..mg.l. <= 0, NA, poAll$Oxygen_SBE.43..mg.l.)
# poAll$Oxygen_SBE.43..mg.l. <-  ifelse (poAll$Oxygen_SBE.43..mg.l. <= 0, NA, poAll$Oxygen_SBE.43..mg.l.)
poAll$Oxygen.Saturation.Garcia.Gordon.umol_kg <-  ifelse (poAll$Oxygen.Saturation.Garcia.Gordon.umol_kg <= 0, NA, poAll$Oxygen.Saturation.Garcia.Gordon.umol_kg)
## recalc other O2 values here?
poAll$Salinity_PSU <- ifelse (poAll$Salinity_PSU < 20, NA, poAll$Salinity_PSU)

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

require (sp)
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

# require ('vizier')
require ("cmocean")  ## for color ramps


options ('cmocean-version' = "2.0") # fix colors to cmocean 2.0
oCol3 <- list (  ## fix versions?
   oceColorsTurbo  # cmocean ("thermal")
  , cmocean ("haline")
  , cmocean ("dense")
  , cmocean ("turbid") #, cmocean ("matter")  # or turbid
  , cmocean ("algae")
  #, oceColorsTurbo # cmocean ("solar")
  , function (n){
    require ("vizier")
    turbo (n, start = 0.25, end = 0.8)
  }
  , cmocean ("oxy")
  , cmocean ("haline") # why is this here? should it be??

)
if (0){
  oCol <- list (  ## old, not used
    # turbo
    oceColorsTemperature
    , oceColorsSalinity
    , oceColorsDensity
    , oceColorsTurbidity
    , oceColorsChlorophyll
    , oceColorsPAR  #, turbo #
    , oceColorsOxygen
  )
}


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
                       , FUN = function(vn){quantile (poAll [,which (names (poAll) == vn)], probs = c(0.02,0.98), na.rm = TRUE)
                         #  quantile (poAll [,which (names (poAll) == vn)], na.rm = TRUE, c(0.01, 0.99), type = 8)
                     }))
## better to do this with colormap(S, breaks=...)? See https://www.clarkrichards.org/2016/04/25/making-section-plots-with-oce-and-imagep/

## manually tune some of these ranges
oRange [1,1] <- 1.5 # fix min temperature
oRange [2,1] <- 27 # fix min salinity  -- 28 about as high as one could go (observed: 20-32, quantile: 29-32)
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
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")
cat ("\n\n             ### End of CTDwall-setup.R ###\n\n\n")
# EOF
