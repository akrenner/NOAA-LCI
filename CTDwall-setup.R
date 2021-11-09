## produce temp file for CTDwall to process further



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



# poAll <- physOc
# poAll <- poAll [with (poAll, order (Transect, year, isoTime, Pressure..Strain.Gauge..db.)),]


## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object

## AlongBay (tn=6), i = 2, sections to process: 4, k= 5 fails


poAll <- subset (physOc, Station %in% as.character (1:12)) # cut out portgraham and pogi -- or translate
rm (physOc)
poAll$Transect <- factor (poAll$Transect)


## compare attenuation and turbidity -- ok to merge? QAQC
summary (poAll$turbidity)
summary (poAll$attenuation)
pdf ("~/tmp/LCI_noaa/media/CTDtests/atten-turb.pdf")
par (mfrow = c (2,1))
hist (log (poAll$attenuation), xlim = range (log (c (poAll$attenuation, poAll$turbidity)), na.rm = TRUE))
hist (log (poAll$turbidity), xlim = range (log (c (poAll$attenuation, poAll$turbidity)), na.rm = TRUE))
dev.off()


pdf ("~/tmp/LCI_noaa/media/CTDtests/O2-temp.pdf")
plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = poAll, col = year)
# legend ("bottomleft", col = levels (poAll$year), pch = 19, legend = levels (poAll$year))
# plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = poAll, col = as.numeric (CTD.serial))
for (i in 1:length (levels (poAll$year))){
  plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = poAll
        , subset = year == levels (poAll$year)[i], col = as.numeric (CTD.serial))
  legend ("topright", col = levels (factor (as.numeric (poAll$CTD.serial)))
          , legend = levels (factor (poAll$CTD.serial)), pch = 19
          , title = levels (poAll$year)[i])
}
## issues: 2017!  (positive spike to >7). 2012: negative values. 2018: low values of 4141 (pre-callibration?)
## 2020: 5028 looks quite different than 4141. 4141 has two groups
dev.off()



## histogram and QAQC -- do this in previous script?!?
## add some thresholds/QAQC; log-scale?
# poAll$Salinity_PSU <- ifelse (poAll$Salinity_PSU < )

## attenuation vs turbidy -- mix them here?!?
poAll$turbidity <- ifelse (is.na (poAll$turbidity), poAll$attenuation, poAll$turbidity) ## is this wise or correct ? XXX


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
poAll$survey <- factor (surveyW)  ## need to reset factor levels
rm (surveyW, h)

# ## new, slow version -- but reliable?
# for (h in 2:length (surveyW)){
#   if (difftime (poAll$isoTime [h], poAll$isoTime [h-1], units = "days") < 7){
#     surveyW [h] <- surveyW [h-1]
#   }
# }
# poAll$survey <- factor (surveyW); rm (surveyW, h)

## check --- QAQC
for (i in 1:length (levels (poAll$survey))){
  x <- subset (poAll, survey == levels (poAll$survey)[i])
  cat (i, levels (factor (x$DateISO)), "\n")
}
rm (x, i)




################ define variables and their ranges #########################
# now in CTDsectionFcts.R --? no, need oRange in here and rest is dependend on it

oVars <- c ("temperature"
            , "salinity" #, "sigmaTheta"
            , "turbidity" # it's really turbidity/attenuation # , "logTurbidity"
            , "fluorescence-chl [mg/m^3]" #, "chlorophyll" #, "logFluorescence"
            , "PAR"  #, "logPAR"
            , "O2 [mg/L]"  # , "O2perc"
)
oVarsF <- c ("temperature"    # need diffrent name for oxygen to use in function
             , "salinity" #, "sigmaTheta"
             , "turbidity" # , "logTurbidity"
             , "fluorescence" #, "chlorophyll" #, "logFluorescence"
             , "PAR" #, "logPAR"
             , "Oxygen"  # , "O2perc"
)

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
## move these into CTDsectionFcts.R -- or not?

# require ('vizier')
require ("cmocean")


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
if (0){
  oCol <- list (  ## old, not used
    # turbo
    oceColorsTemperature
    , oceColorsSalinity #, oceColorsDensity
    , oceColorsTurbidity
    , oceColorsChlorophyll
    , oceColorsPAR  #, turbo #
    , oceColorsOxygen
  )
}


oRange <- t (sapply (c ("Temperature_ITS90_DegC"
                        , "Salinity_PSU" #, "Density_sigma.theta.kg.m.3"
                        , "turbidity" # , "logTurbidity"
                        , "Fluorescence_mg_m3"
                        , "PAR.Irradiance"  #, "logPAR"
                        , "Oxygen_SBE.43..mg.l.")
                     #, FUN = function(vn){range (poAll [,which (names (poAll) == vn)], na.rm = TRUE)
                       , FUN = function(vn){quantile (poAll [,which (names (poAll) == vn)], probs = c(0.05,0.95), na.rm = TRUE)
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
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")
cat ("\n\n             ### End of CTDwall-setup.R ###\n\n\n")
# EOF
