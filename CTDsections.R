## make image for every survey, each section
## showing all data of each survey on one page
## for QAQC and error checking0

## produce temp file for CTDwall to process further



## load data
## start with file from dataSetup.R -- better to get data directly from CTD processing? need to add only coastline + bathy
setwd("~/myDocs/amyfiles/NOAA-LCI/")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## get CTD data (physOc) directly from CTD_cleanup.R, rather than through dataSetup.R

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## turbo colors
#   ## see https://github.com/jlmelville/vizier
#   # install.packages("remotes")
#   # remotes::install_github("jlmelville/vizier")
require ('vizier')


## set-up plot and paper size

### data prep
require ("oce")
## define sections
physOc$DateISO <- format (physOc$isoTime, "%Y-%m-%d")
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

## reproject?  crop? -- review!!
# nGrid <- .... ## define new grid -- the missing link
if (.Platform$OS.type == "unix"){   ## no longer necessary!
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
  # positive depth -- need to turn to negatives elevations? --- topo has neg values = depth
  # bathyL <- as.topo (getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE)) # too coarse for KBay
  bathyL <- getNOAA.bathy (-154, -150, 58.5, 60.3, resolution = 1, keep = TRUE) # too coarse for KBay
  load ("~/tmp/LCI_noaa/cache/bathymetryZ.RData")
}

require ("ocedata") # coastlineWorldFine



poAll <- physOc
poAll <- poAll [with (poAll, order (Transect, year, isoTime, Pressure..Strain.Gauge..db.)),]


## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object

## AlongBay (tn=6), i = 2, sections to process: 4, k= 5 fails

dir.create ("~/tmp/LCI_noaa/media/CTDwall", recursive = TRUE, showWarnings = FALSE)

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
bL <- extract (as.raster (bathyL), poP)
poAll$bathy <- ifelse (is.na (poAll$bathy), -1* bL, poAll$bathy)
rm (poP, bL)


################ define variables and their ranges #########################
oVars <- c ("temperature", "salinity", "sigmaTheta", "chlorophyll"
            , "turbidity", "PAR", "O2perc"
            #, "logFluorescence"
            , "logTurbidity", "logPAR")

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
require ('vizier')
oCol <- list (# turbo
  oceColorsTemperature
  , oceColorsSalinity, oceColorsDensity
              , oceColorsChlorophyll
           , oceColorsTurbidity, oceColorsPAR, oceColorsOxygen
           , oceColorsTurbidity, oceColorsPAR
           )
oRange <- t (sapply (c ("Temperature_ITS90_DegC", "Salinity_PSU", "Density_sigma.theta.kg.m.3"
                        , "Fluorescence_mg_m3"
                     , "Fluorescence_mg_m3", "PAR.Irradiance", "O2perc"
                     , "logTurbidity", "logPAR")
                 , FUN = function(vn){range (poAll [,which (names (poAll) == vn)], na.rm = TRUE)
                  #  quantile (poAll [,which (names (poAll) == vn)], na.rm = TRUE, c(0.01, 0.99), type = 8)
                  }))
if (length (oVars) != length (oCol)){stop ("fix the code above: one color for each variable")}
###########################################################################


## show data availability by year
aggregate (Date~year+Transect, poAll, function (x){length (levels (factor (x)))})[,c(2,1,3)]

## standard-layout based on season?
## aggregate T9 and AB by season?
## special layout for T9 and AB?



## define surveys (by date)
surveyW <- ifelse (duplicated(poAll$DateISO), 'NA', poAll$DateISO)
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
## fill last survey
surveyW [is.na (surveyW)] <- max (poAll$DateISO, na.rm = TRUE)
poAll$survey <- factor (surveyW); rm (surveyW, h)




save.image ("~/tmp/LCI_noaa/cache/ctdwall1.RData") # use this for CTDwall.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall1.RData")


source ("CTDsectionFcts.R")  # get pSec to plot sections


test <- TRUE
test <- FALSE

dir.create("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", showWarnings = FALSE, recursive = TRUE)

if (test){iX <- 1}else{iX <- 1:length (levels (poAll$survey))}
for (sv in iX){
  s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
  s$Transect <- factor (s$Transect)
  if (test){iX <- 1}else{iX <-  1:length (levels (s$Transect))}# by transect
  for (tn in iX){  ## XXX testing XXX
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
           , height = 8.5*200, width = 11*200, res = 300)

      # pdf (paste0 ("~/tmp/LCI_noaa/media/CTDwall/", oVars [ov]
      #              , " T-", levels (poAll$Transect)[tn]
      #              # , "_", levels (physOcY$year)[k]
      #              , ".pdf")
      #      , height = 8.5, width = 11)
      layout (matrix (1:12, 4, byrow = FALSE)) # across, then down




      # stn <- factor (sprintf ("%02d", as.numeric (xC$Station)))
      stn <- factor (sprintf ("%02s", xC$Station), ordered = TRUE)  ## does this order them??
      if (xC$Transect [1] %in% as.character (c(4,6,9))){stn <- factor (stn, levels = rev (levels (stn)), ordered = TRUE)} # only transect that's numbered in other direction


      xC$Match_Name <- factor (xC$Match_Name)
      #          xC <- as.section (lapply (1:length (levels (xC$Match_Name)) # XX rewrite with %>% pipes? XX as function?
      ## need to use station to keep factors in correct order?!?!!
      xCo <- makeSection (xC, stn)
      rm (stn)

      for (ov in 1:length (oVars)){
        pSec1 (xCo, N = oVars [ov], zC = oCol [[ov]]
              # , zlim = oRange [ov,]
              #               , xlim = xRange []  # range of the Transect
              # , custcont = pretty (oRange [ov,], 10)
        )
        # if (ov == 1){
        #   title (main = paste0 ("T", levels (s$Transect)[tn], " ", levels (poAll$survey)[sv]), col = "blue")
        # }
      }
      mtext (paste0 ("T", levels (s$Transect)[tn], " ", levels (poAll$survey)[sv])
             , side = 3, outer = TRUE, line = -0.9, cex = 0.7)
      plot (xCo
            , which = 99
            , coastline = "coastlineWorldFine"
            , showStations = TRUE
            , gird = TRUE
            , map.xlim = c(-154, -151)
            , map.ylim = c(57.5, 60.1)
            , clatitude = 59.4
            , clongitude = -152
            , span = 250
      )
      plot (xCo
            , which = 99
            , coastline = "coastlineWorldFine"
            , showStations = TRUE
            , gird = TRUE
            # , col = "red"
      )
      dev.off()
      if (tn %% 5 == 0){
        cat (tn, " ")
        if (tn %% 100 == 0) cat ("\n")
      }
    }
  }
}


physOc <- poAll
if (!test){
  rm (xCo, tn, oVars, ov, poAll, pSec)
}



# EOF
