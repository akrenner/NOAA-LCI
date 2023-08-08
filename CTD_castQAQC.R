#!/usr/bin/env Rscript

## make CTD plots, profile plots, QA/QC
## checking on instrument integrity
## also location of station??

## from datasetup.R   ## somehow not updated since 2020 -- fix this eventually. use CNV1.RData in the meantime
rm (list = ls())
base::load ("~/tmp/LCI_noaa/cache/CTD.RData") ## still load for Seasonal() function

## start with file from CTD_cleanup.R
# rm (list = ls());
base::load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## have latest version of physOc from CTD_cleanup.R, but also have functions from datasetup.R


dir.create("~/tmp/LCI_noaa/media/CTDcasts/CTDsummarieplots", recursive = TRUE, showWarnings = FALSE)


print (summary (physOc))



## define sensible data ranges
## define conditions to flag questionable values


### QAQC inspections
if (0){
  x <- subset (physOc, isoTime > as.POSIXct("2023-01-31 00:01"))
  names (x)[24:25] <- c("fluorescence", "turbidity")
  aggregate (.~x$File.Name, data=x[,24:25], FUN=min)
  aggregate (.~x$File.Name, data=x[,24:25], FUN=max)
  aggregate (.~x$File.Name, data=x[,24:25], FUN=mean)
}


###########################
## play with TS-Diagrams ##
###########################

summary (physOc[,which (names (physOc) %in% c("Temperature_ITS90_DegC","Salinity_PSU"))])


plotTS <- function (sbst = NULL, fctr = NULL, fn){
    ## subsets, etc. based on poSS, rather than physOc
    ## match using SampleID
    if (exists ("sbst")){
        cT <- subset (physOc, sbst)
    }else{
        cT <- physOc
    }
    png (paste0 ("~/tmp/LCI_noaa/media/CTDcasts/CTDsummarieplots/", fn, ".png"))
    if (exists ("fctr")){
        if (class (fctr) == "character"){
            fctr <- factor (cT [,which (names (cT) == fctr)])
        }
        plot (Temperature_ITS90_DegC~Salinity_PSU, cT, col = fctr, pch = 19
            , xlim = c(13.5, 33.5), ylim = c(-1.1,16)
              )
        legend ("bottomleft", legend = levels (fctr), col = 1:length (levels (fctr))
              , pch = 19)
    }else{
        plot (Temperature_ITS90_DegC~Salinity_PSU, cT, pch = 19
            , smoothScatter = TRUE)
    }
    title (main = fn)
    dev.off()
}


physOc$year <- as.factor (format (physOc$isoTime, "%Y"))
physOc$month <- as.numeric (format (physOc$isoTime, "%m"))
physOc$season <- Seasonal (physOc$month)

if (0){
## Transects 3,6,5,6,9
# physOc$Transect <- grep ("^[A:Z,a-b,0-9]+_", physOc$Match_Name, value = TRUE)            # overwrite
plotTS ((1:nrow (physOc)) %in% grep ("^[9653]_", physOc$Match_Name), "Transect"
        , fn = "T9653")


plotTS (physOc$season == "spring", "year", fn = "spring_year")
plotTS (physOc$season == "summer", "year", fn = "summer_year")
plotTS (physOc$season == "fall", "year", fn = "fall_year")
plotTS (physOc$season == "winter", "year", fn = "winter_year")

plotTS (physOc$Depth.saltwater..m. > 20, "year", fn = "shallow_year")
## plotTS (physOc$year > 2014, "season", fn = "lastyears_season")
plotTS (grepl ("^9_", physOc$Match_Name), "season", fn = "T9_season")
plotTS (grepl ("^9_", physOc$Match_Name), "month", fn = "T9_monthly")
plotTS (grepl ("^Along", physOc$Match_Name), "month", fn = "along_month")
##  plotTS (physOc$Depth.saltwater..m. > 20, factor (physOc$year))
}

## physOc <- subset (physOc, 1:nrow (physOc) %in% 1:500 ) # to prototype
## physOc$File.Name <- factor (physOc$File.Name)

  dirN <- "~/tmp/LCI_noaa/media/CTDcasts/CTDprofiles/"
dir.create(dirN, showWarnings=FALSE)
## PDF ("CTDprofiles/ALLcasts.pdf")
Require ("oce")
plotCTDprof <- function (i){
  if ((i > 1000) & (i %% 2 == 2)){
    cat (i, " ")
    # cat (round (i/length (levels (physOc$File.Name)), digits=3), " ")
  }
  if (i %% 10 == 0){cat ("/", length (levels (physOc$File.Name)), "\n")}
  ctd <- subset (physOc, physOc$File.Name == levels (physOc$File.Name)[i])
  if (nrow (ctd) > 3){
    # pdf (paste0 (dirN, levels (physOc$File.Name)[i], ".pdf"))
    nR <- 4
    png (paste0 (dirN, levels (physOc$File.Name)[i], ".png")
         , res=200, height=2.75*nR*200, width=8.5*200)
    par (mfrow=c(nR,2))
    rm (nR)
    try ({
      ctdF <- with (ctd, as.ctd (salinity = Salinity_PSU
                                 , temperature = Temperature_ITS90_DegC
                                 , pressure = Pressure..Strain.Gauge..db.
                                 , longitude = longitude_DD
                                 , latitude = latitude_DD
      ))
      ## add line, marking pycnocline -- if we can?

      ## add fluorescence
      ctdF <- oceSetData (ctdF, value = ctd$Fluorescence_mg_m3
                          , name = "fluorescence"
                          , unit = "mg/m^3")
      ctdF <- oceSetData (ctdF, value = ctd$PAR.Irradiance
                          , name = "PAR"
                          , unit = "mg/m^3")
      ctdF <- oceSetData (ctdF, value = ctd$Oxygen_SBE.43..mg.l.
                          , name = "O2"
                          , unit = "mg/l")
      ## strength of stratification and depth of pycnocline
      # hist (poSS$swN2)
      ctdF <- oceSetData (ctdF, value = ctd$turbidity
                          , name = "turbidity"
                          , unit = "")
      ## strength of stratification and depth of pycnocline
      # hist (poSS$swN2)
      ## add line, marking pycnocline -- if we can?

      ## make 8 plots
      plot (ctdF, which="salinity+temperature", type="l")
      plot (ctdF, which="density+N2", type="l")
      plot (ctdF, which="TS", type="l")
      plot (ctdF, which="map", span=100)
    }, silent=TRUE)
    try({
      oce::plotProfile (ctdF, xtype = "O2", ytype = "depth", type="l")
      oce::plotProfile (ctdF, xtype = "fluorescence", ytype = "depth", type="l")
      oce::plotProfile (ctdF, xtype = "PAR", ytype = "depth", type="l")
#      oce::plot (ctdF, which="Rrho", type="l")
      plotProfile (ctdF, xtype = "turbidity", ytype = "depth")  ## fails
    }, silent=TRUE)
    mtext (levels (physOc$File.Name)[i],side=3, line=-1.25, outer=TRUE)
    dev.off()
    if (0){ ## combine all into one PDF
      system ("sleep 5", wait = TRUE)
      system (paste ("pdfunite ~/tmp/LCI_noaa/media/CTDprofiles/"
                     , levels (physOc$File.Name)[i]
                     , ".pdf ~/tmp/LCI_noaa/media/CTDprofiles/"
                     , levels (physOc$File.Name)[i]
                     , "_additions.pdf ~/tmp/LCI_noaa/media/CTDprofiles/c_"
                     ,  levels (physOc$File.Name)[i], ".pdf"
                     , sep = ""), wait = FALSE)
      system (paste ("rm ~/tmp/LCI_noaa/media/CTDprofiles/"
                     , levels (physOc$File.Name)[i], "*.pdf", sep = "")
              , wait = FALSE)
    }
  }else{
    warning (paste (levels (physOc$File.Name)[i]), " has less than 4 records\n\n")
  }
}

if (.Platform$OS.type=="unix"){
  Require ("parallel")
  x <- mclapply (1:length (levels (physOc$File.Name)), FUN = plotCTDprof, mc.cores = nCPUs)
}else{
  ## 2023-03-23: length (levels (physOc$File.Name)) == 4160. Consider running only recent casts
  x <- lapply (1:length (levels (physOc$File.Name)), FUN = plotCTDprof)
}
# dev.off()

if (0){
system (paste ("pdfunite" , paste ("~/tmp/LCI_noaa/media/CTDprofiles/c_"
                                 , levels (physOc$File.Name), ".pdf"
                                 , sep = "", collapse = " ")
             , "~/tmp/LCI_noaa/media/CTDprofiles.pdf"))
}

# require ("zip")
# R-internal zip file generation -- still troubled

## find windows equivalent here XXX  -- still needed?
if (.Platform$OS.type!="unix"){
#     Require ("zip")
#     unlink ("~/tmp/LCI_noaa/media/CTDtests/CTDprofiles.zip", force = TRUE)
#     zFiles <- list.files (dirN, pattern = ".pdf", full.names = FALSE)
#     zip::zip ("~/tmp/LCI_noaa/media/CTDprofiles.zip", files = zFiles, recurse = FALSE
#               , include_directories = FALSE)
# #    unlink (zFiles, force = TRUE)
#     rm (zFiles)
}else{
    system ("zip -mjr ~/tmp/LCI_noaa/media/CTDprofiles.zip ~/tmp/LCI_noaa/media/CTDprofiles")  ## XXX Error
}
## system ("zip -m -b ~/tmp/LCI_noaa/media/ --junk-paths CTDprofiles.zip CTDprofiles/*.pdf")
## system ("zip -m -b ~/tmp/LCI_noaa/media/ CTDprofiles.zip ~/tmp/LCI_noaa/media/CTDprofiles/*.pdf CTD.zip")
unlink("~/tmp/LCI_noaa/media/CTDtests/CTDprofiles", recursive = TRUE, force = TRUE)
rm (plotCTDprof)
cat ("\n\n")






if (0){                           # use oce -- not flexible enouth
    Require ("oce")
    ctd <- with (cT, as.ctd (salinity = Salinity_PSU
                           , temperature = Temperature_ITS90_DegC
                           , pressure = Pressure..Strain.Gauge..db.
                           , longitude = longitude_DD
                           , latitude = latitude_DD
                             )
                 )
    plot (ctd, which="TS")
    plotTS (ctd, col = season, pch = 19)
    }
##     plot (Temperature_ITS90_DegC~Salinity_PSU, cT, col = as.factor (year))
## }else{
##     cT <- physOc [grep ("^9_", physOc$Match_Name),]
##     PDF ("TS_example.pdf")
##     plot (Temperature_ITS90_DegC~Salinity_PSU, cT, col = season, pch = 19)
##     legend ("bottomleft", legend = levels (physOc$season), col = 1:length (levels (physOc$season)), pch = 19)
##     dev.off()
## # }


############################################
## Kachemak Bay: Along vs across transect ##
############################################

## this is analysis --- move it into a separate script!

if (0){
KBay <- subset (physOc, Transect %in% c("9", "AlongBay", "9andTutka"
                                      , "9 (part of multiple transects)"
                                      , "AlongBay (part of multiple transects)"))
KBay <- subset (KBay, !is.na (longitude_DD))
KBay <- subset (KBay, !is.na (longitude_DD))
KBay$File.Name <- factor (KBay$File.Name)
KBay$dateP <- with (KBay, isoTime)

## subset to summer months

Require ("oce")
 for (j in 1:length (levels (KBay$season))){
# j <- 1
   pdf (paste ("~/tmp/LCI_noaa/media/KBayCTDplots_", levels (KBay$season)[j]
              , ".pdf", sep = ""))
    ctdS <- subset (KBay, season == levels (KBay$season)[j])
    ctdS <- subset (ctdS, Match_Name %in% c("9_2", "9_6", "AlongBay_1", "AlongBay_12")) # extre
    ctdS$File.Name <- factor (ctdS$File.Name)
#    for (i in 1:length (levels (ctdS$File.Name))){
i <- 1
ctdX <- subset (ctdS, (File.Name == levels (ctdS$File.Name)[i]))
        ctdF <- with (ctdX, as.ctd (salinity = Salinity_PSU
                                  , temperature = Temperature_ITS90_DegC
                                  , pressure = Pressure..Strain.Gauge..db.
                                  , longitude = longitude_DD
                                  , latitude = latitude_DD
                                    )
                      )
        plot (ctdF) # , mar = c(2,1.5,4,1.5))
        title (paste (ctdX$Match_Name, ctdX$isoTime)[1], outer = FALSE, line = 3)
    }
    dev.off()


Require ("oce")
for (j in 1:length (levels (KBay$season))){
    pdf (paste ("~/tmp/LCI_noaa/media/KBayCTDplots_EndStation_", levels (KBay$season)[j]
              , ".pdf", sep = ""))
    ctdS <- subset (KBay, season == levels (KBay$season)[j])
    ## sort (summary (factor (KBay$Match_Name)))
    ctdS <- subset (ctdS, Match_Name %in% c("9_2", "9_6", "AlongBay_1", "AlongBay_12")) # extreme ends, but still frequently sampled
    ctdS$Match_Name<- factor (ctdS$Match_Name)
    ##  average all samples
    ctdS$pressC <- cut (ctdS$Pressure..Strain.Gauge..db., 20, labels = FALSE)
    ctdS$pressC <- ctdS$Pressure..Strain.Gauge..db.
    ctdS <- with (ctdS, aggregate (cbind (Salinity_PSU, Temperature_ITS90_DegC, longitude_DD, latitude_DD) ~ pressC+Match_Name, FUN = mean))

    for (i in 1:length (levels (ctdS$Match_Name))){
        ctdX <- subset (ctdS, (Match_Name == levels (ctdS$Match_Name)[i]))
        ctdF <- with (ctdX, as.ctd (salinity = Salinity_PSU
                                  , temperature = Temperature_ITS90_DegC
                                  , pressure = pressC
                                  , longitude = longitude_DD
                                  , latitude = latitude_DD
                                    )
                      )
        plot (ctdF) # , mar = c(2,1.5,4,1.5))
        title (paste (ctdX$Match_Name, ctdX$isoTime)[1], outer = FALSE, line = 3)
    }
    dev.off()
}
rm (ctdX, ctdF, ctdS, i,j,KBay)
}


## flag unrealistic conditions
## inverted density
## CTD > 4 m than surveyed depth at location


