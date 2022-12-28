#!/usr/bin/env Rscript

## consolidate as much error-checking and QAQC as possible in here
## This has to come after basic checking of file names to have date correct
## check here: stations and transects, dates and times
## also do final link of notebook-DB and files-DB

## start where CTD_cnv-Import.R left off
## produce file to be read by datasetup.R


#####################################################
##
## 1. QAQC
##    times and dates in file names consistent with metadata -> latlon from notebook
##    notebook latlon consistent with master-list
##    transect-stations correct -> match with master-list
# -> fill in from masterlist
# check consistency of metadata and notebook time and date -- do this earlier?
# that station names are consistent with notebook and file names
#
# make sure matchName for link to other datasets is correct
#
## 2. QAQC of CTD measurements
##    consistency across years and instruments.
##    Especially check O2, turbidity, fluorescence
#
##
## 3. produce annual aggregates
#     zip-up aggregates for export
#
#####################################################



## missing stations into master list
## ask for updates on notebooks
## 2020 along-bay station names: review and adjust
## bottom-depth from master/notes: add



rm (list = ls()); base::load ("~/tmp/LCI_noaa/cache/CNV2.RData")



## fix bad/missing time/date info
# physOc$File.Name [is.na (physOc$isoTime)]
if (any (is.na (physOc$isoTime))){
  print (physOc [which (is.na (physOc$isoTime))
                 , which (names (physOc) %in% c("Time", "Date", "File.Name"))])
  ## print (physOc$Time [which (is.na (physOc$isoTime))])
  ## print (physOc$Date [which (is.na (physOc$isoTime))])
  ## print (physOc$File.Name [which (is.na (physOc$isoTime))])
  stop ("still having bad isoTime")
}


## levels (factor (physOc$File.Name [grep ("\\s", physOc$Density_sigma.theta.kg.m.3)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$PAR.Irradiance)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$Fluorescence_mg_m3)]))
## grep ("\\s", factor (physOc$PAR.Irradiance), value = TRUE)
## grep ("\\s", factor (physOc$Density_sigma.theta.kg.m.3), value = TRUE)
## grep ("\\s", factor (physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.), value = TRUE)
## physOc <- physOc [grep ("^\\s", physOc$PAR.Irradiance),] # without spaces
## physOc <- physOc [grep ("^\\s", physOc$Density_sigma.theta.kg.m.3),]





stn <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
stn <- subset (stn, !is.na (Lon_decDegree))
stn$Plankton <- grepl ("Y", stn$Plankton) # force into logical

## fix Match_Name
showBad <- function (po){
  ## print out currently missmatching station names
  x <- match (po$Match_Name, stn$Match_Name)
  y <- levels (factor (po$Match_Name [which (is.na (x))]))
  return (y)
}

physOc$Transect <- trimws (physOc$Transect)
physOc$Station <- gsub ("^0*", "", trimws (physOc$Station))
# physOc$Transect <- as.integer (physOc$Transect)
# physOc$Station <- as.integer (physOc$Station)
Match_Name <- paste (physOc$Transect, physOc$Station, sep = "_") # ignore all prev fixings!
physOc <- cbind (Match_Name, physOc); rm (Match_Name)
oldMatch <- physOc$Match_Name



## existing match-names are based on notebook entries. Need to go back to filenames
## for those where notebook entries are missing (many)
stnFNex <- tolower (substr (physOc$File.Name, 11, nchar (physOc$File.Name)))

## fix file-names not matching convention (missing "-" between transect and station)
stnFNex <- gsub ("t9s06", "t9_s06", stnFNex)
stnFNex <- gsub ("cookinlet", "_", stnFNex)
stnFNex <- gsub ("_skb", "_s", stnFNex)
stnFNex <- gsub ("alongbay_kb", "alongbay_s", stnFNex)

# stnFNex <- gsub ("ptgr", "portgraham", stnFNex)
# stnFNex <- gsub ("ptgraham", "portgraham", stnFNex)
# stnFNex <- gsub ("ptgrm", "portgraham", stnFNex)
# stnFNex <- gsub ("ptgr", "portgraham", stnFNex)

stnFNex <- gsub ("alongbay_p(or|r)*t[a-z]*_", "alongbay_PTGR", stnFNex)
stnFNex <- gsub ("alongbay_[a-z,\\.]*pogi[a-z]*_", "alongbay_POGI", stnFNex) ## check!
stnFNex <- gsub ("-kbay_alongbay_inner-", "alongbay", stnFNex) ## check!

## bad transect! -- audit XXX
stnFNex <- gsub ("_alongbay_chinapoot[_]*", "_subbay_chinapoot", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_bear[_]*", "_subbay_bear", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_halibut[_]*", "_subbay_halibut", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_jbay[_]*", "_subbay_jbay", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_peterson[_]*", "_subbay_peterson", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_sadie[_]*", "_subbay_sadie", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_seldovia[_]*", "_subbay_seldovia", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_coalcove[_]*", "_subbay_coalcove", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_pt.bede[_]*", "_subbay_ptbede", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_say", "_subbay_say", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_dangerouscape[_]*", "_subbay_dangerouscape", stnFNex) ## check!
stnFNex <- gsub ("_alongbay_nanwalek[_]*", "_subbay_nanwalek", stnFNex) ## check!
stnFNex <- gsub ("alongbay_tutka[_]*", "subbay_tutka", stnFNex) ## check!
stnFNex <- gsub ("t9andtutka_tutka[_]*", "subbays_tutka", stnFNex) ## check!
stnFNex <- gsub ("cast", "_cast", stnFNex)
stnFNex <- gsub ("__+", "_", stnFNex) ## no duplicates



## intensive-phytoplankton -- missing station name; same for tutka

## individual files
stnFNex <- gsub ("2012-07-02-kbaysubays-cast152-tub_", "2012-07-02-subbays-tutkab_cast152_", stnFNex) ## check!

## testing, QAQC
# levels (factor (physOc$File.Name [grep ("alongbay_p(or|r)*t[a-z]*_", stnFNex)]))
# levels (factor (physOc$File.Name [grep ("pt\\.", stnFNex)]))

## preventative station-labels for subbays



## kiss -- no leading zero in match-names!
# p <- function (x){formatC (x, width = 2, format = "d", flag = "0")}
# for (i in 1:14){stnFNex <- gsub (paste0 ("kb", p(i)), paste0 ("ab-S", p(i)), stnFNex)}
# for (i in 2:10){stnFNex <- gsub (paste0 ("tutka", p(i)), paste0 ("tutka-S", p(i)))}
# rm (p)

## extract Station and Transect numbers
stnSp <- strsplit(gsub ("_", "-", stnFNex), "-")
## if all is well, order or elements in list should be sufficient. Just correct bad files?
stN <- sapply (1:length (stnSp), function (i){stnSp[[i]][3]}) # 3 is station
tnN <- sapply (1:length (stnSp), function (i){stnSp[[i]][2]}) # 2 is transect

x <- which (stN == "tran3")
stnSp [[x[1]]]
rm (x)

rbind (tran = length (tnN),## check that length is correct
       stat = length (stN),
       CTDd = nrow (physOc))


## fix stations that are out-of-order
levels (factor (stN))
bS <- grep ("cast", stN)
stN [bS] <- sapply (bS, function (i){stnSp[[i]][4]})

## lost cause, if not already fixed earlier
# bS <- grep ("along", stN)
# tnN [bS] <- sapply (bS, function (i){stnSp [[i]][3]})
# stN [bS] <- sapply (bS, function (i){stnSp [[i]][4]})

bS <- grep ("4141", stN)
stnSp [bS]
bS <- grep ("t9", stN)
levels (factor (physOc$File.Name [bS]))
#@stnSp [bS]
# bS <- grep ("t9", stN)
# ; rm (bS) # attempt to fix disorderly cases
# x <- which (stN == "t9")
rm (bS)

if (0){
  stN <- sapply (1:length (stnSp), function (i){
    grep ("^(s|S)[0:9]", stnSp[[i]], value = TRUE)[1]
  })
  tnN <- sapply (1:length (stnSp), function (i){
    grep ("^(t|along|sub|tutk|sadi|seld|jac|kabaysu|kbays|ab)"
          , stnSp[[i]], value = TRUE)[1]
  })

  # for (i in 1:length (tnN)){
#   if (length (tnN [[i]]) != 1){stop (i)}
# }
# stnSp [[i]]
}
stN <- unlist (stN)
length (stN)

## error-corrections and data-extraction for transects
levels (factor (tnN))
tnN [grep ("^t[a-z]*\\d", tnN)] <- gsub ("^t[a-z]*", "", tnN [grep ("^t[a-z]*\\d", tnN)])
tnN <- gsub ("9[a-z]+[0-9]*", "9", tnN) # watch carefully
tnN <- gsub ("^0", "", tnN) # trim leading zero
tnN [grep ("(sub|stevekibler)", tnN)] <- "subbay"
tnN [grep ("^(along|kbay|ab)", tnN)] <- "AlongBay"  ## "AlongBay" for now, as in physOc$Match
tnN [grep ("kachemakbay", tnN)] <- "AlongBay"
is.na (tnN)[tnN == ""] <- TRUE
levels (factor (tnN))
summary (tnN == physOc$Transect)



## error-corrections and data-extraction for stations
levels (factor (stN))
stN <- gsub ("^s0", "", stN)
stN <- gsub ("^s1", "1", stN)
stN <- gsub ("^s2", "2", stN)
stN <- gsub ("t9", "6", stN)
stN <- gsub ("jbay", "jakalof", stN)
stN <- gsub ("say", "sadie", stN)
stN <- gsub ("^along*", "", stN)
stN <- gsub ("extra", "", stN)
stN <- gsub ("south", "", stN)

stN <- gsub ("portgraham", "PTGR", stN)
stN <- gsub ("spgrm", "PTGR", stN)
stN <- gsub ("sptgm", "PTGR", stN)

stN <- gsub ("spogi", "POGI", stN)
stN <- gsub ("pogi", "POGI", stN)
## stN <- gsub ("[a-z]*$", "", stN) ## too gready -- cut it down
stN <- gsub ("^0*", "", stN)
for (i in 0:9){
  stN <- gsub (paste0 (i, "(a|b)"), i, stN)
}


## fix transects to subbay where T9 has subbay stations




levels (factor (stN))


is.na (stN)[stN == ""] <- TRUE
summary (stN == physOc$Station)

newMatch <- paste (tnN, stN, sep = "_")
is.na (newMatch)[is.na (tnN) | is.na (stN)] <- TRUE

physOc$Match_Name <- ifelse (physOc$Match_Name == "NA_NA", newMatch, physOc$Match_Name)
# is.na (physOc$Match_Name)[physOc$Match_Name == "NA_NA"] <- TRUE
levels (factor (physOc$Match_Name))
# levels (factor (physOc$File.Name [which (physOc$Match_Name == "AlongBay_pt.")]))
rm (newMatch, stN, tnN)


physOc$Match_Name <- gsub ("_spgrm|_sptgm", "_PTGR", physOc$Match_Name)
physOc$Match_Name <- gsub ("_spogi|_sptgm", "_POGI", physOc$Match_Name)
physOc$Match_Name <- gsub ("tutkabay_intensive", "subbay_tutkabayIntensive", physOc$Match_Name)
physOc$Match_Name <- gsub ("sadiecove_intensive", "subbay_sadiecoveIntensive", physOc$Match_Name)

if (0){
# summary (stn)
# y1 <- showBad (physOc)
gsubx <- function (tx, nt = "", fixed = TRUE){gsub (tx, nt, physOc$Match_Name, fixed = fixed)}
physOc$Match_Name <- gsubx (" (part of multiple transects)", fixed = TRUE)
physOc$Match_Name <- gsubx ("^AlongBay_", fixed = FALSE)
physOc$Match_Name <- gsubx ("^Subbays_", fixed = FALSE)
physOc$Match_Name <- gsubx ("Sadie0", "Sadie_")
physOc$Match_Name <- gsubx ("Bear ", "Bear_")
physOc$Match_Name <- gsubx ("^J[bB]ay", "Jakolof", fixed = FALSE)
physOc$Match_Name <- gsubx ("^K[bB]ay", "Kasitsna", fixed = FALSE)
physOc$Match_Name <- gsubx ("^KB([0-9])", "AlongBay_\\1", fixed = FALSE)

physOc$Match_Name <- gsub (" (part of multiple transects)", "", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("^AlongBay_", "", physOc$Match_Name) #, fixed = TRUE)
physOc$Match_Name <- gsub ("^Subbays_", "", physOc$Match_Name) #, fixed = TRUE)
physOc$Match_Name <- gsub ("Sadie0", "Sadie_", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("Bear ", "Bear_", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("^J[bB]ay", "Jakolof", physOc$Match_Name)
physOc$Match_Name <- gsub ("^K[bB]ay", "Kasitsna", physOc$Match_Name)
physOc$Match_Name <- gsub ("^KB([0-9])", "AlongBay_\\1", physOc$Match_Name)

physOc$Match_Name <- gsub ("_0", "_", physOc$Match_Name, fixed = TRUE) # as per naming convention
physOc$Match_Name <- gsub ("_(\\d+)[ab]$", "_\\1", physOc$Match_Name)
physOc$Match_Name <- gsub ("^9andTutka_Tutka", "Tutka_", physOc$Match_Name)
physOc$Match_Name <- gsub ("^9andTutka_", "9_", physOc$Match_Name)
physOc$Match_Name <- gsub ("9_Tutka", "9_", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("_13to10m$", "_13", physOc$Match_Name)
physOc$Match_Name <- gsub ("^T4S02$", "4_2", physOc$Match_Name)
physOc$Match_Name <- gsub ("^T7S20$", "7_20", physOc$Match_Name)
physOc$Match_Name <- gsub ("^T9S06$", "9_6", physOc$Match_Name)
physOc$Match_Name <- gsub ("\\s([AB])$", "_\\1", physOc$Match_Name) # " A" or " B"
## all remaining spaces should be in transect names and should be removed (no underscore)
physOc$Match_Name <- gsub ("\\s", "", physOc$Match_Name)
physOc$Match_Name <- gsub ("(Bear|ChinaPoot|Halibut|Jakolof|Kasitsna|Peterson|Sadie|Seldovia|Tutka)([ABC])$", "\\1_\\2", physOc$Match_Name) # "TutkaA" or " B"
physOc$Match_Name <- gsub ("^Tutka0", "Tutka_", physOc$Match_Name)
physOc$Match_Name <- gsub ("^Tutka1", "Tutka_1", physOc$Match_Name)
physOc$Match_Name <- gsub ("PogiPoint|Pt\\.Pogi|Pt\\.KBlandPogi|pogi|spogi", "POGI", physOc$Match_Name)
physOc$Match_Name <- gsub ("_graham|_spgrm|_portgraham", "_PTGR", physOc$Match_Name)
physOc$Match_Name <- gsub ("extra$", "", physOc$Match_Name)

## fixing a few individual stations
physOc$Match_Name <- gsub ("9_North", "9_1", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("9_South", "9_1", physOc$Match_Name)
## could also match some of the Tutka_1-9, but probably little point in doing that
print (badStn <- showBad (physOc))
# print (stn$Match_Name)


## XXX 2019-12-20 -- duplicate cast? unique issue?
# physOc <- subset (physOc, physOc$Match_name != "9_4south")
physOc <- physOc [physOc$Station != "4south",]
###
}

## fix up station and transect names from Match_Name
trst <- strsplit(physOc$Match_Name, "_", fixed = TRUE)
trst <- do.call ("rbind", trst)
physOc$Transect <- trst [,1]
physOc$Station <- trst [,2]
rm (trst)



## fix casts with missing geographic coordinates, if possible
## bad/missing coordinates
# summary (factor (physOc$Match_Name [x]))
# physOc$longitude_DD <- ifelse (physOc$longitude_DD == "NA", "", physOc$longitude_DD)
physOc$longitude_DD <- ifelse (is.na (physOc$longitude_DD)
                               , stn$Lon_decDegree [match (physOc$Match_Name, stn$Match_Name)]
                               , physOc$longitude_DD)
physOc$latitude_DD <- ifelse (is.na (physOc$latitude_DD)
                              , stn$Lat_decDegree [match (physOc$Match_Name, stn$Match_Name)]
                              , physOc$latitude_DD)
physOc$Bottom.Depth <- ifelse (is.na (physOc$Bottom.Depth)
                               , stn$Depth_m [match (physOc$Match_Name, stn$Match_Name)]
                               , physOc$Bottom.Depth)
## executive decision: assign ALL casts nominal positions
physOc$longitude_DD <- stn$Lon_decDegree [match (physOc$Match_Name, stn$Match_Name)]
physOc$latitude_DD <- stn$Lat_decDegree [match (physOc$Match_Name, stn$Match_Name)]
physOc$Bottom.Depth <- stn$Depth_m [match (physOc$Match_Name, stn$Match_Name)]




## print (sort (levels (factor (physOc$longitude_DD))))
## physOc [which (physOc$longitude_DD == "NA"),]
if (max (physOc$longitude_DD, na.rm = TRUE) > 0){stop ("some longites are positive")}


## write stations with missing positions to file
noPos <- levels (factor (physOc$File.Name [which (is.na (physOc$longitude_DD))]))
noPos <- levels (factor (physOc$Match_Name [which (is.na (physOc$longitude_DD))]))
cat ("#\n#\n#\n# missing locations for these stations:\n\n")
print (noPos)
write (noPos, file = "~/tmp/LCI_noaa/data-products/missingLocations_CTD.txt")
rm (noPos)
# summary (physOc$longitude_DD)




## bad densities (some are not sigma theta, up to 1024)
Require ("oce")
# physOc$Density_sigma.theta.kg.m.3 <- with (physOc, swRho (Salinity_PSU, Temperature_ITS90_DegC
#                                                           , Pressure..Strain.Gauge..db.
#                                                           , eos = "unesco"))-1000
# if (0){
# Require ("LakeMetabolizer")
# ## # physOc$Oxygen.Saturation.Garcia.Gordon.mg.l. -- or get straight from seabird!
# O2 <- with (
#   physOc, o2.at.sat.base (temp = Temperature_ITS90_DegC
#                           #                          , baro = Pressure..Strain.Gauge..db. *100 + 1000
#                           , salinity = Salinity_PSU
#                           , model= "garcia"))
# ## plot (O2, physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.)
# physOc$Oxygen.Saturation.Garcia.Gordon.mg.l. <- O2
# rm (O2)
# }
# physOc$O2perc <- with (physOc, Oxygen_SBE.43..mg.l. / Oxygen.Saturation.Garcia.Gordon.mg.l.)
# physOc$Spice <- with (physOc, swSpice (Salinity_PSU
#                                        , Temperature_ITS90_DegC
#                                        , Pressure..Strain.Gauge..db.
# ))
summary (physOc)







#####################################################
## trouble-shooting density and depth calculations ##
#####################################################

## a fair number of casts have highest density/salinity at the surface. Remove those impossible data points.
badDens <- function (i){
  ctd <- subset (physOc, File.Name == levels (physOc$File.Name)[i])
  cntx <- subset (1:nrow (physOc), physOc$File.Name == levels (physOc$File.Name)[i])
  if (!is.na (sum (ctd$Density_sigma.theta.kg.m.3[c(1,2)]))){ # bad/missing values Density_sigma.theta.kg.m.3
    if (ctd$Density_sigma.theta.kg.m.3 [1] >
        ctd$Density_sigma.theta.kg.m.3 [2]){
      bCntr <- cntx [1]
    }else{
      bCntr <- numeric()
    }
  }else{bCntr <- numeric()
  }
  return (bCntr)
}
# bCntr <- unlist (mclapply (1:length (levels (physOc$File.Name)), FUN = badDens
#                          , mc.cores = nCPUs))
bCntr <- unlist (lapply (1:length (levels (physOc$File.Name)), FUN = badDens))

physOc$File.Name <- factor (physOc$File.Name)
cat ("\n\nRemoved first data point from ", length (bCntr), " out of ",
     length (levels (physOc$File.Name)), "CTD casts ("
     , round (length (bCntr)/length (levels (physOc$File.Name))*100)
     ,"%) because surface density \nwas higher than in subsequent samples.\n\n")
physOc <- subset (physOc, !(1:nrow (physOc)) %in% bCntr)
rm (bCntr, badDens)




## dF <- factor (physOc$File.Name)         # Date ?
## dPc <- unlist (mclapply (1:length (levels (dF)), function (i){
##     x <- subset (physOc, dF == levels (dF)[i])
##     r <- cor (x$Pressure..Strain.Gauge..db.,x$Depth.saltwater..m.)
##     return (r)
## }
## , mc.cores = nCPUs))
## summary (dPc)

if (0){
  is.monotone <- function (ts){
    tsd <- ts [2:length (ts)]
    difV <- ts [1:(length (ts)-1)]-tsd
    all (difV <= 0)
  }
  badPAR <- sapply (levels (physOc$File.Name), FUN = function (fn){
    cast <- subset (physOc, File.Name == fn)
    is.monotone (cast$PAR.Irradiance)
  })
  badDens <- sapply (levels (physOc$File.Name), FUN = function (fn){
    cast <- subset (physOc, File.Name == fn)
    is.monotone (cast$Density_sigma.theta.kg.m.3)
  })
}

## exclude bad casts, cast that don't include top water layer
gC <- levels (factor (subset (physOc, Depth.saltwater..m. <= 3)$File.Name))
physOc <- subset (physOc, File.Name %in% gC)
physOc <- physOc [order (physOc$File.Name, physOc$Depth.saltwater..m.),] # for match
physOc$File.Name <- factor (physOc$File.Name)
rm (gC)




## fluorescence and turbidity-- have to be always positive!  -- about 150 readings
is.na (physOc$Fluorescence_mg_m3 [which (physOc$Fluorescence_mg_m3 <= 0)]) <- TRUE
is.na (physOc$turbidity[which (physOc$turbidity <= 0)]) <- TRUE
is.na (physOc$beamAttenuation[which (physOc$beamAttenuation <= 0)]) <- TRUE
# is.na (physOc$attenuation)[which ((physOc$attenuation - 13.82)^2 < 0.1)] <- TRUE
if (1){  ## moved from CTDwall-setup.R
  ## QAQC/Error correction of values -- do this before or after data export??  XXX

  ## attenuation vs turbidy -- mix them here?!?
  physOc$turbidity <- ifelse (is.na (physOc$turbidity), physOc$beamAttenuation, physOc$turbidity) ## is this wise or correct ? XXX


  ## remove implausible values
  physOc$Density_sigma.theta.kg.m.3 <- ifelse (physOc$Density_sigma.theta.kg.m.3 < 15, NA, physOc$Density_sigma.theta.kg.m.3)
  physOc$Oxygen_umol_kg <- ifelse (physOc$Oxygen_umol_kg <= 0, NA, physOc$Oxygen_umol_kg)
  physOc$Oxygen_umol_kg <-  ifelse (physOc$Oxygen_umol_kg <= 0, NA, physOc$Oxygen_umol_kg)
  # physOc$Oxygen_SBE.43..mg.l. <- ifelse (physOc$Oxygen_SBE.43..mg.l. <= 0, NA, physOc$Oxygen_SBE.43..mg.l.)
  # physOc$Oxygen_SBE.43..mg.l. <-  ifelse (physOc$Oxygen_SBE.43..mg.l. <= 0, NA, physOc$Oxygen_SBE.43..mg.l.)
  physOc$Oxygen.Saturation.Garcia.Gordon.umol_kg <-  ifelse (physOc$Oxygen.Saturation.Garcia.Gordon.umol_kg <= 0, NA, physOc$Oxygen.Saturation.Garcia.Gordon.umol_kg)
  ## recalc other O2 values here?
  physOc$Salinity_PSU <- ifelse (physOc$Salinity_PSU < 20, NA, physOc$Salinity_PSU)
  ## end of plots from CTDwall-setup.R
}






## plot all casts: depth-density, temp, salinity
## move this out to another file, CTDWall.R?


dir.create("~/tmp/LCI_noaa/media/CTDtests/", showWarnings = FALSE, recursive = TRUE)
png ("~/tmp/LCI_noaa/media/CTDtests/testPlots%02d.png")
xY <- factor (format (physOc$isoTime, "%Y"))
plot (physOc$Temperature_ITS90_DegC, physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.
      #    , col = physOc$CTD.serial
      , col = as.numeric (xY) # YEAR!!
      , pch = 19
)
legend ("topright", col = 1:length (levels (xY)), pch = 19, legend = levels (xY))
rm (xY)

plot (physOc$Temperature_ITS90_DegC, physOc$Oxygen_SBE.43..mg.l.
      , col = factor (physOc$CTD.serial))
## plot (physOc$Oxygen.Saturation.Garcia.Gordon.mg.l., physOc$Oxygen_SBE.43..mg.l.
##     , col = physOc$CTD.serial)
## plot (physOc$Depth.saltwater..m., physOc$Oxygen_SBE.43..mg.l.
##     , col = physOc$CTD.serial)
dev.off()


# moved from CTDwall-setup.R
## plots from CTDwall-setup.R -- still need to test and verify
## compare attenuation and turbidity -- ok to merge? QAQC
summary (physOc$turbidity)
summary (physOc$beamAttenuation)
pdf ("~/tmp/LCI_noaa/media/CTDtests/atten-turb.pdf")
par (mfrow = c (2,1))
hist (log (physOc$beamAttenuation), xlim = range (log (c (physOc$beamAttenuation, physOc$turbidity)), na.rm = TRUE))
hist (log (physOc$turbidity), xlim = range (log (c (physOc$beamAttenuation, physOc$turbidity)), na.rm = TRUE))
dev.off()


pdf ("~/tmp/LCI_noaa/media/CTDtests/O2-temp.pdf")
year <- factor (as.numeric (format (physOc$isoTime, "%Y")))
plot (Oxygen_umol_kg ~ Temperature_ITS90_DegC, data = physOc, col = year)
# plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = physOc, col = year)
# legend ("bottomleft", col = levels (physOc$year), pch = 19, legend = levels (physOc$year))
# plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = physOc, col = as.numeric (CTD.serial))
for (i in 1:length (levels (year))){
  #  plot (Oxygen_SBE.43..mg.l. ~ Temperature_ITS90_DegC, data = physOc
  plot (Oxygen_umol_kg ~ Temperature_ITS90_DegC, data = physOc
        , subset = year == levels (year)[i], col = as.numeric (CTD.serial))
  legend ("topright", col = levels (factor (as.numeric (physOc$CTD.serial)))
          , legend = levels (factor (physOc$CTD.serial)), pch = 19
          , title = levels (year)[i])
}
## issues: 2017!  (positive spike to >7). 2012: negative values. 2018: low values of 4141 (pre-callibration?)
## 2020: 5028 looks quite different than 4141. 4141 has two groups
dev.off()

# png ("~/tmp/LCI_noaa/media/CTDtests/O2-SBEvsGG.png", width = 600, height = 400)
# plot (Oxygen.Saturation.Garcia.Gordon.umol_kg~Oxygen_SBE.43..mg.l., physOc
#       , col = year, main = "colored by year"
#       , subset = Depth.saltwater..m. < 10)
# dev.off()

## histogram and QAQC -- do this in previous script?!?
## add some thresholds/QAQC; log-scale?
# physOc$Salinity_PSU <- ifelse (physOc$Salinity_PSU < )
rm (i, year)





if (0){  # currently fails -- fix later XXX
## plot cast-profiles
Require ("oce")
cCast <- levels (factor (physOc$File.Name))
outF <- "~/tmp/LCI_noaa/media/CTDtests/profilePlots/"
dir.create(outF, recursive = TRUE, showWarnings = FALSE)
for (i in 1:length (cCast)){
  ## make oce-ctd 0bject
  pdf (paste0 (outF, cCast [i], ".pdf"))
  ## plot profile
  plot (ctdP, which = c("TS", "salinity+temperature", "N2", "text"))
  dev.off()
}
}


save.image ("~/tmp/LCI_noaa/cache/CNV_cache9.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV_cache9.RData")  ## this to be read by dataSetup.R

summary (is.na (physOc$latitude_DD))
summary (is.na (physOc$longitude_DD))
noLL <- which (is.na (physOc$latitude_DD))
levels (factor (physOc$Match_Name[noLL]))
## AlongBay_3.5 -- bad position? Aborted because of high waves. Not in Database, but is in notebooks
## remove unresolved positions -- still too many!
physOc <- subset (physOc, !is.na (latitude_DD))
physOc <- subset (physOc, !is.na (longitude_DD))


## QAQC: plot each day, station in order
pdf ("~/tmp/LCI_noaa/media/CTDstationTest.pdf")
dayF <- factor (physOc$Date)
for (i in 1:length (levels (dayF))){
  crs <- subset (physOc, dayF == levels (dayF)[i])
  crs <- subset (crs, duplicated(crs$Match_Name) == FALSE)
  crs <- crs [order (crs$isoTime),]
  if (nrow (crs) > 0){
    cF <- factor (crs$CTD.serial)
    for (j in 1:length (levels (cF))){
      crsC <- subset (crs, cF == levels (cF)[j])
      plot (latitude_DD~longitude_DD, crsC
            , main = paste (levels (dayF)[i], paste (cF [j], levels (factor (crsC$Transect)), collapse = " and "))
            , type = "l")
      # with (crsC, text (longitude_DD, latitude_DD, labels = Station))
      # with (crsC, text (longitude_DD, latitude_DD, labels = Match_Name))
      with (crsC, text (longitude_DD, latitude_DD, labels = paste (Transect, Station, sep = "-"), cex = 0.7))
    }
  }
}

dev.off()
rm (dayF, crs, cF, crsC)


###############################################
## 2. annual aggregates for GulfWatch export ##
###############################################

save.image ("~/tmp/LCI_noaa/cache/CNVzipC.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVzipC.RData")  ## this to be read by dataSetup.R



## filter out all the non-standard casts ##
phy <- physOc
## double-used AlongBay - use only once??
phy$Match_Name <- ifelse (phy$Match_Name=="AlongBay_6", "9_6", phy$Match_Name) ## needed? both in stn
# phy$Match_Name <- ifelse (phy$Match_Name=="AlongBay_2", "4_3", phy$Match_Name)
# phy$Match_Name <- ifelse (phy$MatchName=="AlongBay_6", "9_6", phy$MatchName)
## all stations are already standard stations
phy$Match_Name <- factor (phy$Match_Name)
phy$File.Name <- factor (phy$File.Name)
x <- aggregate (File.Name~Match_Name+Date, phy, FUN=function (x){length (levels(factor (x)))})
x <- subset (x, File.Name > 1)
cat ("\n##\n##\n## stations with multiple casts per day\n")
dim (x)
x
rm (x)
# phy <- subset (phy, )


outD <- "~/tmp/LCI_noaa/data-products/CTD"
# outD <- "~/GISdata/LCI/CTD-processing/aggregatedFiles"
# manually update files in ~/GISdata/LCI/CTD-processing/ !
dir.create(outD, recursive = TRUE, showWarnings = FALSE)

yr <- factor (format (phy$isoTime, "%Y"))
phyE <- subset (phy, Transect %in% c("AlongBay", "3", "4", "6", "7", "9"))
phy2 <- subset (phy, !Transect %in% c("AlongBay", "3", "4", "6", "7", "9"))
write.csv (phy2, file=paste0 (outD, "/extraCTD.csv"), row.names=FALSE, quote=FALSE)

ctdX <- sapply (1:length (levels (yr)), function (i){
  ctdA <- subset (phyE, yr == levels (yr)[i])
  ctdB <- with (ctdA, data.frame (Station = Match_Name
                                  , Date, Time
                                  , Latitude_DD = latitude_DD
                                  , Longitude_DD = longitude_DD
                                  , Transect
                                  , StationN=ifelse (Station %in% 1:100, paste0 ("S_", Station), Station)
                                  , File.Name, CTD.serial
                                  , Bottom.Depth, pressure_db=Pressure..Strain.Gauge..db.
                                  , Depth=Depth.saltwater..m.
                                  , Temperature_ITS90_DegC, Salinity_PSU
                                  , Density_sigma.theta.kg.m.3
                                  , Oxygen_umol.kg=Oxygen_umol_kg
                                  , Oxygen.Saturation_perc=Oxygen_sat.perc.
                                  # need SBE O2 concentration umol.kg in here
                                  , Nitrogen.saturation..mg.l.  ## make it umol.kg
                                  , PAR.Irradiance
                                  , Fluorescence_mg_m3
                                  , Turbidity = turbidity
                                  , Beam_attenuation = beamAttenuation
                                  , Beam_transmission = beamTransmission
  ))
  # ctdA$turbidity <- ifelse (is.na (ctdA$turbidity), ctdA$attenuation, ctdA$turbidity)
  # ctdA <- ctdA [,-which (names (ctdA) == "attenuation")]
  tF <- paste0 (outD, "/CookInletKachemakBay_CTD_", levels (yr)[i], ".csv")
  write (paste0 ("## Collected as part of GulfWatch on predefined stations in Kachemak Bay/lower Cook Inlet. CTD sampled on every station. Concurrent zoo- and phytoplankton on select stations. 2012-2022 and beyond.")
         , file=tF, append=FALSE, ncolumns=1)
  suppressWarnings(write.table(ctdB, file=tF, append=TRUE, quote=FALSE, sep=","
                               , na="", row.names=FALSE, col.names=TRUE))
  # write.csv (ctdA, file = tF, row.names = FALSE, quote = FALSE)
  rm (tF)
  ctdB
})


if (0){  # if (.Platform$OS.type=="windows"){
  ## zip-up files -- about 100 MB
  Require ("zip")
  wD <- getwd()
  setwd (outD) ## zip blows up otherwise
  unlink ("processedCTD_annual.zip", force = TRUE)
  zFiles <- list.files ("~/tmp/LCI_noaa/data-products/CTD/", pattern = ".csv", full.names = FALSE)
  zip::zip ("processedCTD_annuals.zip", files = zFiles, recurse = FALSE
            , include_directories = FALSE)
  unlink (zFiles, force = TRUE)
  rm (zFiles)
  setwd (wD); rm (wD)
}
rm (showBad, oldMatch, yr, i, j)
# ls()


## no longer save RData dump here -- datasetup.R to read from aggregated files
save (physOc, stn, file = "~/tmp/LCI_noaa/cache/CNV1.RData")  ## this to be read by CTD_DataAvailability.R

cat ("\n# END CTD_cleanup.R #\n")

## EOF
