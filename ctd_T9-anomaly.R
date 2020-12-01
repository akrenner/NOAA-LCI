## CTD anomaly over time


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size


###########################
## bugs/missing features ##
###########################

# x-axis is on shaking grounds. 7000 = eye-balled. Make that programatic
# add vertical lines to mark years?
# move years to be between tick-marks
# salinity-anomaly: all variability in surface-layer
# plot 'normal' seasonal profile
# anomalies: smooth vertically and/or seasonally
# set water depth for section
# center color scale on 0


## salinity: 0-10 m  and 10 to 100 m
## fresh-water content: psu = X, integrate over 0:10
## overlay all years
## => GAK1-comparison has been done,  water from below GAK1 coming into kachemak bay


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
## stn should be no longer needed -- see dataSetup.R
# physOc <- cbind (physOc, stn [match (physOc$Match_Name, stn$Match_Name)
#                               , which (names (stn) %in% c(# "Line",
#                                                           "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor (physOc$Match_Name)
# print (summary (physOc))



## use only T9-6
xC <- subset (physOc, (Transect == "9" & Station == "6"))
xC <- xC [order (xC$isoTime),]
## turn into section
xC$Date <- as.factor (xC$Date)

require ("oce")



## missing feature: plot along time instead of along distance
## Fake it for now? Rescale time into distance.


##########################
### calculate anomalies ##
##########################

## bin by depth (rather than just pressure)
xC$depthR <- factor (round (xC$Depth.saltwater..m.))
xC$month <- factor (format (xC$isoTime, "%m"))
## aggregate useing oce function -- skip aggregation in pre-processing by SBprocessing
## calculate normals
ctdAgg <- aggregate (Temperature_ITS90_DegC ~ depthR+month, xC, FUN = mean)
ctdAgg$Salinity_PSU <- aggregate (Salinity_PSU ~ depthR+month, xC, FUN = mean)$Salinity_PSU
# ctdAgg$Pressure..Strain.Gauge..db. <- aggregate (Pressure..Strain.Gauge..db. ~ depthR+month, xC, FUN = mean)$Pressure..Strain.Gauge..db.

## smooth normals
if (1){
  ctdAgg$monthI <- as.numeric (ctdAgg$month)  ## this messes with things -- don't XXX
  preDF <- ctdAgg; preDF$monthI <- preDF$monthI - 12
  postDF <- ctdAgg; postDF$monthI <- postDF$monthI + 12
  sDF <- rbind (preDF, ctdAgg, postDF); rm (preDF, postDF)
  # require ("mgcv")
  sOut <- sapply (levels (sDF$depthR), FUN = function (i){
    loess(Temperature_ITS90_DegC~monthI, sDF, subset = depthR == i)$fitted[(1:12)+12]
  }
  )  # 12x103 matrix
  ctdAgg$tloess <- sOut [ctdAgg$month, ctdAgg$depthR]
  sOut <- sapply (levels (sDF$depthR), FUN = function (i){
    loess(Salinity_PSU~monthI, sDF, subset = depthR == i)$fitted[(1:12)+12]
  }
  )
  ctdAgg$sloess <- sOut [ctdAgg$month, ctdAgg$depthR] # not quite it??
}


## normal to anomaly
matchN <- match (paste0 (xC$depthR, "-", xC$month), paste0 (ctdAgg$depthR,"-", ctdAgg$month))
xC$nSal <- ctdAgg$Salinity_PSU [matchN]
xC$nTem <- ctdAgg$Temperature_ITS90_DegC [matchN]
xC$nSal <- ctdAgg$sloess [matchN]
xC$nTem <- ctdAgg$tloess [matchN]
xC$anSal <- xC$Salinity_PSU - xC$nSal
xC$anTem <- xC$Temperature_ITS90_DegC - xC$nTem

## scale time as longitude
timeC <- as.numeric (xC$isoTime)
timeC <- timeC / max (timeC) * 359
xC$timeC <- timeC

cL <- lapply (1:length (levels (xC$Date)) # XX rewrite with %>% pipes?
              , FUN = function (i){
                ## for (i in 1:length (levels (xC$Date))){
                sCTD <- subset (xC, xC$Date == levels (xC$Date)[i])
                ocOb <- with (sCTD,
                              as.ctd (salinity = Salinity_PSU
                                , temperature = Temperature_ITS90_DegC
                                , pressure = Pressure..Strain.Gauge..db.
                                , longitude = timeC # as.numeric (longitude_DD
                                , latitude = rep (0, nrow (sCTD)) #latitude_DD
                                , station = Date
                                #, sectionId = transDate
                                , time = isoTime
                               #, waterDepth = rep (100, nrow (sCTD))
                              ))
                ocOb@metadata$waterDepth <- 103
                ocOb <- oceSetData (ocOb, "flourescence", sCTD$Fluorescence_mg_m3)
                ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
                ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                ocOb <- oceSetData (ocOb, "anTem", sCTD$anTem)
                ocOb <- oceSetData (ocOb, "anSal", sCTD$anSal)
                ocOb
              }
)
t9 <- as.section (cL); rm (cL)


plot.station <- function (section, axes = TRUE, ...){
  if (axes == TRUE){
    plot (section, xlab = "", axes = FALSE, showBottom = FALSE, ...)
    axis (2, at = c(0, 50, 100))
    fD <- function (x){ # uses index of grid?
      x <- as.numeric (x)
      X <- as.numeric (xC$isoTime)
      x <- (x - min (X))/ max (X - min (X))
      x
    }
    xDates <- pretty (xC$isoTime) ## need to generalize this to section!! unlist?
    xDates <- as.POSIXct(paste0 (2013:max (as.numeric (format (xC$isoTime, "%Y"))), "-01-01"))
    axis (1
          , at = fD (xDates)*6800  ## fix 7000 or 6800 to correct value!!
          , labels = format (xDates, "%Y")
          , xlab = "")

  }else{
    plot (section, xlab = "", axes = FALSE, showBottom = FALSE, ...)
  }
}

require ("RColorBrewer")
pdf ("~/tmp/LCI_noaa/media/t9s6-profile.pdf", height = 11, width = 8.5)
par (mfrow = c(2,1))
plot.station (t9, which = "anTem", ztype = "image"
              , zcol = rev (brewer.pal (11, "RdBu")) # XXX , zlim = c (-4, 4) #, zbreaks =
#              , axes = FALSE
              )
#axis (2, at = c(0, 50, 100))
## plot.station (t9, which = "anSal", ztype = "image", zcol = brewer.pal (11, "PiYG"), subset = Depth < 10)
plot.station (t9, which = "anSal", ztype = "image", zcol = brewer.pal (11, "PiYG"))
# abline (v = as.numeric (xDates)/as.numeric (max (xC$isoTime)*359)
dev.off()



## plot climatology
cT9 <- lapply (1:12, function (i){
  sCTD <- subset (ctdAgg, month == i)
  ocOb <- with (sCTD, as.ctd (salinity = sloess
                              , temperature = tloess
                              , longitude = i
                              , latitude = rep (0, nrow (sCTD))
  ))
})
cT9 <- as.section (cT9)

rm (ctdAgg)

pdf ("~/tmp/LCI_noaa/media/t9s6-climatology.pdf")
plot.station(cT9)
dev.off()

# sG <- sectionGrid (xC, p = 'levitus')
#
# if (0){
#   plot (xC                        # subscript out of bound
#         , which = "temperature" # = 1, salinity 2, density 3
#         , xtype = "time"
#         , ytype = "depth"
#         ## need to define proper z-matrix! -- initiates correct plot may need sectionGrid, as above?
#         # , coastline = "best"
#   )
# }
# }
# #   plot (xC, xtype = "time")


## alternative display of this data:


