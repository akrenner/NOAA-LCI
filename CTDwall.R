## replot CTD wallpaper for office
## provide line-graph alternatives

rm (list = ls())


## problemss:
## - fluorescence missing (all values NA), e.g. T-3 2012-05-02
## - contours fail, e.g. temperature, T-4 (2), 2019-05-14
## - missing latest data -- processing = ok??
## fix contours! -- make sure to have most recent data!

## PAR: flag night; mark 1% light level contour
## fix distancescale to full transect
## Kris: check on surface PAR and salinity measurements
## by season/month and year. Asterix if there are 2 per slot


## 2021-08-03 -- issues
# - multiple transects per season/month are merged -> pick first
# - fix color scale across all graphs (across Transects as well?)




dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", showWarnings = FALSE, recursive = TRUE)
require ("oce")
x <- load ("~/tmp/LCI_noaa/cache/ctdwall1.RData")  # from CTDsections.R

source ("CTDsectionFcts.R")



test <- TRUE
test <- FALSE

# varRange <- sapply (oVars, function (x){
#   xv <- poAll [,which (names (poAll) == x)]
#   xv <- poAll [,grep (x, tolower(names (poAll)))]
#   # xv <- replace (xv, is.infinite (xv, NA))
#   xv <- ifelse (is.infinite (xv), NA, xv)
#   range (xv, na.rm= TRUE)
# })
# oRange from CDsections.R!


## loop over variable, then transects and then seasons

if (test){iX <- 1}else{iX <- 1:length (oVars)}
for (ov in iX){
  if (test){iY <- 5}else{iY <-   1:length (levels (poAll$Transect))}# by transect
  for (tn in iY){  ## XXX testing XXX
# for (ov in 1:length (oVars)){
#  for (tn in 1:length (levels (poAll$Transect))){
    ## for testing
    ## ov <- 1; tn <- 2
    cat (oVars [ov], " Transect #", levels (poAll$Transect)[tn], "\n")

    ## double-use stations:
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
    if (levels (poAll$Transect)[tn] == "AlongBay"){
      poAll$Transect [(poAll$Transect == "4") & (poAll$Station == "3")] <- "AlongBay"
      poAll$Transect [(poAll$Transect == "9") & (poAll$Station == "6")] <- "AlongBay"
    }
    if (levels (poAll$Transect)[tn] == "4"){
      poAll$Transect [(poAll$Transect == "AlongBay") & (poAll$Station == "3")] <- "4"
    }
    if (levels (poAll$Transect)[tn] == "9"){
      poAll$Transect [(poAll$Transect == "AlongBay") & (poAll$Station == "6")] <- "9"
    }
    physOcY <- subset (poAll, Transect == levels (poAll$Transect)[tn])
#    physOcY <- with (physOcY, physOcY [order (),])

    physOcY$year <- factor  (physOcY$year)
    # physOcY$transDate <- factor (with (physOcY, paste0 ("T-", Transect, " ", DateISO)))
    physOcY$transDate <- with (physOcY, paste0 ("T-", Transect, " ", DateISO))
    physOcY$month <- factor (format (physOcY$DateISO, "%m"))
    physOcY$season <- cut (as.numeric (as.character (physOcY$month))
                                   , c(0,2,4,8,10, 13)
                           , labels = c ("winter", "spring", "summer", "fall", "winter")
                           )
    physOcY$season <- factor (physOcY$season
                              , levels = c ("winter", "spring", "summer", "fall")
                              , ordered = TRUE)


    # png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVars [ov]
    #              , " T-", levels (poAll$Transect)[tn]
    #              # , "_", levels (physOcY$year)[k]
    #              , "%02d.png")
    #      , height = 8.5*200, width = 11*200, res = 300)


    if (levels (poAll$Transect)[tn] %in% c("9", "AlongBay")){
      pH <- 22; pW <- 17  # 2x2 legal size
    }else{
      pH <- 8.5; pW <- 11
    }
    pdf (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVars [ov]
                 , " T-", levels (poAll$Transect)[tn]
                 # , "_", levels (physOcY$year)[k]
                 , ".pdf")
         , height = pH, width = pW)


    ### force sampling regime -- plot empty for missing survey
    if (levels (poAll$Transect)[tn] %in% c("9", "AlongBay")){
      ## monthly
      physOcY$smplIntvl <- physOcY$month
      layout (matrix (1:(12*5), 12, byrow = FALSE)) # across, then down
    }else{
      # quarterly
      physOcY$smplIntvl <- physOcY$season
      layout (matrix (1:12, 4, byrow = FALSE)) # across, then down
    }






    if (test){iZ <- 1}else{iZ <- 1:length (levels (physOcY$year))}# by year
    iZ <- 1:length (levels (physOcY$year)) # by year
    for (k in iZ){
#     for (k in 1:length (levels (physOcY$year))){ # by year -- assuming no surveys span New Years Eve
      ## for testing:
      # k <- 8
      physOc <- subset (physOcY, year == levels (physOcY$year)[k])


      if (0){
        ## allow x-day window to make up a composite transect
        ## better to apply to allPo?
        # algorithm:
        # set start Dates
        # give all data same ID as start date as h, IF they after element h, and are
        # within X days of start date of h
        ## make this a universal function to all data? -> to datasetup?
        physOc <- physOc [order (physOc$isoTime),]
        surveyW <- ifelse (duplicated(physOc$transDate), 'NA', physOc$transDate)
        for (h in 2:nrow (physOc)){
          surveyW <- ifelse (1:length (surveyW) >= h
                             , ifelse (difftime (physOc$isoTime, physOc$isoTime [h-1]
                                                 , units = "days") < 7
                                       , surveyW [h-1], surveyW)
                             , surveyW)
        }
        # ## faster version?  -- not worth the trouble
        # for (h in which (!duplicated (physOc$transDate))[-1]){
        # }
        physOc$transDate <- factor (surveyW); rm (surveyW, h)
        # physOc$transDate <- factor (physOc$transDate)
      }


      ## replace transDate from above!
      ## also making surveyW redundant
      physOc$transDate <- with (physOc, paste0 ("T-", Transect, " ", year, "-", smplIntvl))
      # tdL <- with (physOc, expand.grid (levels (smplIntvl), levels (Transect)))
      # physOc$transDate <- factor (physOc$transDate
      #                             , levels = paste0 ("T-", tdL$Var2, " "
      #                                                , levels (physOcY$year)[k]
      #                                                , "-", tdL$Var1)
      # )
      physOc$transDate <- with (physOc, factor (transDate
                                                , levels = paste0 ("T-", Transect [1]
                                                                   , " ", year [1], "-"
                                                                   , levels (smplIntvl))))


      ## define and plot sections
      cat ("Sections to process: ", length (levels (physOc$transDate)), "\n")

      if (test){iA <- 4}else{iA <-  1:length (levels (physOc$transDate))} # survey #
      iA <-  1:length (levels (physOc$transDate))
      for (i in iA){
      # for (i in 1:length (levels (physOc$transDate))){
          # for testing:
        # i <- 4
        cat ("sec: ", i, " ")
        xC <- subset (physOc, transDate == levels (physOc$transDate)[i])
        if (length (levels (factor (xC$Match_Name))) < 3){
          ## blank plot for missing data
          plot (0:10, type = "n", axes = FALSE, xlab = "", ylab = ""
                , main = paste (levels (physOc$transDate)[i], "-- no data"))
        }else{ ## shouldn't be necessary -- what's up with Transect = NA??
          if (xC$Transect [1] %in% c("4", "9")){
            xC <- xC [order (xC$latitude_DD, decreasing = TRUE),]
          }else{
            xC <- xC [order (xC$longitude_DD, decreasing = FALSE),]
          }
          ## arrange ctd data into sections
          ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

          #if (nrow (xC) > 1){ ## better than unreliable test above

          ## average multiple casts on same date?? XXX


          # stn <- factor (sprintf ("%02d", as.numeric (xC$Station)))
          stn <- factor (sprintf ("%02s", xC$Station), ordered = TRUE)  ## does this order them??
          if (xC$Transect [1] %in% as.character (c(4,6,9))){stn <- factor (stn, levels = rev (levels (stn)), ordered = TRUE)} # only transect that's numbered in other direction


          xC$Match_Name <- factor (xC$Match_Name)
          #          xC <- as.section (lapply (1:length (levels (xC$Match_Name)) # XX rewrite with %>% pipes? XX as function?
          ## need to use station to keep factors in correct order?!?!!
          xCo <- as.section (lapply (1:length (levels (stn))
                                    , FUN = function (x){
                                      #                                      sCTD <- subset (xC, Match_Name == levels (Match_Name)[x])
                                      sCTD <- subset (xC, stn == levels (stn)[x])
                                      ocOb <- with (sCTD,
                                                    as.ctd (salinity = Salinity_PSU
                                                            , temperature = Temperature_ITS90_DegC
                                                            , pressure = Pressure..Strain.Gauge..db.
                                                            , longitude = longitude_DD
                                                            , latitude = latitude_DD
                                                            , station = Match_Name
                                                            #, sectionId = transDate
                                                            , time = isoTime
                                                    ))
                                      # ocOb@metadata$waterDepth <- sCTD$Bottom.Depth [1]
                                      ocOb@metadata$waterDepth <- sCTD$bathy [1]
                                      ocOb <- oceSetData (ocOb, "fluorescence", sCTD$Fluorescence_mg_m3)
                                     # ocOb <- oceSetData (ocOb, "logFluorescence", sCTD$logFluorescence)
                                      ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
                                      ocOb <- oceSetData (ocOb, "logTurbidity", sCTD$logTurbidity)
                                      ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                                      ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                                      ocOb <- oceSetData (ocOb, "logPAR", sCTD$logPAR)
                                    # ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                                    # ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                                      ocOb
                                    }))
          # xCo <- sectionGrid (xCo, method = "boxcar")  -- no good; need depth, not pressure
          rm (stn)

          # T 3 4 6 7 9 Along
          TD <- c (36, 16, 35, 38, 4, 50) # fixed distance per transect
          pSec (xCo, N = oVars [ov], zC = oCol [[ov]]
                , zlim = oRange [ov,] # fixes colors to global range of that variable
                # , xlim = xRange []  # range of the Transect
                , custcont = pretty (oRange [ov,], 100)  ## may often fail? -- no contours in range
                # , custcont = ov
                , ylim = c(0,max (physOc$bathy))
                # , xlim = c(0, TD [tn])
                # , showBottom = bathyL
          )
          title (main = paste (levels (physOc$transDate)[i]))
        }

        # if (i %% 5 == 0){
        #   cat (i, " ")
        #   if (i %% 100 == 0) cat ("\n")
        # }
      }
    }
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
    )
    dev.off()
    cat ("\n")
  }
}


physOc <- poAll

rm (xCo, i, k, tn, oVars, ov, poAll, pSec, physOcY)





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



## for error checking: map of every transect
# double-used plots may appear out-of-line in chronology

if (0){
pdf ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/stationmaps.pdf")
for (i in 1:length ())
plot (xC
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
dev.off()
}


## map of study area, following https://clarkrichards.org/2019/07/12/making-arctic-maps/
require (ocedata) #for the coastlineWorldFine data
data(coastlineWorldFine)

mp <- function() {
  mapPlot(coastlineWorldFine, #projection=proj4string (bR),
          longitudelim = c(-154.2, -150.5),
          latitudelim = c(58.5, 60.5), col='grey')
}


# EOF
