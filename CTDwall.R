## replot CTD wallpaper for office
## provide line-graph alternatives

rm (list = ls())


## problemss:
## - fluorescence missing (all values NA), e.g. T-3 2012-05-02
## - contours fail, e.g. temperature, T-4 (2), 2019-05-14

## PAR: flag night; mark 1% light level contour
## fix distancescale to full transect
## Kris: check on surface PAR and salinity measurements

## 2021-08-03 -- issues
# x multiple transects per season/month are merged -> pick first
# x fix color scale across all graphs (across Transects as well?)

# - 12-month sampling: spread over 2 pages, first plot Jan-Jun, then Jun-Dec.  --  or plotter

## decisions made:
# if more than 1 survey per survey-window, plot the longest section
# only AlongBay and 9 are monthly -- 4?

dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", showWarnings = FALSE, recursive = TRUE)
require ("oce")
load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R
# x <- load ("~/tmp/LCI_noaa/cache/ctdwall1.RData")  # from CTDsections.R
source ("CTDsectionFcts.R")



mnthly <- c ("9", "AlongBay", "4")


## tests
if (0){
  levels (factor (subset (poAll, year == 2012)$DateISO))
  xC <- subset (poAll, (Transect == "9")&(DateISO == "2019-09-16") )
 # xC <- subset (poAll, (Transect == "3")&(DateISO == "2012-03-14"))
  xCo <- sectionize (xC)
  plot (xCo)
  pSec (xCo, 1, zcol = oCol [[1]])
  pSec (xCo, 1, zcol = turbo (10), custcont = c(10, 11, 11.1))
  rm (xC, xCo)
}



test <- TRUE
test <- FALSE



## loop over variable, then transects and then seasons

if (test){iX <- 1}else{iX <- 1:length (oVars)}
for (ov in iX){
  if (test){iY <- 5}else{iY <-   1:length (levels (poAll$Transect))}# by transect
  for (tn in iY){  ## XXX testing XXX
    ## for testing
    ## ov <- 1; tn <- 2
    cat (oVars [ov], " Transect #", levels (poAll$Transect)[tn], "\n")

    ## doubly-used stations:
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
    physOcY$season <- seasonize (physOcY$month)


    # png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVars [ov]
    #              , " T-", levels (poAll$Transect)[tn]
    #              # , "_", levels (physOcY$year)[k]
    #              , "%02d.png")
    #      , height = 8.5*200, width = 11*200, res = 300)


    if (levels (poAll$Transect)[tn] %in% mnthly){
      pH <- 21.25; pW <- 42  # 42 inch = common plotter size. FWS has 44 inch HP DesignJet Z5600
    }else{
      pH <- 8.5; pW <- 14
    }
    pdf (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVarsF [ov]
                 , " T-", levels (poAll$Transect)[tn]
                 # , "_", levels (physOcY$year)[k]
                 , ".pdf")
         , height = pH, width = pW)


    ### force sampling regime -- plot empty for missing survey
    if (levels (poAll$Transect)[tn] %in% mnthly){
      ## monthly
      physOcY$smplIntvl <- physOcY$month
    #  layout (matrix (1:(12*5), 12, byrow = TRUE)) # across, then down
      nY <- as.numeric (format (Sys.time(), "%Y")) - 2012 + 1
      layout (matrix (1:(12*nY), nY, byrow = TRUE)) # across, then down
      rm (nY)
    }else{
      # quarterly
      physOcY$smplIntvl <- physOcY$season
      layout (matrix (1:16, 4, byrow = TRUE)) # across, then down
    }


    # if (test){iZ <- 1:3}else{
      iZ <- 1:length (levels (physOcY$year))
      # }# by year
    # iZ <- 1:length (levels (physOcY$year)) # by year
    for (k in iZ){
#     for (k in 1:length (levels (physOcY$year))){ # by year -- assuming no surveys span New Years Eve
      ## for testing:
      # k <- 8
      physOc <- subset (physOcY, year == levels (physOcY$year)[k])


      ## replace transDate from above!
      ## also making surveyW redundant
      physOc$transDate <- with (physOc, paste0 ("T-", Transect, " ", year, "-", smplIntvl))
      physOc$transDate <- with (physOc, factor (transDate
                                                , levels = paste0 ("T-", Transect [1]
                                                                   , " ", year [1], "-"
                                                                   , levels (smplIntvl))))


      ## define and plot sections
      cat ("   ",  formatC (k, width = 3), "/", max (iZ), " Sections/year:", length (levels (physOc$transDate)), "-- ")

      if (test){iA <- 4}else{iA <-  1:length (levels (physOc$transDate))} # survey #
      iA <-  1:length (levels (physOc$transDate))
      for (i in iA){
      # for (i in 1:length (levels (physOc$transDate))){
          # for testing:
        # i <- 4
        cat (i, " ")
        xC <- subset (physOc, transDate == levels (physOc$transDate)[i])
        if (length (levels (factor (xC$Match_Name))) < 2){
          ## blank plot for missing data -- unless in the future
          ## use empty slots for map rather than starting on blank page if level would be in future

          inFuture <- as.numeric (as.character (physOc$year))[1] >= as.numeric (format (Sys.time(), "%Y")) &&
            i/length (iA) > as.numeric (format (Sys.time(), "%m"))/12
          if (!inFuture){
            plot (0:10, type = "n", axes = FALSE, xlab = "", ylab = ""
                  , main = paste (levels (physOc$transDate)[i], "-- no data"))
          }
          rm (inFuture)
        }else{

          ## check whether there is more than one survey per survey-interval

            ## allow x-day window to make up a composite transectF
            ## better to apply to allPo?
            # algorithm:
            # set start Dates
            # give all data same ID as start date as h, IF they after element h, and are
            # within X days of start date of h
            ## make this a universal function to all data? -> to datasetup?
            xC <- xC [order (xC$isoTime),]
            surveyW <- ifelse (duplicated(xC$DateISO), 'NA', xC$DateISO)
            for (h in 2:nrow (xC)){
              surveyW <- ifelse (1:length (surveyW) >= h
                                 , ifelse (difftime (xC$isoTime, xC$isoTime [h-1]
                                                     , units = "days") < 7
                                           , surveyW [h-1], surveyW)
                                 , surveyW)
            }
            # ## faster version?  -- not worth the trouble
            # for (h in which (!duplicated (physOc$transDate))[-1]){
            # }
            xC$surveys <- factor (surveyW); rm (surveyW, h)
            # physOc$transDate <- factor (physOc$transDate)

            nSurv <- length (levels (xC$surveys))
            if (nSurv > 1){
              ## use the survey with the most stations
              if (1){
                nS <- sapply (levels (xC$surveys), FUN = function (x){
                  length (levels (factor (subset (xC$Station, xC$surveys == x))))
                })
                xC <- subset (xC, surveys == levels (xC$surveys)[which.max (nS)])
                rm (nS)
              }else{

                ## use only the first survey
                nR <- sapply (levels (xC$surveys), FUN = function (x){sum (xC$surveys == x)})
                xC <- subset (xC, surveys == levels (xC$surveys)[which.max(nR)])  # use only the first survey
                # xC$Station <-
                rm (nR)
              }
            }

          if (xC$Transect [1] %in% c("4", "9")){
            xC <- xC [order (xC$latitude_DD, decreasing = TRUE),]
          }else{
            xC <- xC [order (xC$longitude_DD, decreasing = FALSE),]
          }
          ## arrange ctd data into sections
          ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html
          xCo <- sectionize (xC)



          # T 3 4 6 7 9 Along
          TD <- c (36, 16, 35, 38, 4, 50) # fixed distance per transect
          pSec (xCo, N = oVars [ov]
                #, zCol = oColF (ov)
                , zCol = oCol3 [[ov]]
                , zlim = oRange [ov,] # fixes colors to global range of that variable
                # , xlim = xRange []  # range of the Transect
                # , custcont = pretty (oRange [ov,], 20)  ## may often fail? -- no contours in range
                , ylim = c(0,max (physOc$bathy))
                # , xlim = c(0, TD [tn])
          )
          if (nSurv > 1){
            title (main = paste (levels (physOc$transDate)[i], "* -", nSurv), col.main = "red")
          }else{
            title (main = paste (levels (physOc$transDate)[i]))
          }
          rm (nSurv)

          if (!exists ("xMap")){xMap <- xCo; xMc <- xC}  # keep longest section for map
          if (length (levels (factor (xMc$Station))) < length (levels (factor (xC$Station)))){
            xMap <- xCo; xMc <- xC
          }
        }
      }
      cat ("\n")
    }
    plot (xMap
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
    plot (xMap
          , which = 99
          , coastline = "coastlineWorldFine"
          , showStations = TRUE
          , gird = TRUE
    )
    rm (xCo, xMap, xMc)
    dev.off()
    cat ("\n")
  }
}


physOc <- poAll

rm (i, k, tn, oVars, ov, poAll, pSec, physOcY)





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
