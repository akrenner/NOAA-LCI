## Demo-script to try out settings
##
## load data from .RData file rather than csv files. The latter would add much
## code to select relevant files and match locations.

## files in package:
## aggregated CTD data
## CTDsectionFcts.R (in same directory as this file)
## this file CTDsections-example.R

## output: print to graphics device rather than image file


## ---------------------------- general set-up ------------------------------ ##
rm (list = ls())
if (file.exists("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")){
  base::load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R
}else{
  base::load ("ctdwallSetup.RData")
}
require ("oce")
source ("CTDsectionFcts.R")  # get pSec to plot sections


# directory with aggregated CTD files
ctdD <- "~/tmp/LCI_noaa/data-products/CTD/"


## ------- interactive user selection of transect, date, parameter ------- ##
## pick date and transect
cat ("Available survey dates: \n")
print (levels (poAll$survey))
sv <- 26                                        # user to pick index number
cat ("Selected date:", levels (poAll$survey) [sv], "\n")
s <- subset (poAll, survey == levels (poAll$survey)[sv])
s$Transect <- factor (s$Transect)

print (levels (s$Transect))
tn <- 1        # user to pick index number
cat ("Selected Transect", levels (s$Transect)[tn], "\n")
s <- subset (s, Transect==levels (s$Transect) [tn])
s$survey <- factor (s$survey)
s$Match_Name <- factor (s$Match_Name)
cat ("Available stations to plot:", length (levels (s$Match_Name)), "\n")

cat ("Available parameter to plot:\n")
print (oVarsF)
ov <- 3                                # user to pick index number
cat ("Selected parameter to plot: ", oVarsF [ov], "\n")

## --------------- end of interactive user selection -------------------- ##



# for (sv in iX){
#  s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency

  # cat ("Available transects on", levels (poAll$survey)[sv], ":\n")
  # print (levels (s$Transect))
  # cat ("set tn to an index representing the desired transect\n")
  # tn <- 1


    ## doubly-used stations:
  if (0){
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
    if (levels (s$Transect)[tn] == "AlongBay"){
      s$Transect [(s$Transect == "4") & (s$Station == "3")] <- "AlongBay"
      s$Transect [(s$Transect == "9") & (s$Station == "6")] <- "AlongBay"

      ## extended AlongBay Transect
      # AB-3, AB_S-2, AB_S-1, AB_S-0:  T6_S02, T7_S22, AB_SPTGM, AB_SPOGI
      fS <- c ("6_2", "7_22", "AlongBay_PTGR", "AlongBay_POGI")
      # nS <- -3:0
      for (k in 1:length (fS)){
        s$Transect [which (s$Match_Name == fS [k])] <- "AlongBay" ## no need to change station name
      }
    }
    if (levels (s$Transect)[tn] == "4"){
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "3")] <- "4"
    }
    if (levels (s$Transect)[tn] == "9"){
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "6")] <- "9"
    }
  }
    ## transect from station list for use with plot.section
    transectTemplate <- with (subset (stn, Line == levels (s$Transect)[tn]),
                              data.frame (station=Match_Name
                                          , longitude=Lon_decDegree
                                          , latitude=Lat_decDegree))

    # phT <- subset (s, Transect == levels (s$Transect)[tn])
     phT <- s

    if (length (levels (factor (phT$Match_Name))) > 2){ ## shouldn't be necessary -- what's up with Transect = NA??
      xC <- phT
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      # png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", levels (poAll$survey)[sv]
      #              , " T-", levels (s$Transect)[tn]
      #              # , "%02d
      #              ,".png")
      #      , height = 8.5*200, width = 11*200, res = 300  # landscape
      #      # , height = 11*200, width = 8.5*200, res = 300 # portrait
      # )
      # layout (matrix (1:9, 3, byrow = FALSE)) # across, then down

      xCo <- sectionize (xC)


        if (ov %in% c(4,5,6)){ # fix scale for O2, fluorescence, logPAR ## add buoyancy (8)?
          zR <- oRange [ov,]
        }else{
          cDF <- with (xC, data.frame (Temperature_ITS90_DegC, Salinity_PSU
                                       , Density_sigma.theta.kg.m.3
                                       , turbidity
                                       , Fluorescence_mg_m3, logPAR
                                       , Oxygen_umol_kg
                                       # , Oxygen_sat.perc.
                                        , bvf
          ))
          cDF <- sapply (1:ncol (cDF), function (i){ifelse (!is.finite (cDF[,i]), NA, cDF[,i])})
          # zR <- range (cDF [,ov], na.rm = TRUE); rm (cDF)
          zR <- quantile (cDF [,ov], probs = c(0.05, 0.95), na.rm = TRUE); rm (cDF)
        }
        # ov = 3 (turbidity), sv =7 fails. (order of x, y:  all values NA or stuck)
        if (all (is.na (zR))){
          plot (1:10, type = "n")
          text (5,5, labels = "no good data")
        }else{
          pSec (xCo
                , N = oVarsF [ov]      # logPAR does not plot for reasons unknown XXX
                , zCol = oCol3 [[ov]]
                #    , zCol = oColF (ov)
                #     , zcol = oCol2 (ov, 10)  ## doesn't work with zlim
                , zlim = zR
                , zbreaks=NULL # change this for salinity; others?
                , custcont=10, labcex=0.6
          )
          rm (zR)
        }
        ## mark PAR at night
        #   if (oVars [ov] == "PAR"){
        #     if (is.night(xCo@data [[1]][[1]]))
        #       box (lwd = 4, col = "navy")
        #   }


      if (s$Transect[tn] == "AlongBay"){mt <- ""}else{mt <- "T"}
      mtext (paste0 (mt, levels (s$Transect)[tn], " ", levels (poAll$survey)[sv])
             , side = 3, outer = TRUE, line = -1.0, cex =1); rm (mt)
      # plot (xCo  ## large LCI map -- trouble to keep range constant -- start from scratch??
      #       , which = 99
      #       , coastline = "coastlineWorldFine"
      #       , showStations = TRUE
      #       , gird = TRUE
      #       , map.xlim = range (poAll$longitude_DD) # +c(-0.5, 0.5)
      #       # , map.ylim = range (poAll$latitude_DD)+c(-0.3, 0.3)
      #       ## , map.xlim = c(-154, -151)
      #       ## , map.ylim = c(57.5, 60.1)
      #       , clatitude = mean (range (poAll$latitude_DD)) # 59.4
      #       , clongitude = mean (range (poAll$longitude_DD)) # -152
      #       , span = 200
      #       # , showSpine = TRUE
      # )
      # if (0){  ## omit this map -- need the space
      #   plot (xCo
      #         , which = 99
      #         , coastline = "coastlineWorldFine"  ## add hi-res topography?
      #         , showStations = TRUE
      #         , showStart = TRUE
      #         , gird = TRUE
      #         # , col = "red"
      #   )
      # }
     # dev.off()
    }
#  }
# }


# EOF
