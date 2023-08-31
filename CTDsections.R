## make image for every survey, each section
## showing all data of each survey on one page
## for QAQC and error checking0


rm (list = ls())
base::load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R


source ("CTDsectionFcts.R")  # get pSec to plot sections
# save.image ("~/tmp/LCI_noaa/cache/ctdwall2.RData") # use this for CTDwall.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall2.RData")


test <- TRUE
test <- FALSE

dir.create("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", showWarnings = FALSE, recursive = TRUE)

if (test){iX <- 10}else{iX <- 1:length (levels (poAll$survey))}

require ("parallel")
if (.Platform$OS.type=="unix"){
  ncores=12
}else{
  ncores <- 1
}
# mclapply (X=iX, poAll=poAll, mc.cores=ncores, FUN=function (sv,...){
# })

for (sv in iX){
  cat (sv, " ")
  if (sv %% 10 == 0){cat (" ", sv, "/", max (iX), "\n", sep = "")}
  s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
  s$Transect <- factor (s$Transect)
  if (test){iY <- 1}else{iY <-  1:length (levels (s$Transect))}# by transect
  for (tn in iY){  ## XXX testing XXX
    #  for (tn in 1:length (levels (poAll$Transect))){
    ## for testing
    ## sv <- 10; tn <- 1; s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
    # s$Transect <- factor (s$Transect)

    ## doubly-used stations:
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


    ## transect from station list for use with plot.section
    transectTemplate <- with (subset (stn, Line == levels (s$Transect)[tn]),
                              data.frame (station=Match_Name
                                          , longitude=Lon_decDegree
                                          , latitude=Lat_decDegree))


    phT <- subset (s, Transect == levels (s$Transect)[tn])
    phT$transDate <- with (phT, paste0 ("T-", Transect, " ", DateISO))


    if (length (levels (factor (phT$Match_Name))) > 2){ ## shouldn't be necessary -- what's up with Transect = NA??
      xC <- phT
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      #if (nrow (xC) > 1){ ## better than unreliable test above

      ## average multiple casts on same date?? XXX

      png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", levels (poAll$survey)[sv]
                   , " T-", levels (s$Transect)[tn]
                   # , "%02d
                   ,".png")
           , height = 8.5*200, width = 11*200, res = 300  # landscape
           # , height = 11*200, width = 8.5*200, res = 300 # portrait
      )

      # pdf (paste0 ("~/tmp/LCI_noaa/media/CTDwall/", oVars [ov]
      #              , " T-", levels (poAll$Transect)[tn]
      #              # , "_", levels (physOcY$year)[k]
      #              , ".pdf")
      #      , height = 8.5, width = 11)
      layout (matrix (1:9, 3, byrow = FALSE)) # across, then down
      # layout (matrix (1:8, 4, byrow = FALSE)) # across, then down
      #      layout (matrix (1:8, 2, byrow = TRUE)) # across, then down

      xCo <- sectionize (xC)


      for (ov in seq_along(oVarsF)){
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
      }
      if (s$Transect[tn] == "AlongBay"){mt <- ""}else{mt <- "T"}
      mtext (paste0 (mt, levels (s$Transect)[tn], " ", levels (poAll$survey)[sv])
             , side = 3, outer = TRUE, line = -0.9, cex = 0.7); rm (mt)
      plot (xCo  ## large LCI map -- trouble to keep range constant -- start from scratch??
            , which = 99
            , coastline = "coastlineWorldFine"
            , showStations = TRUE
            , gird = TRUE
            , map.xlim = range (poAll$longitude_DD) # +c(-0.5, 0.5)
            # , map.ylim = range (poAll$latitude_DD)+c(-0.3, 0.3)
            ## , map.xlim = c(-154, -151)
            ## , map.ylim = c(57.5, 60.1)
            , clatitude = mean (range (poAll$latitude_DD)) # 59.4
            , clongitude = mean (range (poAll$longitude_DD)) # -152
            , span = 200
            # , showSpine = TRUE
      )
      if (0){  ## omit this map -- need the space
        plot (xCo
              , which = 99
              , coastline = "coastlineWorldFine"  ## add hi-res topography?
              , showStations = TRUE
              , showStart = TRUE
              , gird = TRUE
              # , col = "red"
        )
      }
      dev.off()
    }
  }
}

rm (iY, iX, s, sv, tn)

physOc <- poAll
if (!test){
 # rm (xCo, tn, oVars, ov, poAll, pSec)
  gc()
}


cat ("\nfinished CTDsections.R\n\n")
# EOF
