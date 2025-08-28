## make image for every survey, each section
## showing all data of each survey on one page
## for QAQC and error checking0

## to do list:
## - bathymetry as in wall

if (!exists ("indivPlots")) {
  rm (list = ls())
  # indivPlots <- TRUE  # one section plot per page, instead of cluster of panels
  indivPlots <- FALSE
}
base::load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R

keepV <- c(1,2,4,5,7)  ## cut out sigma, logPar, bvf
oVarsF <- oVarsF [keepV]
oVars <- oVars [keepV]
oCol3 <- oCol3 [keepV]
oRange <- oRange [keepV,]


if (indivPlots) {
  outD <- "~/tmp/LCI_noaa/media/CTDsections/sectionImages_onepage/"
} else {
  outD <- "~/tmp/LCI_noaa/media/CTDsections/sectionImages/"
}
dir.create(outD, showWarnings = FALSE, recursive = TRUE)

source ("CTDsectionFcts.R")  # get pSec to plot sections
# save.image ("~/tmp/LCI_noaa/cache/ctdwall2.RData") # use this for CTDwall.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall2.RData")


test <- TRUE
test <- FALSE

## define survey by month, not date XXX
if (test) {iX <- 10} else {iX <- rev (seq_along (levels (poAll$survey)))} # rev: new years first


for (sv in iX) {
  # sv <- 151
  cat (sv, " ")
  if (sv %% 10 == 0) {cat (" ", sv, "/", max (iX), "\n", sep = "")}
  s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
  s$Transect <- factor (s$Transect)
  if (test) {iY <- 1} else {iY <-  seq_along(levels(s$Transect))} # by transect

  ## standardize some measures across all casts off one survey -- from CTDwall-setup.R
  oRangeS <- t (sapply (c ("Temperature_ITS90_DegC"
    , "Salinity_PSU"
    , "Density_sigma.theta.kg.m.3"
    , "turbidity" # , "logTurbidity"
    , "Chlorophyll_mg_m3"
    , "logPAR"     # , "PAR.Irradiance" ## XXX -- all NAs
    , "Oxygen_umol_kg" # , "Oxygen_SBE.43..mg.l."  # change to umol.kg.! XXX
    , "bvf"
  )[keepV]
  , FUN = function(vn) {range (s [, which (names (s) == vn)], na.rm = TRUE)
  }))




  for (tn in iY) {  ## XXX testing XXX
    #  for (tn in 1:length (levels (poAll$Transect))){
    ## for testing
    ## sv <- 10; tn <- 1; s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency
    # s$Transect <- factor (s$Transect)

    ## doubly-used stations:
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
    if (levels (s$Transect)[tn] == "AlongBay") {
      s$Transect [(s$Transect == "4") & (s$Station == "3")] <- "AlongBay"
      s$Transect [(s$Transect == "9") & (s$Station == "6")] <- "AlongBay"

      ## extended AlongBay Transect
      # AB-3, AB_S-2, AB_S-1, AB_S-0:  T6_S02, T7_S22, AB_SPTGM, AB_SPOGI
      fS <- c ("6_2", "7_22", "AlongBay_PTGR", "AlongBay_POGI")
      # nS <- -3:0
      for (k in seq_along(fS)) {
        s$Transect [which (s$Match_Name == fS [k])] <- "AlongBay" ## no need to change station name
      }
    }
    if (levels (s$Transect)[tn] == "4") {
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "3")] <- "4"
    }
    if (levels (s$Transect)[tn] == "9") {
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "6")] <- "9"
    }


    phT <- subset (s, Transect == levels (s$Transect)[tn])
    phT$transDate <- with (phT, paste0 ("T-", Transect, " ", DateISO))


    if (length (levels (factor (phT$Match_Name))) < 3) {
      ## do nothing, just skip it
      # stop(paste(levels(s$Transect)[tn],
      #   "transect is too short -- should have been caught earlier"))
    }else { ## shouldn't be necessary -- what's up with Transect = NA??
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      xCo <- sectionize (phT)
      bathy_sec <- get_section_bathy(xCo) ## already used up

      # if (nrow (xC) > 1){ ## better than unreliable test above

      ## average multiple casts on same date?? XXX

      if (indivPlots) {
        fN <- paste0 (outD, levels (poAll$survey)[sv]
          , " T-", levels (s$Transect)[tn], "_%02d.png")
        png (fN, height = 8.5 * 200, width = 11 * 200, res = 300)
      } else {
        fN <- paste0 (outD, levels (poAll$survey)[sv]
          , " T-", levels (s$Transect)[tn], ".png")
        png (fN, height = 8.5 * 200, width = 11 * 200, res = 300)
      }
      rm (fN)

      # pdf (paste0 ("~/tmp/LCI_noaa/media/CTDwall/", oVars [ov]
      #              , " T-", levels (poAll$Transect)[tn]
      #              # , "_", levels (physOcY$year)[k]
      #              , ".pdf")
      #      , height = 8.5, width = 11)
      if (!indivPlots) {
        layout (matrix (1:6, 3, byrow = FALSE)) # across, then down
      }
      # if (!indivPlots) {  ## old version, incl. density and bvf
      #   layout (matrix (1:9, 3, byrow = FALSE)) # across, then down
      # }
      # layout (matrix (1:8, 4, byrow = FALSE)) # across, then down
      #      layout (matrix (1:8, 2, byrow = TRUE)) # across, then down

      for (ov in seq_along(oVarsF)) {
        ## ov = 1
        if (ov %in% c(3,5)) { # fix scale for chlorophyll, O2, ## logPAR ## add buoyancy (8)?
                              # turbidity as well??
          zR <- oRange [ov, ]
        } else {
          zR <- oRangeS [ov, ]
        }
        # ov = 3 (turbidity), sv =7 fails. (order of x, y:  all values NA or stuck)
        if (all (is.na (zR)) | any (is.infinite(zR))) {
          plot (1:10, type = "n")
          text (5, 5, labels = "no good data")
        } else {

          if(tn == 1 & sv == 151 & ov == 1) {
            save(pSec, xCo, oVarsF, ov, oCol3, zR, file = "~/tmp/LCI_noaa/cache-t/ctdSectionsDBug.RData")
          }
          ## rm(list = ls()); load("~/tmp/LCI_noaa/cache-t/ctdSectionsDBug.RData"); source("CTDsectionFcts.R")
          # bathy_sec <- get_section_bathy(xCo) ## already used up
          #
          # ## testing
          # max(bathy_sec$dist)
          # max(xCo[['distance']])# GIS says that this is correct (3.9 km), also matching section plot
          # ## end of testing

          pSec (xCo
            , N = oVarsF [ov]      # logPAR does not plot for reasons unknown XXX
            , zCol = oCol3 [[ov]]
            #    , zCol = oColF (ov)
            #     , zcol = oCol2 (ov, 10)  ## doesn't work with zlim
            , zlim = zR
            , zbreaks = NULL # change this for salinity; others?
            , custcont = 7, labcex = 0.6
          )
          addBathy(bathy_sec)
          # legend("bottomright")
          rm (zR)
        }
        ## mark PAR at night
        #   if (oVars [ov] == "PAR"){
        #     if (is.night(xCo@data [[1]][[1]]))
        #       box (lwd = 4, col = "navy")
        #   }
        if (indivPlots) {
          if(substr(s$Transect[tn], start = 1, stop = 1) == "A") {
            mt <- ""
          } else {
              mt <- "T"
          }
          mtext(paste0(mt, levels(s$Transect)[tn], " ", levels(poAll$survey)[sv])
            , side = 3, outer = TRUE, line = -0.9); rm(mt)
        }
      } # end of loop covering all variables measured
      if(!indivPlots) {
        if(substr(s$Transect[tn], start = 1, stop = 1) == "A") {
          mt <- ""
        } else {
            mt <- "T"
        }
        mtext(paste0(mt, levels(s$Transect)[tn], " ", levels(poAll$survey)[sv])
          , side = 3, outer = TRUE, line = -0.9, cex = 0.7); rm(mt)
        if(0) {  ## map for all Transects
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
        } else {  ## focus on 2025+ monitoring transects: AB-ext, T9, T4
          plot(xCo, which = 99, coastline = "best", grid = TRUE,
             showStations = TRUE, span = 50,
             # map.xlim = c(-152.2, -151.0), # range(poAll$longitude_DD),
             map.ylim = c(59.2, 59.75),
             clatitude =  59.4,    # mean(range(poAll$latitude_DD))
             clongitude =  -151.8 # mean(range(poAll$longitude_DD))
               )
        }
      } else {  ## omit this map -- need the space
        plot(xCo, which = 99
          , coastline = "coastlineWorldFine"  ## add hi-res topography?
          , showStations = TRUE, showStart = TRUE, gird = TRUE
          # , col = "red"
        )
      }
      dev.off()
    }
  }
}

rm (iY, iX, s, sv, tn, outD)




require ("parallel")
if (.Platform$OS.type == "unix") {
  ncores = 12
} else {
  ncores <- 1
}
# mclapply (X=iX, poAll=poAll, mc.cores=ncores, FUN=function (sv,...){
# })




physOc <- poAll
if (!test) {
  # rm (xCo, tn, oVars, ov, poAll, pSec)
  gc()
}


cat ("\nfinished CTDsections.R\n\n")
# EOF
