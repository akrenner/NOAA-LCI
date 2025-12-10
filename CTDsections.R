## make image for every survey, each section
## showing all data of each survey on one page
## for QAQC and error checking
## option to run this for any one and only section


if(!exists("indivPlots")) {
  rm(list = ls())
  # indivPlots <- TRUE  # one section plot per page, instead of cluster of panels
  indivPlots <- FALSE
  anomalies <- TRUE
  # anomalies <- FALSE
  plotAll <- TRUE
}else{
  anomalies <- TRUE
  plotAll <- FALSE
}
# base::load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R
poAll <- readRDS("~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")
base::load("~/tmp/LCI_noaa/cache/ctd_anomalies.RData")  # from CTD_anomaly-helpers.R


if(!indivPlots){
  if(anomalies) {
    # decide which anomaly to plot in panneled plots: raw vs scaled by SD
    pV <- expand.grid (c("Temperature_ITS90_DegC", "Salinity_PSU",
                         "Oxygen_umol_kg", "Chlorophyll_mg_m3", "turbidity")
                       , c("" # , "an_"
                           , "anS_"
                         ))
    keepV <- which (oVarsDFname %in% paste0(pV[,2], pV[,1])); rm (pV)
  } else {
    keepV <- which(oVarsF %in% c("temperature", "salinity", "Oxygen_umol_kg",
                                 "Chlorophyll_mg_m3", "turbidity"))
  }
  oVarsF <- oVarsF [keepV]
  oVars <- oVars [keepV]
  oVarsDFname <- oVarsDFname [keepV]
  oCol3 <- oCol3 [keepV]
  oRange <- oRange [keepV,]
  rm(keepV)
}




if(indivPlots) {
  outD <- "~/tmp/LCI_noaa/media/CTDsections/sectionImages_onepage/"
} else {
  outD <- "~/tmp/LCI_noaa/media/CTDsections/sectionImages/"
}
dir.create(outD, showWarnings = FALSE, recursive = TRUE)

source("CTDsectionFcts.R")  # get pSec to plot sections
# save.image("~/tmp/LCI_noaa/cache/ctdwall2.RData") # use this for CTDwall.R
# rm(list = ls()); load("~/tmp/LCI_noaa/cache/ctdwall2.RData")


## define survey by month, not date XXX
if(plotAll) {iX <- rev(seq_along(levels(poAll$survey)))} else {
  iX <- rev(seq_along(levels(poAll$survey)))[1:4] # only plot the last N surveys
}


## make this a function and run in parallel! -- slow
# require(foreach)
# require(doParallel)
# num_cores <- detectCtores(logical = TRUE) - 1
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
# stopCluster(cl)
#
# x <- foreach(sv = iX, .combine = 'c', .export=) %dopar% {

for(sv in iX) {
  # sv <- 151
  cat(sv, " ")
  if(sv %% 10 == 0) {cat(" ", sv, "/", max(iX), "\n", sep = "")}
  s <- subset(poAll, survey == levels(poAll$survey)[sv]) # for testing -- eventually move up for efficiency
  s$Transect <- factor(s$Transect)
  iY <-  seq_along(levels(s$Transect)) # by transect
  # iY <- 1 ## testing

  ## standardize some measures across all casts off one survey -- from CTDwall-setup.R
  oRangeS <- t(sapply(oVarsDFname, FUN = function(vn) {
    range(s [, which(names(s) == vn)], na.rm = TRUE)
  }))


  for(tn in iY) {  ## XXX testing XXX
    #  for(tn in 1:length(levels(poAll$Transect))){
    ## for testing
    ## sv <- 10; tn <- 1; s <- subset(poAll, survey == levels(poAll$survey)[sv]) # for testing -- eventually move up for efficiency
    # s$Transect <- factor(s$Transect)

    ## doubly-used stations:
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6

    # use flexTransect instead?! XXX

    s$Station <- gsub ("^[A-Z,a-z,0-9]+_", "", s$Match_Name)

    if(levels(s$Transect)[tn] == "AlongBay") {
      s$Transect [s$Match_Name == "4_3"] <- "AlongBay"
      s$Transect [s$Match_Name == "9_6"] <- "AlongBay"
    # }
    # if(levels(s$Transect)[tn] == "ABext") {

      ## extended AlongBay Transect -- got function for this already?
      # AB-3, AB_S-2, AB_S-1, AB_S-0:  T6_S02, T7_S22, AB_SPTGM, AB_SPOGI
      fS <- c("6_2", "7_22", "AlongBay_PTGR", "AlongBay_POGI",   # "6_3", "7_21", "7_20",
        paste0("AlongBay_", 1:13))
      # nS <- -3:0
      for(k in seq_along(fS)) {
        s$Transect [which(s$Match_Name == fS [k])] <- "AlongBay" ## no need to change station name
      }
    }

    if(levels(s$Transect)[tn] == "4") {
      s$Transect [s$Match_Name == "AlongBay_3"] <- "4"
    }
    if(levels(s$Transect)[tn] == "9") {
      s$Transect [s$Match_Name == "9_6"] <- "9"
    }


    phT <- subset(s, Transect == levels(s$Transect)[tn])
    phT$transDate <- with(phT, paste0("T-", Transect, " ", DateISO))


    if(length(levels(factor(phT$Match_Name))) < 3) {
      ## do nothing, just skip it
      # stop(paste(levels(s$Transect)[tn],
      #   "transect is too short -- should have been caught earlier"))
    }else { ## shouldn't be necessary -- what's up with Transect = NA??
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      xCo <- sectionize(phT)
      bathy_sec <- get_section_bathy(xCo) ## already used up

      # if(nrow(xC) > 1){ ## better than unreliable test above

      ## average multiple casts on same date?? XXX

      if(indivPlots) {
        fN <- paste0(outD, levels(poAll$survey)[sv]
                     , " T-", levels(s$Transect)[tn], "_%02d.png")
        png(fN, height = 8.5 * 200, width = 11 * 200, res = 300)
      } else {
        fN <- paste0(outD, levels(poAll$survey)[sv]
                     , " T-", levels(s$Transect)[tn], ".png")
        if(anomalies){
          png(fN, height = 11 * 300, width = 8.5 * 300, res = 300)
          layout(matrix(1:12, 6, byrow = FALSE), heights = c(1, 1, 1, 1, 1, 1.2))
          # layout(matrix(1:12, 6, byrow = FALSE))
        } else {
          png(fN, height = 8.5 * 200, width = 11 * 200, res = 300)
          layout(matrix(1:6, 3, byrow = FALSE)) # across, then down
        }

        ## remove !anomalies?
        ## nudge mar to cover up distance along transect for most plots


      }
      rm(fN)

     par(oma=c(2,2,5,2))
      for(ov in seq_along(oVarsF)) {

        ## ov = 1
        if(ov %in% c(1,2)){      # keep local scale for temp, salinity,
          zR <- oRangeS[ov, ]
        } else {
          zR <- oRange [ov, ]
        }
        if(all(is.na(zR)) | any(is.infinite(zR))) {
          plot(1:10, type = "n")
          text(5, 5, labels = "no good data")
        } else {

          ## define zbreaks for anomaly plots (0.5, 1, 1.5, 2, 2.5, 3 SDs)
          if(length(grep("^anS_", oVarsF[ov])) > 0){
            zb <- seq(-3,3, by=0.5)
          }else{
            zb <- NULL
          }

          pSec(xCo
               , N = oVarsF [ov]
               , zCol = oCol3 [[ov]]
               , zlim = zR, zbreaks = zb
               , custcont = 7, labcex = 0.6
               , bathy = bathy_sec, legend.text = oVars [ov]
               , bathycol = rgb(t(col2rgb("darkgray")), max = 255, alpha = 0.5 * 255) # transparent so variable label can be seen
               , xlab = "" # ifelse(ov %in% c(5,10), "Distance along Track [km]", "")
               , ylab = "" # ifelse(ov == 3, "Depth [m]", "")
               , mar = c(2, 3, 1, 1.5)
               # , mgp = c(2.0, 0.7, 0.0)  # see:  getOption("oceMgp")
          )
          rm(zR, zb)
          if(ov %in% c(5,10)){
            mtext("Distance along Track [km]", side = 1, outer = FALSE, line = 2.3)
          }

          ## add column headings
          if(ov == 1) {
            mtext(paste(month.name[phT$month[1]], phT$year[1]), side = 3
              , outer = FALSE, line = 1)
            mtext(ifelse(levels(s$Transect)[tn] %in% c("AlongBay", "ABext"), "W", "N"), side = 3, adj = 0, outer = TRUE)
          }
          if(ov == length(oVarsF)/2+1) {
            mtext("Anomalies from monthly means", side = 3, outer = FALSE, line = 1)
            mtext(ifelse(levels(s$Transect)[tn] %in% c("AlongBay", "ABext"), "E", "S"), side = 3, adj = 1, outer = TRUE)
          }
        }

        ## insert map at a certain position, coordinated with layout above
        if(ov == 5 & anomalies) {
          xP <- par(mar=c(4,4,5,2))
          par(mar=c())
          # map suitable for >= 2025 transects: AB, ABext, T4, T9
          plot(xCo, which = 99, coastline = "best", grid = TRUE,
               showStations = TRUE, span = 50,
               # map.xlim = c(-152.2, -151.0), # range(poAll$longitude_DD),
               map.ylim = c(59.2, 59.75),
               clatitude =  59.6,    # mean(range(poAll$latitude_DD))
               clongitude =  -151.8 # mean(range(poAll$longitude_DD))
          )
          par(xP)
        }
        if(indivPlots) {
          if(substr(s$Transect[tn], start = 1, stop = 1) == "A") {
            mt <- ""
          } else {
            mt <- "Transect"
          }
          mtext(paste0(mt, levels(s$Transect)[tn], " ", levels(poAll$survey)[sv])
                , side = 3, outer = TRUE, line = -0.9); rm(mt)
        }
      } # end of loop covering all variables measured
      if(!indivPlots) {
        if(substr(s$Transect[tn], start = 1, stop = 1) == "A") {
          mt <- ""
        } else {
          mt <- "Transect-"
        }
        mtext(paste0(mt, levels(s$Transect)[tn] )#, " ", levels(poAll$survey)[sv])
              , side = 3, outer = TRUE, line = 1.5, cex = 1.5); rm(mt)
        mtext (text = "Depth [m]", side = 2, outer = TRUE)


        if(anomalies){
          ## insert KBL-NCCOS logo
          KBL <- png::readPNG("pictograms/KBL-Informal-NCCOS_tag_below_22hr.png")
          ## NCCOS-KBL logo
          im_h <- nrow(KBL); im_w <- ncol(KBL)
          # ppar <- par()
          par(mar=c(1, 2, 2, 2.0))
          plot(1:2, type = 'n', axes = FALSE, xlab = "", ylab = "", asp = 1
               , xlim = c(0, im_w), ylim = c(0, im_h), xaxt = "n", yaxt = "n", bty = "n")
          rasterImage(KBL, xleft = 0, ybottom = 0, xright = im_w, ytop = im_h)
        } else {
          if(0) {  ## map for all Transects
            plot(xCo  ## large LCI map -- trouble to keep range constant -- start from scratch??
                 , which = 99
                 , coastline = "coastlineWorldFine"
                 , showStations = TRUE
                 , gird = TRUE
                 , map.xlim = range(poAll$longitude_DD) # +c(-0.5, 0.5)
                 # , map.ylim = range(poAll$latitude_DD)+c(-0.3, 0.3)
                 ## , map.xlim = c(-154, -151)
                 ## , map.ylim = c(57.5, 60.1)
                 , clatitude = mean(range(poAll$latitude_DD)) # 59.4
                 , clongitude = mean(range(poAll$longitude_DD)) # -152
                 , span = 200
                 # , showSpine = TRUE
            )
          } else {  ## focus on 2025+ monitoring transects: AB-ext, T9, T4
            par(mar=c(4,4,4,0))
            plot(xCo, which = 99, coastline = "best", grid = TRUE,
                 showStations = TRUE, # span = 50,
                 # map.xlim = c(-152.2, -151.0), # range(poAll$longitude_DD),
                 map.ylim = c(59.25, 59.77),
                 clatitude =  59.7,    # mean(range(poAll$latitude_DD))
                 clongitude =  -151.8 # mean(range(poAll$longitude_DD))
            )
          }
        }
        if(0) {  ## omit this map -- need the space
          plot(xCo, which = 99
               , coastline = "coastlineWorldFine"  ## add hi-res topography?
               , showStations = TRUE, showStart = TRUE, gird = TRUE
               # , col = "red"
          )
        }
      }
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
    graphics.off()
  }
  # dev.off()
}

rm(iY, iX, s, sv, tn, outD)




# require("parallel")
if(.Platform$OS.type == "unix") {
  ncores = 12
} else {
  ncores <- 1  # 2025 laptop: 4 cores, 8 threads
}
# parallel::mclapply(X=iX, poAll=poAll, mc.cores=ncores, FUN=function(sv,...){
# })

## future_map from furrr


physOc <- poAll
# rm(xCo, tn, oVars, ov, poAll, pSec)
gc()



cat("\nfinished CTDsections.R\n\n")
# EOF
