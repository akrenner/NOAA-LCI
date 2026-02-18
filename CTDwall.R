## replot CTD wallpaper for office
## provide line-graph alternatives




rm(list = ls())
## get bathymetry, standard colors, and data ranges
base::load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R -- only for stn?
poAll <- readRDS("~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")
base::load("~/tmp/LCI_noaa/cache/ctd_anomalies.RData")  # from CTD_anomaly-helpers.R


## problems:
## - chlorophyll missing(all values NA), e.g. T-3 2012-05-02
## - contours fail, e.g. temperature, T-4(2), 2019-05-14

## PAR: flag night; mark 1% light level contour
## fix distancescale to full transect
## Kris: check on surface PAR and salinity measurements

## 2021-08-03 -- issues
# x fix color scale across all graphs(across Transects as well?)

## 9-10 2012-5:  double cast?
## T-3 2012-winter: stations mislabled/out of order? 3_4 should be 3_7?

## color scales: make custom breaks to show more details


## decisions made:
# if more than 1 survey per survey-window, plot the longest section


stopatDate <- "2022-07-31"
stopatDate <- Sys.Date()



## add AlongBay-short transect as a new virtual transect
levels(poAll$Transect) <- c(levels(poAll$Transect), "ABext")


## anomaly adjustments already all in CTD_anomaly-helpers.R
## pick the variables to plot (only to save time, really). Disable to run
## everything under the sun
## code copied from CTDsections.R
if(1){  ## running everything takes hours -- don't do that on a routine basis!
  pV <- expand.grid (c("Temperature_ITS90_DegC", "Salinity_PSU", "Oxygen_umol_kg",
                       "Chlorophyll_mg_m3", "turbidity")  #, "bvf")
                     , c("", "an_", "anS_"))
  ## for testing/speed-up
  if(1){
    pV <- expand.grid (c("Temperature_ITS90_DegC",
                         "Salinity_PSU"# , "Oxygen_umol_kg", "Chlorophyll_mg_m3", "turbidity", "bvf"
    )
    , c("anS_" # , "an_", ""
    ))
  }
  keepV <- which (oVarsDFname %in% paste0(pV[,2], pV[,1])); rm (pV)
  oVarsF <- oVarsF [keepV]
  oVars <- oVars [keepV]
  oVarsDFname <- oVarsDFname [keepV]
  oCol3 <- oCol3 [keepV]
  oRange <- oRange [keepV,]
  rm(keepV)
}

oceanvarC <- seq_along(oVarsF)
# oceanvarC <- rev(oceanvarC)                           # for testing
transectC <- c(which(levels(poAll$Transect) == "9"),    # plot T9 first
               seq_along(levels(poAll$Transect))) |>
  unique()
# transectC <- which(levels(poAll$Transect) == "9")




################################
## enf of user configurations ##
################################

if(class(stopatDate)[1] == "character") {stopatDate <- as.POSIXct(stopatDate)}
source("CTDsectionFcts.R")
dir.create("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", showWarnings = FALSE, recursive = TRUE)

if(!exists("useSF")) {useSF <- FALSE}  ## should have useSF from CTDwall-setup.R
mnthly <- c("9", "4", "AlongBay")  ## for which transects to produce 12x n-year plots


if(0) { ## tests
  levels(factor(subset(poAll, year == 2012)$DateISO))
  xC <- subset(poAll,(Transect == "9") &(DateISO == "2019-09-16"))
  # xC <- subset(poAll,(Transect == "3")&(DateISO == "2012-03-14"))
  xCo <- sectionize(xC)
  plot(xCo)
  pSec(xCo, 1, zcol = oCol [[1]])
  pSec(xCo, 1, zcol = turbo(10), custcont = c(10, 11, 11.1))
  rm(xC, xCo)
}


## combine oceanvarC and transectC to run in parallel!
pPage <- expand.grid(oceanvarC=oceanvarC, transectC=transectC)
for(iV in seq_len(nrow(pPage))){
  ov <- pPage$oceanvarC[iV] # ov = OceanVariable(temp, salinity, etc)
  tn <- pPage$transectC[iV] # tn: transect
  ## for testing
  ## ov <- 1; tn <- 6 ## AlongBay
  ## ov <- 1; tn <- 2
  ## anS_salinity, T9
  ## ov <- 22; tn <- 5

  cat("\n\n", oVarsF [ov], " T-", levels(poAll$Transect)[tn], "\n")

  ## doubly-used stations:  XXX bug here!
  stn$Line <- flexTransect(levels(poAll$Transect)[tn], stn)  ## function from CTDsectionFcts.R
  lvl <- levels(poAll$Transect)
  poAll$Transect <- stn$Line [match(poAll$Match_Name, stn$Match_Name)]
  poAll$Transect <- factor(poAll$Transect, levels = lvl); rm(lvl)

  stnT <- subset(stn, stn$Line == levels(poAll$Transect)[tn])

  ## select transect, year, classify monthly/seasonal survey
  physOcY <- subset(poAll, Transect == levels(poAll$Transect)[tn])
  physOcY$year <- factor(physOcY$year)
  physOcY$month <- factor(format(physOcY$DateISO, "%m"))
  physOcY$season <- seasonize(physOcY$month)

  ## set-up page size for large poster-PDF
  ### monthly or quarterly samples -- by transect. 9, 4, AlongBay = monthly
  if(levels(poAll$Transect)[tn] %in% mnthly) {
    ## monthly
    pH <- 21.25; pW <- 42  # 42 inch = common plotter size. FWS has 44 inch HP DesignJet Z5600
    ## pH <- 44; pW <- 88     # FWS plotter, but paper is 42 inch
    pH <- 32; pW <- 42  ## full-width version -- Small version of T9/AlongBay
    ## for T9/AlongBay, full-width: adjust pH dynamically with N-years; all on one page of expanding length
    yearPP <- diff(range(as.numeric(format(physOcY$DateISO, "%Y")))) + 1 + 1 # extra line for color scale and map
    pW <- 42; pH <- 3.2 *(1 + yearPP)

    omcex <- 2   # size of mtext annotations
    sampleTimes <- stringr::str_pad(1:12, 2, pad = "0")
    physOcY$smplIntvl <- physOcY$month
    nY <- as.numeric(format(stopatDate, "%Y")) - min(as.integer(levels(poAll$year))) + 1
    nY <- yearPP
    layoutM <- matrix(1:(12 * nY), nY, byrow = TRUE) # across, then down
    omText <- month.name
    rm(nY)
  } else {
    ## quarterly
    pH <- 8.5; pW <- 14    # legal size
    yearPP <- 4
    omcex <- 1
    sampleTimes <- levels(physOcY$season)
    physOcY$smplIntvl <- physOcY$season
    layoutM <- matrix(1:16, 4, byrow = TRUE)
    omText <- sampleTimes
  }

  pdf(paste0("~/tmp/LCI_noaa/media/CTDsections/CTDwall/", oVarsF [ov]
             , " T-", levels(poAll$Transect)[tn], ".pdf"), height = pH, width = pW)
  layout(layoutM); rm(layoutM)
  if(pH > 14) {
    par(oma = c(3, 5, 12, 2), mar = c(4, 4, 3, 0.1))
  } else {
    par(oma = c(3, 5, 8, 2), mar = c(4, 4, 3, 0.1))
  }
  if(0) { # testing
    par(oma = c(3, 5, 15, 2), mar = c(4, 4, 16, 0.1))
  }
  ## test-option
  yearC <- seq_along(levels(physOcY$year))  ## or fix subset below
  for(iY in yearC) {
    ## for testing:
    # iY <- 7 # pick 2018
    # iY <- 2
    physOc <- subset(physOcY, year == levels(physOcY$year)[iY])


    ## replace transDate from above!
    ## also making surveyW redundant
    physOc$transDate <- with(physOc, factor(paste0("T-", Transect, " ", year,
                                                   "-", smplIntvl), levels = paste0("T-", Transect [1], " ", year [1],
                                                                                    "-", sampleTimes)))

    ## define and plot sections
    ## turn this into a function to use in section plots as well
    cat("   ",  formatC(iY, width = 3), "/", length(levels(physOcY$year)),
        " Sections/year:", length(levels(physOc$transDate)), "-- ")


    ## already defined surveys in CTDwall-setup.R -- use physOc$survey
    # sL <- 1:5 # for testing
    sL <-  seq_along(levels(physOc$transDate))
    for(iS in sL) {              # cycle through individual survey
      # iS <- 2  # for testing
      cat(iS, " ")
      xC <- subset(physOc, transDate == levels(physOc$transDate)[iS])
      if(length(levels(factor(xC$Match_Name))) < 2) {
        ## if a scheduled survey was not done, plot a blank placeholder in the panel instead
        ## unless the scheduled survey is in the future from runtime(do nothing then)
        inFuture <- as.numeric(as.character(physOc$year))[1] >=
          as.numeric(format(stopatDate, "%Y")) &&
          iS / length(sL) > as.numeric(format(stopatDate, "%m")) / 12
        if(!inFuture) {
          nS <- length(levels(factor(xC$Match_Name)))
          plot(0:10, type = "n", axes = FALSE, xlab = "", ylab = ""   ## XXX in here or in CTDsectionFcts.R? XXX
               , main = paste0(levels(physOc$transDate)[iS], ": ", nS
                               , ifelse(nS > 1, " stations", " station")
               ))
          rm(nS)
        }
        rm(inFuture)
      } else {
        if(0) {  ## combine all casts in survey window(watch RAM!) -- MUCH faster!
          ## or pick out long survey within X days?
          nSurv <- 1
        } else {
          ## check whether there is more than one survey per survey-interval

          if(0) {save.image("~/tmp/LCI_noaa/cache-t/wallCache1.RData")} # for testing
          # rm(list = ls()); load("~/tmp/LCI_noaa/cache-t/wallCache1.RData"); source("CTDsectionFcts.R")

          ## allow x-day window to make up a multi-day composite transect
          ## better to apply to allPo?
          ## make this a function for all data? -> move to CTDwall-setup.R / datasetup?

          xC$survey <- factor(xC$survey)
          nSurv <- length(levels(xC$survey))  ## used again below to mark plots
          if(nSurv > 1) {
            ## use the survey with the most stations
            nS <- sapply(levels(xC$survey), FUN = function(x) {
              length(levels(factor(subset(xC$Station, xC$survey == x))))
            })
            xC <- subset(xC, survey == levels(xC$survey)[which.max(nS)])
            rm(nS)
          }

          # remove duplicate stations(shared positions)
          ## any duplicated stations? -- if so, keep the closest in time
          posF <- factor(xC$longitude_DD * 10e6 - xC$latitude_DD)
          xC$File.Name <- factor(xC$File.Name)
          if(length(levels(xC$File.Name)) != length(levels(posF))) {
            ## there are duplicate stations
            ## identify duplicate file names; remove time outlier
            nFN <- sapply(seq_along(levels(posF)), function(iQ) {
              length(unique(subset(xC, posF == levels(posF)[iQ])$File.Name))
            })
            mT <- mean(xC$isoTime)

            for(iQ in seq_along(levels(posF))) {
              sec <- subset(xC, posF == levels(posF)[iQ])
              sec$File.Name <- factor(sec$File.Name)
              if(nFN [iQ] > 1) {
                bF <- sapply(seq_along(levels(sec$File.Name)), function(iQ) {
                  cx <- subset(sec, File.Name == levels(sec$File.Name)[iQ])
                  # qal <- nrow(cx)
                  qal <- 1 / abs(as.numeric(difftime(mT, cx$isoTime [1], "minutes")))
                  # cx$File.Name [1]
                  qal
                })
                ## pick the best cast
                sec <- subset(sec
                              , File.Name == levels(sec$File.Name)[which.max(bF)])
              }
              if(iQ == 1) {
                nSec <- sec
              } else {
                nSec <- rbind(nSec, sec)
              }
            }
            xC <- nSec
            rm(iQ, nFN, posF, nSec, sec, bF)
          }
        }

        ## arrange ctd data into sections
        ## define section -- see oce-class "section"
        if(0) {save.image("~/tmp/LCI_noaa/cache-t/wallCache.RData")} # for testing
        ## rm(list = ls()); load("~/tmp/LCI_noaa/cache-t/wallCache.RData"); source("CTDsectionFcts.R")

        ##
        ## construct, pad, and sort section
        ##
        xCo <- sectionize(xC)

        ## sectionPad to plot incomplete sections
        xCo <- sectionPad(sect = xCo, transect = data.frame(station = stnT$Match_Name
                                                            , line = stnT$Line
                                                            , latitude = stnT$Lat_decDegree
                                                            , longitude = stnT$Lon_decDegree
        ))
        if(!exists("bathy_sec")){
          bathy_sec <- get_section_bathy(xCo)
        }

        ##
        ## plot the section/transect
        ##
        ## if(ov == 2){zB <- c(28, seq(30, 33, 0.2))}else{zB <- NULL} ## fudge salinity colors
        if(oVarsF [ov] == "bvf") {cCont <- NULL} else {cCont <- pretty(oRange [ov, ], 20)}

        if(length(grep("^anS_", oVarsF[ov])) > 0){
          zb <- seq(-3,3, by=0.5)
          # zb <- subset (zb, zb != 0)  ## include zero or not?? center a color around zero?
        }else{
          zb <- NULL
        }

        pSec(xCo, N = oVarsF [ov]
             , zCol = oCol3 [[ov]]
             , zlim = oRange [ov, ] # fixes colors to global range of that variable
             , zbreaks=zb
             # , custcont = pretty(oRange [ov,], 20)  ## may often fail? -- no contours in range
             , ylim = c(max(physOcY$Depth.saltwater..m., na.rm = TRUE) + 5, 0)  ## need to fix CTDwall-setup.R first
             , drawPalette = FALSE, custcont = cCont, bathy = bathy_sec
             , legend.text = oVars [ov]
        )
        if(0) {   ## for QAQC: add station labels to x-axis # for testing
          dist <- unique(xCo[['distance']])
          stnID <- sapply(seq_along(xCo@data$station), function(m) {
            xCo@data$station[[m]]@metadata$stationId
            #              xCo@data$station[[m]]@metadata$filename
          })
          try(axis(side = 3, at = dist, labels = stnID, cex = 0.2, las = 2))
          rm(dist, stnID)
        }

        if(nSurv > 1) {
          title(main = paste(levels(physOc$transDate)[iS], "* -", nSurv), col.main = "red")
        } else {
          # title(main = paste(levels(physOc$transDate)[iS]))
          title(main = paste0(levels(physOc$transDate)[iS], "-"
                              , format(mean(xCo@metadata$time, na.rm = TRUE), "%d")))
        }
        if(iY == 1) { ## big title on top
          mtext(text = oVars [ov], side = 3, cex = 4, outer = TRUE, line = 3)  ## always?
        }
        ## addBorder(xCo, TD[ov]-1)

        # keep longest section for map
        if(!exists("xMap")) {xMap <- xCo}
        if(length(xCo@data$station) > length(xMap@data$station)) {
          xMap <- xCo
        }
        rm(xCo, nSurv)
      }
    }
    ## covering yearPP years per page. Write out at end of each year
    mtext(text = c(levels(physOcY$year)[iY], "") # blank line for map and scale?
          , side = 2, line = 1.0, outer = TRUE, cex = omcex
          , at = 1 -((iY - 1) %% yearPP) / yearPP - 0.5 / yearPP
    )
    for(n in seq_along(omText)) {
      mtext(text = omText [n], side = 3, line = 1.0, outer = TRUE, cex = omcex
            , at =(n - 1) / length(omText) + 0.5 / length(omText))
    }
    cat("\n")
  }

  ##############################################################
  ## add color scale and map after all section plots are done ##
  ##############################################################

  ## maps
  xLim <- c(-154, -151)
  yLim <- c(57.5, 60.1)
  plot(xMap
       , which = 99
       , coastline = "coastlineWorldFine" ## or a coastline object(from gshhg, removing dependency on ocedata)
       , showStations = TRUE
       , gird = TRUE
       , map.xlim = c(-154, -151)
       , map.ylim = c(57.5, 60.1)
       , clatitude = 59.4
       , clongitude = -152
       , span = 250
  )
  ## add eye for perspective;  save eye to eye.ps with Inkscape
  if(1) {
    if(levels(poAll$Transect)[tn] == "3") {
      xU <- -152.5; yU <- 59.4; rU <- 60
    } else if(levels(poAll$Transect)[tn] == "4") {
      xU <- -152.8; yU <- 59.4; rU <- 0
    } else if(levels(poAll$Transect)[tn] == "6") {
      xU <- -151.5; yU <- 58.4; rU <- 115
    } else  if(levels(poAll$Transect)[tn] == "7") {
      xU <- -152.5; yU <- 58.7; rU <- 85
    } else if(levels(poAll$Transect)[tn] == "9") {
      xU <- -152.8; yU <- 59.0; rU <- 20
    } else {                         # AlongBay
      xU <- -150.5; yU <- 59.1; rU <- 130
    }

    if(0) {  ## vector based -- not windows compatible and doesn't rotate
      grImport::PostScriptTrace("pictograms/eye.ps", "pictograms/eye.ps.xml")
      p <- grImport::readPicture("pictograms/eye.ps.xml")
      unlink("pictograms/eye.ps.xml")
      grImport::grid.picture(p  # no easy way to rotate p by n-degrees?; placed on page, not panel
                             , x = unit(xU, "npc"), y = unit(yU, "npc")
                             # , angle=rU
                             , width = unit(0.07, "npc"), height = unit(0.07, "npc")
      )
      ## read directly from SVG -- not working yet; better in MacOs, no advantage in Windows
      # if(.Platform$OS.type=="unix"){
      #   if(!require("grConvert")){
      #     devtools::install_github("sjp/grConvert")
      #     library(grConvert)}
      #   grConvert::convertPicture("pictograms/eye.svg", "pictograms/eye2.svg")
      # }
      # require("grImport2")
      # p <- grImport2::readPicture("pictograms/eye2.svg")
      # g <- grImport2::pictureGrob(p)
      # grid.picture(g)
    }
    if(.Platform$OS.type == "unix") {
      system("convert pictograms/eye.svg pictograms/eye.png") # requires ImageMagic to make PNG file
    }
    p <- png::readPNG("pictograms/eye.png")
    # require("OpenImageR")
    # p <- rotateImage(p, angle=rU, method="nearest")
    rasterImage(p
                , xleft = xU       # -152       +3*1*(xU-0.5)
                , xright = xU + 0.3  # -152+0.3  +3*1*(xU-0.5)
                , ybottom = yU     # 59.4     +1*1*(yU-0.5)
                , ytop =   yU + 0.3 # 59.4+0.3 +1*1*(yU-0.5)
                , angle = rU)
    rm(p, xU, yU, rU)
    # ## fine-scale map -- no longer needed?
    # plot(xMap  # plot.section(which=99) should return xlim and ylim of map, not section
    #       , which = 99
    #       , coastline = "coastlineWorldFine"
    #       , showStations = TRUE
    #       , gird = TRUE
    #       # , span=50
    # )
  }
  rm(xMap)

  ###############################################
  ## draw palette, color scale into next panel ##
  ###############################################

  ## needs better scaling for infrequent/quarterly surveys

  if(length(zb)>0){
    nCol <- length(zb) # +1 # to compensate for missing 0 ?
  }else{
    nCol <- 100
  }
  t.ramp <- oCol3[[ov]](nCol)
  if(pH > 30) {
    yL <- 1.8
    par(mar = c(5, 1, 5, 1))
    #       par(mar=c(14, 1,3,1))
  } else {
    yL <- 1.2
    #      par(mar=c(10, 1,3,1))
  }
  bp <- barplot(rep(1, nCol), axes = FALSE, space = 0, col = t.ramp
                , border = NA, ylim = c(-10, yL)  # ylim to make bar narrower, less high
  )
  title(main = oVars [ov], cex = 3, line = 0.5)
  lVal <-  pretty(c(oRange [ov, 1], oRange [ov, 2]))
  axis(1, at =(lVal - oRange [ov, 1]) /(oRange [ov, 2] - oRange[ov, 1]) * nCol
       , labels = lVal, lwd = 0, line = -12.4, lwd.ticks = 1, tick = TRUE)

  ## add date and logos for reference
  text(1, -12, paste("Kasistna Bay Lab\n", Sys.Date())
       , adj = 1, cex = 1)
  mtext(text = paste("Kasitsna Bay Lab\n", Sys.Date())
        , side = 1, line = 3, outer = FALSE, cex = 1, adj = 1)
  ## add NCCOS and KBNERR logos

  KBL <- png::readPNG("pictograms/KBL-Informal-NCCOS_tag_below_22hr.png")
  # im_h <- nrow(KBL); im_w <- ncol(KBL)
  #
  # plot(1:2, type = 'n', axes = FALSE, xlab = "", ylab = "", asp = 1
  #      , xlim = c(0, im_w), ylim = c(0, im_h), xaxt = "n", yaxt = "n"
  #      , bty = "n", add = TRUE)
  # rasterImage(KBL, xleft = 0, ybottom = 0, xright = im_w, ytop = im_h)
  #
  # nccos <- jpeg::readJPEG("~/My Pictures/Logos/nccos_logofile.jpg", native = TRUE)
  rasterImage(KBL, nCol / 20, -8, nCol, -2.5) # xleft, ybottom, xright ytop
  #    rm(bp, lVal, nCol, yL, nccos)

  ### end of wall poster
  dev.off()
  rm(bathy_sec)  ## flush bathymetry to fetch new one for next page
  cat("\n")
}


physOc <- poAll
rm(tn, oVars, oVarsF, ov, poAll, pSec, physOcY, yearC, iY, sL, iS, oceanvarC, transectC)






## for error checking: map of every transect
# double-used plots may appear out-of-line in chronology
## this should come at the end of CTDwall-setup.R
if(0) {
  xC <- xC [order(xC$isoTime), ]
  xC$survey <- factor(xC$survey)
  pdf("~/tmp/LCI_noaa/media/CTDsections/CTDwall/stationmaps.pdf")
  for(i in seq_along(levels(xC$survey)))
    xCx <- subset(xC, survey == levels(xC$survey)[i])
  ## cat(xC$File.Name[i], "\n")
  xCx <- sectionize(xCx)
  plot(xCx
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

  ## map of study area, following https://clarkrichards.org/2019/07/12/making-arctic-maps/
  require(ocedata) # for the coastlineWorldFine data
  data(coastlineWorldFine)

  mp <- function() {
    mapPlot(coastlineWorldFine, # projection=proj4string(bR),
      longitudelim = c(-154.2, -150.5),
      latitudelim = c(58.5, 60.5), col = 'grey')
  }
}

cat("\n# END CTDwall.R #\n")
# EOF
