#!/usr/bin/env Rscript

## CTD anomaly
## for each station location, calculate averages for all
## oceanographic parameters


## priorities to fix
## improve monthly mean algorithm (circular seasonal mean)
## elsewhere: in CTD_cleanup / QAQC (merge those!): calc turbidity from loess function of beamTransmission~beamAttenuation
## review all XXX


rm(list = ls())
graphics.off()
load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R
normDir <- "~/tmp/LCI_noaa/media/CTDsections/CTDwall-normals/"


## copied from dataSetup.R(should have a package)
Seasonal <- function(month) {           # now using breaks from zoop analysis -- sorry for the circularity
  month <- as.numeric(month)
  cut(month, breaks = c(-13, 0, 2, 4, 8, 10, 25)
    , labels = c("fall", "winter", "spring", "summer", "fall", "winter"))
}



#################################
## monthly and quarterly means ##
#################################

# monthly mean
#   XXX could do this better, using circular annual means, as elsewhere

# min for a stable mean:
nMin <- 2
current_year <- format(Sys.Date(), "%Y")


## output: as poAll, but with month as factor
poAll$month <- as.numeric(format(poAll$isoTime, "%m"))
poAll <- dplyr::select(poAll, - Nitrogen.saturation..mg.l.)
# poAll$logPAR <- log(poAll$PAR.Irradiance)      ## XXX QAQC -- can PAR ever be negative?
oVars  <- oVars [-which(oVarsF == "logPAR")]       ## shouldn't have to -- fix in CTDsectionFcts.R!
oVarsF <- oVarsF[-which(oVarsF == "logPAR")]
oCol3$logPAR <- NULL

## XXXX move forward to CTD_cleanup.R!!!       ================================= XXX
poAll$Match_Name <- as.character(poAll$Match_Name)
poAll$Match_Name <- ifelse (poAll$Match_Name == "4_3", "AlongBay_3", poAll$Match_Name)
poAll$Match_Name <- ifelse (poAll$Match_Name == "AlongBay_6", "9_6", poAll$Match_Name)



oM <- as.matrix(poAll [, which(names(poAll) == "Temperature_ITS90_DegC")
  :which(names(poAll) == "bvf")])

ctdAgg <- function(df = poAll, FUN=mean, ...) {
  aggregate(oM ~ Match_Name + month + Depth.saltwater..m., data = df,
    # subset = format(isoTime, "%Y") != current_year,       ## exclude current year -- a good idea? XXX
    FUN = FUN, ...) |>
    dplyr::arrange(Match_Name, month, Depth.saltwater..m.)
}



poNorm <- ctdAgg(df = poAll, FUN = mean, na.rm = TRUE)
poN <- ctdAgg(df = poAll, FUN = function(x) {sum(!is.na(x)) })
nC <- which(names(poNorm) == colnames(oM)[1]):ncol(poNorm)
poNorm [,nC] <- sapply(nC, function(i) {ifelse(poN[,i] < nMin, NA, poNorm [,i])} )
# ## add stn data and Pressure for oce
poNorm$latitude_DD <- stn$Lat_decDegree[match(poNorm$Match_Name, stn$Match_Name)]
poNorm$longitude_DD <- stn$Lon_decDegree[match(poNorm$Match_Name, stn$Match_Name)]
poNorm$Pressure..Strain.Gauge..db. <- predict(lm(Pressure..Strain.Gauge..db. ~
   Depth.saltwater..m., poAll), newdata = list(Depth.saltwater..m. =
   poNorm$Depth.saltwater..m.))
poNorm$File.Name <- "climatology"
poNorm$isoTime <- as.POSIXct(paste0("2000-", poNorm$month, "-15 12:00"))
poNorm$Bottom.Depth_main <- stn$Depth_m [match(poNorm$Match_Name, stn$Match_Name)]


## fix turbidity -- do this earlier in ctd qaqc! XXX
## start with attenuation on x-axis
## better match for 'turbidity' -- and the one that's on current instrument
## better to do this by month, then average the models!
## do this in merged version of CTD_cleanup, CTD_notesQAQC.R, CTD_castQAQC.R
turbM <- loess(beamTransmission~beamAttenuation, poNorm)
if(0) {
  plot (beamTransmission~beamAttenuation, poNorm)
  ndat <- data.frame(beamAttenuation = seq(min(poNorm$beamAttenuation, na.rm =
    TRUE), max(poNorm$beamAttenuation, na.rm = TRUE), length.out = 100))
  lines (ndat$beamAttenuation, predict(turbM, newdata=ndat), col="blue", lwd=2)
  # par(ylog=TRUE)
  # plot (beamAttenuation~beamTransmission, poNorm) , log="y")
}
poNorm$turbidityIdx <- predict(turbM, newdata=poNorm$beamAttenuation)

## export this model to all CTD data -- move forward into ctd processing?? -- don't include in published files XXX
poAll$turbidity <- ifelse(is.na(poAll$beamAttenuation),
  predict(turbM, newdata=poAll$beamAttenuation),
  poAll$beamAttenuation)
rm(turbM)


poSD <- ctdAgg(df = poAll, sd, na.rm = TRUE)
poSD [,nC] <- sapply(nC, function(i) {ifelse(poN[,i] < nMin, NA, poSD [,i])} )

if(0) { ## quarterly means -- not really enough data for these?
  season <- Seasonal(poAll$month)
  poSorm <- aggregate(oM ~ Match_Name + season + Depth.saltwater..m., data = poAll,
    FUN = mean, na.rm = TRUE) |>
    dplyr::arrange(Match_Name, month, Depth.saltwater..m.)
  poSorm [,nC] <- sapply(nC, function(i) {ifelse(poN[,i] < nMin, NA, poSorm [,i])} )
}


## calculate anomalies
poAno <- sapply(seq_len(ncol(oM)), function(i) {
  pC <- which(names(poAll) == colnames(oM)[i])
  poAll [,pC] - poNorm [match(paste(poAll$Match_Name, poAll$month),
    paste(poNorm$Match_Name, poNorm$month)), nC[i]]
}) |>
  as.data.frame()
names(poAno) <- paste0("an_", colnames(oM))

## scale anomalies by SD
poAno_scale <- sapply(seq_len(ncol(poAno)), function(i) {
  poAno[,i] / poSD [match(paste(poAll$Match_Name, poAll$month),
    paste(poSD$Match_Name, poSD$month)),nC[i]]
}) |>
  as.data.frame()
names(poAno_scale) <- paste0("anS_", colnames(oM))


poAllan <- cbind(poAll, poAno, poAno_scale)
saveRDS(poAllan, file="~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")





#####################################
## plot normals of T9 and AlongBay ##
#####################################


## section plots of normals
save.image("~/tmp/LCI_noaa/cache-t/ctdanomalies.RData")
# rm(list=ls()); load("~/tmp/LCI_noaa/cache-t/ctdanomalies.RData"); graphics.off()


source("CTDsectionFcts.R")
dir.create(normDir, showWarnings=FALSE, recursive=TRUE)


## could do this for T9 and AlongBay. Not enough data for the other transects
## make it a poster?

## 12 months plot in a circle/rectangle, map in the middle
posterP <- TRUE
# posterP <- FALSE

levels(poAll$Transect) <- c(levels(poAll$Transect), "ABext")
oVarsDFname <- colnames(oM)[c(1, 2, 3, 8, 7, 4, 11)] ## dirty hack
# print(cbind(oVarsF, oVarsDFname, colors=names(oCol3)[seq_along(oVarsF)]))



# for(h in seq_along(levels(poAll$Transect))) {
#  j <- levels(poAll$Transect)[h]
for (j in c("AlongBay", "9", "ABext")){
  # j = "AlongBay"
  # j = "9"
  # j = "ABext"
  ## build section

  stn$Line <- flexTransect(j, stn)
  poNorm$Transect <- factor(stn$Line [match(poNorm$Match_Name, stn$Match_Name)])  ## needed!
  phT <- subset (poNorm, Transect == j)  ## Field name "Transect" is required!
  ## calc ranges
  zR <- sapply(nC, function(i) {range(phT[i], na.rm = TRUE)})
  colnames(zR) <- colnames(oM)


  # for(ov in seq_along(colnames(oM))){
  for (ov in seq_along(oVarsF)) {
    # ov=2

    # bathy_sec <- sectionize(phT) |>
    #   get_section_bathy()

    ## start graphics device
    if(posterP) {
      # pdf(paste0(normDir, "T_", j, "_", oVarsF[ov], ".pdf"),
      #     height = 11, width = 8.5)
      png(paste0(normDir, "T_", j, "_", oVarsF[ov], ".png"),
          height = 11*300, width = 8.5*300, res=300)
      ## make a ring layout for the annual cycle
#       layout(matrix(c(1:3, 12, 13, 4, 11, 13, 5, 10, 13, 6, 9:7), ncol = 3, byrow = TRUE))
      layout(matrix(rev(c(12, 1:2, 11, 14, 3, 10, 13, 4, 9, 15, 5, 8:6)), ncol = 3, byrow = TRUE))  ## winter on top
      # layout.show(n=13)
    } else {
      png(paste0(normDir, j, colnames(oM)[ov], "%02d.png")) ## for testing
    }
    for(k in seq_len(12)) {
      # k = 8
      xCo <- subset (phT, month == k &
                       !is.na(phT[,which(names(phT) == oVarsDFname[ov])])  ## weed out NAs
      )
      if(length (levels(factor(xCo$Match_Name))) > 2) {
        xCo <- sectionize(xCo)
        # keep long one for map
        if (k == 7) {xCoM <- xCo} else if(!exists("xCom")) {xCom <- xCo}

        #         dplyr::filter(!is.na(colnames(oM)[ov])) |>  ## THAT's NEEDED, BUT NOT WORKING AS-IS!! -- now under ov=2
        #|>
        # sectionPad(transect=data.frame(station = unique(phT$Match_Name),
        #   line = j, latitude = unique(phT$latitude_DD)
        #   , longitude = unique(phT$longitude_DD)))

        bathy_sec <- get_section_bathy(xCo)

        pSec(xCo
             , N=oVarsF[ov]
             # , colnames(oM)[ov]
             , ylim = c(max(phT$Depth.saltwater..m.)+5, 0)
             , drawPalette = FALSE, zCol = oCol3[[ov]]
             , zlim = zR[,which(colnames(oM) == oVarsDFname[ov])]
             , zbreaks = NULL    ## getting range correct was the hold-up
             , bathy = bathy_sec, legend.text = oVars [ov]
             , custcont=5, labcex = 0.8
        )
      } else {
        plot(1:10, type="n")
        warning(paste(ov, j, oVarsF[ov]))
      }
      mtext(month.name[k], 3, line = 0.5)
    }
    if(posterP) {

      ## map
      oce::plot(xCoM, which = 99, coastline = "best", grid = TRUE, showStations = TRUE)
# XX      mtext(paste0("Transect: ", j), line = -2)
      rm(xCoM)

      ## NCCOS-KBL logo
      if(!exists("KBL")){
        KBL <- png::readPNG("pictograms/KBL-Informal-NCCOS_tag_below_22hr.png")
        im_h <- nrow(KBL); im_w <- ncol(KBL)
        }
      plot(1:2, type = 'n', axes = FALSE, xlab = "", ylab = "", asp = 1
        , xlim = c(0, im_w), ylim = c(0, im_h), xaxt = "n", yaxt = "n", bty = "n")
      rasterImage(KBL, xleft = 0, ybottom = 0, xright = im_w, ytop = im_h)

      ## color scale bar
      nCol <- 100
      t.ramp <- oCol3[[ov]](nCol)
      yL <- 1.5
      #      par(mar=c(10, 1,3,1))

      bp <- barplot(rep(1, nCol), axes = FALSE, space = 0, col = t.ramp
                    , border = NA, ylim = c(-10, yL)  # ylim to make bar narrower, less high
      )
      title(main = oVars [ov], cex = 3, line = 0.5)
      lVal <-  pretty(c(oRange [ov, 1], oRange [ov, 2]))
      axis(1, at = (lVal - oRange [ov, 1]) / (oRange [ov, 2] - oRange[ov, 1]) * nCol
           , labels = lVal, lwd = 0, line = -10.3, tick = TRUE, lwd.ticks = 1)    ## any way to calculate line = x?
      ## main title:
      mtext(paste0("Transect: ", j), side = 1, line = -3, cex = 1.5)

    }
    dev.off(); cat("Transect:", j, oVarsF [ov], "\n")
  }
}
rm(KBL, im_h, im_w)




if(0) {
  ## layout attempts for circular year of twelve months
  pdf(paste0(normDir, "1_layouts.pdf"),
      width = 11-1, height = 8.5-1)
  layout(matrix(c(1:4, 12, 13, 13, 5, 11, 13, 13, 6, 10:7), ncol = 4, byrow = TRUE))
  layout.show(n=13)  ## large center, most balanced panels

  layout(matrix(c(1:5, 12, rep(13, 3), 6, 11:7), nrow = 3, byrow = TRUE))
  layout.show(n=13)  ## less center waste, panels portrait

  layout(matrix(c(1:3, 12, 13, 4, 11, 13, 5, 10, 13, 6, 9:7), ncol = 3, byrow = TRUE))
  layout.show(n=13)  ## less center waste, panels landscape
  dev.off()
}


cat("#\n#end of CTDwall_normals.R\n\n")
# EOF
