#!/usr/bin/env Rscript

## CTD anomaly
## for each station location, calculate averages for all
## oceanographic parameters


## priorities to fix
## elsewhere: in CTD_cleanup / QAQC (merge those!): calc turbidity from loess function of beamTransmission~beamAttenuation
## improve monthly mean algorithm (circular seasonal mean)
## review all XXX




## -- fix submission to NCEI first
## then tweak turbidity upstream. Fix PAR while at it
## then fix things here, using turbidity and none of the beam stuff






rm(list = ls())
graphics.off()
load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R
normDir <- "~/tmp/LCI_noaa/media/CTDsections/CTDsection-normals/"



## copied from dataSetup.R(should have a package)
Seasonal <- function(month) {           # now using breaks from zoop analysis -- sorry for the circularity
  month <- as.numeric(month)
  cut(month, breaks = c(-13, 0, 2, 4, 8, 10, 25)
    , labels = c("fall", "winter", "spring", "summer", "fall", "winter"))
}
dir.create(normDir, showWarnings=FALSE, recursive=TRUE)



#################################
## monthly and quarterly means ##
#################################

# monthly mean
#   XXX could do this better, using circular annual means, as elsewhere

# min for a stable mean:
nMin <- 3 # SD estimate become unstable if too low, affecting range of scaled anomalies
current_year <- format(Sys.Date(), "%Y")


## output: as poAll, but with month as factor
poAll$month <- as.numeric(poAll$month)



## XXXX move forward to CTD_cleanup.R!!!       ================================= XXX
poAll$Match_Name <- as.character(poAll$Match_Name)
# poAll$Match_Name <- ifelse (poAll$Match_Name == "4_3", "AlongBay_3", poAll$Match_Name)
# poAll$Match_Name <- ifelse (poAll$Match_Name == "AlongBay_6", "9_6", poAll$Match_Name)


oM <- as.matrix(poAll [, which(names(poAll) == "Temperature_ITS90_DegC")
  :ncol(poAll)])

ctdAgg <- function(df = poAll, FUN=mean, ...) {
  aggregate(oM ~ Match_Name + month + Depth.saltwater..m., data = df,
    # subset = format(isoTime, "%Y") != current_year,       ## exclude current year -- a good idea? XXX
    FUN = FUN, ...) |>
    dplyr::arrange(Match_Name, month, Depth.saltwater..m.)
}


poNorm <- ctdAgg(df = poAll, FUN = mean, na.rm = TRUE)
nC <- which(names(poNorm) == colnames(oM)[1]):ncol(poNorm)
# pN <- ctdAgg(df = poAll, FUN = function(x) {sum(!is.na(x)) }) |>
#   dplyr::select(-Match_Name, -month, -Depth.saltwater..m.) |>
#   apply(MARGIN=1, FUN=sd)
# if(!all.equal(pN, rep(0, nrow(poNorm)))) {stop("investigate discrepancy")}; rm(pN)
pN <- ctdAgg(df = poAll, FUN = function(x) {sum(!is.na(x)) }) |>
  dplyr::pull(Temperature_ITS90_DegC) # inefficient to compute, but easy to code

# drop values with N < nMin
poNorm[,nC]<-sapply(nC, function(i) {ifelse(pN < nMin, NA, poNorm [,i])} )
# ## add stn data and Pressure for oce



poSD <- ctdAgg(df = poAll, stats::sd, na.rm = TRUE)
poSD[,nC] <- sapply(nC, function(i) {ifelse(pN < nMin, NA, poSD   [,i])} )


names(poSD) <- paste0("SD_", names(poSD))
poRA <- ctdAgg(df = poAll, function(x){diff(range(x, na.rm = TRUE))})
names(poRA) <- paste0("Range_", names(poRA))
poNorm <- cbind (poNorm,
    poSD [,which(names(poNorm) == colnames(oM)[1]):ncol(poSD)]
  , poRA [,which(names(poNorm) == colnames(oM)[1]):ncol(poRA)]
  )

if(0) { ## quarterly means -- not really enough data for these?
  season <- Seasonal(poAll$month)
  poSorm <- aggregate(oM ~ Match_Name + season + Depth.saltwater..m., data = poAll,
    FUN = mean, na.rm = TRUE) |>
    dplyr::arrange(Match_Name, month, Depth.saltwater..m.)
  poSorm [,nC] <- sapply(nC, function(i) {ifelse(pN < nMin, NA, poSorm [,i])} )
}



## calculate anomalies and save for further plotting
normMatch <- match(paste(poAll$Match_Name, poAll$month,
                         poAll$Depth.saltwater..m.),
  paste(poNorm$Match_Name, poNorm$month, poNorm$Depth.saltwater..m.))


poAno <- sapply(seq_len(ncol(oM)), function(i) {
  pC <- which(names(poAll) == colnames(oM)[i])
  poAll [,pC] - poNorm [normMatch, nC[i]]
}) |>
  as.data.frame()
names(poAno) <- paste0("an_", colnames(oM))

## scale anomalies by SD
poASc <- sapply(seq_len(ncol(oM)), function(i) {
#  poAno[,i] / poSD [normMatch, nC[i]]
  (poAll [,which(names(poAll) == colnames(oM)[i])] -
    poNorm[normMatch, which(names(poNorm) == colnames(oM)[i])]) /
    poSD [normMatch, which(names(poSD) == paste0("SD_", colnames(oM)[i]))]
}) |>
  as.data.frame()
names(poASc) <- paste0("anS_", colnames(oM))
rm(normMatch)

poAll <- cbind(poAll, poAno, poASc); rm (poAno, poASc)
saveRDS(poAll, file="~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")






#####################################
## plot normals of T9 and AlongBay ##
#####################################


## use poNorm as list of CTD casts
poNorm <- data.frame(
  latitude_DD = stn$Lat_decDegree[match(poNorm$Match_Name, stn$Match_Name)],
  longitude_DD = stn$Lon_decDegree[match(poNorm$Match_Name, stn$Match_Name)],
  Pressure..Strain.Gauge..db. = predict(lm(Pressure..Strain.Gauge..db. ~
    Depth.saltwater..m., poAll), newdata = list(Depth.saltwater..m. =
    poNorm$Depth.saltwater..m.)),
  File.Name = "climatology",
  isoTime = as.POSIXct(paste0("2000-", poNorm$month, "-15 12:00")),
  Bottom.Depth_main = stn$Depth_m [match(poNorm$Match_Name, stn$Match_Name)],
  # to get Transect in the right position, used below
  Transect = factor(stn$Line [match(poNorm$Match_Name, stn$Match_Name)]),
  poNorm
)
nC <- which(names(poNorm) == colnames(oM)[1]):ncol(poNorm)


## could do this for T9 and AlongBay. Not enough data for the other transects
## make it a poster?

levels(poAll$Transect) <- c(levels(poAll$Transect), "ABext")
oVarsDFname <- colnames(oM)
# print(cbind(oVarsF, oVarsDFname, colors=names(oCol3)[seq_along(oVarsF)]))
oVarsTitle <- oVarsF |>
  stringr::str_replace("sigmaTheta", "density") |>
  stringr::str_replace("bvf", "stratification") |>
  stringr::str_replace("_", " ") |>
  stringr::word(1) |>
  stringr::str_to_title()





## section plots of normals
save.image("~/tmp/LCI_noaa/cache-t/ctdanomalies.RData")
# rm(list=ls()); load("~/tmp/LCI_noaa/cache-t/ctdanomalies.RData"); graphics.off()

## adapt to cbind of poNorm and poSD -- these are of no use for anomalies
oVarsF <- c(oVarsF, paste0("SD_", oVarsDFname), paste0("Range_", oVarsDFname))
oVarsDFname <- c(oVarsDFname, paste0("SD_", oVarsDFname), paste0("Range_", oVarsDFname))
oVarsTitle <- c(oVarsTitle, paste0("SD-", oVarsTitle), paste0("Range-", oVarsTitle))
oVars <- rep(oVars,2)
# oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {viridis::plasma}))
# oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {viridis::rocket}))
# oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {viridis::viridis}))
# oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {oce::oceColorsViridis}))
# oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {viridis::mako}))
# oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {RColorBrewer::XXXblues}))  # or yellow-green-blue?
oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {heat.colors}), # for SD
           lapply(seq_along(oCol3), function(x) {heat.colors})         # range
           )


## 12 months plot in a circle/rectangle, map in the middle
posterP <- TRUE
# posterP <- FALSE

source("CTDsectionFcts.R")
## load KBL logo
KBL <- png::readPNG("pictograms/KBL-Informal-NCCOS_tag_below_22hr.png")



if(0) {## parallelize this plotting code for speed? -- maybe too much trouble?
plotNorm <- function (i, cmbs) {
  j <- cmbs [i,2]
  ov <- cmbs [i,1]
  ## prep transect -- j
  ### ....
  cat("Transect:", j, oVarsTitle [ov], "\n")
}

require("parallel")
cmbs <- expand.grid(ov = seq_along(oVarsF), j = c("AlongBay", "9", "ABext"))
x <- lapply(seq_len(nrow(cmbs)), FUN = plotNorm, cmbs=cmbs)
}


# for(h in seq_along(levels(poAll$Transect))) {
#  j <- levels(poAll$Transect)[h]
for (j in c("AlongBay", "9", "ABext")) {
  # j = "AlongBay"
  # j = "9"
  # j = "ABext"
  ## build section

  stn$Line <- flexTransect(j, stn)
  poNorm$Transect <- factor(stn$Line [match(poNorm$Match_Name, stn$Match_Name)])  ## needed!
  # poSD$Transect   <- factor(stn$Line [match(poSD$Match_Name,   stn$Match_Name)])
  phT <- subset (poNorm, Transect == j)  ## Field name "Transect" is required!
  # phT <- subset (poSD,   Transect == j)
  ## calc ranges
  zR <- sapply(nC, function(i) {range(phT[i], na.rm = TRUE)})
  colnames(zR) <- colnames(phT)[nC]


 for (ov in seq_along(oVarsF)) {
 # for(ov in 8:length(oVarsF)) {
      # ov=2
     # ov=8

    # bathy_sec <- sectionize(phT) |>
    #   get_section_bathy()

    ## start graphics device
    if(posterP) {
      # pdf(paste0(normDir, "T_", j, "_", oVarsF[ov], ".pdf"),
      #     height = 11, width = 8.5)
      png(paste0(normDir, "T_", j, "_", sprintf("%02d", ov), "-", oVarsF[ov],
        ".png"), height = 11*300, width = 8.5*300, res=300)
      ## make a ring layout for the annual cycle
#       layout(matrix(c(1:3, 12, 13, 4, 11, 13, 5, 10, 13, 6, 9:7), ncol = 3, byrow = TRUE))
      layout(matrix(rev(c(12, 1:2, 11, 14, 3, 10, 13, 4, 9, 15, 5, 8:6)), ncol = 3, byrow = TRUE))  ## winter on top
      # layout.show(n=13)
    } else {
      png(paste0(normDir, j, colnames(oM)[ov], "%02d.png")) ## for testing
    }
    for(k in seq_along(month.abb)) {
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
             , drawPalette = FALSE
             , zCol = oCol3[[ov]]
             , zlim = zR[,which(colnames(zR) == oVarsDFname[ov])]
             , zbreaks = NULL    ## getting range correct was the hold-up
             , bathy = bathy_sec, legend.text = "" #oVars [ov]
             , custcont=5
             , labcex = 0.8
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
      im_h <- nrow(KBL); im_w <- ncol(KBL)
      # ppar <- par()
      par(mar=c(1,2,1,2.0))
      plot(1:2, type = 'n', axes = FALSE, xlab = "", ylab = "", asp = 1
        , xlim = c(0, im_w), ylim = c(0, im_h), xaxt = "n", yaxt = "n", bty = "n")
      rasterImage(KBL, xleft = 0, ybottom = 0, xright = im_w, ytop = im_h)

      ## color scale bar
      nCol <- 100
      t.ramp <- oCol3[[ov]](nCol)
      yL <- 1.5
      #      par(mar=c(10, 1,3,1))
      par(mar=c(1,2,4.1,2.0))
      bp <- barplot(rep(1, nCol), axes = FALSE, space = 0, col = t.ramp
                    , border = NA, ylim = c(-10, yL)  # ylim to make bar narrower, less high
      )
      title(main = oVars [ov], cex = 3, line = 0.5)
      lVal <-  pretty(c(zR [1, ov], zR [2, ov]))
      axis(1, at = (lVal - zR [1, ov]) / (zR [2, ov] - zR [1, ov]) * nCol
           , labels = lVal, lwd = 0, line = -10.0, tick = TRUE, lwd.ticks = 1)    ## any way to calculate line = x?
      ## main title:
      mtext(paste0(oVarsTitle [ov], "\nTransect: ", j), side = 1, line = -2, cex = 1.5)

    }
    dev.off()
    cat("Transect:", j, oVarsTitle [ov], "\n")
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
