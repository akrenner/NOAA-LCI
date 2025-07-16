#!/usr/bin/env Rscript


###################################################
## plot community composition over season on map ##
###################################################

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/postKrige.RData") # from plotMaps.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R
## calculated community for x most common species
## plot community
## plot community on map





## aggregate/merge stations that are too close together to plot on map
## summary (factor (phyp$Match_Name))
agStn <- function(MN) {
  #    MN <- ifelse (grepl ("^[9A-Z]", MN), "9_6", MN)
  MN <- ifelse (grepl ("^AlongBay_[123]$", MN), "4_4", MN) # western AlongBay with T4
  MN <- ifelse (grepl ("^AlongBay_1[012]$", MN), "AlongBay_9", MN) # eastern AlongBay
  MN <- ifelse (grepl ("^4_", MN), "4_4", MN)
  MN <- ifelse (grepl ("^[9A-Z]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("^6_1[01]", MN), "6_10", MN)
  MN <- ifelse (grepl ("^6_9", MN), "6_10", MN)
  MN <- ifelse (grepl ("^6_2[05]", MN), "6_20", MN)
  MN <- ifelse (grepl ("Halibut_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Jakolof_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Bear_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("ChinaPoot_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Kasitsna_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Peterson_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Sadie_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Tutka_[AB]", MN), "AlongBay_9", MN)
  MN <- ifelse (grepl ("Seldovia_[AB]", MN), "AlongBay_9", MN)
  return (MN)
}

phyp$Match_Name <- agStn (phyp$Match_Name)
zooCenv$Match_Name <- agStn (zooCenv$Match_Name)
birdS$Match_Name <- agStn (birdS$Match_Name)

## summary (factor (phyp$Match_Name)); summary (factor (zooCenv$Match_Name))
rm (agStn)

phypC <- phyp@data [, which (names (phyp) == "Alexandrium.spp."):
which (names (phyp) == "unknown.pennate.diatom")]
phypCenv <- phyp [, c (1:which (names (phyp) == "Sampling.location"), ncol (phyp))]

birdC <- birdS [, which (names (birdS) == "ALTE"):
which (names (birdS) == "YBLO")]
## zoop: work with densities
zooCd <- zooC / zooCenv$volSample


## make community-table
cTab <- function(cM, nS = 6, agL) {             # move this into ecoAn.R
  comAb <- apply (cM, 2, mean, na.rm = TRUE)
  ## comAb <- comAb [order (comAb, decreasing = TRUE)[1:nS]]
  ## cMr <- cM [,names (cM) %in% names (comAb)]
  cMr <- cM [, order (comAb, decreasing = TRUE)[1:nS]]
  cMrA <- aggregate (cMr, by = agL, FUN = mean, na.rm = TRUE, drop = FALSE)
  return (cMrA)
}

phypCF <- cTab (phypC, agL = list (phypCenv$season, factor (phypCenv$Match_Name)))
names (phypCF)[1:2] <- c ("season", "Match_Name")
# rm (phyp)
zoopCF <- cTab (zooC, agL = list (zooCenv$season, factor (zooCenv$Match_Name)))
names (zoopCF)[1:2] <- c ("season", "Match_Name")

birdCF <- cTab (birdC, agL = list (birdS$season, factor (birdS$Match_Name))) # test!@
names (birdCF)[1:2] <- c ("season", "Match_Name")



plotComm <- function(mtx, subW = TRUE, colName = "Accent", main = ""
                     , subset = NULL) {
  # put colName into parameter to have different palletes for phytop and zoop
  require (TeachingDemos)
  require (RColorBrewer)
  colB <- function(n) {brewer.pal (n, colName)} # pick colors
  #   colB <- function (n){rainbow (n)}

  ## overplotLine <- FALSE
  overplotLine <- TRUE                # abundance line in same panel as spp frequency

  p4 <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
  stnC <- coordinates (spTransform (stn, CRS (p4)))
  lonL <- c (-153.5, -151.2)
  latL <- c (58.7, 60.2)

  par (mar = c(3, 3, 2, 1))
  plot (spTransform (coast, CRS (p4)), add = FALSE, col = "beige"
    , xlim = lonL, ylim = latL, axes = TRUE, main = main)
  box()

  ## add bathymetry or turbidity or similar as underlying image
  #   image (bath, add = TRUE)

  # plot (lonL, latL, type = "n")
  mtx$Match_Name <- factor (mtx$Match_Name)

  legend ("topleft"
    , legend = gsub (".", " ", names (mtx)[3:ncol (mtx)], fixed = TRUE)
    , fill = colB (ncol (mtx) - 2)
    , bty = "n"
    ## , inset = 0.01
  )
  bP <- function(...) {barplot(...) # ; box()
  }
  subplot (bP (matrix (rep (1, 4), nrow = 1)
    , col = "gray", axes = FALSE
    , names.arg = levels (mtx$season)) # legend of season
  , x = -151, y = 60.15
  , size = c (0.7, 0.7)
  , type = "fig", pars = list (mar = rep (0, 4), las = 2)
  )

  for (i in seq_along(levels (mtx$Match_Name))) {
    cmS <- subset (mtx, mtx$Match_Name == levels (mtx$Match_Name)[i])
    row.names (cmS) <- cmS$season
    if (subW) {
      cmS <- subset (cmS, cmS$season != "winter")
    }
    pMtx <- t (as.matrix (cmS [, 3:ncol (cmS)]))
    pMtx <- apply (pMtx, 2, function(x) {x / sum(x, na.rm = TRUE)}) # standardize seasons
    if (overplotLine) {
      yL <- NULL
    } else {
      yL <- c (0, 2.3)                  # bigger to make space for abundance line
    }
    pMtx <- ifelse (is.na (pMtx), 0, pMtx)
    ## also considered mapplots:draw.xy and mapplots:barplot2D (good function!)
    seaAb <- rowSums (cmS [, 3:ncol (cmS)]) /
      max (rowSums (mtx [, 3:ncol (mtx)]), na.rm = TRUE)
    barplotB <- function(...) {
      bX <- barplot (...) # ; box()
      if (overplotLine) {
        lines (bX, seaAb, lwd = 3)
        points (bX, seaAb, pch = 19)
      } else {
        lines (bX, 1.1 + seaAb * 0.9, lwd = 2)
        points (bX, 1.1 + seaAb * 0.9, pch = 19)
        # abline (h = 1.1)
      }
    }
    subplot (barplotB (pMtx, col = colB (ncol (mtx) - 2), axes = FALSE
      , names.arg = rep ("", ncol (pMtx))
      , ylim = yL
    )
    , x = stnC [match (cmS$Match_Name [1], stn$Match_Name), 1]
    , y = stnC [match (cmS$Match_Name [1], stn$Match_Name), 2]
    # overplotLine:
    #               , size = c(0.7, 1)
    # regular:
    , size = c (0.7, 0.7)
    , vadj = 0.5, hadj = 0.5
    , type = "fig"
    , pars = list (mar = rep (0, 4), las = 2)
    )
  }
}


PDF ("PhyPcomMapW.pdf", height = 8, width = 8)
plotComm (phypCF, FALSE)
dev.off()

PDF ("ZoopcomMap.pdf", height = 8, width = 8)
plotComm (zoopCF, FALSE, "Set1")
dev.off()

PDF ("BirdComMap.pdf", height = 8, width = 8)
plotComm (birdCF, FALSE)
dev.off()


##########################################
## map and barcharts by warm/cold years ##
##########################################

cY <- c (2012, 2013)                    # cold
wY <- c(2014, 2015, 2016)               # warm
yL <- list (cY, wY)



PDF ("PhyYearsMap.pdf", height = 8, width = 8)
# par (mfrow = c(2,1))
for (i in seq_along(yL)) {
  pyC <- subset (phypC, phypCenv$year %in% yL [[i]])
  pyCe <- subset (phypCenv, phypCenv$year %in% yL [[i]])
  yP <- cTab (pyC, agL = list (factor (pyCe$season), factor (pyCe$Match_Name)))
  names (yP)[1:2] <- c ("season", "Match_Name")
  plotComm (yP, FALSE, main =
    c(paste ("cold (", paste (yL [[i]], collapse = ", ")
      , ")", sep = ""),
    paste ("warm (", paste (yL [[i]], collapse = ", ")
      , ")", sep = "")
    )[i]
  )
}
dev.off()


# print (zooC)
print (summary (zooCenv$year))
print (summary (zooCenv))
print (names (zooC))

PDF ("ZoopYearsMap.pdf", height = 8, width = 8)
# par (mfrow = c(2,1))
for (i in seq_along(yL)) {
  print (yL[[i]])
  zooCenv$year <- as.numeric (as.character (zooCenv$year))
  pyC <- subset (zooC, zooCenv$year %in% yL [[i]])
  pyCe <- subset (zooCenv, zooCenv$year %in% yL [[i]])
  print (i)
  print (dim (pyC))
  yP <- cTab (pyC, agL = list (factor (pyCe$season), factor (pyCe$Match_Name)))
  names (yP)[1:2] <- c ("season", "Match_Name")
  plotComm (yP, FALSE, main =
    c(paste ("cold (", paste (yL [[i]], collapse = ", ")
      , ")", sep = ""),
    paste ("warm (", paste (yL [[i]], collapse = ", ")
      , ")", sep = "")
    )[i]
  )
}
dev.off()



pngC <- function(pdfFN) {
  system (paste ("convertHQ", pdfFN, gsub ("pdf$", "png", pdfFN)))
}
pngC ("~/tmp/LCI_noaa/media/phyPcomMapW.pdf")
pngC ("~/tmp/LCI_noaa/media/ZoopcomMap.pdf")



## first try
# phypCF2 <- cTab (phypC, factor (phypCenv$Match_Name))
# names (phypCF2)[1] <- "Match_Name"
# barplot (t (as.matrix (phypCF2 [,2:ncol (phypCF2)])), col = rainbow (ncol (phypCF2)-1))



# require (mapplots)
# draw.barplot2D  ## by season?
# nice pie-chart alternative otherwise
# draw.xy {mapplots}                      # that's the ticket!

## most abundand species only
