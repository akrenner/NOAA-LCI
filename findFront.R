#!/usr/bin/env Rscript

## location of fronts in transects
## use steepest part of transect-surface to mark front
## aggregate over all measurements
##


## only use Along and numerical transects


## output from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
# require ("raster")
# require (rgeos)
require ("terra")
require ("sf")

poT <- subset (poSS, grepl ("^[1-9]|Along", poSS$Match_Name))
tranS <- factor (paste (substr (poT$Match_Name, 1, 1)
                      , format (poT$timeStamp, "%Y-%m-%d")))
poT <- poT [,names (poT) %in% c("Match_Name", "timeStamp"
                                             , "SST", "aveTemp", "SSS"
                                             , "PARdepth5p", "Fluorescence"
                                             , "minO2", "O2perc", "stability"
                                               , "pcDepth")]
# poTo <- poT [order (tranS, coordinates (poT)[,1]),]


dir.create ("~/tmp/LCI_noaa/media/fronts/", recursive = TRUE, showWarnings = FALSE)

tFront <- data.frame (levels (tranS), matrix (nrow = length (levels (tranS))
                                   , ncol = ncol (poT)-1))

names (tFront) <- c ("tranS", "mDist", names (poT)[3:ncol (poT)])
##, fDist <- numeric (length (tranS)))


for (i in 1:length (levels (tranS))){
    trD <- subset (poT, tranS == levels (tranS)[i])

    if (nrow (trD) > 3){
        ## sort transect E to W or N to S (by timeStamp -- reverse if necessary)

        ## is T E-W or N-S?
        # crds <- coordinates (trD)
        crds <- sf::st_coordinates (trD)
        if (diff (range (crds [,1])) > diff (range (crds [,2]))){ # E-W, dlat > dlon
            trD <- trD [order (crds [,1], decreasing = TRUE),]
        }else{
            trD <- trD [order (crds [,2], decreasing = TRUE),]
        }
        rm (crds)

        PDF (paste ("fronts/", levels (tranS)[i], ".pdf", sep = "")
           , width = 7, height = 10)
        par (mfrow = c (9,1), mar = c(3,4,0,1)
             )
        # cat ("\n\n, transect = ", i, "###")
        tDist <- sf::st_distance (trD [1,], trD, by_element=FALSE) |>  #, byid = TRUE
                                  as.numeric() / 1000  # in km
        tFront$mDist [i] <- max (tDist)
        for (j in 3:6){  # range of relevant parameters
            fDistInt <- data.frame (dDist = diff (tDist))
            parInt <- st_drop_geometry (trD [,j])[,1] |>
              diff()
            fDistInt$slope <- abs (parInt / fDistInt$dDist)
         #   print (j)
            if (all (is.na (fDistInt$slope))){
                cat (i, j, "can't do this one \n")
            }else{
                tFront [i,j] <- mean (c(tDist [which.max (fDistInt$slope)]
                                          , tDist [which.max (fDistInt$slope)+1]))
                plot (tDist, st_drop_geometry (trD [,j])[,1]
                      , axes = FALSE, ylab = names (trD)[j]
                    , type = "l"
                    ## , ylim = range (poT [,j])
                      )
                axis (2)
                abline (v = tFront [i,j], lwd = 2, col = "blue")
            }
        }
        axis (1, xlab = "distance [km]")
        dev.off()
    }
}

save.image ("~/tmp/LCI_noaa/cache-t/frontEnd.RData")
# load ("~/tmp/LCI_noaa/cache-t/frontEnd.RData")


tranCat <- factor (substr (tFront$tranS, 1, 2))
for (i in 1:length (levels (tranCat))){
    PDF (paste ("fronts/sumHist", levels (tranCat)[i], ".pdf", sep = ""))
   # par (mfrow = c(9,1)) #, mar = c(3,4,0,1))
    sumF <- subset (tFront, tranCat == levels (tranCat)[i])

    for (j in 3:6){  # range of relevant parameters -- could extend
        hist (sumF [,j]
            , main = ifelse (levels (tranCat)[i] == "A",
                             paste (names (sumF)[j], "AlongBay"),
                             paste0 (names (sumF)[j], " T-", levels (tranCat)[i])
                             )
            , xlim = c(0, max (sumF$mDist, na.rm = TRUE)*1.1)
              , xlab = ifelse (levels (tranCat)[i] %in% c("4", "9"),
                               "distance [km] South to North",
                               "distance [km] West to East")
              )
        abline (v = mean (sumF [,j], na.rm = TRUE), col = "blue", lwd = 2)
        box()
    }
    dev.off()
}

saveRDS(tFront, file = "~/tmp/LCI_noaa/cache/frontlocation.rds")

## EOF ###
