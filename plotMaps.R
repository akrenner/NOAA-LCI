#!/usr/bin/env Rscript

## use CTD station data from dataSetup.R and plot it
## using automap to interpolate stations (kriging)
## do one map per season

## old script, currently broken -- cool results, though, worth to resurrect?
## need to migrate all code from sp to sf


rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ecoAn.RData") # from ecoAn.R

require (parallel)
require (automap)
require (geoR)
# require (rgdal)

nCPUs <- detectCores()
gRes <- 2e3
gRes <- 5e3



seasonSelect <- "all"

if (seasonSelect == "all") {
  ##    poSS <- subset (poSS, (1:nrow (poSS)) %in% grep ("^Along", poSS$Match_Name, invert = TRUE))
  ## cut "Along Bay", if using full year

  ## Kris' suggestion to make sure equal weights across season -- not sure I agree
  poSS <- subset (poSS, (1:nrow (poSS)) %in% grep ("^\\d_\\d", poSS$Match_Name))

} else {
  poSS <- subset (poSS, season == seasonSelect)
}


system ("mkdir -p ~/tmp/LCI_noaa/media/krige/all")
system ("mkdir -p ~/tmp/LCI_noaa/media/krige/spring")
system ("mkdir -p ~/tmp/LCI_noaa/media/krige/summer")
system ("mkdir -p ~/tmp/LCI_noaa/media/krige/fall")
system ("mkdir -p ~/tmp/LCI_noaa/media/krige/winter")


# grd <- SpatialPoints (bath)
## grd <- bbox (bath)
## grd <- expand.grid (seq (grd [1,1], grd [1,2], by = 10e3)
##                   , seq (grd [2,1], grd [2,2], by = 10e3)
##                     )
grd <- expand.grid (seq (-15e3, 180e3, by = gRes)
  , seq (980e3, 1130e3, by = gRes)
)
## better yet: start with a polygon
coordinates (grd) <- ~ Var1 + Var2
proj4string (grd) <- CRS (proj4string (poSS))  ## XXX currently broken
grd <- SpatialPixels (grd)

## plot (grd)
## plot (coast, add = TRUE, col = "beige")
## axis (1); axis (2)
print (dim (poSS@data))
summary (poSS@data)

##  this needs testing -- cut-out all non-numeric variables!
nonNum <- unlist (sapply (1:ncol (poSS@data), FUN = function(i) {
  is.numeric (poSS@data[, i])}))
## non-numeric columns to cut
names (poSS@data)[which (!nonNum)]
poSS@data <- poSS@data [, nonNum]
rm (nonNum)

cor (poSS@data [, which (names (poSS) == "SST"):ncol (poSS@data)]
  , use = "pairwise.complete")



## subsets
## average common locations

## require (ocedata)
## data(coastlineWorldFine)
## require (oceanmap)


# coast <- spTransform (coast, CRS (LLprj))



## inCol <- which (names (poSS) %in% c("maxDepth", "tideRange", "sunAlt"))
## inCol <- which (names (poSS) == "SST"):(which (names (poSS) == "tideRange")-1) # exclude tideRange
## inCol <- subset (inCol, !(names (poSS)[inCol] %in% c("deepSal_old", "deepPyc")))

inCol <- which (!names (poSS) %in% c ("maxDepth", "tideRange", "sunAlt", "deepSal_old"
  , "deepPyc", "stability2", "Year", "month"
  , "aveSalinity", "aveTemp", "minTemp"
))

poPCA <- prcomp (~.
  , poSS@data [, inCol]
  , na.action = 'na.omit', center = TRUE
  , scale = TRUE)
poPCA$rotation [, 1:3]

PDF ("pcaPlots.pdf")
biplot (poPCA)
screeplot (poPCA)
dev.off()

poSS@data <- cbind (poSS@data, predict (poPCA, poSS@data)[, 1:2])
inCol <- c (inCol, ncol (poSS) + c(-1, 0))

print (dim (poSS@data))
summary (poSS@data)
cor (poSS@data [, inCol], use = "pairwise.complete")



ak <- function(i, poSS = poSS) {

  ## poSS <- subset (poSS, !is.na (poSS$minO2))
  ## poSS <- subset (poSS, !is.na (poSS$stability))
  poSS <- subset (poSS, !is.na (poSS@data [, i]))


  ## jitter duplicate coordinates
  require (geoR)
  jC <- jitter2d (coordinates (poSS), max = 1e3, min = 10)
  poSSA <- data.frame (jC, poSS@data [, i])
  names (poSSA) <- c("x", "y", names (poSS@data)[i])
  coordinates (poSSA) <- ~ x + y
  proj4string (poSSA) <- CRS (proj4string (poSS))
  ##     ## average same positions
  ##     poSSdf <- cbind (coordinates (poSS), poSS@data)
  ##     poSSa <- aggregate (poSSdf [,c(1,2, i+2)]~poSSdf$SampleID, FUN = mean)
  ## ##   poSSA <- aggregate (poSS, by = list (poSS@data$SampleID), FUN = mean)

  ## also test: akima
  ## interp.new

  if (0) {
    require (intamap)                   # abstraction beyond automap -- good choice!
    obj = createIntamapObject(
      observations = poSSA
      , predictionLocations = grd
      # Prediction for a different projection possible if rgdal is available
      #    targetCRS = ifelse(require(rgdal), "+init=epsg:3035", "+init=epsg:28992"),
      , targetCRS = CRS (proj4string (poSS))
      , class = "idw"
      #            , boundaries = coast
    )
    ## run test:
    checkSetup(obj)
    ## do interpolation steps:

    ## obj = preProcess(obj)
    ## obj = estimateParameters(obj) # faster
    ## obj = spatialPredict(obj)
    ## obj = postProcess(obj)
    #        obj <- interpolate (obj)

    kr <- obj$predictions

  } else {
    kr <- autoKrige (formula (paste (names (poSS)[i], "~1")), poSSA
      ##             , grd
      ##             , maxdist =50e3    # approx 1/2 way across study area
    )
    PDF (paste ("krige/krigemodel_", names (poSS)[i], ".pdf", sep = ""))
    plot (kr)
    dev.off()
    return (kr)
  }
}


krigL <- mclapply (inCol, FUN = ak, mc.cores = nCPUs, poSS = poSS)
names (krigL) <- names (poSS)[inCol]
save.image ("~/tmp/LCI_noaa/cache/krigResults.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/krigResults.RData")
## build SpatialGridDataFrame or raster-brick or similar of output layers






######################################################################################
## options to a map:                                                                ##
## 1. basic graphics. Good axes, use image. Easy to expand. Hard: color scale       ##
##    basic + raster (not image)?
## 2. trellis (spplot or automapPlot or. Easy color scale, difficult to add         ##
##             geographic coordinates                                               ##
## 3. oce  looks promissing. Not familiar with it.  Need to start in LL             ##
######################################################################################


## package for coordinates, etc? oce, marmap?


plotKrige <- function(i) {
  #    fieldN <- i-1+which (names (poSS) == "SST")
  fieldN <- names (krigL)[i]
  if (0) {                             # spplot
    pdf (paste ("~/tmp/LCI_noaa/media/krige/krige_", fieldN
      , ".pdf", sep = ""))
    ## automapPlot (kr$krige_output, "var1.pred") # trellis
    ## http://rstudio-pubs-static.s3.amazonaws.com/53530_1af2d0b5ae1f4a36a75e611d3566f777.html#1
    require (sp)
    require (raster)
    lciP <- spplot (krigL[[i]]$krige_output, "var1.pred"
      , main = fieldN
      , col.regions = heat.colors(64) # or rainbow (64)
      , colorkey = TRUE
      , scales = list (draw = FALSE)
      , xlim = c (bbox (stn)[1, 1] * 0.98, bbox (stn)[1, 2] * 1.02)
      , ylim = c(bbox (stn)[2, 1] * 0.98, bbox (stn)[2, 2] * 1.02)
      , as.table = TRUE           # see bering.layout
      , sp.layout = list (
        ## list ("sp.lines", rasterToContour (bath, levels = c(-100, 100, 200, 500, 1000)))

        list ("sp.polygons", coast, lwd = 0.1, fill = "beige", first = FALSE)
        , list ("sp.lines", bathCont, lwd = 1, col = "black")
        , list ("sp.points", stn, pch = 1, lwd = 2, cex = 0.5, col = "black")
      )
    )
    ## add graticules? or use basic graphis for that
    plot (lciP)
    dev.off()
  }

  if (1) {    ## using base graphics to include graticules
    PDF (paste ("krige/krige_", fieldN, "_base.pdf", sep = "")
      , width = 7, height = 5
    )

    ## convert automap to raster, then plot raster (includes legend!)
    ## see https://cran.r-project.org/web/packages/graticule/vignettes/graticule.html
    #        rk <- raster (kr$krig_output)
    # graticule could be improved upon!
    kout <- krigL[[i]]$krige_output
    plot (kout
      ## , xlim = c (bbox (stn)[1,1]*0.98, bbox (stn)[1,2]*1.02)
      ##   , ylim = c(bbox (stn)[2,1]*0.98, bbox (stn)[2,2]*1.02)
      , axes = FALSE
    )                         # now includs a legend :)
    plot (coast, add = TRUE, col = "beige")
    #         plot (stn, add = TRUE, pch = 19, cex = 0.3)
    plot (poSS, add = TRUE, pch = 19, cex = 0.3)
    ## text (coordinates (stn), labels = stn$Match_Name)

    ## require (graticule)
    ## graticule
    ## or in rgdal
    #        require (rgdal)
    llgridlines (kout, lty = 3, side = "WS", offset = -0.5, lwd = 0.5, cex = 0.5)
  }

  if (0) {
    image (unique (coordinates (kr$krige_output)[, 1])
      , unique (coordinates (kr$krige_output)[, 2])
      , kr$krige_output$var1.pred)


    require (oce)                       # a real possibility!!
    drawPalette(colormap = cm)
    mapPlot(coastlineWorld, projection = "+proj=moll", grid = FALSE)
    par(mar = c(2, 2, 1, 1))
    lonlim <- c(-80, 0)
    latlim <- c(20, 60)
    mapPlot(coastlineWorld, projection = "+proj=lcc +lat_1=30 +lat_2=50 +lon_0=-40",
      longitudelim = lonlim, latitudelim = latlim)
    mapImage (coordinates (kr$krige_output)$Var1, coordinates (kr$krige_output)$Var2, kr$krige_output$var1.pred, colormap = cm)


    krR <- raster (kr$krige_output)
    require (raster)
    #    plot (as.raster (

    #   plot (kr$krige_output$var1.pred, xaxs="i", yaxs = "i")
    plot (stn, add = TRUE)
    plot (coast, col = "beige", add = TRUE)
    dev.off()
  }


  dev.off()
}


## plot kriging results
# plotKrige (4)
dummy <- mclapply (1:length (krigL), plotKrige, mc.cores = nCPUs)
for (i in 1:length (krigL)) {
  print (names (krigL)[i])
  plotKrige (i)
}
## print (dummy)
rm (poPCA)
print (Sys.time())



save.image ("~/tmp/LCI_noaa/cache/postKrige.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/postKrige.RData")

if (0) {
  ## find slope and max slope to detect fronts
  ## require ("SDMTools")
  ## slope (
  require ("raster")
  dir.create ("~/tmp/LCI_noaa/media/fronts", recursive = TRUE)
  slopeF <- function(i) {
    require ("raster")
    dR <- krigL[[i]]$krige_output
    dR@data <- data.frame (dR@data$var1.pred)
    if (var (dR@data [, 1]) > 0) {
      tR <- terrain (raster (dR)
        , opt = "slope", unit = "tangent", neighbors = 8)
      PDF (paste ("front/", names (krigL)[i], ".pdf", sep = ""))
      spplot (tR)
      dev.off()
      return (tR)
    }
  }
  slopeL <- lapply (1:length (krigL), slopeF) # , mc.cores = nCPUs) # raster not working with mclapply
  require (dplyr)



  poS <- poSS
  poS@data <- data.frame (poS$deepTemp)
  sPO <- terrain (poS, opt = "slope", unit = "tangent", neighbors = 8)
  spplot (sPO)
}

q()










pdf (paste ("~/tmp/LCI_noaa/media/krige_", names (poSS)[i], ".pdf", sep = ""))
plot (kr)
spplot (kr$krige_output, "var1.pred")

ggplot(aes(x = x, y = y, fill = var1.pred), data = dat) +
  geom_tile() +
  scale_fill_continuous(low = "white", high = muted("blue")) +
  coord_equal()


# "var1.pred", "var1.var", "var1.stdev"
#     spplot (kr$krige_output, "var1.pred")
dev.off()



require (oceanmap)                      # requires Lat Lon, not projected
## plotmap ()
## v()
pdf (paste ("~/tmp/LCI_noaa/media/", names (poSS)[i], "_map.pdf", sep = ""))
#   v (kr$krige_output, "var1.pred")
#   plotmap (lon=c(-154.5,-150.9), lat = c(58.7,60.5), main ="", grid = FALSE, add = TRUE)
## plotmap with projected coordinates?!!!
## reproject raster??
## just use akima interpolation??
plot (coast, col = "beige", axes = TRUE, xaxs = "i", yaxs = "i", xlim = lonL, ylim = latL
)
krL <- spTransform (kr)
plot (krL, add = TRUE)              # color scale
plot (coast, col = "beige", add = TRUE)
##  go back to spplot because of color scale on the side
## good-looking color scale in basic graphics? You'd think so!?
dev.off()

return (kr)



# for (i in 9:ncol (poSS)){
krigResults <- mclapply (which (names (poSS) == "SST"):ncol (poSS)
  , FUN = ak, mc.cores = nCPUs)

## build SpatialGridDataFrame or raster-brick or similar of output layers


save.image ("~/tmp/LCI_noaa/cache/krigResults.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/krigResults.RData")



## cor (as.numeric (kr$krige_output$var1.pred), as.numeric (kr$krige_output$stdev))

plotLCI <- function(rstr, fn) {
  pdf (paste ("~/tmp/LCI_noaa/media/", fn, ".pdf", sep = ""))
  plotmap (lon = c(-154.5, -150.9), lat = c(58.7, 60.5), main = "", grid = FALSE, add = TRUE)
  dev.off()
}

# mclapply (9:ncol (poSS), plotLCI, mc.cores = nCPUs)

cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
  , usetz = FALSE)
, "\n# \n# End of plotMaps.R\n#\n#\n")
## EOF
