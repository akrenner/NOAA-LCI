## visualize current transect sampling and balanced spatial sampling


rm (list = ls())

## set-up for processing on HPC facility
tr <- require ("pacman")
if (!tr){
  install.packages ("pacman")
}
tr <- try (load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")) # from dataSetup.R -- of interest: zooC and zooCenv
if (class (tr) == "try-error"){
  dir.create ("~/tmp/LCI_noaa/cache/", recursive = TRUE, showWarnings = FALSE)
  file.copy (from = "P:/My Documents/Documents/tmp/LCI_noaa/cache/dataSetupEnd.RData"
             , to = "~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
  load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
}
set.seed (9)
require ("SDraw")
# require ("Rgshhs")
require ("sp")
require ("raster")
dir.create ("~/tmp/LCI_noaa/media/2021/", recursive = TRUE, showWarnings = FALSE)
dir.create ("~/tmp/LCI_noaa/cache/", recursive = TRUE, showWarnings = FALSE)



## fix duplicate station names
zooCenv$Match_Name <- ifelse (zooCenv$Match_Name == "AlongBay_3", "4_3", zooCenv$Match_Name)
#not relevant here...
zooCenv$Match_Name <- ifelse (zooCenv$Match_Name == "AlongBay_6", "9_6", zooCenv$Match_Name)

## reduce to core stations -- build index to subset
keep <- which (zooCenv$Transect %in% c ("KB", "4", "9"))
# keep <- c (keep, which (zooCenv$Match_Name %in% c ("3_E", "7_E", "6_E")))
zooC <- zooC [keep,]
zooCenv <- zooCenv [keep,]
rm (keep)


phyCenv <- spTransform(phyCenv, CRSobj = CRS (proj4string(coast)))
## define study area -- Kachemak Bay
## cookie-cutter a box
bb <- bbox (zooCenv)
# 
# bb <- rbind (c(118100, 1150000) #easting
#              , c(102000, 10868000))  # northing
kBs <- rbind (bb [,1]
              ,c (bb [1,1], bb [2,2])
              ,bb [,2]
              ,c (bb [1,2], bb [2,1])
              # ,bb [c(1,2),c(1)]
              , bb [,1]
)
kBs [1,] <- c (114500,  1038000) # SW corner
#              119162.5 1058273.3 
kBs [5,] <- kBs [1,]
kBs [3,] <- c (176000, 1102000)  # east, north  -- extend into upper bay -- NE corner
#                      1087091
kBs [4,] <- c (176000,  1045000)
#              164823.8 1058273.3 

p <- SpatialPolygons (list (Polygons (list (Polygon (kBs)), 1)))
proj4string(p) <- CRS (proj4string(zooCenv))

# plot (p, col = "blue", add = TRUE)
# plot (coast, col = "beige", add = TRUE)

## restrict study area to water
require ("rgeos")
studyA <- gDifference (p, coast)



pdf ("~/tmp/LCI_noaa/media/KBayZoopSampling.pdf", width = 11, height = 8.5)
## plot map
plot (studyA, col = "lightblue")
plot (coast, col = "beige", add = TRUE)

blues <- colorRampPalette(rev (c ("red", "purple", "blue", "cadetblue1", "white")))
# blues <- colorRampPalette (rev (c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")))
plot (bath, col = blues (100), add = TRUE)
# require ("marmap")
# plot.bathy (bath, bpal = blues (100) , add = TRUE)


lines (studyA)
# lines (coast)


plot (zooCenv, pch = 19, col = "red", add = TRUE)
# points (phyCenv, pch = 3, col = "black")
# points (poSS, pch = 3)
text (zooCenv, labels = zooCenv$Station, pos = 4, offset = 0.5)
dev.off()


# require ("dismo")
# zooD <- voronoi (zooCenv, ext = 10)
# plot (zooD, add = TRUE)
# spplot (zooD, "id")


require ("sf")
## should add envelope to st_voronoi
zPt <- coordinates (zooCenv) %>%
  st_multipoint() %>%
  st_voronoi() %>% 
  st_collection_extract()
zPt <- as (zPt, "Spatial") ## convert sf to sp spatial
proj4string(zPt) <- CRS (proj4string(zooCenv))


## intersect/cookie-cutter polygon with study area
# vor <- gDifference (studyA, zPt)
# gIntersection
# gSymdifference
# gUnion

## cluster analysis onto polygons
## plot colored polygons


pdf ("~/tmp/LCI_noaa/media/KBayZoopBioGeo.pdf", width = 11, height = 8.5)
plot (studyA)
plot (zPt, add = TRUE) # color by cluster
plot (coast, col = "beige", add = TRUE)

dev.off()




# write.gif (x, filename, delay = 100)

## EOF