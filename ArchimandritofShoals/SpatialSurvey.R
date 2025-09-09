## given a study area and a starting point, design a pseudo-randomized,
## spatially balanced survey with an optimal route.
## Modeled after Russia ALTE surveys -- now applied to Archimandritof Shoals
## for mini-drop-cam survey


## missing features to implement:
## - cut-out land from sa
## - use SDraw::sdraw (type="BAS")
## - link Concorde TSP solver for speed (only *nix or cygwin)


## author: Martin Renner
rm (list = ls())


#######################################################
ns <- 25  # number of samples
bas <- FALSE
bas <- TRUE
#######################################################


require ("sf"); require ("TSP"); require ("foreach")
require ("lwgeom") # for great circle random sampling
# require ("terra")  # for bathymetry

set.seed(7)

## study area
sa <- sf::read_sf("~/GISdata/LCI/shapefiles/ArchimandritofShoals/ArchimandritofShoals.shp")
# %>%
# st_transform(crs=3467) # alaska albert
# sa <- st_transform(sa, crs=3467) # alaska albert
if (0) {
  wrd <- sf::read_sf ("~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/f/GSHHS_f_L1.shp") |>
    st_make_valid()
  # contour <- terra::rast ("~/GISdata/LCI/bathymetry/Kachemak_Bay_DEM_1239/kachemak_bay_ak.asc") |>
  #   as.contour (levels=0)

  ## clip sa polygon
  # sac <- st_intersection(wrd, sa)  ## error in wk_handle.wk_wkb.... edge 746 has duplicate near loop 1
}

## non-clustered random samples
pnts <- st_sample (sa, ns, type = "random")  ##

## Spatially balanced Pseudo-random samples
if (bas) {
  # require ("SDraw")             # use Trent McDonald's BAS sampling. THANK YOU, Trent!!
  ## make SDraw samples additive, i.e. non-overlapping. Can use all together.
  # spotMother <- sdraw (sa, n = sum (nSList), type = "BAS")

  require ("spbal") ## in place of SDraw
  pnts <- spbal::BAS(sa, n = ns, minRadius = 10)$sample
}


## optimal Traveling Salesperson route
## starting point

ptT <- rbind (st_coordinates (pnts)[, 1:2], c(-151.52321, 59.63387)) |> ## start at my house
  as.data.frame() |>
  st_as_sf (crs = 4326, coords = c("X", "Y"))
rm (pnts)


nRep <- 64
require ("doParallel")
registerDoParallel (8)
## transform to UTM and solve TSP problem
etsp <- ETSP (ptT %>%
  st_transform(3467) %>%
  st_coordinates())
tspS <- solve_TSP (etsp, method = "arbitrary_insertion", two_opt = TRUE, rep = nRep)

## concorde -- requires cygwin or mac/linux
# concorde_path("/Users/Martin.Renner/Applications/concorde.exe")
# tspS <- solve_TSP (etsp, method="concorde", control=list (clo="-v -V"))
# stopCluster (cl)


pdf ("~/tmp/LCI_noaa/media/Archimandritof_route.pdf")
# plot (sa, type="n")
# plot (wrd, col="beige", add=TRUE)
# plot (sa, col="lightblue")
plot (etsp, tspS, tour_col = "red")
legend ("topright", bty = "n",  legend = paste0 ("tour length: "
  , round (tour_length(tspS) / 1e3, 1), " km"))
# print (paste (round (tour_length(tspS)/1e3,1), "km"))
dev.off()
rm (etsp)


## reorder to start with artificial starting point
resort <- function(idx, startI = length (idx)) {
  if (startI == idx[length (idx)]) {
    idx <- rev (idx)
  } else {
    idx <- c (idx [which (idx == startI):length (idx)]
      , idx [1:(which (idx == startI) - 1)])
  }
  idx
}
tsp0 <- resort (as.integer (tspS))
ptT <- ptT [as.integer (tsp0), ]
ptT$name <- paste0 ("AS_", sprintf ("%03d", seq_len(nrow (ptT))))

## points to route
# sRte <- lapply (1:nrow (ptT)-1, function (i){
#   st_cast (ptT[i:i+1,], "LINESTRING")
# }) |>
#   st_multilinestring()
# sRtem <- st_multilinestring(do.call ("rbind", sRte))


## export to gpx file for GPS
dir.create("~/tmp/LCI_noaa/data-products/Archimandritof/", showWarnings = FALSE, recursive = TRUE)
write_sf (ptT, paste0 ("~/tmp/LCI_noaa/data-products/Archimandritof/Archimandritof_N="
  , ns, "bas=", bas, ".gpx"), driver = "GPX"
, dataset_options = "GPX_USE_EXTENSIONS=YES")
write.csv(ptT, file = paste0 ("~/tmp/LCI_noaa/data-products/Archimandritof/Archimandritof_N="
  , ns, "bas=", bas, ".csv"))
