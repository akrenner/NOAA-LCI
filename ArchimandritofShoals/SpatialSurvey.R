## given a study area and a starting point, design a pseudo-randomized,
## spatially balanced survey with an optimal route.
## Modeled after Russia ALTE surveys.

## author: Martin Renner
rm (list=ls())
require ("sf"); require ("TSP"); require ("foreach")
require ("lwgeom") # for great circle random sampling

ns <- 15  # number of samples
set.seed(7)

## study area
sa <- sf::read_sf("~/GISdata/LCI/shapefiles/ArchimandritofShoals/ArchimandritofShoals.shp")
# %>%
# st_transform(crs=3467) # alaska albert
# sa <- st_transform(sa, crs=3467) # alaska albert


## non-clustered random samples
pnts <- st_sample (sa, ns, type="random")  ##
# pnts <- st_sample (sa, ns, type="hexagonal")  ##
# pnts <- st_sample (sa, ns, type="spatstat.random::")  ##

## Spatially balanced Pseudo-random samples

# require ("SDraw")             # use Trent McDonald's BAS sampling. THANK YOU, Trent!!
## make SDraw samples additive, i.e. non-overlapping. Can use all together.
# spotMother <- sdraw (sa, n = sum (nSList), type = "BAS")


## optimal Traveling Salesperson route
## starting point

pt <- rbind (st_coordinates (pnts)[,1:2], c(-151.52321, 59.63387))
## transform to UTM or
ptT <- st_as_sf(as.data.frame (pt), crs=4326, coords=c("X", "Y"))


nRep <- 50
require ("doParallel")
registerDoParallel (8)
etsp <- ETSP (ptT %>%
                st_transform(3467) %>%
                st_coordinates())
tspS <- solve_TSP (etsp, method = "arbitrary_insertion", two_opt=TRUE, rep = nRep)
# stopCluster (cl)

## reorder to start with artificial starting point
resort <- function (idx, startI=length (idx)){
  if (startI==idx[length (idx)]){
    idx <- rev (idx)
  }else{
    idx <- c (idx [which (idx==startI): length (idx)]
              , idx [1:(which (idx == startI)-1)])
  }
  idx
}
tsp0 <- resort (as.integer (tspS))

ptT <- ptT [as.integer (tsp0),]


plot (etsp, tspS, tour_col="red")
print (paste (round (tour_length(tspS)/1e3,1), "km"))

# plot (sa, col="blue")
# points (pnts, col="yellow", pch=4)


## export to gpx file for GPS
write_sf (ptT, paste0 ("~/tmp/LCI_noaa/data-products/Archimandritof_N=", ns,".gpx")
          , driver="GPX", dataset_options="GPX_USE_EXTENSIONS=YES")

