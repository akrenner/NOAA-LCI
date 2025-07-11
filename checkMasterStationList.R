## check station master sheet

## making sure positions of all station numbers are monotonously increasing/decreasing,
## i.e. that they are all in the correct order -- yes, they are.

stn <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
stn$Line <- factor (stn$Line)

for (i in seq_along(levels (stn$Line))){
  x <- subset (stn, Line == levels (stn$Line)[i])
  x <- x [order (x$Station),]

  cat (levels (stn$Line) [i], ":\n")
  loC <- is.unsorted(x$Lon_decDegree)
  laC <- is.unsorted(x$Lat_decDegree)
  if (laC && loC){cat ("is good")}else{
    print (cbind (x$Lon_decDegree, x$Lon_decDegree - c(NA, x$Lon_decDegree [1:(nrow (x)-1)])))
    print (cbind (x$Lat_decDegree, x$Lat_decDegree - c(NA, x$Lat_decDegree [1:(nrow (x)-1)])))
  }
  cat ("\n\n")
}

