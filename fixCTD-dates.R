tst <- read.csv ("~/GISdata/LCI/CTD/2017-21/4_ctd-aggregated-files/2018_Aggregatedfiles.csv")
f2D <- substr(tst$File.Name, 1, 10)
f3D <- f2D
# for (i in 1:length (f2D)){
#   cat (i, "\n")
#   f3D [i] <- as.POSIXct.Date(f2D [i], tryFormats = c("%Y_&m-&d"))
# }

f2D <- as.POSIXct(f2D, tryFormats = c("%Y_%m-%d"))

tst$Date <- strftime(f2D,  format = "%m/%d/%Y")
write.csv (tst, file = "~/GISdata/LCI/CTD/2017-21/4_ctd-aggregated-files/2018_Aggregatedfiles2.csv"
  , row.names = FALSE, na = "")
