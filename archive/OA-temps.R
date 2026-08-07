## find temperatures (and salinity) for OA samples

rm (list=ls())
oa <- read.csv ('~/GISdata/LCI/Archive/OA_2022.01.31-2022.12.6_kbnerr.csv')
load ("~/tmp/LCI_noaa/cache/CTDcasts.RData") # from dataSetup.R



## prep data, then merge
oa$comments.notes <- ""
oa$isodate <- as.Date(oa$date, format="%d-%b-%y")
MN <- gsub ("AB", "AlongBay_", oa$sample)
MN <- gsub ("T6", "6_", MN)
MN <- gsub ("T9", "9_", MN)
MN <- gsub ("_0", "_", MN)
## flex poSS transect
oa$Match_Name <- gsub ("9_6", "AlongBay_6", MN)
poSS$Match_Name <- ifelse (poSS$Match_Name=="9_6", "AlongBay_6", poSS$Match_Name)
# levels (factor (poSS$Match_Name))

## matching
oasampleID <- paste (oa$Match_Name, oa$isodate, ifelse (oa$depth > 1, "B", "S"))
## bottom
poSSSampleID <- paste (poSS$Match_Name, poSS$Date, "B")
oa$temperature <- poSS$TempBottom [match (oasampleID, poSSSampleID)]
oa$salinity <- poSS$SalBottom [match (oasampleID, poSSSampleID)]
## surface
poSSSampleID <- paste (poSS$Match_Name, poSS$Date, "S")
oa$temperature <- ifelse (is.na (oa$temperature)
                          ,  poSS$TempSurface [match (oasampleID, poSSSampleID)]
                          , oa$temperature)
oa$salinity <- ifelse (is.na (oa$salinity)
                       , poSS$SalSurface [match (oasampleID, poSSSampleID)]
                       , oa$salinity)
rm (MN, oasampleID, poSSSampleID)
oa


write.csv(oa, file="C:/Users/Martin.Renner/Desktop/OA_2022.01.31-2022.12.6_kbnerr_MR.csv", row.names=FALSE, quote=FALSE)
# EOF
