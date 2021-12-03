## test Access access using RODBC

## run this in 32-bit version of R (until updated)
## source ()
## export tables, process further elsewhere


# require ("odbc")
# require ("DBI")
# con <- dbConnect (odbc::odbc(), driver = "Access Driver", database = "EVOS")


# require ("RODBC")
# myconn <- odbcConnect ("~/GISdata/LCI/EVOS_LTM.accdb")
# myconn <- odbcConnect ("C:/Users/Martin.Renner/Documents/GISdata/LCI/EVOS_LTM.accdb")
#
#
# myconn <- odbcConnect ("EVOS")


require ("RODBC")
db <- odbcConnectAccess2007(access.file = 'C:/Users/Martin.Renner/Documents/GISdata/LCI/EVOS_LTM.accdb')
odbcGetInfo(db)
# x <- sqlTables (db)
taT <- sqlFetch (db, sqtable = "tblAllTransectsCTD", colnames = FALSE)
tC <- sqlFetch (db, sqtable = "tblCTDall", colnames = FALSE)
tSe <- sqlFetch (db, sqtable = "tblStationEvent", colnames = FALSE)
tSS <- sqlFetch (db, sqtable = "tblStationSpecies", colnames = FALSE)
tTE <- sqlFetch (db, sqtable = "tblTransectEvent", colnames = FALSE)
tZ <- sqlFetch (db, sqtable = "tblZooplankton", colnames = FALSE)
# sqlQuery
odbcClose (db)
rm (db)

## combine degrees and minutes for QGIS
tSe$LonDD <- tSe$LongitudeDeg - tSe$LongitudeMins/60
tSe$LatDD <- tSe$LatitudeDeg + tSe$LatitudeMins/60

## treat bad data
tSe$LonDD <- ifelse (tSe$LonDD == 0, NA, tSe$LonDD)
tSe$LatDD <- ifelse (tSe$LatDD == 0, NA, tSe$LatDD)
tSe$LonDD <- ifelse (is.na (tSe$LatDD), NA, tSe$LonDD)
tSe$LatDD <- ifelse (is.na (tSe$LonDD), NA, tSe$LatDD)

save.image("~/tmp/LCI_noaa/cache/ctdAccessDBtables.RData")

write.csv (taT, "~/GISdata/LCI/EVOS_LTM_tables/tblAllTransectsCTD.csv", row.names = FALSE)
# write.csv (tC, "~/GISdata/LCI/EVOS_LTM_tables/tblCTDall.csv", row.names = FALSE)
write.csv (tSe, "~/GISdata/LCI/EVOS_LTM_tables/tblStationEvent.csv", row.names = FALSE)
write.csv (tTE, "~/GISdata/LCI/EVOS_LTM_tables/tblTransectEvent.csv", row.names = FALSE)
tSe <- subset (tSe, !is.na (tSe$LonDD)) # just for QGIS
write.csv (tSe, "~/GISdata/LCI/EVOS_LTM_tables/tblStationEventQGIS.csv", row.names = FALSE)

# db <- odbcConnectAccess(access.file = 'C:/Users/Martin.Renner/Documents/GISdata/LCI/EVOS_LTM.accdb')
#
#
# DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
# MDBPATH <- "C:/Users/Martin.Renner/Documents/GISdata/LCI/EVOS_LTM.accdb"
# PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
# channel <- odbcDriverConnect (PATH)
#
# close (channel)
#
#
#
#
# dsn <- paste0("Driver={Microsoft Access Driver (*.mdb)};Dbq=",
#               #"Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
#               file, ";Uid=;Pwd=;")
# dsn <- paste0("Driver={Microsoft Access Driver (*.mdb)};Dbq=",
#               #"Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
#               file, ";Uid=;Pwd=;")
#
