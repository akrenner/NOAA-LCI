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
save.image("~/tmp/LCI_noaa/cache/ctdAccessDBtables.RData")


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
