##

## download updates for SWMP and NOAA weather stations
## save these as .RData file for further processing


## aggregate functionality from noaaWeather.R, annualPlotFct.R and annual-wind.R
## noaaWeather.R -- not updating homer airport beyond 2023-04-30 (station has
## taken down).


# still need to fix annual-wind.R to match this
# test annual-airTemp.R


## all NOAA stations
stations <- c ("")


##########################################################
## parameters to agree upon
metstation <- "kachomet"  # SWMP
# metstation <- "FILA2"     # Flat Island  -- something with subsets
# metstation <- "AUGA2"     # Augustine Island  --- subsets
# metstation <- "46105"     # 10 NM NW of east Amatuli  --- no data? in stmet only?
# metstation <- "AMAA2"       # East Amatuli, Barren
metstation <- "HMSA2"     # Homer Spit (starts in 2012) -- crash at gale pictogram
# metstation <- "PAHO"      # Homer Airport -- using riem package


wStations <- c("kachomet", "FILA2"
               # , "AUGA2" #, "46105"
               # , "AMAA2" #, "HMSA2"
)



## --------------define functions to fetch weather data -----------------------

source ("annualPlotFct.R")  ## pull these punctions into here since only used once?
# currently in annualPlotFct.R


## ---------------- execute functions and get data ----------------------------
sAir <- getSWMP (station="kachomet", QAQC=TRUE)

nAir <- getNOAAweather (stationID="PAHO", clearcache=FALSE)  ## function in annualPlotFct
nAiro <- getNOAA ("HMSA2", clearcache=FALSE)  ## function in annualPlotFct

nWave <- getNOAA(clearcache=FALSE)



## ------------clean up weather data and move to metric units ----------------

## match noaa to swmp data
hmr <- with (nAir, data.frame (datetimestamp = valid
                               , jday=as.numeric (format (valid, "%j"))
                               , year=as.numeric (format (valid, "%Y"))
                               , atemp=(tmpf-32)*5/9
                               , rh=relh
                               , bp=rep (is.na (nrow (nAir)))
                               , wspd=sknt * 0.5144444444 # convert knots to m/s
                               , maxwspd=peak_wind_gust * 0.5144444444
                               , wdir=drct # peak_wind_drct
                               # , wdir=drct
                               , sdwdir=rep (is.na (nrow (nAir)))
                               , totpar=rep (is.na (nrow (nAir)))
                               , totprcp=p01i  ## need to check on units!!
                               , totsorad=rep (is.na (nrow (nAir)))
))
rm (nAir)

## minimum NOAA stations
## accessible with package riem




## --------------- dump file for use by other scripts ------------------------
# hmr:  Homer weather
# hmr$atemp # air temperature (*C)
save (nWave, file="~/tmp/LCI_noaa/cache/annual-Wave.RData")
save (hmr, file="~/tmp/LCI_noaa/cache/annual-noaaAirWeather.RData")
save (hmr=sAir, file="~/tmp/LCI_noaa/cache/annual-SWMPAirWeather.RData")

# save.image ("~/tmp/LCI_noaa/cache/annual-AirWeather.RData")
save (nWave, hmr, sAir, file="~/tmp/LCI_noaa/cache/annual-AirWeather.RData")
## EOF
