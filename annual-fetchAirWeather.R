##

## download updates for SWMP and NOAA weather stations
## save these as .RData file for further processing


## aggregate functionality from noaaWeather.R, annualPlotFct.R and annual-wind.R
## noaaWeather.R -- not updating homer airport beyond 2023-04-30(station has
## taken down).


# still need to fix annual-wind.R to match this
# test annual-airTemp.R


## all NOAA stations
stations <- c("")
clearC = FALSE
# clearC=TRUE  ## if errors occur, try this


##########################################################
## parameters to agree upon
metstation <- "kachomet"  # SWMP
# metstation <- "FILA2"     # Flat Island  -- something with subsets
# metstation <- "AUGA2"     # Augustine Island  --- subsets
# metstation <- "46105"     # 10 NM NW of east Amatuli  --- no data? in stmet only?
# metstation <- "AMAA2"     # East Amatuli, Barren
metstation <- "HMSA2"       # Homer Spit(starts in 2012) -- crash at gale pictogram
# metstation <- "PAHO"      # Homer Airport -- using riem package


wStations <- c("kachomet", "FILA2"
  , "AUGA2", "46105"
  , "AMAA2", "HMSA2"
)
wStations <- c("HOMER AIRPORT", "HOMER SPIT"
  , "FLAT ISLAND LIGHT", "KACHEMAK BAY RESERVE"
  , "EAST AMATULI STATION LIGHT  AK", "AUGUSTINE ISLAND")


## weather stations above are largely moot, coming from rnoaa. Two options now:
## worldmet package or buoydata package. buoydata has not been updated recently,
## so worldmet seems like a safer bet for long-term stability.
## Issue pull-request for caching additions to worldmet package.




if(0) {
  ## list availabel buoy stations nearby
  ## less refined than worldmet, so only use this for waverider buoy!
  require(buoydata)
  buoydata::buoyDataWorld |>
    dplyr::filter(LAT > 58, LAT < 61) |>
    dplyr::filter(LON > -154, LON < -149) |>
    dplyr::filter(nYEARS >= 10)
}




## --------------define functions to fetch weather data -----------------------

source("annualPlotFct.R")  ## pull these functions into here since only used once?
# currently in annualPlotFct.R


## ---------------- execute functions and get data ----------------------------
sAir <- getSWMP(station = "kachomet", QAQC = TRUE)

nAir <- getNOAAweather_airports(stationID = "PAHO", clearcache = clearC)  ## function in annualPlotFct
nAiro <- getNOAA("HMSA2", clearcache = clearC)  ## function in annualPlotFct -- NDBC site

nWave <- try(getNOAA(buoyID = "46108", clearcache = clearC))
wave.46108 <- nWave



# weather.homer.spit <- getNOAAweather(stationID="xxx")

cF <- "~/tmp/LCI_noaa/cache/noaaWeather/worldmet/"

weatherL <- list(homer.airport = gNOAAS(station = "Homer Airport", clearcache = clearC, cacheF = cF, showsites = TRUE)  ## Homer Airport weather station)
  , homer.spit = gNOAAS(station = "Homer Spit", clearcache = clearC, cacheF = cF)  ## Homer Spit weather station
  , homer.spit2 = gNOAAS(station = "KACHEMAK BAY RESERVE", clearcache = clearC, cacheF = cF)  ## SWMP Homer Spit weather station
  , kachomet = sAir
  , augustine = gNOAAS(station = "Augustine Island", clearcache = clearC, cacheF = cF)  ## Augustine Island weather station
  , flat.island = gNOAAS(station = "Flat Island Light", clearcache = clearC, cacheF = cF)  ## Flat Island weather station
  , east.amatuli = gNOAAS(station = "East Amatuli Station Light  AK", clearcache = clearC, cacheF = cF)  ## East Amatuli weather station
)
weather.spit.buoy <- getNOAA(buoyID = "hmsa2", clearcache = clearC)   ## SWMP Homer Spit weather station



## ------------clean up weather data and move to metric units ----------------

## match noaa to swmp data -- move this into a annualPltFct.R function XX !
## move all to noaa buoy dataset, rather than airport/U of Iowa

hmr <- with(nAir, data.frame(datetimestamp = valid
  , jday = as.numeric(format(valid, "%j"))
  , year = as.numeric(format(valid, "%Y"))
  , atemp =(tmpf - 32) * 5 / 9
  , rh = relh
  , bp = rep(is.na(nrow(nAir)))
  , wspd = sknt * 0.5144444444 # convert knots to m/s
  , maxwspd = peak_wind_gust * 0.5144444444
  , wdir = drct # peak_wind_drct
  # , wdir=drct
  , sdwdir = rep(is.na(nrow(nAir)))
  , totpar = rep(is.na(nrow(nAir)))
  , totprcp = p01i * 25.4 ## need to check on units -- inches -> mm XXX
  , totsorad = rep(is.na(nrow(nAir)))
))
rm(nAir)



## minimum NOAA stations
## accessible with package riem




## --------------- dump file for use by other scripts ------------------------
# hmr:  Homer weather
# hmr$atemp # air temperature(*C)
save(nWave, file = "~/tmp/LCI_noaa/cache/annual-Wave.RData")
save(hmr, file = "~/tmp/LCI_noaa/cache/annual-noaaAirWeather.RData")
save(hmr = sAir, file = "~/tmp/LCI_noaa/cache/annual-SWMPAirWeather.RData")

save.image("~/tmp/LCI_noaa/cache/annual-AirWeather.RData")
# rm(list=ls()); load("~/tmp/LCI_noaa/cache/annual-AirWeather.RData")
save(nWave, hmr, sAir, weatherL, file = "~/tmp/LCI_noaa/cache/annual-AirWeather.RData")
## EOF
