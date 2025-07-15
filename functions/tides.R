
## tide function to replace rtide
## see https://github.com/poissonconsulting/rtide/blob/main/R/tide-slack.R

## NOAA tide APIs https://api.tidesandcurrents.noaa.gov/api/prod/


## need to replace the following functions from rtide, as used in KBL scripts:
# annual-waves.R:  wDB$tideHght <- tide_height_data (timetable)$TideHeight
# dataSetup.R: tRange (tstmp), tide_height_data....


## base function to get tide data from NOAA server
fetchNOAAtideData <- function(year = as.numeric (format (Sys.Date(), "%Y")), station = 9455500) {
  # station=9455500  # Seldovia
  # station=9455517  # Kasitsna Bay

  yL <- levels (factor (year))
  fN <- paste0 ("~/tmp/LCI_noaa/cache/tides/tide_", station, ".RData")
  if (file.exists (fN)) {
    load (fN)
    nL <- tide$Date.Time |> format ("%Y") |> factor() |> levels()
    yL <- yL [-which (yL %in% nL)]; rm (nL)
  }
  if (length (yL) > 0) {
    for (i in seq_along(yL)) {
      begin_date = paste0 (yL[i], "0101")
      end_date = paste0 (yL[i], "1231")
      url <- paste0 ("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date="
        , begin_date, "&end_date=", end_date, "&station=", station,
        "&product=predictions&datum=MLLW&time_zone=gmt",
        "&interval=hilo&units=metric&application=DataAPI_Sample&format=csv")
      tideT <- read.csv (url)
      tideT$Date.Time <- as.POSIXct (tideT$Date.Time, tz = "GMT")
      if (!exists ("tide")) {
        tide <- tideT
      } else {
        tide <- rbind (tide, tideT)
      }
    }
  }
  dir.create("~/tmp/LCI_noaa/cache/tides", recursive = TRUE, showWarnings = FALSE)
  save (tide, file = fN)
  # write.csv (tide, file="~/tmp/LCI_noaa/cache/tides/tideCache.csv", row.names=FALSE)
  tide
}



## tRange -- from dataSetup.R
tRange <- function(tstmp, station = 9455500) {
  ## compute the difference in elevation between the nearest high and low tides
  ## at given time-stamp and station
  yrs <- format (tstmp, "%Y") |>
    as.character() |>
    unique() |>
    sort()
  tide <- fetchNOAAtideData (yrs, station)
  tide <- subset (tide, format (tide$Date.Time, "%Y") == yrs)

  ## faster alternative?
  if (0) {
    ## see https://adomingues.github.io/2015/09/24/finding-closest-element-to-a-number-in-a-list/
    # x <- tide$Date.Time
    # your.number <- tstmp
    require (data.table)
    # dt = data.table (x, val=x)
    # setattr(dt, "sorted", "x")
    # setkey (dt, x)
    # ## binary search and roll
    # dt[J(your.number), roll="nearest"]
    dt <- data.table (tide, )
  }

  range <- sapply (1:length (tstmp), FUN = function(i) {
    h <- subset (tide, Type == "H")
    H <- h$Prediction [which.min (abs (tstmp[i] - h$Date.Time))]
    l <- subset (tide, Type == "L")
    L <- l$Prediction [which.min (abs (tstmp[i] - l$Date.Time))]
    H - L
  })
  range
}




## the following functions are user-facing documented functions in rtide
# tide_datetimes(minutes=60L, from=as.Date("2015-01-01"), to=as.Date ("2015-12-31")
#                , tz="America/Los_Angeles"){
#   ## not really needed
# }

# tide_height(stations, minutes, from, to, tz){
tide_height <- function(DateTime, station = 9455500) {
  # , tz = "UTC", local time?

  ## timetable = data.frame with columns: Station, DateTime
  ## return: data.frame with TideHeight
  # if (class (timetable) != "data.frame"){stop ("timetable needs to be a data.frame")}
  # if (names (timetable) != c("Station", "DateTime")){stop ("check names of timetable")}
  tide <- fetchNOAAtideData(DateTime, station)
  ## interpolate from H and L values
  TideHeight <- spline (tide$Date.Time, tide$Prediction, xout = DateTime
    , method = "fmm", ties = "ordered")$y
  out <- data.frame(DateTime, TideHeight)
  out
}

tide_height_data <- function(data) {
  tide_height(data$DateTime, station = data$Station)
}

tide_slack_data <- function(data) {
  # data: data frame with columns Station and DateTime
}

# tide_stations(){
# provides harmonics
# }


## dependent functions that are most useful to the user
# tide_height <- function (station=9455517, from, to, minutes=60L, tz){
# }

tide_slack_date_time <- function(d, h, high = TRUE, forward = TRUE) {
  ## return timing of nearest slack time to date and station given
  tide <- fetchNOAAtideData(year, station = station)
}




if (0) {
  station = 9455500  # Seldovia
  # station=9455517  # Kasitsna Bay
  # yL <- levels (drift$year)

  yL <- levels (factor (2012:as.numeric (format (Sys.time(), "%Y"))))
  if (file.exists("~/tmp/LCI_noaa/cache/tides/tideCache.csv")) {
    tide <- read_csv ("~/tmp/LCI_noaa/cache/tides/tideCache.csv", show_col_types = FALSE)
    # provides date.time, level and slack-type (H/L)
    tide$Date.Time <- tide$Date.Time |> as.POSIXct(tz = "GMT")
    ## fetch only the missing ones
    nL <- tide$Date.Time |> format ("%Y") |> factor () |> levels()
    yL <- yL [-which (yL %in% nL)]; rm (nL)
  }

  if (length (yL) > 0) {
    for (i in seq_along(yL)) {
      begin_date = paste0 (yL [i], "0101")
      end_date = paste0 (yL [i], "1231")
      url <- paste0 ("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date="
        , begin_date, "&end_date=", end_date, "&station=", station,
        "&product=predictions&datum=MLLW&time_zone=gmt",
        "&interval=hilo&units=metric&application=DataAPI_Sample&format=csv")
      tideT <- read.csv (url)
      tideT$Date.Time <- as.POSIXct (tideT$Date.Time, tz = "GMT")
      if (!exists ("tide")) {
        tide <- tideT
      } else {
        tide <- rbind (tide, tideT)
      }
    }
    rm (tideT, station, end_date, begin_date, url, i)
    ## find closest date for each moment
    dir.create("~/tmp/LCI_noaa/cache/tides", recursive = TRUE, showWarnings = FALSE)
    write.csv (tide, file = "~/tmp/LCI_noaa/cache/tides/tideCache.csv", row.names = FALSE, append = TRUE)
  }
  rm (yL, station)
}


## example:
## dontrun
if (0) {
  ## find biggest tidal range in the year
  rTble <- data.frame (date = tide$Date.Time [2:nrow (tide)] - 6 * 3600
    , year = format (rTble$date, "%Y")
    , dT_s = diff (tide$Date.Time, units = "s")
    , range = diff (tide$Prediction))
  rTble$tSpeed <- rTble$range / as.numeric (rTble$dT)

  rTble [which.max (rTble$tSpeed), ]
  rTble [which.max (rTble$range), ]
  rTble [which.min (rTble$dT_s), ]

  aTide <- aggregate (tSpeed ~ year, data = rTble, FUN = max, subset = year > 2009)
  aTide$date <- as.POSIXct (nrow (aTide))
  aTide$range <- aggregate (range ~ year, data = rTble, FUN = max, subset = year > 2009)$range
  aTide$rDate <- as.POSIXct(nrow (aTide))

  for (i in 1:nrow (aTide)) {
    aTide$date  [i] <- rTble$date [which (rTble$tSpeed == aTide$tSpeed [i])]
    aTide$rDate [i] <- rTble$date [which (rTble$range == aTide$range [i])]
  }

  aTide
  summary (aTide)
}
