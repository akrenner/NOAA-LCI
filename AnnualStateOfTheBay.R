#! /usr/bin/env Rscript

# ## execute all Kachemak Bay/Cook Inlet scripts
if(length(grep("NOAA-LCI", getwd())) < 1) {
  stop("Please open the R Project in NOAA-LCI/")
}


# Surf - legend appears to have 2020 and 2021 reversed.
# Precip - remove 2019 from legend
# Wave height - remove 2019, put 2021 back in.

rm(list = ls())
print(sT <- Sys.time())


quarterly <- TRUE
# quarterly <- FALSE

## State of the Bay Report 2019

## plot SWMP weather data for annual state of the bay report

## get weather data -- SWMP and NOAA
source("annual-fetchAirWeather.R")
source("SeldoviaTemp.R") ## fetch SWMP water data

## plot seasonal means and current/previous year
source("annual-waterTempSal.R")
source("annual-wind.R")  ## qaqc.swmpr: no qaqc columns in input data
# source("precipSalinity.R")  # calls the scripts below and makes a combined multi-panel PDF
source("annual-airTemp.R")  # lots of warnings (min returning Inf -- fix this)
source("annual-rainy.R")
source("annual-snowpack.R")
try(source("annual-waves.R"))  # issues with access to bouy data again
# source("annual-nutrients.R") # not working -- data is too sparce to fit into existing framework


cat("Finished AnnualStateOfTheBay.R\n")
print(Sys.time())

## EOF
