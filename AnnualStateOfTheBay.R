#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
if (.Platform$OS.type=="unix"){
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}else{
  setwd ("~/myDocs/amyfiles/NOAA-LCI/")
}


# Surf - legend appears to have 2020 and 2021 reversed.
# Precip - remove 2019 from legend
# Wave height - remove 2019, put 2021 back in.

rm (list = ls())
print (sT <- Sys.time())


quarterly <- TRUE
# quarterly <- FALSE

## State of the Bay Report 2019

## plot SWMP weather data for annual state of the bay report
source ("SeldoviaTemp.R")
source ("annual-wind.R")  ## qaqc.swmpr: no qaqc columns in input data

# source ("precipSalinity.R")  # calls the scripts below and makes a combined multi-panel PDF
source ("annual-waterTempSal.R")
source ("annual-airTemp.R")  # lots of warnings (min returning Inf -- fix this)
source ("annual-rainy.R")

source ("annual-snowpack.R")
source ("annual-waves.R")
# source ("annual-nutrients.R") # not working -- data is too sparce to fit into existing framework


cat ("Finished AnnualStateOfTheBay.R\n")
print (Sys.time())
# print (difftime(Sys.time(), sT, units = NULL)) ## not going to work here because rm (list = ls())

## EOF
