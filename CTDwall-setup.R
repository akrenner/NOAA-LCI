## produce temp file for CTDwall to process further
## define color ramps
## define data ranges
## clean data(move to earlier??)


## issues:
## - fix O2perc scale across plots
## - fix all scales across plots??
## - add contours
## attenuation vs turbidity



## load data
## start with file from dataSetup.R -- better to get data directly from CTD processing? need to add only coastline + bathy
# rm(list = ls()); base::load("~/tmp/LCI_noaa/cache/CTDcasts-sf.RData")  # physOc and stn from dataSetup.R
rm(list = ls()); base::load("~/tmp/LCI_noaa/cache/CTDcasts.RData")  # physOc and stn from dataSetup.R
# rm(list = ls()); base::load("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")

if(length(grep("darwin", version$os)) > 0) {
  setwd("~/Documents/amyfiles/NOAA/NOAA-LCI/")
} else {
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}


## link physOc and stn
## should be poSS and stn -- check!


### data prep
## define sections
physOc$DateISO <- as.Date(format(physOc$isoTime, "%Y-%m-%d"))
physOc$Transect <- factor(physOc$Transect)
physOc$year <- factor(format(physOc$isoTime, "%Y"))
## combine CTD and station meta-data
physOc <- subset(physOc, !is.na(physOc$Transect)) ## who's slipping through the cracks??
## stn should be no longer needed -- see dataSetup.R
# physOc <- cbind(physOc, stn [match(physOc$Match_Name, stn$Match_Name)
#                               , which(names(stn) %in% c(# "Line",
#                                                           "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor(physOc$Match_Name)
# print(summary(physOc))




## get coastline and bathymetry
## bathymetry and coastline  --  they should come from dataSetup.R
require("ocedata") # coastlineWorldFine

## either collate PDF-pages on wall manualy, or piece things together using LaTeX
# or is there a way to put all together in R?? sounds complicated -- aim for solution #1?
#
# add flourescence to other variables. To do that, need to make section from oce-ctd object


# poAll <- subset(physOc, Station %in% as.character(1:12)) # cut out portgraham and pogi -- or translate -- keep them in!
poAll <- physOc
rm(physOc)
poAll$Transect <- factor(poAll$Transect)



## log transformations
slog <- function(x) {
  x <- suppressWarnings(log(x))
  x <- ifelse(is.infinite(x), NA, x)
  x
}


poAll$logPAR <- slog(poAll$PAR.Irradiance)
# poAll$logFluorescence <- slog(poAll$Fluorescence_mg_m3)
poAll$logTurbidity <- slog(poAll$turbidity)
rm(slog)




# save.image("~/tmp/LCI_noaa/cache-t/ctdwall0.RData")
# rm(list = ls()); load("~/tmp/LCI_noaa/cache-t/ctdwall0.RData")



##################### define surveys(by date-range) ##########################
# surveyW <- ifelse(duplicated(poAll$DateISO), NA, poAll$DateISO)
# poAll <- poAll [order(poAll$Transect, poAll$isoTime),]data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
# surveyT <- factor(with(poAll, paste(Transect,)))
#
# for(i in 1:length(levels(factor(poAll$Transect)))){
# }


poAll <- poAll [order(poAll$isoTime), ]
surveyW <- factor(poAll$DateISO)

## use nominal month and year from notebook -- eventually
for(h in 2:length(levels(surveyW))) {
  if(difftime(levels(surveyW)[h], levels(surveyW)[h - 1], units = "days") < 7) {
    surveyW [which(surveyW == levels(surveyW)[h])] <- levels(surveyW)[h - 1]
  }
}
surveyW <- factor(format(poAll$isoTime, "%Y-%m"))  ## KISS -- no more fudging of partial transects into the previous month; at least not for now

poAll$survey <- factor(surveyW)  ## need to reset factor levels after combining days
rm(surveyW, h)

## migrate code over from CTDwall.R:
## several surveys in one month
## several surveys on one day
## replicate station casts
## if two surveys in one month, asign early survey to previous month if that month is empty

# ## new, slow version -- but reliable?
# for(h in 2:length(surveyW)){
#   if(difftime(poAll$isoTime [h], poAll$isoTime [h-1], units = "days") < 7){
#     surveyW [h] <- surveyW [h-1]
#   }
# }
# poAll$survey <- factor(surveyW); rm(surveyW, h)

## check --- QAQC
if(0) {
  cat("Surveys window QAQC -- testing code\n")
  for(i in seq_along(levels(poAll$survey))) {
    x <- subset(poAll, survey == levels(poAll$survey)[i])
    if(1) {  # length(levels(factor(x$DataISO))) > 1){
      cat(i, levels(factor(x$DateISO)), "\n")
    }}
  rm(x, i)
}



################ define variables and their ranges #########################
# now in CTDsectionFcts.R --? no, need oRange in here and rest is dependend on it

oVars <- expression(Temperature ~ "[" * ""^o ~ C * "]"
  , Salinity ~ "[" * PSU * "]"
  , Density ~ "[" * sigma[theta] * "]"  # "sigmaTheta"  ## spell in Greek?
  , "Turbidity" # it's really turbidity/attenuation # , "logTurbidity"
  , Chlorophyll ~ "[" * mg ~ m^-3 * "]" # , "chlorophyll" #, "logFluorescence"
  # , "PAR"
  , log ~(PAR)
  , Oxygen ~ "[" * mu * mol ~ kg^-1 * "]"  # , "O2perc"  ## use bquote ?
  , Buoyancy ~ frequency ~ N^2 ~ "[" * s^-2 * "]"  # , "N^2[s^-2]"  # density gradient [Δσ/Δdepth]"# , expression(paste0(N^2, "[", s^-2, "]"))
)
oVarsF <- c("temperature"    # need diffrent name for oxygen to use in function
  , "salinity"
  , "sigmaTheta"
  , "turbidity" # , "logTurbidity"
  ,  "Chlorophyll_mg_m3" #"fluorescence" # , "chlorophyll" #, "logFluorescence"
  # , "PAR.Irradiance"
  , "logPAR"
  , "Oxygen_umol_kg"  # , "O2perc"
  , "bvf"
)

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
## move these into CTDsectionFcts.R -- or not?



########################
## define color ramps ##
########################

## ODV colors from https://theoceancode.netlify.app/post/odv_figures/
odv <- rev(c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa"))

require("cmocean")  ## for color ramps
options('cmocean-version' = "2.0") # fix colors to cmocean 2.0

## ColorRamp bias: default=1, positive number. Higher values give more widely spaced colors at the high end.
oCol3 <- list( ## fix versions?
  # colorRampPalette(oce::oceColorsTurbo(8), bias=0.5)
  temperature = oce::oceColorsTurbo  # colorRampPalette(cmocean("thermal")(10)
  , salinity = colorRampPalette(col = odv, bias = 0.3) # , colorRampPalette(cmocean("haline")(5), bias=0.7)  # cmocean("haline")
  , density = colorRampPalette(cmocean("dense")(5), bias = 0.3)
  , turbidity = colorRampPalette(cmocean("turbid")(5), bias = 3) # , cmocean("matter")  # or turbid
  , chlorophyll = colorRampPalette(cmocean("algae")(5), bias = 3)
  # , oceColorsTurbo # cmocean("solar")
  , logPAR = function(n) {require("viridis"); turbo(n, begin = 0.25, end = 0.8)}
  , oxygen = cmocean("oxy")
  , bvf = colorRampPalette(c("white", rev(cmocean("haline")(32)))) # for densityGradient
  , spice = cmocean("haline") # why is this here? should it be??
)
rm(odv)
## oceColorsTemperature and the likes are dated -- don't use them
##(stick to algorithmic pic of scale limits. Cleanups.)


oRange <- t(sapply(c("Temperature_ITS90_DegC"
  , "Salinity_PSU"
  , "Density_sigma.theta.kg.m.3"
  , "turbidity" # , "logTurbidity"
  , "Chlorophyll_mg_m3"
  , "logPAR"  # , "PAR.Irradiance"
  , "Oxygen_umol_kg"  # , "Oxygen_SBE.43..mg.l."  # change to umol.kg.! XXX
  , "bvf"
)
, FUN = function(vn) {range(poAll [, which(names(poAll) == vn)], na.rm = TRUE)
  # , FUN = function(vn){quantile(poAll [,which(names(poAll) == vn)], probs = c(0.01,0.99), na.rm = TRUE)
}))
## better to do this with colormap(S, breaks=...)? See https://www.clarkrichards.org/2016/04/25/making-section-plots-with-oce-and-imagep/

## manually tune some of these ranges
# oRange [1,1] <- 1.5 # fix min temperature
# oRange [2,1] <- 27 # fix min salinity  -- 28 about as high as one could go(observed: 20-32, quantile: 29-32)
# oRange [6,] <- c(-3,5)      # fix logPAR range
## what's better to use here, umol/kg or mg/l?
# oRange [7,] <- c(-0.1,1.5)  # fix O2 perc range
# oRange [7,] <- c(2,12)  # fix O2 conc range. Gulf of Mexico: low O2 = 5 and lower(down to 1-2 mg/L)
# https://repository.oceanbestpractices.org/bitstream/handle/11329/417/56281.pdf?sequence=1&isAllowed=y
## umol/l = 31.2512* cO2 mg/l
# oRange [7,] <- c(2,12) * 31.2512  ## this is messed up!
# if(length(oVars) != length(oCol)){stop("fix the code above: one color for each variable")}
oRange [which(row.names(oRange) == "bvf"), ] <- quantile(poAll$bvf, probs = c(0.01, 0.99), na.rm = TRUE)
###########################################################################

if(0) {
  hist(poAll$Temperature_ITS90_DegC)
  abline(v = oRange [1, ])
}


save.image("~/tmp/LCI_noaa/cache/ctdwallSetup.RData") # use this for CTDwall.R
# save(oRange, oCol, poAll, file="~/tmp/LCI_noaa/cache/ctdwallSetup.RData")
# rm(list = ls()); load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")
cat("\n\n             ### End of CTDwall-setup.R ###\n\n\n")
# EOF
