## SNOTEL snow data

## watch out: delete cache if new data does not plot

# rm(list = ls())
if(.Platform$OS.type == "unix") {
  setwd("~/Documents/amyfiles/NOAA/NOAA-LCI/")
} else {
  setwd("~/myDocs/amyfiles/NOAA-LCI/")
}
source("annualPlotFct.R")

currentYear <- as.numeric(format(Sys.Date(), "%Y")) - 1
snowsites <- c("mcneil canyon", "anchor river divide", "port graham"
  #,"middle fork bradley", "nuka glacier", "lower kachemak creek", "kachemak creek"
  ## the commented-out sites have only temperature data
  )


maO <- 31
qntl <- c(0.9) # , 0.8)
ongoingYear = TRUE
pastYear = FALSE


## delete cache if it's too old
# -- not yet implemented; do it manually
flushCache <- FALSE

#########################
## try interactive map:
# renv::install(c('DT', 'plotly', 'shinydashboard','leaflet'), prompt = FALSE)
if(0) {
  snotelr::snotel_explorer()
}
#########################


currentCol <- RColorBrewer::brewer.pal(3, "Blues")


# snotel_info(path = ".")

# Mcneil Canyon  1003
# Anchor River Divide 1062
# Lower Kachemak Creek 1265 # something bad??
# Middle Fork Bradley 1064  # nothing
# Nuka Glacier 1037         # nothing
# Port Graham 987
# kachemak creek 1063       # nothing

require(snotelr)




##############################
### single site treatments
##############################

snowCache <- "~/tmp/LCI_noaa/cache/snow/"
if(flushCache & dir.exists(snowCache)) {unlink(snowCache, recursive = TRUE)}
dir.create(snowCache, recursive = TRUE, showWarnings = FALSE)

if(file.exists(paste0(snowCache, "snow.rds"))) {
  x <- readRDS(paste0(snowCache, "snow.rds"))
} else {
  x <- try(snotel_info()) # site list, metadata -- for offline use
  if(class(x)[1] == "try-error") {
    stop("Get online to download snow data")
  } else {
    saveRDS(x, file = paste0(snowCache, "snow.rds"))
  }
}
if(0) {
  cat("\n\n# Local available snotel sites:\n")
  print(x [grep("Kenai", x$county), c(3:6,9,11)])
}


for(i in seq_along(snowsites)) {
  sitePickNo <- x$site_id [which(trimws(x$site_name) == snowsites[i])]
  # for(sitePickNo in c(1003, 1062, 987)){
  suppressWarnings(
    lC <- try(load(paste0(snowCache, sitePickNo, ".RData")), silent = TRUE)
  )
  if(class(lC)[1] != "try-error") {
    if(difftime(max(as.Date(snowMc$date), na.rm = TRUE), Sys.Date(), units = "days") < 7) {
      fetchNew <- FALSE
    } else {fetchNew <- TRUE}
  } else {fetchNew <- TRUE}

  if(fetchNew) {
    snowMc <- try(snotel_download(site = sitePickNo
      , path = snowCache
      , internal = TRUE
    ))
    if(class(snowMc) != "try-error") {
      siteN <- x$site_name [which(x$site_id == sitePickNo)]
      save(snowMc, siteN, file = paste0(snowCache, sitePickNo, ".RData"))
    }
  } else {
    load(paste0(snowCache, sitePickNo, ".RData"))
  }


  ## cleanup and helpers
  # fixGaps!?
  snowMc$datetimestamp <- as.POSIXct(snowMc$date)
  snowMc$jday <- as.numeric(format(snowMc$datetimestamp, "%j"))
  snowMc$year <- as.numeric(format(snowMc$datetimestamp, "%Y"))
  snowMc$month <- as.numeric(format(snowMc$datetimestamp, "%M"))

  # 1991 seems to be missing, with only one year before that
  snowMc <- subset(snowMc, datetimestamp > as.POSIXct("1990-01-01"))
  # snowMc <- fixGap(snowMc, intvl = 24 * 60 * 60)


  ## from example in ?toupper
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
      {s <- substring(s, 2); if(strict) tolower(s) else s},
      sep = "", collapse = " ")
    capO <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    gsub("Mcn", "McN", capO)
  }



  ## one site at a time
  if(length(levels(factor(snowMc$site_id))) > 1) {
    stop("work on only one site at a time!")
  }
  df <- prepDF(dat = snowMc, varName = "snow_water_equivalent", qntl = qntl, maO = maO
    , currentYear = currentYear
    #             , currentYear = 2012
  )


  ## plotting
  pdf(paste0("~/tmp/LCI_noaa/media/StateOfTheBay/sa-snowPack_",
    gsub(" ", "", siteN), ".pdf")
    , width = 9, height = 6)
  aPlot(df, "snow_water_equivalent", MA = FALSE, currentCol = currentCol
    , ylab = "snow-water equivalent [mm]"
    , ylim = c(0, max(snowMc$snow_water_equivalent, na.rm = TRUE))
    , ongoingYear = ongoingYear, pastYear = pastYear
  )
  title(main = capwords(siteN))
  ## add min and max years
  suppressWarnings(
    phy <- snotel_phenology(snowMc)
  )
  maxY <- phy$year [which.max(phy$max_swe)]
  minY <- phy$year [which.min(phy$max_swe)]
  lines(snow_water_equivalent ~ jday, subset(snowMc, year == maxY), lwd = 2
    , lty = "dashed", col = "black")
  lines(snow_water_equivalent ~ jday, subset(snowMc, year == minY), lwd = 2
        , lty = "dotted", col = "black")
  # yL <- levels(factor(snowMc$year))
  # for(i in 1:length(yL)){
  #   lines(snow_water_equivalent~jday, subset(snowMc, year == yL [i]), col = i)
  # }
  cLegend("topright"
    , mRange = c(min(snowMc$year, na.rm = TRUE), currentYear - 1)
    , currentYear = currentYear, cYcol = currentCol
    , qntl = qntl [1]
    , sYears = c(paste("max year:", maxY), paste("min year:", minY))
    , sLwd = c(2, 2)
    , sLty = c(2, 3)
    , sLcol = c("black", "black")
    , ongoingYear = ongoingYear, pastYear = pastYear
  )
  box()
  dev.off()

  # plot(snow_water_equivalent~timestamp, snowMc, type = "l")

  ### swe for 2019 -- where is that max value of 493??? XXX

  summary(phy)
  phy [which(phy$year == currentYear), ]
  rm(phy, minY, maxY)
}





###############################
## PCA of Kachemak Bay sites ##
###############################

sites <- x$site_id [which(trimws(x$site_name) %in% snowsites)]

snoS <- lapply(sites, function(x) {
  load(paste0("~/tmp/LCI_noaa/cache/snow/", x, ".RData"))
  return(snowMc)
}
)
snowMcDF <- do.call("rbind", snoS)

## reshape dataframe to have sites side by side for PCA
sDF <- with(snowMcDF, data.frame(site_name, date, snow_water_equivalent))
sDF$site_name <- trimws(sDF$site_name)
sDF$site_name <- gsub(" ", "_", sDF$site_name)
snowMc <- reshape(sDF, v.names = "snow_water_equivalent", idvar = "date"
  , timevar = "site_name", direction = "wide")
rm(snoS, snowMcDF)
names(snowMc) <- gsub("snow_water_equivalent.", "", names(snowMc))


## define helper variables
snowMc$datetimestamp <- as.POSIXct(snowMc$date)
snowMc$jday <- as.numeric(format(snowMc$datetimestamp, "%j"))
snowMc$year <- as.numeric(format(snowMc$datetimestamp, "%Y"))
snowMc$month <- as.numeric(format(snowMc$datetimestamp, "%M"))

# 1991 seems to be missing, with only one year before that
snowMc <- subset(snowMc, datetimestamp > as.POSIXct("1990-01-01"))
## impute somewhere here!
# Dray & Josse 2015: ipca is best way:
# require("missMDA")
# require("FactoMineR")
iDF <- missMDA::imputePCA(snowMc [, 2:(length(sites) + 1)], method = "EM", ncp = 2)

pdf("~/tmp/LCI_noaa/media/snowPCA.pdf")
snowPCA <- FactoMineR::PCA(iDF$completeObs, graph = TRUE, ncp = 2)
snowMc$PCA1 <- predict(snowPCA, iDF$completeObs)$coord [, 1]
dev.off()
unlink("~/tmp/LCI_noaa/media/snowPCA.pdf")
rm(snowPCA, iDF)

## turn this into a snotel df
snowMc$snow_water_equivalent <- snowMc$PCA1
snowMc$temperature_min <- 0 # place-holder dummy for snotel check
# XXXX ---- is this legit ????? XXXX
## ensure there are no negative values!
# snowMc$snow_water_equivalent <- ifelse(snowMc$snow_water_equivalent < 0
#                                         , 0, snowMc$snow_water_equivalent)
# snowMc$snow_water_equivalent <- snowMc$snow_water_equivalent - min(snowMc$snow_water_equivalent)
# XXXX ---- is this legit ????? XXXX
df <- prepDF(snowMc, varName = "snow_water_equivalent"
  , qntl = qntl[1], maO = maO, currentYear = currentYear)


pdf(paste0("~/tmp/LCI_noaa/media/StateOfTheBay/sa-snowPackPCA.pdf"), width = 9, height = 6)
aPlot(df, "snow_water_equivalent", MA = FALSE, currentCol = currentCol
  , ylab = "PCA1 of snow-water equivalent [mm]"
  , ylim = c(-1, max(snowMc$snow_water_equivalent))
  , ongoingYear = ongoingYear, pastYear = pastYear
)
## add min and max years
sMax <- aggregate(snow_water_equivalent ~ year, snowMc, FUN = max)
minY <- sMax$year [which.min(sMax$snow_water_equivalent)]
maxY <- sMax$year [which.max(sMax$snow_water_equivalent)]
lines(PCA1 ~ jday, subset(snowMc, year == minY), lwd = 2
  , lty = "dotted", col = "black")
lines(PCA1 ~ jday, subset(snowMc, year == maxY), lwd = 2
  , lty = "dashed", col = "black")
cLegend("topright"
  , mRange = c(min(snowMc$year), currentYear - 1)
  , currentYear = currentYear, cYcol = currentCol
  , qntl = qntl [1]
  , sYears = c(paste("max year:", maxY), paste("min year:", minY))
  , sLwd = c(2, 2)
  , sLty = c(2, 3)
  , sLcol = c("black", "black")
  , ongoingYear = ongoingYear, pastYear = pastYear
)
title(main = "Imputed PCA of 3 Kachemak Bay sites")
box()
dev.off()
rm(sMax, minY, maxY)


cat("Finished snowpack.R\n")
# EOF
