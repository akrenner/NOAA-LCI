## SNOTEL snow data

rm (list = ls())

setwd ("~/myDocs/amyfiles/NOAA-LCI/")
source ("annualPlotFct.R")
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
fetchNew <- TRUE  # get fresh data from server? takes longer
# fetchNew <- FALSE  # get fresh data from server? takes longer
maO <- 31
qntl <- c (0.9, 0.8)
currentCol <- c ("lightblue", "aquamarine")


# require ("snotelr")
# require ("plotly")
# require ("shinydashboard")
# snotel_explorer()

# snotel_info(path = ".")

# Mcneil Canyon  1003
# Anchor River Divide 1062
# Lower Kachemak Creek 1265 # something bad??
# Middle Fork Bradley 1064  # nothing
# Nuka Glacier 1037         # nothing
# Port Graham 987
# kachemak creek 1063       # nothing
##
# sitePickNo <- 1003
# sitePickNo <- 1062
# sitePickNo <- 987
##

require (snotelr)




##############################
### single site treatments
##############################


for (sitePickNo in c(1003, 1062, 987)){
  if (fetchNew){
    x <- snotel_info()
    x$lat <- x$latitude
    x$lon <- x$longitude

    xK <- x [grep ("Kenai", x$county),]
    xK [,c(3,9,11)]

    # require ("leaflet")
    # leaflet(data = xK) %>%
    #   addTiles() %>%
    #   addCircles()


    snowMc <- snotel_download (site = sitePickNo
                               , path = "~/tmp/LCI_noaa/cache/"
                               , internal = TRUE
    )
    siteN <- x$site_name [which (x$site_id == sitePickNo)]


    save (snowMc, siteN, file = paste0 ("~/tmp/LCI_noaa/cache/snow1_", sitePickNo, ".RData"))
  }else{
    load (paste0 ("~/tmp/LCI_noaa/cache/snow1_", sitePickNo, ".RData"))
  }


  ## cleanup and helpers
  # fixGaps!?
  snowMc$datetimestamp <- as.POSIXct(snowMc$date)
  snowMc$jday <- as.numeric (format (snowMc$datetimestamp, "%j"))
  snowMc$year <- as.numeric (format (snowMc$datetimestamp, "%Y"))
  snowMc$month <- as.numeric (format (snowMc$datetimestamp, "%M"))

  # 1991 seems to be missing, with only one year before that
  snowMc <- subset(snowMc, datetimestamp > as.POSIXct("1990-01-01"))
  # snowMc <- fixGap(snowMc, intvl = 24 * 60 * 60)


  ## from example in ?toupper
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    capO <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    gsub ("Mcn", "McN", capO)
  }



  ## one site at a time
  if (length (levels (factor (snowMc$site_id))) > 1){
    stop ("work on only one site at a time!")
  }
  df <- prepDF(dat = snowMc, varName = "snow_water_equivalent", qntl = qntl, maO = maO
               , currentYear = currentYear
               #             , currentYear = 2012
  )


  ## ColorBrewer for pretty colors

  ## plotting
  pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-snowPack", sitePickNo, ".pdf"), width = 9, height = 6)
  aPlot (df, "snow_water_equivalent", MA = FALSE, currentCol = currentCol #"lightblue"
         , ylab = "snow-water equivalent [mm]"
         , ylim = c (0, max (snowMc$snow_water_equivalent, na.rm = TRUE))
  )

  ## add min and max years
  phy <- snotel_phenology(snowMc)
  minY <- phy$year [which.min (phy$max_swe)]
  maxY <- phy$year [which.max (phy$max_swe)]
  lines (snow_water_equivalent~jday, subset (snowMc, year == minY), lwd = 2
         , lty = "dotted", col = "darkblue")
  lines (snow_water_equivalent~jday, subset (snowMc, year == maxY), lwd = 2
         , lty = "dashed", col = "black")
  # yL <- levels (factor (snowMc$year))
  # for (i in 1:length (yL)){
  #   lines (snow_water_equivalent~jday, subset (snowMc, year == yL [i]), col = i)
  # }

  cLegend ("topright"
           , mRange = c (min (snowMc$year, na.rm = TRUE), currentYear -1)
           , currentYear = currentYear, cYcol = currentCol #"lightblue"
           , qntl = qntl [1]
           , sYears = c (paste ("max year:", minY), paste ("min year:", maxY))
           , sLwd = c(2, 2)
           , sLty = c(2, 3)
           , sLcol = c("black", "darkblue")
  )

  title (main = capwords (siteN))
  dev.off()
  rm (phy, minY, maxY)


  # plot (snow_water_equivalent~timestamp, snowMc, type = "l")

  ### swe for 2019 -- where is that max value of 493??? XXX


  phy <- snotel_phenology(snowMc)
  summary (phy)
  phy [which (phy$year == currentYear),]
}





###############################
## PCA of Kachemak Bay sites ##
###############################

sites <- c(1003, 1062, 987)
snoS <- lapply (sites, function (x){
  load (paste0 ("~/tmp/LCI_noaa/cache/snow1_", x, ".RData"))
  return (snowMc)
}
)
snowMcDF <- do.call ("rbind", snoS)

## reshape dataframe to have sites side by side for PCA
sDF <- with (snowMcDF, data.frame (site_name, date, snow_water_equivalent))
sDF$site_name <- trimws (sDF$site_name)
sDF$site_name <- gsub (" ", "_", sDF$site_name)
snowMc <- reshape (sDF, v.names = "snow_water_equivalent", idvar = "date"
                   , timevar = "site_name", direction = "wide")
rm (snoS, snowMcDF)
names (snowMc) <- gsub ("snow_water_equivalent.", "", names (snowMc))


## define helper variables
snowMc$datetimestamp <- as.POSIXct(snowMc$date)
snowMc$jday <- as.numeric (format (snowMc$datetimestamp, "%j"))
snowMc$year <- as.numeric (format (snowMc$datetimestamp, "%Y"))
snowMc$month <- as.numeric (format (snowMc$datetimestamp, "%M"))

# 1991 seems to be missing, with only one year before that
snowMc <- subset(snowMc, datetimestamp > as.POSIXct("1990-01-01"))


## impute somewhere here!
# Dray & Josse 2015: ipca is best way:  library (missMDA)
require ("missMDA")
iDF <- imputePCA (snowMc [,2:(length (sites)+1)], method = "EM", ncp =2)
require ("FactoMineR")
pdf ("~/tmp/LCI_noaa/media/snowPCA.pdf")
snowPCA <- PCA (iDF$completeObs, graph = TRUE, ncp = 2)
dev.off()
snowMc$PCA1 <- predict (snowPCA, iDF$completeObs)$coord [,1]


rm (snowPCA, iDF)

## turn this into a snotel df
snowMc$snow_water_equivalent <- snowMc$PCA1
snowMc$temperature_min <- 0 # place-holder dummy for snotel check
# XXXX ---- is this legit ????? XXXX
## ensure there are no negative values!
# snowMc$snow_water_equivalent <- ifelse (snowMc$snow_water_equivalent < 0
#                                         , 0, snowMc$snow_water_equivalent)
# snowMc$snow_water_equivalent <- snowMc$snow_water_equivalent - min (snowMc$snow_water_equivalent)
# XXXX ---- is this legit ????? XXXX
df <- prepDF (snowMc, varName = "snow_water_equivalent"
              , qntl = qntl[1], maO = maO, currentYear = currentYear)


pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-snowPackPCA.pdf"), width = 9, height = 6)
aPlot (df, "snow_water_equivalent", MA = FALSE, currentCol = currentCol
       , ylab = "PCA1 of snow-water equivalent [mm]"
       , ylim = c(-1, max (snowMc$snow_water_equivalent))
)
## add min and max years
# phy <- snotel_phenology(snowMc)
# minY <- phy$year [which.min (phy$max_swe)]
# maxY <- phy$year [which.max (phy$max_swe)]
sMax <- aggregate (snow_water_equivalent~year, snowMc, FUN = max)
minY <- sMax$year [which.min (sMax$snow_water_equivalent)]
maxY <- sMax$year [which.max (sMax$snow_water_equivalent)]
lines (PCA1~jday, subset (snowMc, year == minY), lwd = 2
       , lty = "dotted", col = "darkblue")
lines (PCA1~jday, subset (snowMc, year == maxY), lwd = 2
       , lty = "dashed", col = "black")
cLegend ("topright"
         , mRange = c(min (snowMc$year), currentYear - 1)
         , currentYear = currentYear, cYcol = currentCol
         , qntl = qntl [1]
         , sYears = c (paste ("max year:", minY), paste ("min year:", maxY))
         , sLwd = c(2, 2)
         , sLty = c(2, 3)
         , sLcol = c("black", "darkblue")
)
title (main = "Imputed PCA of 3 Kachemak Bay sites")
dev.off()
rm (sMax, minY, maxY)






cat ("Finished snowpack.R\n")
# EOF