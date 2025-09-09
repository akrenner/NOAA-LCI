## plot time series of bottom-temperature (for ADF&G)

## load CTD data
## extract T9_6



rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## get CTD data (physOc) directly from CTD_cleanup.R, rather than through dataSetup.R

ctd6 <- subset (physOc, Match_Name == "9_6")  ## deepest station in 9
ctd6 <- ctd6 [order (ctd6$isoTime), ]

## what's the maximal waterdepth commonly sampled?
botInfo <- aggregate (Depth.saltwater..m. ~ factor (isoTime), ctd6, FUN = max)
summary (botInfo$Depth.saltwater..m.)
quantile (botInfo$Depth.saltwater..m., 0.05)  ## pick 60 m


btm <- subset (ctd6, (58 > Depth.saltwater..m.) | (Depth.saltwater..m. > 62)) ## more robust than ask for 60m
btm <- subset (ctd6, Depth.saltwater..m. == 60)

# btm$YM <- paste0 (format (btm$isoTime, "%Y-%m"), "-15")
# btmA <- aggregate (Temperature_ITS90_DegC~factor (YM), data = btm, FUN=mean)
# btmA$isoTime <- as.POSIXct (btmA$`factor(YM)`)
# pdf ()
plot (Temperature_ITS90_DegC ~ isoTime, data = btm, type = "n"
  , ylab = "Temperature [°C]", xlab = "time"
  , main = "Station 9_6 at 60 m depth")
abline (h = seq (0, 16, by = 2), col = "blue", lty = "dashed")
abline (v = as.POSIXct (paste0 (seq (2012, 2040, by = 2), "-01-01")), col = "blue", lty = "dashed")
# lines (Temperature_ITS90_DegC ~ isoTime, data = btm, lwd = 2)
points (Temperature_ITS90_DegC ~ isoTime, data = btm, pch = 4)
sm <- smooth.spline (btm$isoTime, btm$Temperature_ITS90_DegC, spar = 0.005) # adjust spar
nT <- seq (min (btm$isoTime), max (btm$isoTime), length.out = 601)
pred <- predict (sm, as.numeric (nT))
# lines (nT, pred$y, col = "red")
lines (nT, pred$y, col = "black", lwd = 2)
# dev.off()

btmD <- subset (ctd6, Depth.saltwater..m. == 90)
# points (Temperature_ITS90_DegC ~ isoTime, btmD)
