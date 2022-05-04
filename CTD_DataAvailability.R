##

## recreate data availability matrix, as done by Jim
## show which transects have been sampled in which years/dates


# rm (list = ls())
load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## from CTD_cleanup.R: physOc, stn

## add time variables
phy <- physOc
phy$year <- factor (format (phy$isoTime, "%Y"))
phy$month <- factor (format (phy$isoTime, "%m"))
phy$Transect <- factor (ifelse (phy$Transect == "AlongBay", "AB", paste0 ("T", phy$Transect)))
phy$Station <- factor (phy$Station)

## transect by year and month
dAv <- aggregate (Station~Transect+month+year, data = phy
                  , function (x){length(levels (factor (x)))}
)
aC <- expand.grid (levels (dAv$month), levels (dAv$year))
dAv$sTime <- factor (paste (dAv$year, dAv$month, sep = "-")
                          , levels = paste (aC$Var2, aC$Var1, sep = "-")
); rm (aC)
# dAv <- aggregate (Station~Transect+sampleTime, data = phy
#                   , function (x){length(levels (factor (x)))}
# )
# dAv2 <- aggregate (Transect~sampleTime, data = dAv
#                    , function (x){length (x)})
xT <- xtabs(Station~sTime+Transect, dAv)

## data prep for image-plot
xTs <- sapply (1:ncol (xT), function (i){  # express transect as proportion sampled
  # scale (xT [,i], center = FALSE)
  xT [,i] / max (xT [,i], na.rm = TRUE)
})
dimnames(xTs) <- dimnames(xT)
xT <- xTs; rm (xTs)
xT <- ifelse (xT == 0, NA, xT)  # not sampled = white
xT <- xT [nrow (xT):1,]   # top = first samples

if (0){
  dAv2 <- xtabs(~year+month+Transect, phy)
  dAv2 <- xtabs(~month+Transect+year, phy)
  dAv2 <- xtabs(~year+month+Transect, phy)

  dAv <- aggregate (Station~Transect+month+year, data = phy
                    , function (x){length(levels (factor (x)))}
  )
  dAv2 <- aggregate (Transect~month+year, data = dAv
                     , function (x){length (x)})
  dAv2 <- xtabs(~year+month+Transect, phy)
  dAv2 <- xtabs(~month+Transect+year, phy)
  dAv2 <- xtabs(~year+month+Transect, phy)
}

pdf ("~/tmp/LCI_noaa/media/CTDsections/availability.pdf", height = 22, width = 8.5)
par (las = 1, mar = c(1,6,4,2))
image (t (xT), axes = FALSE)
axis (3, at = ((1:ncol (xT))-1)/(ncol (xT)-1), labels = colnames(xT)) # axis is 0:1
xL <- paste (as.character (sapply (1:length(levels (dAv$year)), FUN = function (i){c (levels (dAv$year)[i], rep ("", 11))}))
             , rep (levels (dAv$month), length (levels (dAv$year)))
)
# axis (2, at = ((1:nrow (xT))-1)/(nrow (xT)-1), labels = row.names(xT), tick = FALSE, cex = 0.8)
axis (2, at = ((1:nrow (xT))-1)/(nrow (xT)-1), labels = rev (xL), tick = FALSE, cex = 0.8)
box()
dev.off()
