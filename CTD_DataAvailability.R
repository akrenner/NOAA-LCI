##

## recreate data availability matrix, as done by Jim
## show which transects have been sampled in which years/dates
## missing feature: consider making plot in two columns (or panels, one for each year?)
## current version: advantage that it can extend for many years to come


## to conform to single-page: plot in two columns
rm (list = ls())


##############
nColumns <- 2  # how many columns to plot. May need to adjust cex.axis
##############


load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## from CTD_cleanup.R: physOc, stn

## add time variables
phy <- physOc; rm (physOc)
phy$year <- factor (format (phy$isoTime, "%Y"))
## should ensure that length (levels (phy$year)) is even so that two columns are scalled equally

exY <- length (levels (phy$year))%%nColumns
if (exY !=0){  # balance length of columns
  levels (phy$year) <- c (levels (phy$year), max (as.numeric (levels (phy$year)))+exY)
}
rm (exY)

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
  xT [,i] / max (xT [,i], na.rm = TRUE)
})
dimnames(xTs) <- dimnames(xT)
xT <- xTs; rm (xTs)
xT <- ifelse (xT == 0, NA, xT)  # not sampled = white
xT <- xT [nrow (xT):1,]   # top = first samples




yL <- function (mx){
  ## construct lables for y-axis from rownames of matrix of from YYYY-MM
  yr <- factor (substr (row.names(mx), 1, 4))
  mt <- factor (1:12, ordered = TRUE)
  paste (as.character (sapply (1:length (levels (yr)), FUN = function (i){
    c(levels (yr)[i], rep ("", 11))}))
         , rep (levels (mt), length (levels (yr))))
}

xAxis <- function (side=3, ...){
  axis (side=side, at = ((1:ncol (xT))-1)/(ncol (xT)-1)
        , labels = colnames(xT), tick = FALSE, ...) # axis is 0:1
}

yAxis <- function (lab, mx, ...){
  axis (2, at = ((1:nrow (mx))-1)/(nrow (mx)-1)
        , labels = rev (lab)
        , tick = FALSE, cex.axis = 0.7 # any bigger and labels will skip
        , ...)
  # for (i in lab){axis (2, i)}  ## trick to force overlapping labels?
}



## two columns
## split matrix into two:
xYears <- as.factor (substr (row.names (xT), 1, 4))
cO <- ceiling (length (levels (xYears)) / nColumns) # should be guaranteed to be even anyway (see levels above)
xT1 <- xT [xYears %in% levels (xYears)[1:cO],]
xT2 <- xT [xYears %in% levels (xYears)[(cO+1):length (levels (xYears))],]


## generalize to nColumn
## split xT into list of length nColumn

if (nrow (xT) %% nColumns != 0){stop ("length of columns of xT is messed up")}
xTL <- lapply (1:nColumns, function (i){
  nR <- nrow (xT)/nColumns
  xT [(1+(i-1) * nR) : (i*nR),]
})





pdf ("~/tmp/LCI_noaa/media/CTDsections/availability.pdf", height = 11, width = 8.5)
par (mfrow = c(1,nColumns))
par (las = 1, mar = c(4,5,5,1))
for (i in length (xTL):1){
  image (t (xTL [[i]]), axes = FALSE)
  xAxis (side=3); xAxis (side=1)
  xL <- yL (xTL [[i]])
  yAxis (xL, xTL [[i]])
  box()
}
mtext ("Available CTD samples, Kachemak Bay and lower Cook Inlet", side = 3, outer = TRUE, line = -2)
dev.off()

## EOF