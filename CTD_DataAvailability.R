##

## recreate data availability matrix, as done by Jim
## show which transects have been sampled in which years/dates
## current version: advantage that it can extend for many years to come

## bug: adjust cex.axis for x-axis if nColumns > 3, or force overlap


## to conform to single-page: plot in two columns
rm (list = ls())


##############
nColumns <- 2  # (1:4 make sense)
##############

las <- 1
if (nColumns == 1){CA <-0.4}else if (nColumns == 2){CA <- 0.7}else {CA <- 1}
if (nColumns > 3){Cx <- 1; las = 2}else{Cx <- 1}


base::load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## from CTD_cleanup.R: physOc, stn
# base::load ("~/tmp/LCI_noaa/cache/CTDcasts.RData")


## summary of casts per station
aggregate(format (isoTime, "%Y-%m-%d")~Match_Name, data=physOc, FUN=function (x){length (unique(x))})



## add time variables
phy <- physOc; rm (physOc)
phy$year <- factor (format (phy$isoTime, "%Y"))
## should ensure that length (levels (phy$year)) is even so that two columns are scalled equally

exY <- length (levels (phy$year))%%nColumns
if (exY !=0){  # balance length of columns
  levels (phy$year) <- seq (min (as.numeric (levels (phy$year)))
                            , max (as.numeric (levels (phy$year)))+(nColumns-exY))
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



## keep lables at normal size?
yL <- function (mx){  ##  BUG in here?
  ## construct labels for y-axis from rownames of matrix of from YYYY-MM
  yr <- factor (substr (row.names(mx), 1, 4))
  mt <- factor (1:12, ordered = TRUE)
  lbl <- paste (as.character (sapply (1:length (levels (yr)), FUN = function (i){
    c(levels (yr)[i], rep ("", 11))  ## label only first month with year
    }))
    , rep (levels (mt), length (levels (yr))))
  # lbl <- paste (as.character (sapply (1:length (levels (yr)), FUN = function (i){
  #   c("", levels (yr)[i], rep ("", 10))}))
  #   , rep (levels (mt), length (levels (yr))))
  lbl
}

xAxis <- function (side=3, ...){
  axis (side=side, at = ((1:ncol (xT))-1)/(ncol (xT)-1)
        , labels = colnames(xT), tick = FALSE, cex.axis = Cx, ...) # axis is 0:1
}

yAxis <- function (lab, mx, ...){
  axis (2, at = ((1:nrow (mx))-1)/(nrow (mx)-1)
        , labels = rev (lab)
        , tick = FALSE, cex.axis = CA # any bigger and labels will skip
        , ...)
  # for (i in lab){axis (2, i)}  ## trick to force overlapping labels?
}



## split into nColumn
## split xT into list of length nColumn

if (nrow (xT) %% nColumns != 0){stop ("length of columns of xT is messed up")}
xTL <- lapply (1:nColumns, function (i){
  nR <- nrow (xT)/nColumns
  xT [(1+(i-1) * nR) : (i*nR),]
})




dir.create("~/tmp/LCI_noaa/media/CTDsections/", showWarnings=FALSE, recursive=TRUE)
pdf ("~/tmp/LCI_noaa/media/CTDsections/availability.pdf", height = 11, width = 8.5)
par (mfrow = c(1,nColumns))
par (las = las, mar = c(4,5,5,1))
for (i in length (xTL):1){
  image (t (xTL [[i]]), axes = FALSE)
  xAxis (side=3); xAxis (side=1)
  xL <- yL (xTL [[i]])
  yAxis (xL, xTL [[i]])
  box()
}
mtext ("Available CTD samples, Kachemak Bay and lower Cook Inlet", side = 3, outer = TRUE, line = -2)
# dev.off()
rm (xL, xTL, las, i, Cx, yL)



## alternative, focus on month
# for each transect:
# month over years
# pdf ("~/tmp/LCI_noaa/media/CTDsections/dataAvail2.pdf", height = 11, width = 8.5)

nT <- length (levels (dAv$Transect))
# par (mfrow = c(nT,nColumns))
par (mfrow = c(nT/2,2), las = 1) #, mar = c())
for (i in 1:nT){
  dat <- subset (dAv, Transect == levels (dAv$Transect)[i])
  xT <- xtabs(Station~year+month, dat)
  if (rowSums(xT)[nrow (xT)] == 0){xT <- xT [-nrow (xT),]}
  is.na (xT)[xT == 0] <- TRUE
  xT <- xT / max (xT, na.rm = TRUE)
#  image (t (xT)[nrow (xT:1),], axes=FALSE)  # watch out to rotate xT correctly
  image (t (xT [nrow (xT):1,]), axes=FALSE)  # watch out to rotate xT correctly
  title (main = levels (dAv$Transect)[i], line = 2.5)
  yAxis (row.names(xT), xT)
  axis (3, at = ((1:12)-1)/(12-1), labels = 1:12 #month.abb
        , tick = FALSE)
  box()
}
dev.off()


rm (dat, dAv, i, nColumns, nT, xAxis, yAxis, xT, CA)






## check for duplicated surveys to look at tidal effects

## for each station: sort all surveys and calculate difference to next survey
## mark those within less than N = 7 days
if (0){
nD <- 7 *24  ## min time interval (days) between casts

smpl <- phy [,(which (names (phy) %in% c("Match_Name", "isoTime", "File.Name")))] |>
  unique ()
smpl <- smpl [which (!duplicated (smpl$File.Name)),]  ## there are still duplicated files XXX
smpl <- smpl [order (smpl$Match_Name, smpl$isoTime),]

dT <- diff (smpl$isoTime, units="hours") |>
  as.numeric()
dT <- ifelse (dT < 0, NA, dT)
dT <- ifelse (smpl$Match_Name [1:(nrow (smpl)-1)] == smpl$Match_Name [2:nrow(smpl)]
              , dT, NA)
smpl$dT <- c(NA, dT); rm (dT)

dups <- which (smpl$dT <= nD)
dups <- c (dups, dups -1) |>
  sort() |>
  unique()  # to avoid duplicates when >2 casts in nD-window
repl <- smpl [dups,]; rm (dups)
repl$dT <- ifelse (repl$dT > nD, NA, round (repl$dT, 1))
nrow(repl)
tail (repl, n=10)

write.csv (repl, file="~/tmp/LCI_noaa/data-products/replicatesCTD.csv"
           , row.names=FALSE, na="")

pdf ("~/tmp/LCI_noaa/data-products/replicateCDT-interval.pdf")
hist (repl$dT, main="interval between replicate casts", xlab="time [h]")
box()
dev.off()

rm (repl, nD, smpl)
}


## EOF
