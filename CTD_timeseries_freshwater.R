## freshwater content

## compute time series of freshwater content by station and for all of T9, T4

rm(list = ls()); load("~/tmp/LCI_noaa/cache/CTDcasts.RData") # frm dataSetup.R
## need to get physOc.

physOc$freshwater <- ceiling(max(physOc$Salinity_PSU, na.rm=TRUE)) -
  physOc$Salinity_PSU

stationsL <- c("9_8", "9_6", "9_2", "4_2", "4_6", "4_7"
               , "AlongBay_3", "AlongBay_10"
               , "T9", "T4")
depth_layer <- c("surface", "deep", "all")
deepThd <- 20   ## deep vs surface layer -- copied from CTD_timeseries.R
dir_plot <- "~/tmp/LCI_noaa/media/freshwater/"
dir_data <- "~/tmp/LCI_noaa/data-products/freshwater/"
for (i in seq_along(c(dir_plot, dir_data))) {
  dir.create(c(dir_plot, dir_data)[i], showWarnings = FALSE, recursive = TRUE)
}



## combine stationsL and depth_layer, then loop over that aggregate
freshwater_ts <- function(stn, depth, data=physOc) {
  data$datetimestamp  <- factor (as.Date(data$isoTime)) ## to ensure same length of TSs
  data$datetimestamp  <- paste (format(data$isoTime, "%Y-%m"), "15", sep = "-") |>
    as.Date() |>
    as.factor()
  if (depth == "surface"){
    po_sample <- subset (data, Depth.saltwater..m. < deepThd)
  } else if (depth == "deep") {
    po_sample <- subset (data, Depth.saltwater..m. >= deepThd)
  } else if (depth == "all") {
    po_sample <- subset (data)
  } else {
    stop ("Unknown depth layer: ", depth)
  }
  if (stn %in% c("T9", "T4")) {
    if (stn == "T9") {
      po_stn <- subset(po_sample, Match_Name %in% paste0("9_", 1:10))
    } else if (stn == "T4") {
      po_stn <- subset(po_sample, Match_Name %in% paste0("4_", 1:10))
    }
    ## average over entire transect
    po_stn$DateISO <- rep (mean (po_stn$isoTime), nrow (po_stn))
  }else {
    po_stn <- subset(po_sample, Match_Name == stn)
    po_stn$DateISO <- po_stn$isoTime
  }
  ## define sample and aggregate
  po_agg <- aggregate(freshwater ~ datetimestamp , data = po_stn, FUN = mean
                      , na.rm = TRUE, drop=FALSE)
  po_agg$DateISO <- aggregate(isoTime ~ datetimestamp , data = po_stn, FUN = mean
                              , drop=FALSE)$isoTime
  po_agg$station <- stn
  po_agg$depth <- depth
  po_agg$cat <- paste0 (stn, "_", depth)
  po_agg$d_days <- c(0, diff(po_agg$DateISO, units="days"))
  # png (paste0(dir_plot, stn, "_", depth, ".png"), width = 800, height = 600)
  # dev.off()
  po_agg
}



samp_grid <- expand.grid(depth_layer, stationsL) |>
  setNames(c("depth", "station"))

if (0){
  require ("parallelly")
  sqs <- seq_len(nrow(samp_grid))
  pfct <- function(i) {
    freshwater_ts(samp_grid$station[i], samp_grid$depth[i], data=physOc)
  }
  if (.Platform$OS.type=="windows"){
    cl <- makeClusterPSOCK(detectCores() - 1)  # leave one core free
    clusterExport(cl, c("samp_grid", "physOc", "freshwater_ts", "pfct", "sqs"))
    freshL <- parLapply(sqs, pfct, cl)
    stopCluster(cl); rm (cl)  # stop parallel cluster
  }else{
    freshL <- mclapply(sqs, pfct, mc.cores = detectCores() - 1)
  }
}else{
  freshL <- lapply(seq_len(nrow(samp_grid)), function(i) {
    freshwater_ts(samp_grid$station[i], samp_grid$depth[i], data=physOc)
  })
}
rm (freshwater_ts)

freshM <- data.frame (freshL[[1]][,1]
                      ,do.call(cbind, lapply(seq_along(freshL), function (i) {
                        freshL[[i]]$freshwater
                      }))
)
names (freshM) <- c("datetimestamp ", do.call("paste", samp_grid))




save.image("~/tmp/LCI_noaa/cache-t/freshwater_ts.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache-t/freshwater_ts.RData")


require("ggplot2")
require("reshape2")
require ("dplyr")
require ("ggplot2")


get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}


## correlation matrix

## select single station/depth
dL <- depth_layer [1]  # "surface" or "deep" or "all"
dL <- "9_6"
dL <- "T9"
dL <- "T4"
freshAll <- freshM[,grep (dL, names(freshM))]
names (freshAll) <- gsub (dL, "", names (freshAll)) |>
  trimws()

## compare surface and deep
freshAll <- freshM [,-grep ("all", names (freshM))] |>
  select(-1)

## big matrix
freshAll <- freshM |>
  select(-1)  # remove date row
dL <- ""


cM <- cor(freshAll, use = "pairwise.complete.obs") |>
  #  get_upper_tri() |>
  as.data.frame() |>
  # select(-1) |>       # remove first column
  # slice(-n()) |>      # remove last row
  as.matrix()
cM <- ifelse(cM == 1, NA, cM) ## remove diagonale
# melt_cor <- reshape2::melt(cM)

pdf (paste0 (dir_plot, "fresh_correlations.pdf"), width=11, height=11)
reshape2::melt(cM) |>
  ggplot (aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = ifelse (is.na (value), ""
                                , sprintf("%.2f", round(value, 2))))) +
  scale_fill_gradient(low="yellow", high="red", na.value = "white") +
  # scale_fill_gradient2(low = "blue", mid="green", high = "yellow", na.value = "white",
  #                      midpoint=mean (melt_cor$value, na.rm=TRUE)) +
  theme_minimal() +
  labs(title=paste("Correlations of Freshwater Content by Station and depth"),
       x="", y="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_fill_viridis_d()
dev.off()

## T4: all are highly correlated, but especially deep and all (not much stratification)
## AlongBay_10: only here is there a significant decoupling between surface and
##   deep waters (r=0.64).
## AlongBay_10: biggest decoupling with 4_8 deep (r=0.50)
## recommendation: simply look at T9 all (or T9_6 all).
## Alternatively use 2: AlongBay_10 (9_2 has more data!) surface and 4_8 deep


## clean-up
rm (cM, coast, deepThd, bathyZ, depth_layer, dL, freshAll, freshM
    , get_upper_tri, i, poSS, stationsL, stn)

freshLng <- do.call(rbind, freshL)

## plot TS
pdf (paste0 (dir_plot, "timeseries_spaghetti.pdf"))
plot (freshwater ~ DateISO, type = "n", data = freshLng
      , xlab = "Date", ylab = "Freshwater content (PSU)"
      , main = "Freshwater content by station and depth layer")
for (i in seq_len (nrow (samp_grid))){
  lines (freshwater ~ DateISO, data = subset (freshLng,
                                              station == samp_grid$station[i] & depth == samp_grid$depth[i])
         , col=i, lwd=2)
}
dev.off()



save.image("~/tmp/LCI_noaa/cache-t/freshwater_ts2.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache-t/freshwater_ts2.RData")


require ("imputeTS")
require ("SWMPr")


## filter seasonal signal: mean and sd
## arima / loess filter. Also see SWMP package and SoB files
if(0) {
freshTS <- subset (freshLng, cat %in%
                     c("T9_all", "AlongBay_10_surface", "4_8_deep"))
}
## loess (or see what SWMPr does) -- seasonal mean and SD. -> anomaly


## univariate
## construct time series; and ensure that TS is fully populated
t9ts <- freshLng |>
  dplyr::filter(cat == "T9_all") |>
  # dplyr::filter(!is.na(freshwater)) |>
  # dplyr::select(c("datetimestamp", "freshwater")) |>
  dplyr::select(c("DateISO", "freshwater", "datetimestamp", "cat"))
ts1 <- expand.grid(month=stringr::str_pad(1:12, 2, pad="0", side="left"),
  year=2012:as.numeric(format(max(freshLng$DateISO, na.rm=TRUE), "%Y")))
tIdx <- match(paste(ts1$year, ts1$month, "15", sep = "-")
                           , t9ts$datetimestamp)
# fts <- ts (t9ts [tIdx, c("freshwater", "DateISO")]
#            , frequency = 12, start = c(2012,1)
# )
fts <- ts (t9ts [tIdx, c("freshwater")]
           , frequency = 12, start = c(2012,1)) |>
  imputeTS::na_seadec()
rm (t9ts, ts1, tIdx)


if (0){
## multivariate
## construct time series; and ensure that TS is fully populated
t9ts <- freshLng |>
  #  dplyr::filter(cat == "T9_all") |>
  dplyr::filter(cat == c("AlongBay_10_surface", "4_8_deep")) |>
  # dplyr::filter(!is.na(freshwater)) |>
  # dplyr::select(c("datetimestamp", "freshwater")) |>
  #  dplyr::select(c("DateISO", "freshwater", "datetimestamp", "cat")) |>
  dplyr::select(c("freshwater", "datetimestamp", "cat")) |>
  tidyr::pivot_wider(names_from="cat", values_from="freshwater")

ts1 <- expand.grid(month=str_pad(1:12, 2, pad="0", side="left"),
                   year=2012:as.numeric(format(max(freshLng$DateISO, na.rm=TRUE), "%Y")))
tIdx <- match(paste(ts1$year, ts1$month, "15", sep = "-")
              , t9ts$datetimestamp)



fts <- ts (t9ts [tIdx, c("freshwater", "DateISO")]
           , frequency = 12, start = c(2012,1)
)
fts <- ts (t9ts [tIdx, c("freshwater")]
           , frequency = 12, start = c(2012,1)) |>
  imputeTS::na_seadec()
rm (t9ts, ts1, tIdx)
}



pdf (paste0 (dir_plot, "freshwater_timeseries.pdf"))
spectrum(fts, method="ar")
spectrum(fts, method="pgram")

plot (decompose(fts, type="additive"))
dev.off()


# spectrum(fts, method="ar")


## make ts object with monthly frequency?


decomp(fts, param="freshwater")


## cross-correlation with precipitation and SWMP data
## temperature and rainfall from Homer Airport; Seldovia airport(?)

## fetch weather
## add this to annulPlotFct.R
if(!require("GSODR")) {
  renv::install("GSODR", repos="https://ropensci.r-universe.dev")
}
require("GSODR")
## has good wind data, including GUSTS and MAXSPD,
nb <- nearest_stations(LAT=59.6, LON=-151.5, distance=100)

yrs <- 2012:as.numeric(format(Sys.Date(), "%Y"))
## daily weather data from Seldovia airport, Homer airport, Homer spit, KBNERR
weather <- lapply (c("703621-25516", "703410-25507", "997176-99999"
  , "998167-99999"), function(i) {get_GSOD(years = yrs, station = i)
  })
# sldap   <- get_GSOD(years=yrs, station = "703621-25516") ## seldovia airport
# hmrap   <- get_GSOD(years=yrs, station = "703410-25507") ## homer airport
# hmrspt  <- get_GSOD(years=yrs, station = "997176-99999") ## Homer spit -- no PRCP
# hmrspt2 <- get_GSOD(years=yrs, station = "998167-99999") ## KBNERR

# lapply(seq_along(weather), function(i) {
#   summary(weather[[i]]$PRCP)
# })
rm (yrs)




### cross-correlation
## match times of CTD and weather -- by day

## prelim: probably not enough power to extract anything meaningful from CTD
## data. All correlations about 0.4-0.5. Try SWMP instead.


save.image("~/tmp/LCI_noaa/cache-t/freshwater_ts3.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache-t/freshwater_ts3.RData")



frsh <- subset (freshLng, subset = (station == "T9") & (depth == "all"))
frsh$datetimestamp <- as.Date(as.character(frsh$datetimestamp))


# wther = weather[[1]]; freshStn = frsh; ld = 30; wVar = "PRCP"; k = 31

corCalc <- function(wther, freshStn, ld = 0, k = 31, wVar = "PRCP"){
  ld <- as.integer(ld)
  ## moving average/sum of XX days prior to ocean measurement
  wther$ma <- wther |>
    dplyr::pull(wVar) |>
    data.table::frollmean(algo = "fast", align = "right", hasNA = TRUE, n = k,
      na.rm = TRUE) |>
    dplyr::lead(n = ld)  ## it's lag or lead?
  wther$fresh <- freshStn$freshwater[match(wther$YEARMODA,
    freshStn$datetimestamp)]
  # wther <- subset(wther, !is.na(fresh))
  cor(wther$ma, wther$fresh, use = "pairwise.complete.obs")
}

# corCalc (wther = weather[[1]], freshStn = frsh, lag = 0, wVar = "PRCP")

sapply (1:120, function (ld) {
  corCalc(weather[[1]], frsh, ld = ld, wVar = "PRCP", k = 31)
}) |>
  plot(type="l")

sapply (1:4, function (w){
  sapply (1:120, function (ld) {
    corCalc(weather[[w]], frsh, ld = ld, wVar = "TEMP", k = 31)
  }) |>
    max()
})



## using salinity as a tracer -- cross-correlations of AlongBay_15 with all
## all stations to see how fast freshwater is spreading
## map max lag

