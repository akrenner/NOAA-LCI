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
fori in seq_along(c(dir_plot, dir_data))) {
  dir.create(c(dir_plot, dir_data)[i], showWarnings = FALSE, recursive = TRUE)
}



## combine stationsL and depth_layer, then loop over that aggregate
freshwater_ts <- function(stn, depth, data=physOc) {
  data$datetimestamp  <- factoras.Date(data$isoTime)) ## to ensure same length of TSs
  data$datetimestamp  <- pasteformat(data$isoTime, "%Y-%m"), "15", sep = "-") |>
    as.Date() |>
    as.factor()
  ifdepth == "surface"){
    po_sample <- subsetdata, Depth.saltwater..m. < deepThd)
  } else ifdepth == "deep") {
    po_sample <- subsetdata, Depth.saltwater..m. >= deepThd)
  } else ifdepth == "all") {
    po_sample <- subsetdata)
  } else {
    stop"Unknown depth layer: ", depth)
  }
  ifstn %in% c("T9", "T4")) {
    ifstn == "T9") {
      po_stn <- subset(po_sample, Match_Name %in% paste0("9_", 1:10))
    } else ifstn == "T4") {
      po_stn <- subset(po_sample, Match_Name %in% paste0("4_", 1:10))
    }
    ## average over entire transect
    po_stn$DateISO <- repmeanpo_stn$isoTime), nrowpo_stn))
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
  po_agg$cat <- paste0stn, "_", depth)
  po_agg$d_days <- c(0, diff(po_agg$DateISO, units="days"))
  # pngpaste0(dir_plot, stn, "_", depth, ".png"), width = 800, height = 600)
  # dev.off()
  po_agg
}



samp_grid <- expand.grid(depth_layer, stationsL) |>
  setNames(c("depth", "station"))

if(0) {
  require("parallel")
  sqs <- seq_len(nrow(samp_grid))
  pfct <- function(i) {
    freshwater_ts(samp_grid$station[i], samp_grid$depth[i], data=physOc)
  }
  if(.Platform$OS.type == "windows") {
    cl <- makeCluster(detectCores() - 1, type="PSOCK")  # leave one core free
    clusterExport(cl, c("samp_grid", "physOc", "freshwater_ts", "pfct", "sqs"))
    freshL <- parLapply(sqs, pfct, cl)
    stopCluster(cl); rmcl)  # stop parallel cluster
  }else{
    freshL <- mclapply(sqs, pfct, mc.cores = detectCores() - 1)
  }
}else{
  freshL <- lapply(seq_len(nrow(samp_grid)), function(i) {
    freshwater_ts(samp_grid$station[i], samp_grid$depth[i], data=physOc)
  })
}
rmfreshwater_ts)

freshM <- data.frame(freshL[[1]][,1]
                      ,do.call(cbind, lapply(seq_along(freshL), functioni) {
                        freshL[[i]]$freshwater
                      }))
)
namesfreshM) <- c("datetimestamp ", do.call("paste", samp_grid))




save.image("~/tmp/LCI_noaa/cache-t/freshwater_ts.RData")
# rmlist=ls()); load"~/tmp/LCI_noaa/cache-t/freshwater_ts.RData")


require("ggplot2")
require("reshape2")
require"dplyr")
require"ggplot2")


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
freshAll <- freshM[,grepdL, names(freshM))]
namesfreshAll) <- gsubdL, "", namesfreshAll)) |>
  trimws()

## compare surface and deep
freshAll <- freshM [,-grep"all", namesfreshM))] |>
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

pdfpaste0dir_plot, "fresh_correlations.pdf"), width=11, height=11)
reshape2::melt(cM) |>
  ggplotaes(x=Var1, y=Var2, fill=value)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = ifelseis.navalue), ""
                                , sprintf("%.2f", round(value, 2))))) +
  scale_fill_gradient(low="yellow", high="red", na.value = "white") +
  # scale_fill_gradient2(low = "blue", mid="green", high = "yellow", na.value = "white",
  #                      midpoint=meanmelt_cor$value, na.rm=TRUE)) +
  theme_minimal() +
  labs(title=paste("Correlations of Freshwater Content by Station and depth"),
       x="", y="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_fill_viridis_d()
dev.off()

## T4: all are highly correlated, but especially deep and allnot much stratification)
## AlongBay_10: only here is there a significant decoupling between surface and
##   deep watersr=0.64).
## AlongBay_10: biggest decoupling with 4_8 deepr=0.50)
## recommendation: simply look at T9 allor T9_6 all).
## Alternatively use 2: AlongBay_109_2 has more data!) surface and 4_8 deep


## clean-up
rm(cM, coast, deepThd, bathyZ, depth_layer, dL, freshAll, freshM
    , get_upper_tri, i, poSS, stationsL, stn)

freshLng <- do.call(rbind, freshL)

## plot TS
pdf(paste0dir_plot, "timeseries_spaghetti.pdf"))
plot(freshwater ~ DateISO, type = "n", data = freshLng
      , xlab = "Date", ylab = "Freshwater content"
      , main = "Freshwater content by station and depth layer")
for(i in seq_lennrowsamp_grid))){
  lines(freshwater ~ DateISO, data = subset(freshLng,
    station == samp_grid$station[i] & depth == samp_grid$depth[i]),
    col=i, lwd=2)
}
for(i in seq_along(levels(freshLng$station))) {
  subD <- subset(freshLng, station == levels(freshLng$station)[i])
  plotfreshwater ~ DateISO, type="n", data = freshLng, xlab = "Date",
    ylab = "Freshwater content",
    main = paste0("Station ", levels(freshLng$station)[i]))
  par(lwd=3)
  lines(freshwater ~ DateISO, data = subset(subD, depth=="deep"), col = "blue")
  lines(freshwater ~ DateISO, data = subset(subD, depth=="surface"), col = "green")
  legend("topright", col = c("green", "blue"), legend = c("surface", "deep")
         , lwd = 3)
}
dev.off()



save.image("~/tmp/LCI_noaa/cache-t/freshwater_ts2.RData")
# rmlist=ls()); load"~/tmp/LCI_noaa/cache-t/freshwater_ts2.RData")


require"imputeTS")
require"SWMPr")


## filter seasonal signal: mean and sd
## arima / loess filter. Also see SWMP package and SoB files
if(0) {
freshTS <- subsetfreshLng, cat %in%
                     c("T9_all", "AlongBay_10_surface", "4_8_deep"))
}
## loessor see what SWMPr does) -- seasonal mean and SD. -> anomaly


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
# fts <- tst9ts [tIdx, c("freshwater", "DateISO")]
#            , frequency = 12, start = c(2012,1)
# )
fts <- tst9ts [tIdx, c("freshwater")]
           , frequency = 12, start = c(2012,1)) |>
  imputeTS::na_seadec()
rmt9ts, ts1, tIdx)


if0){
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



fts <- tst9ts [tIdx, c("freshwater", "DateISO")]
           , frequency = 12, start = c(2012,1)
)
fts <- tst9ts [tIdx, c("freshwater")]
           , frequency = 12, start = c(2012,1)) |>
  imputeTS::na_seadec()
rmt9ts, ts1, tIdx)
}



pdfpaste0dir_plot, "freshwater_timeseries.pdf"))
spectrum(fts, method="ar")
spectrum(fts, method="pgram")
plotdecompose(fts, type="additive"))
dev.off()


# spectrum(fts, method="ar")


## make ts object with monthly frequency?
# decomp(fts, param="freshwater")


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

## Sitka
nb <- nearest_stations(LAT=57.05, LON=-135.3, distance=50)
# nb

yrs <- 2012:as.numeric(format(Sys.Date(), "%Y"))
## daily weather data from Seldovia airport, Homer airport, Homer spit, KBNERR, Sitka
weather <- lapplyc("703621-25516", "703410-25507", "997176-99999"
  , "998167-99999", "703710-25333"), function(i) {get_GSOD(years = yrs, station = i)
  })

names(weather) <- c("Seldovia Airport", "Homer Airport", "Homer Spit"
                     , "KBNERR", "Sitka Airport")
# lapply(seq_along(weather), function(i) {
#   summary(weather[[i]]$PRCP)
# })
rmyrs)





##########################
### cross-correlatiosn ###
##########################

## match times of CTD and weather -- by day

## prelim: probably not enough power to extract anything meaningful from CTD
## data. All correlations about 0.4-0.5. Try SWMP instead.


save.image("~/tmp/LCI_noaa/cache-t/freshwater_ts3.RData")
# rmlist=ls()); load"~/tmp/LCI_noaa/cache-t/freshwater_ts3.RData")



frsh <- subsetfreshLng, subset =station == "T9") &depth == "all"))
frsh$datetimestamp <- as.Date(as.character(frsh$datetimestamp))


# wther = weather[[1]]; freshStn = frsh; ld = 30; wVar = "PRCP"; k = 31
# wther = weather[[1]]; freshStn = frsh; ld = 30; wVar = "TEMP"; k = 31

corCalc <- function(wther, freshStn, ld = 0, k = 31, wVar = "PRCP", CI=FALSE){
  ld <- as.integer(ld)
  ## moving average/sum of XX days prior to ocean measurement
  wther$ma <- wther |>
    dplyr::pull(wVar) |>
    data.table::frollmean(algo = "fast", align = "center", hasNA = TRUE, n = k,
      na.rm = TRUE) |>
    dplyr::lead(n = ld)  ## it's lag or lead?
  wther$fresh <- freshStn$freshwater[match(wther$YEARMODA,
    freshStn$datetimestamp)]
  # wther <- subset(wther, !is.na(fresh))
  cor(wther$ma, wther$fresh, use = "pairwise.complete.obs")
  ## add bootstrapped 98% CIs to correlation
  # require"confintr")
  rci <- confintr::ci_cor(wther$ma, wther$fresh, method = "pearson",
    use = "pairwise.complete.obs", boot_type="basic")
  if(CI) {
    out <- crci$estimate, rci$interval)
  } else {
    out <- rci$estimate
  }
  out
}


# corCalcwther = weather[[1]], freshStn = frsh, ld = 0, k = 31, wVar = "PRCP")


## optimize for: kMA), ldlag), station, depth
## need to go parallel here!


## given station and weather, find optimal k and lag combination
# wh=weather[[1]]; st=subset(freshLng, sdcombo == "AlongBay_10 surface"); wVar="TEMP"; ld=0:120; k=1:70
optimalkLd <- function(wh, st, ld=0:120, k=1:60, wVar = "PRCP", parE = FALSE) {
  ld_k <- expand.grid(ld, k)
  namesld_k) <- c("ld", "k")

  if(parE) {  ## parallelize this sapply call
    require("parallel")
    if(.Platform$OS.type=="unix") {
      cC <- mclapply(seq_len(nrow(ld_k)), FUN = function(i) {
        corCalc(wther = wh, freshStn = st, ld = ld_k$ld[i], k = ld_k$k[i]
          , wVar = wVar)
        }, mc.cores=detectCores()-1)
      cC <- do.call("cbind", cC)
    } else {
      cl <- makeClusterPSOCK(detectCores()-1)
      clusterExport(cl, c("wh", "st", "ld_k", "wVar", "corCalc")
        , envir = environment())
      cC <- parSapply(cl, seq_len(nrow(ld_k)), function(i) {
        corCalc(wther = wh, freshStn = st, ld = ld_k$ld[i], k = ld_k$k[i]
          , wVar=wVar, CI=TRUE)
      })
      stopCluster(cl)
    }
  } else {
    cC <- sapply(seq_len(nrow(ld_k)), function(i) {
      corCalc(wther = wh, freshStn = st, ld = ld_k$ld[i], k = ld_k$k[i]
        , wVar=wVar, CI=TRUE)
    })  # this is a matrix with 3 rows, many columns
  }

  ld_k$r <- cC[1,]
  ld_k$CIl <- cC[2,]
  ld_k$CIu <- cC[3,]
  # ld_k [which.maxcC),]
  ld_k
}

freshLng$sdcombo <- factor(paste(freshLng$station, freshLng$depth))
lags <- 0:140
maWs <- 1:120
clim <- c("TEMP", "PRCP")
# lags<-0:120
# maWs<-1:90
combs <- expand.grid(airport = c("Homer", "Seldovia"), clim = clim)
combs$climate <- factor(ifelse(combs$clim == "TEMP", "temperature"
  , "precipitation"))

## could wrap this lapply call into optimalkLd above, but output would be
## unnecessarily complicated, so don't
s <- Sys.time()
maxR <- lapply(seq_len(nrow(combs)), function(i) {
  optimalkLd(weather[[i %% 2 + 1]],
    subsetfreshLng, sdcombo == "AlongBay_10 surface"),
    ld=lags, k=maWs, wVar = combs$clim[i], parE=TRUE)
})
difftime(s, Sys.time())
## serial: user: 884 system 14.69 elapsed 1105
## timing stopped at 885 14.9 3660
## parallel: user: 2.35 1.09 403
saveRDS(maxR, file="~/tmp/LCI_noaa/cache-t/freshWaterRmax.rds")
# readRDS("~/tmp/LCI_noaa/cache-t/freshWaterRmax.rds")

## color scale: dividing
##


pdf(paste0(dir_plot, "cross-corr weather x AB10 s.pdf"), width=8, height=6)
for(i in seq_len(nrow(combs))) {
  filled.contour(x = lags, y = maWs,
    z=matrix(maxR[[i]]$r, nrow = length(lags), byrow = FALSE),
    xlab = "lag [d]", ylab = "MA window [d]",
    main = pastecombs$airport[i], "Airport", levels(combs$climate)[i],
       "x freshwater at AlongBay_10 surface")
    , asp = 1)
}
dev.off()




###############################
## Sitka vs AlongBay_3 deep ##
###############################

lags <- 0:360
maWs <- 1:120
maxRS <- lapplyseq_along(clim), function(i) {
  optimalkLd(weather[[5]], subset(freshLng, sdcombo == "AlongBay_3 deep"),
             ld=lags, k=maWs, wVar = clim [i], parE=TRUE)
})
lapply(1:2, function(i) {summary(maxRS[[i]]$r)})

pdf(paste0(dir_plot, "cross-corr weather x AB3 d.pdf"), width=8, height=6)
for(i in seq_along(clim)) {
  filled.contour(x = lags, y = maWs,
    z=matrix(maxRS[[i]]$r, nrow = length(lags), byrow = FALSE),
    xlab = "lag [d]", ylab = "MA window [d]",
    main = paste"Sitka Airport",
       ifelseclim[i] == "TEMP", "tempeature", "precipitation"),
       "x freshwater at AlongBay_3 deep")
    , asp = 1)
}
dev.off()

rmlags, maWs, combs)






for(wV in c("TEMP", "PRCP")) {
  cat"\n\n", wV, "\n")
    maxR <- sapply(seq_along(levels(freshLng$sdcombo)), functionw){
      sapply(seq_len(70), functionk) {
        ## return multiple values: optimal k, max r, CIs
      frsh <- subset(freshLng, sdcombo == levelsfreshLng$sdcombo)[w])
      sapply(1:120, function(ld) {
        corCalc(weather[[2]], frsh, ld = ld, wVar = wV, k = k)  ## no PRCP for weather[[3]]!
      }) |>
        max()
    }) |>
      which.max()
  })
}



freshLng$sdcombo <- factor(paste(freshLng$station, freshLng$depth))
for(wV in c("TEMP", "PRCP")) {
  cat"\n\n", wV, "\n")
  maxR <- sapply(seq_along(levels(freshLng$sdcombo)), functionw){
    frsh <- subset(freshLng, sdcombo == levelsfreshLng$sdcombo)[w])
    sapply(1:120, function(ld) {
      corCalc(weather[[2]], frsh, ld = ld, wVar = wV, k = 31)  ## no PRCP for weather[[3]]!
    }) |>
      max()
  })
  ## 9-6 and AlongBay-10 are best
  out <- data.frame(station=levels(freshLng$sdcombo), maxR) |>
    dplyr::arrange(desc(maxR)) |>
    head()
  print(out)
}
# AlongBay-10 stands out repeatedly
# Station 1 is good, 4 less so
# 2 is not badfor temp, not great for precip!), but 1 is better


## take Seldovia and
## optimize



## plot cross-correlation of T9 and precipitation 0-90 days

## fix color scale
r_col <- ggplot2::scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
           '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
            n.breaks=10, limits=c(-1,1), show.limits=T)


pdfpaste0dir_plot, "crosscor-AB10-SxSeldoviaPRCP.pdf"))
frsh <- subsetfreshLng, sdcombo == "AlongBay_10 surface")
cC <- sapply1:120, functionld) {
  corCalc(weather[[1]], frsh, ld = ld, wVar = "PRCP", k = 31)
})
plot(cC, type="l", xlab = "lag [days]", ylab = "correlation coefficient"
  , main = paste0"AlongBay-10 surface freshwater x Seldovia precipitation")
  , sub = "Moving average of 31 days")
abline(h = 0, lty = "dashed")
abline(v = which.max(cC), lty = "dashed")
dev.off()




## plot cross-correlation of T9 and precipitation 0-90 days
## Juneau and deep water -- what is the time lag?
pdfpaste0dir_plot, "crosscor-AB10-SxSeldoviaPRCP.pdf"))
frsh <- subsetfreshLng, sdcombo == "AlongBay_10 surface")
cC <- sapply1:120, functionld) {
  corCalc(weather[[1]], frsh, ld = ld, wVar = "PRCP", k = 31)
})
plot(cC, type="l", xlab = "lag [days]", ylab = "correlation coefficient"
     , main = paste0"AlongBay-10 surface freshwater x Seldovia precipitation")
     , sub = "Moving average of 31 days")
abline(h = 0, lty = "dashed")
abline(v = which.max(cC), lty = "dashed")
dev.off()



## using salinity as a tracer -- cross-correlations of AlongBay_15 with all
## all stations to see how fast freshwater is spreading
## map max lag

