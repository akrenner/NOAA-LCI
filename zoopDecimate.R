#!/usr/bin/env Rscript

## zooplankton community distances between samples
## figure out which stations/transects to cull
## use zoopCoomunity as a template


## source ("P:/My Documents/Documents/myDocs/amyfiles/NOAA-LCI/zoopDecimate.R")

rm (list = ls())

## set-up for processing on HPC facility
tr <- require ("pacman")
if (!tr){
  install.packages ("pacman")
}
tr <- try (load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")) # from dataSetup.R -- of interest: zooC and zooCenv
if (class (tr) == "try-error"){
  dir.create ("~/tmp/LCI_noaa/cache/", recursive = TRUE, showWarnings = FALSE)
  file.copy (from = "P:/My Documents/Documents/tmp/LCI_noaa/cache/dataSetupEnd.RData"
             , to = "~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
  load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
}
set.seed (9)
Require ("sp")
Require ("raster")
Require ("vegan")
dir.create ("~/tmp/LCI_noaa/media/2019/", recursive = TRUE, showWarnings = FALSE)
dir.create ("~/tmp/LCI_noaa/cache/", recursive = TRUE, showWarnings = FALSE)



## fix duplicate station names
zooCenv$Match_Name <- ifelse (zooCenv$Match_Name == "AlongBay_3", "4_3", zooCenv$Match_Name)
#not relevant here...
zooCenv$Match_Name <- ifelse (zooCenv$Match_Name == "AlongBay_6", "9_6", zooCenv$Match_Name)

## fixed bad station name (temporary line)
# zooCenv$Match_Name <- ifelse (zooCenv$Match_Name == "4_10", "9_10", zooCenv$Match_Name)


## cluster analysis of zoop, label by station (and year/month?)

## average stations over seasons and years
## iteratively remove closest station
## plot total multi-D variance against sample size



## merge eastern-most Cook Inlet Stations
if (0){ ## plot zoop stations to be sure:
  Tx <- subset (poSS, !duplicated (poSS@data$Match_Name))
  Tx$Transect <- zooCenv$Transect [match (Tx$Match_Name, zooCenv$Match_Name)]
  Tx$Station <- zooCenv$Station [match (Tx$Match_Name, zooCenv$Match_Name)]

  # Tx <- subset (Tx, Tx@data$Match_Name)
  plot (Tx,  pch = "2", cex = 0.1)
  text (Tx, labels = Tx$Station)
  lines (coast)
  rm (Tx)
}
zD <- zooCenv@data
suppressWarnings({
  zD$Match_Name <- ifelse ((zD$Transect == "3") & (as.numeric (zD$Station) > 10),  "3_E", zD$Match_Name) # 4, 8, 12, 13, 14
  zD$Match_Name <- ifelse ((zD$Transect == "7") & (as.numeric (zD$Station) > 10),  "7_E", zD$Match_Name) # 1, 3, 4, 5, 9, 11, 25
  zD$Match_Name <- ifelse ((zD$Transect == "6") & (as.numeric (zD$Station) < 9),  "6_E", zD$Match_Name) # 1, 3, 4, 5, 9, 11, 25 -- order reversed
  zooCenv@data <- zD
  rm (zD)
})


## reduce to core stations -- build index to subset
keep <- which (zooCenv$Transect %in% c ("KB", "4", "9"))
keep <- c (keep, which (zooCenv$Match_Name %in% c ("3_E", "7_E", "6_E")))
zooC <- zooC [keep,]
zooCenv <- zooCenv [keep,]
if (1){
  agZoop <- aggregate (zooC, by = list (factor (zooCenv$Match_Name)), sum)
  row.names (agZoop) <- agZoop [,1]
  agZoop <- agZoop [,2:ncol (agZoop)]
}else{ ## instead use season x station
  agZoop <- aggregate (zooC, by = list (factor (zooCenv$Match_Name),  zooCenv$season), sum)
  row.names (agZoop) <- paste (as.character (agZoop [,1]), as.character (agZoop [,2]), sep = "-")
  agZoop <- agZoop [,3:ncol (agZoop)]
}
row.names (agZoop)


## reduce to core stations
if (0){
  cS <- grep ("^(Along|[1-9])", row.names (agZoop))

  cS <- levels (factor (subset (zooCenv$Match_Name, zooCenv$Year > 2017)))
  cS <- grep (paste0 ("^(", paste (cS, collapse = "|"),")"), row.names (agZoop))

  agZoop <- agZoop [cS,]; rm (cS)
}
require ("vegan")


if (0){
  ## run nMDS -- not of much use here
  nM <- metaMDS (agZoop, distance = "bray", k = 3, try = 200, trymax = 500)

  pdf ("~/tmp/LCI_noaa/media/zoopStationYear-nMDS.pdf")
  plot (nM, "sites", type = "n")
  text (nM, "sites")
  dev.off()
}

## cluster analysis
Require ("RColorBrewer")
sCol <- brewer.pal(4, "Set2")
sColC <- factor (gsub ("^(\\d|AlongBay)*_\\d+-", "", row.names (agZoop)))
sColC <- sCol [as.integer(sColC)]

tColF <- factor (gsub ("_\\d+-(summer|fall|winter|spring)$", "", row.names (agZoop)))
tCol <- brewer.pal (length (levels (tColF)), "Set2")
tColC <- tCol [as.integer (tColF)]


if (1){ ## mark T4
  sCol <- c ("black", "red")
  sCol <-  (1:nrow (agZoop)) %in% grep ("^4_", row.names (agZoop), value = FALSE)
  sColC <- ifelse (sCol, "red", "black")
}

Require ("vegan")
zClust <- hclust (vegdist (agZoop, "bray"), method = "ward.D")


pdf ("~/tmp/LCI_noaa/media/zoopStationYear-Cluster.pdf"
     , width = 12, height = 12)
# plot (hclust (vegdist (agZoop, "bray"), method = "ward.D"))

Require ("ape")
plot (as.phylo (zClust)
      , tip.color = sColC, label.offset = 0.0, cex = 1 # 0.7
#      , type = "fan"
#      , type = "cladogram"
      )
# legend ("topleft", fill = sCol, legend = c("spring", "summer", "fall", "winter"))
plot (as.phylo (zClust)
      , tip.color = tColC, label.offset = 0.0, cex = 0.7
)
legend ("topleft", fill = tCol, legend = levels (tColF))
dev.off()


pdf ("~/tmp/LCI_noaa/media/zoopStation-Cluster-simple.pdf"
     , width = 12, height = 12)
# plot (hclust (vegdist (agZoop, "bray"), method = "ward.D"))

Require ("ape")
plot (as.phylo (zClust)
      #, tip.color = sColC, label.offset = 0.0, cex = 1
      #      , type = "fan"
      #      , type = "cladogram"
)
# legend ("topleft", fill = sCol, legend = c("spring", "summer", "fall", "winter"))
dev.off()








## extract N groups from hclust -- or use kmeans?
bioG <- cutree (zClust, k = 3)   ## set N groups from hclust
agZPt <- subset (zooCenv, !duplicated(zooCenv$Match_Name))  # remove extras
# agZPt$clust <- factor (bioG [match (names (bioG), agZPt$Match_Name)])
agZPt$clust <- factor (bioG [match (agZPt$Match_Name, names (bioG))])

rm (bioG)

## voronoi diagram / biogeography from cluster diagram
Require ("sf")
## should add envelope to st_voronoi
vor <- coordinates (agZPt) %>%
  st_multipoint() %>%
  st_voronoi() %>%
  st_collection_extract()
vor <- as (vor, "Spatial") ## convert sf to sp spatial
proj4string(vor) <- CRS (proj4string(zooCenv))

## assign bioG to points, then polygons
vor <- SpatialPolygonsDataFrame (vor, data = data.frame (over (vor, agZPt)))

Require ("RColorBrewer")
cCol <- brewer.pal (length (levels (vor$clust)), "Set2") # set3 = longest

pdf ("~/tmp/LCI_noaa/media/KBayZoopBioGeo.pdf", width = 11, height = 8.5)
plot (agZPt)  # set-up area
plot (vor, add = TRUE, col = cCol [vor@data$clust])
plot (coast, col = "beige", add = TRUE)
# plot (agZPt, col = "black", pch = 19, add = TRUE)
text (agZPt, agZPt$Match_Name, cex = 0.5)
dev.off()















## estimate size of the problem -- calculated combinations
n <- nrow (agZoop)
df <- data.frame (k = 1:n, opt = NA)
df$opt <- factorial (n) / (factorial (df$k)*factorial(n-df$k))
pdf ("~/tmp/LCI_noaa/media/zoop_combinations.pdf")
plot (opt~k, df, type = "l")
# abline (v = 13)
dev.off()
sum (df$opt)/1e6
sum (subset (df$opt, df$k < 14))/1e6
rm (n, df)




## iterative reduction
disM <- function (cM){# distance measure
  # dist (cM, "manhattan")
  Require ("vegan")
  vegdist (cM, "bray")
}

## measure of diversity/distance/variance
dM <- function (cM, meas = "div"){
  require ("vegan")
  ## ideas: mean species CV as a measure of variance
  #  mean (apply (cM, 2, function (x){sd(x)/mean(x)}))


  ## species diversity
  diversity (rowSums (cM), index = "shannon", MARGIN = 1)  ## late saturation
  # diversity (rowSums (cM), index = "simpson", MARGIN = 1) ## early saturation

  # species richness
  #  specnumber (rowSums(cM))   ## slow rising straight line

  ## total distance
  #  sum (disM (cM))  ## straight line
}
## total distance and species richness show a slope with no indication of leveling off
## diversity is a more useful measure here





## heuristic: remove shortest distance, repeat
rdS <- data.frame (Nsamp = nrow (agZoop):2, divM = NA, drop = NA
                   , stns = NA, diveE = NA, stnsE = NA)




## heuristic -- iterative search

bestYofXLeaf <- function (zM, nOut = nrow (zm)-1){  # recursive leave-one-out
  dScreen <- lapply (1:row (zM), function (i){dM (zM [-i,])})
  nSet <- 1:nrow (zM) [-which.max(dScreen)]
  ## unfinished, non-functional
}


if (1){
  nZ <- agZoop
  for (i in 1:(nrow (agZoop)-1)){
    rdS$Nsamp [i] <- nrow (nZ)
    rdS$divM [i] <- dM (nZ)
    rdS$stns [i] <- paste (rownames(nZ), collapse = ", ")
    cbn <- combn (1:nrow (nZ), 2) # only look at pairs
    cOut <- which.min (sapply (1:ncol (cbn), function (j){dM (nZ [cbn [,j],])}))
    # remove first, random, or optimal station XXX -- random for now
    # sOut <- cbn [sample (1:2, 1), cOut]

    ## remove optimal station
    n1 <- nZ [-cbn [1,cOut],]
    n2 <- nZ [-cbn [2,cOut],]
    sOut <- cbn [which.max (c (dM (n1), dM (n2))), cOut]; rm (n1, n2)

    rdS$drop [i] <- rownames(nZ) [sOut]
    nZ <- nZ [-sOut,]  # watch out here for scoping XXX
  }


  write.csv (rdS, file = "~/tmp/LCI_noaa/media/zoopStation-unoptimization.csv", row.names = FALSE)


  pdf ("~/tmp/LCI_noaa/media/zoopStation-unoptimization.pdf", width = 12)
  par (mar = c (8,4,0.1,0.1))
  plot(divM~Nsamp, rdS, type = "s", lwd = 2, ylab = "species diversity"
#      , xlab = "N stations-seasons"
       , xlab = "")

  ## with seasons
#  axis (1, at = rdS$Nsamp, labels = rdS$drop, las = 3, tick = FALSE, cex.axis = 0.5, line = 1.5, gap.axis = 0.1)
  ## without seasons
  axis (1, at = rdS$Nsamp - 0.5, labels = rdS$drop, las = 3, tick = FALSE
        , cex.axis = 0.8, line = 1.5, gap.axis = 0.1)

  xS <- agZoop [-grep ("^3_E", row.names (agZoop), value = FALSE),]
  abline (h = dM (xS), col = "gray")
  points (nrow (xS), dM (xS), pch = 19, col = "red")
  text (nrow (xS), dM (xS), labels = "-T3", pos = 4, offset = 0.5)

  dev.off()



  ## plots:
  # bar charts
  # curve with custom reduction
  pdf ("~/tmp/LCI_noaa/media/zoopStation-LOO_zoom.pdf")
  plot (divM~Nsamp, rdS, type = "s"
        , xlab = "N station-seasons", ylab = "species diversity"
        # , ylim = c (3.3, 3.7), xlim = c(30, 55)  ## with seasons
        , ylim = c(2.14, 2.35), xlim = c(10, 19)  ## without seasons
        )
  ## all but 3_E
  xS <- agZoop [-grep ("^3_E", row.names (agZoop), value = FALSE),]
  abline (h = dM (xS), col = "gray")
  points (nrow (xS), dM (xS), pch = 19, col = "red")
  text (nrow (xS), dM (xS), labels = "-T3", pos = 4, offset = 0.5)

  xS <- agZoop [-grep ("^6_E", row.names (agZoop), value = FALSE),]
#  xS <- xS [-grep ("^6_E", row.names (xS), value = FALSE),]
  points (nrow (xS), dM (xS), pch = 19, col = "red")
  text (nrow (xS), dM (xS), labels = "-T6", pos = 4, offset = 0.5)

#  xS <- agZoop [-grep ("^7_E", row.names (agZoop), value = FALSE),]
  xS <- xS [-grep ("^7_E", row.names (xS), value = FALSE),]
  points (nrow (xS), dM (xS), pch = 19, col = "red")
  text (nrow (xS), dM (xS), labels = "-T7 and - T6", pos = 4, offset = 0.5)

  xS <- agZoop [-grep ("^7_E", row.names (agZoop), value = FALSE),]
  #  xS <- xS [-grep ("^7_E", row.names (xS), value = FALSE),]
  points (nrow (xS), dM (xS), pch = 19, col = "red")
  text (nrow (xS), dM (xS), labels = "-T7", pos = 2, offset = 0.5)

  dev.off()


  save.image ("~/tmp/LCI_noaa/cache/zoopDec.RData")
}





# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopDec.RData")
# zM <- agZoop; nOut <- 50; nIter <- 500

## alternative heuristic algorithm
bestYofX <- function (zM, nOut = 10, nIter = 1000#, seed = 8  ## -- evolutionary hill climbing
                      ){ ## inherintly linear or parallel?
#  set.seed(seed)
    ## select random stations
  rSelect <- sort (sample.int (nrow (zM), nOut, replace = FALSE)) # random selection
  # nSelect <- (1:nrow (zM))[-rSelect] # all left behind
  # sM <- zM [rSelect,]
  ## replace 1 location at random -- keep if improvement
  mutate <- function (testSelect){ ## mutate and test initial selection
    sM <- zM [testSelect,]
    notSelect <- (1:nrow (zM))[-testSelect]
    startC <- dM (zM [testSelect,])

    mutS <- testSelect
    mutS [sample.int (nOut,1)] <- notSelect [sample.int(length (notSelect), 1)]
    if (startC < dM (zM [mutS,])){
      outS <- mutS
    }else{
      outS <- testSelect
    }
    outS
  }
  ## trace convergence
  tF <- data.frame (iter = 1:nIter, div = NA)
  # for (i in 1:nIter){   # vectorize somehow? any way to speed up? It's intrinsically serial
  #   rSelect <- mutate (rSelect)
  #   tF$div [i] <- dM (zM [rSelect,])
  # }
  i <- 1; conv <- FALSE
  while (i < nIter){
  #while ((i < nIter)&(conv == FALSE)){
      rSelect <- mutate (rSelect)
    tF$div [i] <- dM (zM [rSelect,])
    # if (i > 100){ # minimal iterations before considering convergence
    #   if ((tF$div [i] - tF$div [i-100])/diff (range (tF$div, na.rm = TRUE)) < 0.001){ # this can fail: missing value where TRUE/FALSE needed
    #     conv <- TRUE
    #   }
    # }
    i <- i + 1
  }
  # plot (div~iter, tF, type = "l")
  # list (rSelect, dM (zM [rSelect,]))  # return results: best sequence and diversity of result
  sort (rSelect)
  ## end after nIter (or convergence?)
}


nRuns <- 600
nRuns <- 4  # better to have few long runs
nIter <- 10

## big run
nRuns <- 100
nIter <- 1000

ncutStn <-  1:(nrow (agZoop)-2) # N stations x seasons to cut

## need to set-up cluster outside of function
## tens-hundreds of scenarios to test (N stations to cut) -- sequential (or parallel?)
## gizillions of options within -- rerun several times (on on each core) with random start

## use future_map instead of parLapply :
# maybe no conflict with NOAA firewall settings (main reason)
# more flexible, simpler code
# avoid struggle with what's passed to the cluster and what's not
# could use future_apply instead of furrr::future_map -- difference is only semantics?
# it's the future?

Require ("purrr")  # for %>%
Require ("furrr")
Require ("tictoc")
plan (multisession (workers = availableCores()-1)) # still triggers firewall?

tic()
parBest <- ncutStn %>%
  future_map (function (x){
    1:nRuns %>%
      future_map (function (X){
        bestYofX(agZoop, nOut = x, nIter = nIter)}
        , .options = furrr_options (seed = TRUE)
      )
  }, .options = furrr_options(seed = TRUE), .progress = TRUE)
toc()
## go back to normal
plan (sequential)


# ## cluster this one:  bestYofX (agZoop, 50, nIter = 10)
# ## dM (agZoop)=4; nrow (agZoop)=78
#
# # x <- parBest (zM = agZoop, nOut = 60, nRuns = 60)  ## better to have fewer but longer runs
# # plot (sort (x), type = "l")
# #
# #
# outDF <- data.frame (nSite = 1:(nrow (agZoop)-2), div = NA, sites = NA)
# for (i in 1:nrow (outDF)){
#   oSeq <- parBest (zM = agZoop, nOut = nrow (agZoop)-i, nRuns = 60)
#   rdS$heuDiv [i] <- dM (agZoop [oSeq,])
#   rdS$heuSites [i] <- oSeq
# }


save.image ("~/tmp/LCI_noaa/cache/zoopDec.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopDec.RData")




## best for each ncutStn (1:78)
heurDiv <- unlist (lapply (ncutStn, function (i){
  diverMeasRun <- unlist (lapply (1:nRuns, function (j){
    dM (agZoop [parBest [[i]][[j]],])}))
  max (diverMeasRun)
}))
## compare heuristic to leave-one-out
pdf ("~/tmp/LCI_noaa/media/zoopDecimate_methods.pdf")
plot (divM~Nsamp, rdS, type = "l", lwd = 2, col = "black"
      , ylab = "diversity", xlab = "N stations in sample")
lines (heurDiv~1:ncutStn, lwd = 2, col = "blue")
legend ("bottomright", lwd = 2, col = c("black", "blue")
        , legend = c("leave-one-out", "heuristic"))
dev.off()

## for optimal station, ranking of inclusions for different runs

bestRun <- unlist (lapply (ncutStn, function (i){
  diverMeasRun <- unlist (lapply (1:nRuns, function (j){
    dM (agZoop [parBest [[i]][[j]],])}))
  which.max (diverMeasRun)
}))



## all runs identical?
runSD <- unlist (lapply (ncutStn, function (i){
  diverMeasRun <- unlist (lapply (1:nRuns, function (j){
    dM (agZoop [parBest [[i]][[j]],])}))
   sd (diverMeasRun)
  # diff (range (diverMeasRun))
}))

pdf ("~/tmp/LCI_noaa/media/zoopDecimate_SDofRuns.pdf")
plot (runSD, xlab = "N stations in sample", ylab = "SD of runs")
lines (predict (loess (runSD~ncutStn)))
dev.off()





pdf ("~/tmp/LCI_noaa/media/zoopDecimate_site-frequency.pdf"
     , height = 4, width = 12)
siteFreq <- summary (factor (row.names (agZoop)[unlist (parBest[[60]])]))
sO <- order (siteFreq)
barplot (siteFreq [sO], col = sColC [sO])
legend ("topleft", fill = sCol, legend = c("spring", "summer", "fall", "winter"))

barplot (siteFreq [sO], col = tColC [sO])
legend ("topleft", fill = tCol, legend = levels (tColF))
dev.off()

## still keep this one?
pdf ("~/tmp/LCI_noaa/media/zoopDecimate_site-frequency2.pdf"
     , height = 6, width = 25)
par (las = 3, mar = c (8,3,1,1))
mRN <- factor (gsub ("AlongBay", "AB", row.names (agZoop)))
barplot (sort (summary (mRN[unlist (parBest)])))
dev.off()


# allS <- paste (rdS$heuSites, collapse = ", ")











if (0){



## exhaustive search -- try whether it can be done (NP-hard?!)
## yes, it's NP hard and impossible!!

## would use SMP on windows
#  if (version$os == "mingw32"){
## parallel options: parallel: socket-cluster, parLapply
## doSMP (by REvolution) -- it's dead
## plyr? (seems just right)


Require ("parallel")
Require ("vegan")
nCPUs <- detectCores (logical = TRUE)-1
if (version$os == "mingw32"){
  ## set up cluster
  #  Require ("doParallel")
  cl <- makeCluster (nCPUs, type = "PSOCK")
  #  registerDoParallel (cl)
  clusterExport (cl, c ("agZoop", "dM", "Require"))
  x <- clusterEvalQ (cl, Require ("vegan")) # suppress output
  # clusterEvalQ (cl, library (lm4))
  # system.time (save3 <- parLapply (cl, 1:100, ))
}


## replace just the selected sample size
for (i in 1:(nrow (agZoop)-1)){
  # for (i in 10){
  sT <- Sys.time()
  cbn <- combn (1:nrow (agZoop), nrow (agZoop)-i+1)
  if (version$os == "mingw32"){
    #    oCbn <- which.max (sapply (1:ncol (cbn), function (j){dM (agZoop [cbn [,j],])}))
    clusterExport (cl, c ("cbn"))
    oCbn <- which.max (unlist (parLapply (cl, 1:ncol (cbn), fun = function (j){dM (agZoop [cbn [,j],])})))
    # x <- foreach (j = 1:ncol (cbn)) %dopar% dM (agZoop [cbn [,j],])
  }else{ # forking on Mac or GNU/Linux
    oCbn <- which.max (unlist (mclapply (1:ncol (cbn), function (j){dM (agZoop [cbn [,j],])}, mc.cores = nCPU)))
  }
  oVect <- cbn [,oCbn]
  # rdS$Nsamp [i] <- nrow (cbn)
  rdS$divE [i] <- dM (agZoop [oVect,]) # could recycle above
  rdS$stnsE [i] <- paste (rownames (agZoop)[oVect], collapse = ", ")
  save.image (paste0 ("~/tmp/LCI_noaa/cache/zoopDecStatYear", i, ".RData"))
  cat (i, difftime (Sys.time(), sT, units = "min"), " min\n")
}

if (version$os == "mingw32"){
  ## tear-down cluster
  stopCluster (cl)
  rm (cl)
}


## needs to run on fast parallel machine. On NOAA laptop:
# 1 0.0001332641  min
# 2 0.002182035  min
# 3 0.0407367  min
# 4 0.470615  min
# 5 3.421406  min
# 6 21.23535  min
# 7 110.3046  min
# 8 492.16  min
# 9 -- unknown


pdf ("~/tmp/LCI_noaa/media/zoopStation-exactoptimization.pdf")
plot(divM~Nsamp, rdS, type = "l", lwd = 2, ylab = "species diversity", xlab = "N stations")
lines (divE~Nsamp, rdS, lwd = 2, col = "green")
dev.off()

write.csv (rdS, file = "~/tmp/LCI_noaa/media/zoopStation-exactoptimization.csv", row.names = FALSE)




## map of all and subset stations
}
## EOF
