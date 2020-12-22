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
# dir.create ("~/tmp/LCI_noaa/cache/", recursive = TRUE, showWarnings = FALSE)



## cluster analysis of zoop, label by station (and year/month?)

## average stations over seasons and years
## iteratively remove closest station
## plot total multi-D variance against sample size


agZoop <- aggregate (zooC, by = list (factor (zooCenv$Match_Name)), sum)
row.names (agZoop) <- agZoop [,1]
agZoop <- agZoop [,2:ncol (agZoop)]


## instead use season x station
if (1){
  agZoop <- aggregate (zooC, by = list (factor (zooCenv$Match_Name),  zooCenv$season), sum)
  row.names (agZoop) <- paste (as.character (agZoop [,1]), as.character (agZoop [,2]), sep = "-")
  agZoop <- agZoop [,3:ncol (agZoop)]
}


## reduce to core stations
if (1){
  cS <- grep ("^(Along|[1-9])", row.names (agZoop))

  cS <- levels (factor (subset (zooCenv$Match_Name, zooCenv$Year == 2017)))
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
Require ("vegan")
pdf ("~/tmp/LCI_noaa/media/zoopStationYear-Cluster.pdf")
#plot (hclust (dist (agZoop, "manhattan"), method = "ward.D"))
plot (hclust (vegdist (agZoop, "bray"), method = "ward.D"))
dev.off()





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



pdf ("~/tmp/LCI_noaa/media/zoopStation-unoptimization.pdf")
plot(divM~Nsamp, rdS, type = "l", lwd = 2, ylab = "species diversity", xlab = "N stations")
dev.off()
write.csv (rdS, file = "~/tmp/LCI_noaa/media/zoopStation-unoptimization.csv", row.names = FALSE)
save.image ("~/tmp/LCI_noaa/cache/zoopDec.RData")



# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopDec.RData")
# zM <- agZoop; nOut <- 50; nIter <- 10000

## alternative heuristic algorithm
bestYofX <- function (zM, nOut = 40, nIter = 1000#, seed = 8
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
  for (i in 1:nIter){   # vectorize somehow? any way to speed up? It's intrinsically serial
    rSelect <- mutate (rSelect)
    tF$div [i] <- dM (zM [rSelect,])
  }
  # plot (div~iter, tF, type = "l")
  # list (rSelect, dM (zM [rSelect,]))  # return results: best sequence and diversity of result
  rSelect
  ## end after nIter (or convergence?)
}

parBest <- functin (zM, nOut = 40, nRuns = 1000){
  ## call bestYofX x times in parallel, return the best result. x = nCPUs
  #  Require ("future.apply")
  Require ("parallel"); Require ("vegan")
  nCPUs <- detectCores (logical = TRUE)-1
  cl <- makeCluster (nCPUs, type = "PSOCK")
  clusterSetRNGStream (cl, iseed = 42)
  clusterExport (cl, c("zM", "dM", "Require", "bestYofX", "nOut"))
  x <- clusterEvalQ (cl, Require ("vegan")) # suppress output
  ## set seed in parallel XXX
  # x <- clusterEvalQ (cl, bestYofX (zM, nOut, nIter = 500))
  pSet <- parLapply (1:1000, bestYofX (zM, nOut, nIter = 500))
  bestSet <- which.max (sapply (1:length (pSet), function (i){dM (zM [pSet[[i]],])}))
  stopCluster (cl)
  pSet [[bestSet]]
}


## cluster this one:  bestYofX (agZoop, 50, nIter = 10)
## dM (agZoop)=4; nrow (agZoop)=78

outDF <- data.frame (nSite = 1:(nrow (agZoop)-2), div = NA, sites = NA)
for (i in 1:nrow (outDF)){
  oSeq <- parBest (agZoop, nOut = nrow (agZoop)-i, nRuns = 640)
  rdS$heuDiv [i] <- dM (agZoop [oSeq,])
  rdS$heuSites [i] <- oSeq
}


save.image ("~/tmp/LCI_noaa/cache/zoopDec.RData")



## estimate size of the problem -- calculated combinations
n <- nrow (agZoop)
df <- data.frame (k = 1:n, opt = NA)
df$opt <- factorial (n) / (factorial (df$k)*factorial(n-df$k))
pdf ("~/tmp/LCI_noaa/media/zoop_combinations.pdf")
plot (opt~k, df, type = "l")
abline (v = 13)
dev.off()
sum (df$opt)/1e6
sum (subset (df$opt, df$k < 14))/1e6
rm (n, df)





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

## EOF