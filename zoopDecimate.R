#!/usr/bin/env Rscript

## zooplankton community distances between samples
## figure out which stations/transects to cull
## use zoopCoomunity as a template


rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R -- of interest: zooC and zooCenv
set.seed (9)
Require ("sp")
Require ("raster")
Require ("vegan")
dir.create ("~/tmp/LCI_noaa/media/2019/", recursive = TRUE, showWarnings = FALSE)



## cluster analysis of zoop, label by station (and year/month?)

## average stations over seasons and years
## iteratively remove closest station
## plot total multi-D variance against sample size


agZoop <- aggregate (zooC, by = list (factor (zooCenv$Match_Name)), sum)
row.names (agZoop) <- agZoop [,1]
agZoop <- agZoop [,2:ncol (agZoop)]

## instead use season x station
agZoop <- aggregate (zooC, by = list (factor (zooCenv$Match_Name),  zooCenv$season), sum)
row.names (agZoop) <- paste (as.character (agZoop [,1]), as.character (agZoop [,2]), sep = "-")
agZoop <- agZoop [,3:ncol (agZoop)]


## reduce to core stations
if (1){
  cS <- grep ("^(Along|[1-9])", row.names (agZoop))
  agZoop <- agZoop [cS,]; rm (cS)
}
require ("vegan")
if (1){
nM <- metaMDS (agZoop, distance = "bray", k = 3, try = 200, trymax = 500)

pdf ("~/tmp/LCI_noaa/media/zoopStationYear-nMDS.pdf")
plot (nM, "sites", type = "n")
text (nM, "sites")
dev.off()

pdf ("~/tmp/LCI_noaa/media/zoopStationYear-Cluster.pdf")
plot (hclust (dist (agZoop, "manhattan"), method = "ward.D"))
dev.off()
}




## iterative reduction
disM <- function (cM){# distance measure
  dist (cM, "manhattan")
}

## measure of diversity/distance/variance
dM <- function (cM){
  require ("vegan")
  ## ideas: variance

  ## species diversity
  diversity (rowSums (cM), index = "shannon", MARGIN = 1)
  # diversity (rowSums (cM), index = "simpson", MARGIN = 1)

  # species richness
  #  specnumber (rowSums(cM))

  ## total distance
  #  sum (disM (cM))
}
## total distance and species richness show a slope with no indication of leveling off
## diversity is a more useful measure here



## heuristic: remove shortest distance, repeat
rdS <- data.frame (Nsamp = nrow (agZoop):2, divM = NA, drop = NA, stns = NA)

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
save.image ("~/tmp/LCI-noaa/cache/zoopDec.RData")


## exhaustive search -- try whether it can be done (NP-hard?!)
## yes, it's NP hard and impossible!!
nCPUs <- detectCores (logical = TRUE)-1
require ("parallel")
## would use SMP on windows
#  if (version$os == "mingw32"){

## replace just the selected sample size
for (i in 1:(nrow (agZoop)-1)){
    sT <- Sys.time()
    cbn <- combn (1:nrow (agZoop), nrow (agZoop)-i+1)
  if (version$os == "mingw32"){
        oCbn <- which.max (sapply (1:ncol (cbn), function (j){dM (agZoop [cbn [,j],])}))
    }else{oCbn <- which.max (unlist (mclapply (1:ncol (cbn), function (j){dM (agZoop [cbn [,j],])}, mc.cores = nCPU)))
    }
    oVect <- cbn [,oCbn]
    rdS$Nsamp [i] <- nrow (cbn)
    rdS$div [i] <- dM (agZoop [oVect,]) # could recycle above
    rdS$stns [i] <- paste (rownames (agZoop)[oVect], collapse = ", ")
    cat (i, difftime (Sys.time(), sT, units = "mins"), "\n")
    save.image (paste0 ("~/tmp/zoopDecStatYear", i, ".RData"))
    cat (i, difftime (Sys.time(), nT, units = "min"), " min\n")
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
