#!/usr/bin/env Rscript

## CTD anomaly
## for each station location, calculate averages for all
## oceanographic parameters

rm(list = ls())
normDir <- "~/tmp/LCI_noaa/media/CTDsections/CTDwall-normals/"
load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R


## copied from dataSetup.R(should have a package)
Seasonal <- function(month) {           # now using breaks from zoop analysis -- sorry for the circularity
  month <- as.numeric(month)
  cut(month, breaks = c(-13, 0, 2, 4, 8, 10, 25)
    , labels = c("fall", "winter", "spring", "summer", "fall", "winter"))
}



#################################
## monthly and quarterly means ##
#################################

# monthly mean
#   XXX could do this better, using circular annual means, as elsewhere

# min for a stable mean:
nMin <- 5


## output: as poAll, but with month as factor
poAll$month <- factor(format(poAll$isoTime, "%m"))
oM <- as.matrix(poAll [, which(names(poAll) == "Temperature_ITS90_DegC")
  :which(names(poAll) == "bvf")])

poNorm <- aggregate(oM ~ Match_Name + month   + Depth.saltwater..m., data = poAll,
  FUN = mean, na.rm = TRUE)
poN    <- aggregate(oM ~ Match_Name + month   + Depth.saltwater..m., data = poAll,
  FUN = function(x) {sum(!is.na(x)) })
nC <- which(names(poNorm) == colnames(oM)[1]):ncol(poNorm)
poNorm [,nC] <- sapply(nC, function(i) {ifelse(poN[,i] < nMin, NA, poNorm [,i])} )



poSD <- aggregate(oM ~ Match_Name + month     + Depth.saltwater..m., data = poAll,
  FUN = sd, na.rm = TRUE)
poSD [,nC] <- sapply(nC, function(i) {ifelse(poN[,i] < nMin, NA, poSD [,i])} )

## quarterly means
season <- Seasonal(poAll$month)
poSorm <- aggregate(oM ~ Match_Name + season + Depth.saltwater..m., data = poAll,
                     FUN = mean, na.rm = TRUE)
poN <- aggregate(oM~Match_Name + season + Depth.saltwater..m., data = poAll,
                 FUN = function(x) {sum(!is.na(x)) })
poSorm [,nC] <- sapply(nC, function(i) {ifelse(poN[,i] < nMin, NA, poSorm [,i])} )




### XXX end of edits
## calculate anomalies
poAno <- sapply(seq_len(ncol(oM)), function(i) {
  pC <- which(names(poAll) == colnames(oM)[i])
  poAll [,pC] - poNorm [match(paste(poAll$Match_Name, poAll$month),
    paste(poNorm$Match_Name, poNorm$month)), nC[i]]
}) |>
  as.data.frame()
names(poAno) <- paste0("an_", colnames(oM))


## scale anomalies by SD
poAno_scale <- sapply(seq_len(ncol(poAno)), function(i) {
  poAno[,i] / poSD [match(paste(poAll$Match_Name, poAll$month),
    paste(poSD$Match_Name, poSD$month)),nC[i]]
}) |>
  as.data.frame()
names(poAno_scale) <- paste0("anS_", colnames(oM))

poAll <- cbind(poAll, poAno, poAno_scale)

#
#
# poAno <- cbind(poAll [, 1:(which(names(poAll)==colnames(oM)[1])-1)], poAno)
# poAno_scale <- cbind(poAll [, 1:(which(names(poAll)==colnames(oM)[1])-1)],
#   poAno_scale)
#
# rm(poN, sidx, oM, nC, nMin)
#
#
#
# ## add stn data
# sidx <- match(poNorm$Match_Name, stn$Match_Name)
# poNorm$latitude_DD <- stn$Lat_decDegree [sidx]
# poNorm$logitude_DD <- stn$Lon_decDegree [sidx]
#
# sidx <- match(poSorm$Match_Name, stn$Match_Name)
# poSorm$latitude_DD <- stn$Lat_decDegree [sidx]
# poSorm$logitude_DD <- stn$Lon_decDegree [sidx]

saveRDS(poAll, file="~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")

# save(poNorm, poSD, poSorm, poAno,
#   file = "~/tmp/LCI_noaa/cache/ctdwallAnomalies.RData")   # from CTDwallSetup.R




if(0) {  ## not ready yet! XXX

## plot normals
source("CTDsectionFcts.R")
dir.create(normDir, showWarnings=FALSE, recursive=TRUE)


poNorm$Transect <- factor(stn$Line [match(poNorm$Match_Name, stn$Match_Name)])
levels(poNorm$Transect) <- c(levels(poNorm$Transect), "ABext")

for(tn in 1:length(levels(poNorm$Transect))){
  tranN <- levels(poNorm$Transect)[tn]

  monthly <- TRUE

  ## doubly-used stations:
  stn$Line <- flexTransect(tranN, stn)  ## function from CTDsectionFcts.R
  transect <- stn$Line [match(poNorm$Match_Name, stn$Match_Name)]
  #  sect <- subset(stn, subse)
  ## get bathymetry


  # bottom <- getBathy(tranN, stn)
  bottom <- getBathy(subset(stn, Line==tranN))
  poNormT <- subset(poNorm, Transect==transect)


  poNorm$Transect <- factor(stn$Line [match(poNorm$Match_Name, stn$Match_Name)])
  for(tn in seq_along(levels(poNorm$Transect))) {
    ## doubly-used stations:
    stn$Line <- flexTransect(levels(poAll$Transect)[tn], stn)  ## function from CTDsectionFcts.R
    poNorm$Transect <- stn$Line [match(poAll$Match_Name, stn$Match_Name)]
    sect <- subset(stn, subse)
    ## get bathymetry
    require("sf")
    sect <- st_as_sf(sect, coords = c("loni", "lati"))
    sf::st_crs(sect) <- 4326  ## WGS84 definition
    require("stars")
    sectP <- sf::st_transform(sect, st_crs(bathyZ))
    bottomZ <- stars::st_extract(bathyZ, at = sectP)$w001001.adf


  ## build CTD object
  require("oce")

  pdf(paste0(normDir, levels(poNorm$Transect)[tn], ".pdf")
       , width=10, height=7.5)
  if(monthly){
    layoutM <- matrix(1:12, 1, byrow=TRUE)
    omText <- month.name
  }
  for(mo in nrow(layoutM)){
      ## construct oce-object
      require("oce")

    ## plotting code

    }
    dev.off()
  }
}
}

cat("#\n#end of CTDwall_normals.R\n\n")
# EOF
