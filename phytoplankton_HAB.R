## frequency of Alexandrium and Pseudo-nitschia in phytoplankton samples
rm(list=ls())
## 9 years go look at quantitatively
if(0) {
phyt <- read.csv("~/GISdata/LCI/phytoplankton/phytoplankton.csv", skip = 1)
# phyt2 <- read.csv("~/tmp/LCI_noaa/data-products/phytoplankton.csv", skip = 1)

# phyt$ISOtime <- as.POSIXct(paste(phyt$Date, phyt$Time))
phyt$ISOtime <- as.Date(phyt$Date)
phyt$Station <- factor(phyt$Station)
phyt$year <- format(phyt$ISOtime, "%Y") |> as.numeric()
phyt$jday <- format(phyt$ISOtime, "%j") |> as.numeric()
phyt$Alex <- phyt$Alexandrium.spp.
phyt$PN <- phyt$Pseudo.nitzschia.spp.

cor (phyt$Alex, phyt$PN,   use = "pairwise.complete.obs")


hist(subset(phyt$jday, phyt$Alex > 0))
hist(subset(phyt$jday, phyt$PN > 0))



phyAlex <- aggregate(Alex~jday+Station, phyt, FUN = mean, na.rm=TRUE)
plot(phyAlex)



phyAlex <- aggregate (Alex ~ year + Station, phyt, FUN = mean, na.rm = TRUE)
plot (Alex~year, phyAlex)
summary (factor (subset (phyAlex, Alex > 0)$year))

phyPN <- aggregate (PN~year+Station, phyt, FUN = mean, na.rm = TRUE)
plot (PN~year, phyPN)
summary (factor (subset (phyPN, PN > 0)$year))




phyt [which.max(phyt$Alexandrium.spp.),1:8]
phyt [which.max(phyt$Pseudo.nitzschia.spp..),1:7]



t9 <- subset (phyt, phyt$Station=="9_6")

plot(log1p(Alexandrium.spp.)~ISOtime, t9)
plot(log1p(Pseudo.nitzschia.spp.)~ISOtime, t9)


phyt [which.max(t9$Alexandrium.spp.),1:8]
phyt [which.max(t9$Pseudo.nitzschia.spp..),1:7]



summary(phyt$Pseudo.nitzschia.spp.)
summary(phyt$Alexandrium.spp.)


}




#######################################
### KBNERR ordinate categorical data ##
#######################################


## 17 years
rm(list=ls())
dir.create("~/tmp/LCI_noaa/media/HAB/", recursive = TRUE, showWarnings = FALSE)

# t1 <- read.csv("~/GISdata/LCI/phytoplankton/KBNERR_tables/PN_Alex_forKBL_allgenera.csv")
kA <- read.csv("~/GISdata/LCI/phytoplankton/KBNERR_tables/PN_Alex_forKBL_PN-alexandrium.csv")
kd <- read.csv("~/GISdata/LCI/phytoplankton/KBNERR_tables/PN_Alex_forKBL_sampledates.csv")
ks <- read.csv("~/GISdata/LCI/phytoplankton/KBNERR_tables/PN_Alex_forKBL_sites.csv")

## merge them into one wide table
kA$sID <- with(kA, paste(Site.ID, SampleDate, SampleTime))
kd$sID <- with(kd, paste(Site.ID, SampleDate, SampleTime))
kd$SampleTime <- ifelse(kd$SampleTime=="", "00:00", kd$SampleTime)
ks$Longitude <- as.numeric(ks$Longitude)
ks$Latitude <- as.numeric(ks$Latitude)
kA$Species <- gsub(" spp.", "", kA$Species)  # to avoid problems in pivot
kA$Species <- gsub("-", "_", kA$Species)

## clean-up: find non-unique samples
kA <- kA[order(kA$sID, kA$Species, kA$Abundance),]

print(nrow(kA))
kA <- unique(kA)
print(nrow(kA))

dups <- which(duplicated(paste0(kA$sID, kA$Species)))
for (i in seq_along(dups)){
  print(kA[c(dups[i]-1, dups[i]),])
}

## simply add the all-sample data frame to this, then ditch the duplicates
kA <- rbind (kA,
  cbind(Species=rep("Pseudo_nitzschia", nrow(kd)),Abundance=rep("N", nrow(kd)), kd),
  cbind(Species=rep("Alexandrium"     , nrow(kd)),Abundance=rep("N", nrow(kd)), kd)
)
kA$Abundance <- factor (kA$Abundance, levels=rev(c("N", "P", "A", "B")), ordered = TRUE)

if(length(dups) > 0) {
  kA <- kA[!duplicated(paste0(kA$sID, kA$Species)),]
}


kphyto <- tidyr::pivot_wider(kA, names_from=Species, values_from=Abundance
  , id_cols=sID) |>
  as.data.frame()

kphyto$timestamp <- as.POSIXct(paste(kd$SampleDate, kd$SampleTime))[match(kphyto$sID, kd$sID)]


kphyto$Site.ID <- kd$Site.ID [match(kphyto$sID, kd$sID)]
kphyto$longitude <- ks$Longitude[match(kphyto$Site.ID, ks$Site.ID)]
kphyto$latitude <- ks$Latitude  [match(kphyto$Site.ID, ks$Site.ID)]

kphyto <- kphyto[!is.na(kphyto$Alexandrium),]
kphyto <- kphyto[!is.na(kphyto$Pseudo_nitzschia),]

## add year, and months
kphyto$year <- as.numeric(format(kphyto$timestamp, "%Y"))
kphyto$month <- as.numeric(format(kphyto$timestamp, "%m"))

## subset to Kachemak Bay and LCI
kphyto <- subset (kphyto, -154 < longitude & longitude < -150)



## ideas for analysis:
## annual percentage of samples with presence
## monthly percentage w presence
## RF analysis of raw data against
## - current SST
## - mean bottom temp in preceeding 30 days
## - mean SWMP temp in preceeding 30 days
## - daylight time / solar radiation in preceeding 30 days
## - mean salinity in preceeding 30 days

kphyto$P_pa <- ifelse (kphyto$Pseudo_nitzschia=="N", 0, 1)
kphyto$A_pa <- ifelse (kphyto$Alexandrium=="N", 0, 1)

write.csv(kphyto, file="~/tmp/LCI_noaa/data-products/KBNERR-HAB.csv", row.names = FALSE)



## exploratory analysis

siteP <- aggregate(P_pa ~ Site.ID, data=kphyto, FUN=mean)
siteP$A_pa <- aggregate(A_pa~Site.ID, data=kphyto, FUN=mean)$A_pa
siteP$N <- aggregate(P_pa~Site.ID, data=kphyto, FUN=length)$P_pa
siteP$lat <- kphyto$latitude [match(siteP$Site.ID, kphyto$Site.ID)]
siteP$lon <- kphyto$longitude [match(siteP$Site.ID, kphyto$Site.ID)]
siteP <- siteP[order(siteP$P_pa, siteP$N, decreasing=TRUE),]
cor(siteP$A_pa, siteP$P_pa)
# head(siteP)
write.csv(siteP, file="~/tmp/LCI_noaa/data-products/KBNERR-hab_sites.csv", row.names = FALSE)

x <- subset(siteP, N > 10)
head(x)



annualP <- aggregate (P_pa~year, kphyto, FUN=mean)
annualP$A_pa <- aggregate (A_pa~year, kphyto, FUN=mean)$A_pa



## barplot of Alexandrium/Pseudo-nitzschia frequencies
pdf("~/tmp/LCI_noaa/media/HAB/frequencyTS_HAB.pdf", height=8, width=7)
par(mfrow=c(2,1), mar=c(1,4,2,1), oma=c(2, .5, .5, .5))

spL <- c("Pseudo_nitzschia", "Alexandrium")
cols <-  rev(RColorBrewer::brewer.pal(3, "Set1"))
for(i in 2:1) {
  kp <- kphyto
  kp$spp <- kp[,which(names(kp)==spL[i])]
  kp$spp <- factor(kp$spp, levels = rev(levels(kp$Alexandrium)))
  aClass <- aggregate(spp~year, kp, FUN=summary)
  aClass <- data.frame(year=aClass$year, N_sample=rowSums(aClass[,2:ncol(aClass)]),
                       aClass[,2:ncol(aClass)])
  proTmp <- (aClass[,3:ncol(aClass)] / aClass$N_sample) |>
    as.data.frame() |>
    dplyr::select(-N)

  crds <- barplot(t(proTmp), col = cols,
    main = c("Pseudo-nitschia spp.", "Alexandrium spp.")[i],
    ylab="", ylim=list(c(0, 0.8), c(0,0.24))[[i]]
    )

  if(i==2){legend("topleft", legend=c("present", "abundant", "bloom")
                  , fill=cols, bty="n")
  } else{
    pIdx <- seq_along(aClass$year)
    pIdx <- pIdx[which(pIdx %% 2 == 0)]
    mtext(aClass$year[pIdx], at = crds[pIdx], side = 1, line = 0.5)
    mtext("Year", side=1, line = 1.5)
    mtext("Proportion of samples", side=2, outer=TRUE, line = -1.5)
    mtext("Data courtesy of KBNERR", side=1, cex=0.7, adj=1, line = 1.5)
  }
  box()
}
dev.off()
rm(kp, i, proTmp, spL, cols)



if(0){
  hist(summary(factor(kphyto$year)), main="Samples per year", xlab="")
  plot(summary(factor(kphyto$year))~levels(factor(kphyto$year)), type="l"
       , ylab="N samples", xlab="year", ylim=c(0,510))

  pdf("~/tmp/LCI_noaa/media/HAB/hab-timeseries.pdf", height=4, width=7)
  pcol <- c("brown", "red")
  plot(P_pa~year, annualP, type="b", col=pcol[1], lwd=3, ylim=c(0,max(annualP$P_pa)),
       ylab="Proportion of positive samples")
  points (A_pa~year, annualP, type="b", col=pcol[2], lwd=3, new=TRUE)
  legend("topleft", bty="n", legend=c("Pseudo-nitschia", "Alexandrium"), lwd=3, col=pcol)
  dev.off()

}




## stack temp months plots (2+)
## same x-lim as HAB plot



## match this with earliest date reaching threshold bottom temp

CTD <- read.csv("~/tmp/LCI_noaa/data-products/CTDcastSummaries.csv")
CTD$timeStamp <- as.POSIXct(CTD$timeStamp)
# SWMP <- read

## bottom time over 8 degrees C

pdf("~/tmp/LCI_noaa/media/HAB/TS_TempBottom_days.pdf", height=5, width=7)

stN <- c("AlongBay_7", "9_6", "AlongBay_5")
cols <- RColorBrewer::brewer.pal(length(stN), "Set2")

plot(TempBottom~Year, data = CTD, ylim=c(25,165), type="n",
     xlim=c(min(kphyto$year), max(kphyto$year)),
     xlab="Year", ylab="Time with bottom temperature > 8 °C [days]"
     , main="Temperature")
for(i in seq_along(stN)) {
  cStn <- subset(CTD, Match_Name==stN[i])
  # if(stN[i] == "AlongBay_7") {cStn <- subset(cStn, Year > 2016)}
  spl <- with(cStn, smooth.spline(x=timeStamp, y=TempBottom, cv = TRUE))
  ## lFit <- loess(TempBottom~timeStamp, data = cStn)
  nData <- seq(from=as.POSIXct("2012-02-01 12:00"), to=Sys.time(), by="1 day")
  splInt <- predict(spl, x = as.numeric(nData))
  sDF <- data.frame(timeStamp=as.POSIXct(splInt$x), TempBottom=splInt$y)
  sDF$Year <- as.numeric(format(sDF$timeStamp, "%Y"))
  rm(spl, nData, splInt)

  sY <- c(2016, 2012, 2016)[i]
  sDF <- subset(sDF, (sY < Year) & (Year < 2026))

  # plot (aggregate(TempBottom~Year, data = sDF, function(x){sum(x > 8)})
  #        , type = "b", col = i
  # )
  lines (aggregate(TempBottom~Year, data = sDF, function(x){sum(x > 8)})
         , type = "b", col = cols[i], lwd=3
         )
}
legend("topleft", legend=c("AlongBay-7", "T9-6", "AlongBay-5"),
       lwd=3, col=cols[1:i], bty="n")
dev.off()
rm(stN, cols, sDF, cStn)



##########################
## timing of temperature #
##########################

pdf("~/tmp/LCI_noaa/media/HAB/TS_TempTiming.pdf", height=5, width=7)

stN <- c("AlongBay_7", "9_6", "AlongBay_5")
cols <- RColorBrewer::brewer.pal(length(stN), "Set2")
yR <- c(175, 247)

plot(TempBottom~Year, data = CTD, ylim=yR, type="n",
     xlim=c(min(kphyto$year), max(kphyto$year)),
     xlab="Year", ylab="First day with bottom temperature > 8 °C",
     main="Timing of warming", axes = FALSE)
axis(1)
fD <- 2
axis(2, at=pretty(yR)+fD,
  labels = format(seq(as.POSIXct("2026-01-01 12:00"), as.POSIXct("2026-12-31 12:00"),
    by="1 day"), "%d %b")[pretty(yR)+fD])
box()
for(i in seq_along(stN)) {
  cStn <- subset(CTD, Match_Name==stN[i])
  ## fit spline for interpolating threshold day
  spl <- with(cStn, smooth.spline(x=timeStamp, y=TempBottom, cv = TRUE))
  ## lFit <- loess(TempBottom~timeStamp, data = cStn)
  nData <- seq(from=as.POSIXct("2012-02-01 12:00"), to=Sys.time(), by="1 day")
  splInt <- predict(spl, x = as.numeric(nData))
  sDF <- data.frame(timeStamp=as.POSIXct(splInt$x), TempBottom=splInt$y)
  sDF$Year <- as.numeric(format(sDF$timeStamp, "%Y"))
  rm(spl, nData, splInt)

  sY <- c(2016, 2012, 2016)[i]
  sDF <- subset(sDF, (sY < Year) & (Year < 2026))

  # plot (aggregate(TempBottom~Year, data = sDF, function(x){min(which(x > 8))})
  #        , type = "b", col = i
  # )
  lines (aggregate(TempBottom~Year, data = sDF, function(x){min(which(x > 8))})
         , type = "b", col = cols[i], lwd=3
  )
}
legend("topleft", legend=c("AlongBay-7", "T9-6", "AlongBay-5"),
  lwd=3, col=cols[1:i], bty="n")
dev.off()
rm(cols, sDF, cStn)


## predictive variables to extract/calculate from t96:
# dT8:  first bottom temp of 8 per year
# mSST: mean SST in preceeding N days
# mBotTemp: mean bottom temp in preceeding N days
# mSSSal: mean salinity in preceeding N days
# mBotSal: mean salinity in preceeding N days
# mBVF: mean of bvfMean in preceeding N days
# mChl: mean Chlorophyl conc in preceeding N days
# mPCL: mean depth of pycnocline in preceeding N days
# mStab: mean stability

if(0) {## not yet

ctdEx <- function(Match_Name = "9_6", sdate, thrT=8, Nd=40) {
  tPr <- subset(CTD, CTD$Match_Name==Match_Name)

  yD <- subset (tPr, (tPr$Year == as.numeric(format(sdate, "%Y"))) &
                  (TempBottom > thrT)
  )
  if(nrow(yD) < 1) {
    dT8 <- 13
  } else {
    dT8 <- min(yD$month, na.rm=TRUE)
  }


  rm(yD)
  dS <- subset (tPr, as.numeric(difftime(sdate, tPr$timeStamp, units="days")) <= Nd)
  mSST <- mean(dS$SST, na.rm=TRUE)
  mBotTemp <- mean(dS$TempBottom, na.rm=TRUE)
  mSSSal <- mean(dS$SalSurface, na.rm=TRUE)
  mBotSal <- mean(dS$SalBottom, na.rm=TRUE)
  mBVF <- mean(dS$bvfMean, na.rm=TRUE)
  mBVFmax <- mean(dS$bvfMax, na.rm=TRUE)
  mChl <- mean(dS$Chlorophyll, na.rm=TRUE)
  mPCL <- mean(dS$pclDepth, na.rm=TRUE)
  mStab <- mean(dS$stability, na.rm=TRUE)
  mO2 <- mean(dS$O2perc, na.rm=TRUE)

  out <- data.frame(dT8, mSST, mBotTemp, mSSSal, mBotSal, mBVF, mBVFmax, mChl, mPCL, mStab, mO2)
  out
}

if(0) {
  n_cores <- parallel::detectCores()
  clust <- parallel::makeCluster(n_cores, type="PSOCK")
  parallel::clusterExport(cl=clust, varlist=c("kphyto", "ctdEx", "CTD"))
  pO <- parallel::parLapply(cl=clust, X = seq_len(nrow(kphyto)), fun = function(i) {
    ctdEx(sdate=kphyto$timestamp[i])
  })
  parallel::stopCluster(clust); rm(clust, n_cores)
} else {
  pO <- lapply(seq_len(nrow(kphyto)), FUN=function(i) {ctdEx(sdate=kphyto$timestamp[i])})
}
kDF <- cbind(kphyto, do.call(rbind, pO)); rm(pO)


# x <- data.frame(kDF$Alexandrium)
# y <- kDF[,which(names(kDF)=="dT8"):ncol(kDF)]
# rf <- Rborist::Rborist(x,y)

rADF <- kDF[,c(which(names(kDF)=="Alexandrium"), which(names(kDF)=="dT8"):ncol(kDF))]
rf <- randomForest::randomForest(Alexandrium~., data=rADF)
print(rf)
randomForest::varImpPlot(rf)

# rf <- randomForestSRC::

}


