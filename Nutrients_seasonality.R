
## graphics of nutrients for EVOS report
rm (list=ls())

## 1. link to physical oceanography data.

## -- seasonality
## -- add to signature data: time series?

nut <- read.csv ("~/GISdata/LCI/CookInletKachemakBay_Nutrients_2021.csv")

## cleanup
nut$Date <- as.Date(nut$Date)
# nut$Station <- ifelse (nut$Station == "9_6", "AlongBay_6", nut$Station)
nut$Station <- ifelse (nut$Station == "4_4", "AlongBay_3", nut$Station)
nut$Station <- factor (nut$Station)
nut <- nut [with (nut, order (Station, Date, Depth_m, Replicate)),]


## deal with 2 samples -- take lower sample
sampleID <- paste (nut$Station, nut$Date, nut$Depth_m)

## exploratory analysis
summary (nut$Station)
summary (nut$Depth_m)
summary (nut$Depth_m > 10)
summary (nut$Date)

dir.create("~/tmp/LCI_noaa/media/EVOS/", recursive=TRUE, showWarnings=FALSE)
pdf ("~/tmp/LCI_noaa/media/EVOS/depthHisto.pdf")
hist (subset (nut, Depth_m > 10) $Depth_m, main="Depth distribution of deep samples"
      , xlab="depth [m]")
dev.off()

summary (subset (nut$Station, nut$Depth_m > 10))
summary (subset (nut$Station, nut$Depth_m < 10))
## AlongBay_10, AlongBay_3, AlongBay_6 are the only stations with > 4 samples at surface




## seasonality of PO4, and NOx, and CHLa
require ("RColorBrewer")

stations <- levels (nut$Station)[summary (nut$Station) > 10][c(2,3,1)]
nutL <- names (nut)[c(5,7,9,11)]
nutE <- c (expression (NH[3]), expression (NO[x]), expression (PO[4]), "Si")

## add chlorophyll?
if (1){
  nutL <- c(nutL, names (nut)[13])
  nutE <- c (nutE, expression (Chl~a))
}

pdf ("~/tmp/LCI_noaa/media/EVOS/nutrientsSeason.pdf", width=8, height=5)
## one color per station (3)
## one panel per nutrient (4)
par (mfrow=c(2,2), mar=c(3, 4.5, 3, 1))

for (i in 1:length (nutL)){
  nut$x <- nut [,which (names (nut)==nutL[i])]

  # summary (with (nut, factor (paste (Station, Date, Depth_m))))
  nutA <- aggregate (x~Station+Date+Depth_m, nut, FUN=mean, na.rm=TRUE) ## use mean, min, max ?? XXX
  # nut <- aggregate (x~Station+Date+Depth_m, nut, FUN=max, na.rm=TRUE) ## use mean, min, max ?? XXX
  # nut <- aggregate (x~Station+Date+Depth_m, nut, FUN=min, na.rm=TRUE) ## use mean, min, max ?? XXX
  # nutA <- nut
  nutA <- nutA [with (nutA, order (Date)),]
  nutS <- subset (nutA, Depth_m < 10)
  nutD <- subset (nutA, Depth_m > 10)

  plot (x~Date, nut, type="n",main="", xlab="", ylab="", axes=FALSE)
  title (main=nutE[i], line=1)
  axis(2)
  axis (1, at=as.Date(paste0("2021-", 1:12, "-01")), labels=FALSE, tick=TRUE)
#  axis (1, at=as.Date(paste0("2021-", 1:12, "-15")), labels=1:12, tick=FALSE)
  axis (1, at=as.Date(paste0("2021-", 1:12, "-15")), labels=month.abb, tick=FALSE)
  box()
  if (i %% 2 == 1){
    title (ylab=expression (mg~l^-1))
}
  for (j in 1:length (stations)){
    lines (x~Date, nutS, subset=Station==stations [j], lwd=3, col=brewer.pal(length (stations), "Set2")[j])
    lines (x~Date, nutD, subset=Station==stations [j], lwd=3, col=brewer.pal(length (stations), "Set2")[j]
           , lty="dashed")
    if (i==1){
      legend ("topright", lwd=3, col=c(brewer.pal (length (stations), "Set2"), rep ("gray",2))
              , legend=c(stations, "surface", "near bottom")
              , lty=c(rep ("solid", length (stations)+1), "dashed")
              , seg.len=3
              , bty="n", ncol=2)
    }
  }
}
dev.off()



## QAQC
nut [which.max (nut$NH3_mg.L),]






##################################
##                              ##
##  SWMP Nutrient data analysis ##
##                              ##
##################################


## set-up parameters
source ("annualPlotFct.R") # already loads SWMPr
suppressPackageStartupMessages (Require ("R.utils"))

sF <- list.files("~/GISdata/LCI/SWMP/", pattern="*.zip", full.names=TRUE)
SWMPfile <- sF [which.max (file.info(sF)$ctime)]; rm (sF)

## load and process SWMP data
Require ("SWMPr")
gS <- function (station){
  sws <- getSWMP(station)
  sws$jday <- as.numeric (format (sws$datetimestamp, format="%j"))
  sws$month <- as.numeric (format (sws$datetimestamp, format="%m"))
  sws$year <- as.factor (format (sws$datetimestamp, format="%Y"))
  sws
}


Hd <- gS ("kachdnut") # Homer deep
Hs <- gS ("kachsnut") # Homer shallow
# Hh <- gS ("kachhnut") # Homer Harbor -- isco sample, surface at petro marine; 11 samples per 24 h -- automated
Sd <- gS ("kacsdnut") # Seldovia deep
Ss <- gS ("kacssnut") # Seldovia shallow

save.image ("~/tmp/LCI_noaa/cache/nutrients1.RData")
## rm (list=ls()); load ("~/tmp/LCI_noaa/cache/nutrients1.RData")

## time series
if (0){
plot (po4f~datetimestamp, Hd, type="l")
plot (nh4f~datetimestamp, Hd, type="l")
plot (no2f~datetimestamp, Hd, type="l")  ## skip -- too few
plot (no3f~datetimestamp, Hd, type="l")  ## skip -- too few
plot (no23f~datetimestamp, Hd, type="l")
plot (chla_n~datetimestamp, Hd, type="l")
}

## seasonality
nL <- c ("po4f", "nh4f", "no23f", "chla_n")
nusw <- as.data.frame (rbind (Hd, Hs, Sd, Ss))
nusw$station <- factor (c (rep ("Homer", nrow (Hd)+nrow (Hs))
                   , rep ("Seldovia", nrow (Sd)+nrow (Ss))))
nusw$depth <- factor (c (rep ("deep", nrow (Hd)), rep ("shallow", nrow (Hs))
                   , rep ("deep", nrow (Sd)), rep ("shallow", nrow (Ss))))

ndL <- list (Hd, Hs, Sd, Ss)
names (ndL) <- c ("Homer deep", "Homer shallow", "Seldovia deep", "Seldovia shallow")



pdf ("~/tmp/LCI_noaa/media/EVOS/nutSWMP_historgrams.pdf")
for (i in 1:length (nL)){
  nusw$var <- nusw [,which (names (nusw)==nL[i])]
  hist (nusw$var, main=nL[i])
  abline (v=mean (nusw$var, na.rm=TRUE))
  # abline (v=mean (nusw$var, na.rm=TRUE)+1*sd (nusw$var, na.rm=TRUE), lty="dashed")
  # abline (v=mean (nusw$var, na.rm=TRUE)+2*sd (nusw$var, na.rm=TRUE), lty="dashed", lwd=2)
  abline (v=mean (nusw$var, na.rm=TRUE)+3*sd (nusw$var, na.rm=TRUE), lty="dotted", lwd=3)
}
dev.off()


pdf ("~/tmp/LCI_noaa/media/EVOS/swmpNutrients.pdf")
par (mfrow=c(2,2))
for (j in 1:length (ndL)){
  nda <- ndL [[j]]
for (i in 1:length (nL)){
  nda$var <- nda [,which (names (nda)==nL[i])]
  plot (var~jday, nda, type="n", main="")
  title (main = paste (nL[i], names (ndL)[j]))
  for (k in 1:length (levels (nda$year))){
    yD <- subset (as.data.frame (nda), nda$year==levels (nda$year)[k])
    lines (var~jday, yD, col=k)
  #  lines (var~jday, subset (as.data.frame (nda), nda$year==levels (nda$year)[k]))
  }
}
}
dev.off()
rm (ndL)


## seasonal averages
pdf ("~/tmp/LCI_noaa/media/EVOS/swmpNutSeasonalAve.pdf")
par (mfrow = c(2,2))
for (i in 1:length (levels (nusw$station))){
  nda <- subset (nusw, station == levels (nusw$station)[i])
  for (j in 1:length (nL)){
    nda$var <- nda [,which (names (nda)==nL[j])]
    nSeason <- aggregate (var~month+depth, nda, mean, na.rm=TRUE)
    plot (var~month, nSeason, type="n", main="")
    title (main=paste (levels (nusw$station)[i], nL[j]))
    lines (var~month, subset (nSeason, depth=="deep"), lty = "dashed", lwd=2)
    lines (var~month, subset (nSeason, depth=="shallow"), lty = "solid", lwd=2)
    if (j == 1){
      legend ("top", lty = c("solid", "dashed"), legend=c("shallow", "deep")
              , bty="n", lwd=2)
    }
  }
}
dev.off()


nusw <- nusw [order (nusw$datetimestamp),]
## seasonal -- move Homer and Seldovia onto one plot
pdf ("~/tmp/LCI_noaa/media/EVOS/swmpNutSeasonal3.pdf"
     , width=8, height=6)
par (mfrow = c(2,2))
for (j in 1:length (nL)){
  nusw$var <- nusw [,which (names (nusw)==nL[j])]
  n1 <- nusw
  ## QC
  if (j==1){n1 <- subset (nusw, var < 0.05)
  }else if (j==2){n1 <- subset (nusw, var < 0.1)
  }else{n1 <- nusw}
  n1 <- aggregate (var~month+year+depth, data=n1, FUN=mean, na.rm=TRUE)
  plot (var~month, n1, type="n", main=nL[j], axes=FALSE, xlab="", ylab="")
  axis (1)
  axis (2)
  box()
  for (k in 1:length (levels (nusw$year))){
    n2 <- subset (n1, year==levels (nusw$year)[k])
    for (l in 1:length (levels (n2$depth))){
      n3 <- subset (n2, depth==levels (n2$depth)[l])
      for (m in 1:length (levels (n3$station))){
        lines (var~month, n3, lty=l, col=k)
      }
    }
  }
  if (j==1){
    legend ("top", lty=1:length (levels (n1$depth)), legend=levels (n2$depth), bty="n")
  }
}
dev.off()


if (1){
pdf ("~/tmp/LCI_noaa/media/EVOS/swmpNutSeasonalAve2.pdf"
     , width=8, height=6)
par (mfrow = c(2,2))
for (j in 1:length (nL)){
  nusw$var <- nusw [,which (names (nusw)==nL[j])]
  nSeason <- aggregate (var~month+depth+station, nusw, mean, na.rm=TRUE)

  plot (var~month, nSeason, type="n", main="", axes=FALSE
        , ylab="", xlab="")
  if (j %in% c(1,3)){
    title (ylab=expression (mg~l^-1), xlab="", line=2.5)
  }
  axis (1)
  axis (2)
  box()
  title (main=nL[j])
  for (k in 1:length (levels (nusw$station))){
    lines (var~month, subset (nSeason, depth=="deep" & station==levels (station)[k])
           , lty = "dashed", lwd=2, col=k)
    lines (var~month, subset (nSeason, depth=="shallow" & station==levels (station)[k])
           , lty = "solid", lwd=2, col=k)
  }
  # if (j == 1){
  #   legend ("top", lty = c("solid", "dashed"), legend=c("shallow", "deep")
  #           , bty="n", lwd=2)
  # }else if (j == 2){
  #   legend ("top", col=c(1,2), legend=levels (nusw$station), bty="n", lwd=2)
  # }
  if (j==1){
    legend ("top", lty=c("solid", "dashed", "solid", "solid"), lwd=2, bty="n"
            , legend=c(rev (levels (nusw$depth)), levels (nusw$station))
            , col=c(1,1,1,2)
            , ncol=2)
  }
}
dev.off()
}


## anomaly time series
